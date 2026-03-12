/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/
package xiangshan.mem

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles.DynInst
import xiangshan.mem.Bundles._
import xiangshan.cache._

class LoadQueueRAR(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(Valid(new Redirect))

    // Allocation from dispatch
    val alloc = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
    val doAlloc = Input(Bool())

    // Completion
    // Last one is for tl_d refill
    val complete = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO))

    // release cacheline
    val release = Flipped(Valid(new Release))

    // from VirtualLoadQueue
    val ldWbPtr = Input(new LqPtr)

    // global
    /**
     * At most one rollback per cycle, because at most one release per cycle
     */
    val rollback = Output(Valid(new Redirect))

    // Check ptr for current probe
    val check = Valid(new ReleaseCheck)

    // CSR controls (for TSO activation)
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
  })

  // FIXME(meow):
  // This module may have two problems
  // 1. nc is not correctly handled. NC should also induce a immediate release at the same cycle, but we don't have
  //   an extra CAM to detect same-address loads
  // 2. There might be an extra racing condition if the metadata is read at LDU s0, returned at LDU s1
  //  but release come in at LDU s1, so latestRelease is at LDU s2.
  //  Whether this require us to do anything depends on how DCache handles this release. Is it possible?
  //  - If the release is handled as LATER THAN the load, and the release takes at least two cycle so no loads can enter
  //    DCache the exact next cycle, then we may be able to just bump the latestRelease delay
  //  - Otherwise, headaches

  private val PartialPAddrStride: Int = 6
  require(PartialPAddrStride >= log2Up(blockBytes))
  private val PartialPAddrBits: Int = 16
  private val PartialPAddrLowBits: Int = (PartialPAddrBits - PartialPAddrStride) / 2 // avoid overlap
  private val PartialPAddrHighBits: Int = PartialPAddrBits - PartialPAddrLowBits
  private def boundary(x: Int, h: Int) = if (x < h) Some(x) else None
  private def lowMapping = (0 until PartialPAddrLowBits).map(i => Seq(
      boundary(PartialPAddrStride + i  , PartialPAddrBits),
      boundary(PartialPAddrBits - i - 1, PartialPAddrBits)
    )
  )
  private def highMapping = (0 until PartialPAddrHighBits).map(i => Seq(
      boundary(i + PartialPAddrStride     , PAddrBits),
      boundary(i + PartialPAddrStride + 11, PAddrBits),
      boundary(i + PartialPAddrStride + 22, PAddrBits),
      boundary(i + PartialPAddrStride + 33, PAddrBits)
    )
  )
  private def genPartialPAddr(paddr: UInt) = {
    val ppaddr_low = Wire(Vec(PartialPAddrLowBits, Bool()))
    ppaddr_low.zip(lowMapping).foreach {
      case (bit, mapping) =>
        bit := mapping.filter(_.isDefined).map(x => paddr(x.get)).reduce(_^_)
    }

    val ppaddr_high = Wire(Vec(PartialPAddrHighBits, Bool()))
    ppaddr_high.zip(highMapping).foreach {
      case (bit, mapping) =>
        bit := mapping.filter(_.isDefined).map(x => paddr(x.get)).reduce(_^_)
    }
    Cat(ppaddr_high.asUInt, ppaddr_low.asUInt)
  }

  println("LoadQueueRAR: size: " + LoadQueueRARSize)
  //  LoadQueueRAR field
  //  +-------+-------+-------+----------+
  //  | Valid |  Uop  | PAddr | Released |
  //  +-------+-------+-------+----------+
  //
  //  Field descriptions:
  //  Allocated   : entry is valid.
  //  MicroOp     : Micro-op
  //  PAddr       : physical address.
  //  Released    : DCache released.
  val tsoActive = RegNext(io.csrCtrl.tso_enable, true.B)
  val allocated = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val pending = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))
  val meow_debug_pending = MarkDebug(WireDefault(pending.asUInt))
  val meow_debug_allocated = MarkDebug(WireDefault(allocated.asUInt))
  val uop = Reg(Vec(LoadQueueRARSize, new DynInst))
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PartialPAddrBits.W),
    numEntries = LoadQueueRARSize,
    numRead = 0,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 1,
    enableCacheLineCheck = false, // Now `RARQueue` has no need to check cacheline.
    paddrOffset = 0 // If you need to check cacheline, set the offset relative to the original paddr correctly.
  ))
  paddrModule.io := DontCare

  require(VirtualLoadQueueSize == LoadQueueRARSize)

  // Real-allocation: load_s2
  // <del>PAddr write needs 2 cycles, release signal should delay 1 cycle so that</del>
  // This is now purely for timing optimization
  // load enqueue can catch release.
  var latestRelease = Pipe(io.release, 1)

  /*
  val release1Cycle = io.release
  // val release2Cycle = RegNext(io.release)
  // val release2Cycle_dup_lsu = RegNext(io.release)
  val release2Cycle = RegEnable(io.release, io.release.valid)
  release2Cycle.valid := RegNext(io.release.valid)
  //val release2Cycle_dup_lsu = RegEnable(io.release, io.release.valid)
  */

  // Deallocation
  // when the loads that "older than" current load were writebacked,
  // current load will be released.
  val deallocating = for (i <- 0 until LoadQueueRARSize) yield {
    val deqNotBlock = isBefore(uop(i).lqIdx, io.ldWbPtr)
    val needFlush = uop(i).robIdx.needFlush(io.redirect)
    deqNotBlock || needFlush
  }

  val allocations = for ((alloc, idx) <- io.alloc.zipWithIndex) yield {
    // alloc.valid -> !allocated(alloc.bits.value)
    // because if that lqptr is present in a uop that's being dispatched,
    // the previous owner of that lqptr has to be committed
    assert(!(alloc.valid && io.doAlloc) || !allocated(alloc.bits.lqIdx.value) || !deallocating(idx))
    val canceled = alloc.bits.robIdx.needFlush(io.redirect)
    val lower = alloc.bits.lqIdx
    val upper = alloc.bits.lqIdx + alloc.bits.numLsElem
    (
      io.doAlloc && alloc.valid && !canceled,
      alloc.bits,
      (lower.flag =/= upper.flag, lower, upper),
    )
  }

  // Allocation
  for (i <- 0 until LoadQueueRARSize) {
    val allocationHit = allocations.map({ case (doAlloc, _, (wrapped, lower, upper)) => (
      doAlloc && Mux(
        wrapped,
        lower.value <= i.U || i.U < upper.value,
        lower.value <= i.U && i.U < upper.value,
      )
    )})
    allocated(i) := allocated(i) && !deallocating(i) || ParallelOR(allocationHit)
    pending(i) := pending(i) || ParallelOR(allocationHit)
    uop(i) := MuxCase(uop(i), allocationHit.zip(allocations.map({ case (_, uop, _) => uop })))
  }

  var sameCycleCompletion = WireDefault(0.U(LoadQueueRARSize.W))

  // Completions
  for ((c, w) <- io.complete.zipWithIndex) {
    // Allocate ready
    c.req.ready := true.B

    // Write CAM regardless of data_valid
    val idx = c.req.bits.uop.lqIdx.value
    paddrModule.io.waddr(w) := idx
    paddrModule.io.wdata(w) := genPartialPAddr(c.req.bits.paddr)
    paddrModule.io.wen(w) := c.req.valid
    sameCycleCompletion = sameCycleCompletion | (Mux(c.req.valid, 1.U(LoadQueueRARSize.W), 0.U(LoadQueueRARSize.W)) << idx).take(LoadQueueRARSize)

    // Completion
    when(RegNext(c.req.valid && c.req.bits.data_valid) && !c.revoke) {
      pending(RegNext(idx)) := false.B
    }

    // Assert: already allocated
    assert(!c.req.valid || allocated(idx))

    // NC is uncachable and will not be explicitly released.
    // So NC requests are not allowed to have RAR
    // FIXME: uncache gets implicitly released immediately
    // Find any older susceptible loads
    /*
    val justReleased = enq.bits.is_nc || (
      enq.bits.data_valid && ParallelOR(releases.map(
        r => r.valid && enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === r.bits.paddr(PAddrBits-1, DCacheLineOffset)
      ))
    )
    */
  }

  val meow_debug_sameCycleCompletion = MarkDebug(WireDefault(sameCycleCompletion))

  // LoadQueueRAR Query
  // Load-to-Load violation check condition:
  // 1. Physical address match by CAM port.
  // 2. release or nc_with_data is set.
  // 3. Younger than current load instruction.

  val releaseQuery = genPartialPAddr(latestRelease.bits.paddr)
  paddrModule.io.releaseViolationMdata(0) := releaseQuery
  val releasedRaw = paddrModule.io.releaseViolationMmask(0).asUInt
  // Because paddrModule has one cycle latency, also check recent writes
  val recentWrittenMasks = for((en, addr, data) <- paddrModule.io.wen.lazyZip(paddrModule.io.waddr).lazyZip(paddrModule.io.wdata)) yield {
    val writeMask = Fill(releasedRaw.getWidth, en) & (1.U(1.W) << addr).take(releasedRaw.getWidth)
    val dataHit = RegNext(data) === releaseQuery
    val hitMask = Fill(releasedRaw.getWidth, dataHit)
    require(writeMask.getWidth == releasedRaw.getWidth)
    require(hitMask.getWidth == releasedRaw.getWidth)
    hitMask & RegNext(writeMask)
  }
  val recentWrittenMerged = recentWrittenMasks.reduce(_ | _)
  val released = (releasedRaw | recentWrittenMerged).take(releasedRaw.getWidth)

  // What to check against?
  val check = dontTouch(allocated.asUInt & Mux(tsoActive, pending.asUInt, released & pending.asUInt))

  // Find oldest check
  // Use last cycle value of ldWbPtr, because deallocating takes one cycle
  // In theory, all pending bits should be unset during deallocation
  // But currently uncached requests are handled differently
  val lastIdx = RegNext(io.ldWbPtr.value)
  val checkPresent = check.orR && latestRelease.valid
  def findOldest(mask: UInt): UInt = {
    val backMask = dontTouch(((-1).S(mask.getWidth.W).asUInt << lastIdx).take(mask.getWidth))
    require(backMask.getWidth == mask.getWidth)
    assert(PopCount(backMask) + lastIdx === (mask.getWidth).U)
    val front = PriorityEncoderOH(mask)
    val back = PriorityEncoderOH(mask & backMask)
    Mux(back.orR, back, front)
  }
  val oldestCheck = dontTouch(findOldest(check))
  val checkRobIdx = Mux1H(oldestCheck, uop.map(_.robIdx))
  // Sanity check
  for ((c, i) <- check.asBools.zipWithIndex) {
    assert(!c || uop(i).robIdx >= checkRobIdx)
  }

  io.check.bits.older := checkRobIdx
  io.check.bits.released := latestRelease.bits.paddr
  io.check.valid := checkPresent

  // Find oldest victim
  val victim = (
    allocated.asUInt
    & ((~pending.asUInt).asUInt | sameCycleCompletion)
    & released
    & (VecInit(uop.map(u => isAfter(u.robIdx, checkRobIdx))).asUInt)
  )
  val victimPresent = victim.orR
  val oldestVictim = dontTouch(findOldest(victim))
  val selectedVictim = Mux1H(oldestVictim, uop)
  // Sanity check
  for ((v, i) <- victim.asBools.zipWithIndex) {
    assert(!v || uop(i).robIdx >= selectedVictim.robIdx)
  }

  io.rollback.valid := checkPresent && victimPresent
  io.rollback.bits := DontCare
  io.rollback.bits.isRVC       := selectedVictim.preDecodeInfo.isRVC
  io.rollback.bits.robIdx      := selectedVictim.robIdx
  io.rollback.bits.ftqIdx      := selectedVictim.ftqPtr
  io.rollback.bits.ftqOffset   := selectedVictim.ftqOffset
  io.rollback.bits.level       := RedirectLevel.flush
  io.rollback.bits.cfiUpdate.target := selectedVictim.pc
  io.rollback.bits.debug_runahead_checkpoint_id := selectedVictim.debugInfo.runahead_checkpoint_id

  // perf cnt
  /*
  val canEnqCount = PopCount(io.query.map(_.req.fire))
  val validCount = freeList.io.validCount
  val allowEnqueue = validCount <= (LoadQueueRARSize - LoadPipelineWidth).U
  val ldLdViolationCount = PopCount(io.query.map(_.resp).map(resp => resp.valid && resp.bits.rep_frm_fetch))

  QueuePerf(LoadQueueRARSize, validCount, !allowEnqueue)
  XSPerfAccumulate("enq", canEnqCount)
  XSPerfAccumulate("ld_ld_violation", ldLdViolationCount)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", canEnqCount),
    ("ld_ld_violation", ldLdViolationCount)
  )
  */
  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()
  // End
}
