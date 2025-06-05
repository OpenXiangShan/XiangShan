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

    // violation query
    val query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO))

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
  })

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
  val tsoActive = RegInit(true.B)
  val allocated = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val pending = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))
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
  // PAddr write needs 2 cycles, release signal should delay 1 cycle so that
  // load enqueue can catch release.
  val ReleaseLaniency = 1
  var latestRelease = io.release
  val releases = Seq(io.release) ++ (
    for(i <- 0 until ReleaseLaniency) yield {
      val newRelease = RegEnable(latestRelease, latestRelease.valid)
      newRelease.valid := RegNext(latestRelease.valid)
      latestRelease = newRelease
      newRelease
    }
  )
  /*
  val release1Cycle = io.release
  // val release2Cycle = RegNext(io.release)
  // val release2Cycle_dup_lsu = RegNext(io.release)
  val release2Cycle = RegEnable(io.release, io.release.valid)
  release2Cycle.valid := RegNext(io.release.valid)
  //val release2Cycle_dup_lsu = RegEnable(io.release, io.release.valid)
  */

  // LoadQueueRAR enqueue condition:
  // There are still not completed load instructions before the current load instruction.
  // (e.g. "not completed" means that load instruction get the data or exception).
  val canEnqueue = io.query.map(_.req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val hasNotWritebackedLoad = io.query.map(_.req.bits.uop.lqIdx).map(lqIdx => isAfter(lqIdx, io.ldWbPtr))
  val needEnqueue = canEnqueue.zip(hasNotWritebackedLoad).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }

  // Allocate logic
  val acceptedVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadQueueRARSize).W)))

  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    //  Allocate ready
    val offset = PopCount(needEnqueue.take(w))
    enq.ready := true.B

    val enqIdx = enq.bits.uop.lqIdx.value
    enqIndexVec(w) := enqIdx
    paddrModule.io.waddr(w) := enqIdx
    paddrModule.io.wdata(w) := genPartialPAddr(enq.bits.paddr)
    val doEnq = needEnqueue(w) && enq.ready

    // Commit
    acceptedVec(w) := doEnq
    paddrModule.io.wen(w) := doEnq

    when (doEnq) {
      //  Allocate new entry
      allocated(enqIdx) := true.B
      pending(enqIdx) := !enq.bits.data_valid
      //  Fill info
      uop(enqIdx) := enq.bits.uop
    }

    // NC is uncachable and will not be explicitly released.
    // So NC requests are not allowed to have RAR
    // FIXME
    val justReleased = enq.bits.is_nc || (
      enq.bits.data_valid && ParallelOR(releases.map(
        r => r.valid && enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === r.bits.paddr(PAddrBits-1, DCacheLineOffset)
      ))
    )

    val debug_robIdx = enq.bits.uop.robIdx.asUInt

    XSError(
      needEnqueue(w) && enq.ready && allocated(enqIdx),
      p"LoadQueueRAR: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")
  }

  // when the loads that "older than" current load were writebacked,
  // current load will be released.
  for (i <- 0 until LoadQueueRARSize) {
    val deqNotBlock = !isBefore(io.ldWbPtr, uop(i).lqIdx)
    val needFlush = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needFlush)) {
      allocated(i) := false.B
    }
  }

  // if need replay revoke entry
  val lastCanAccept = GatedRegNext(acceptedVec)
  val lastAllocIndex = GatedRegNext(enqIndexVec)

  for ((revoke, w) <- io.query.map(_.revoke).zipWithIndex) {
    val revokeValid = revoke && lastCanAccept(w)
    val revokeIndex = lastAllocIndex(w)

    when (allocated(revokeIndex) && revokeValid) {
      allocated(revokeIndex) := false.B
    }
  }

  // LoadQueueRAR Query
  // Load-to-Load violation check condition:
  // 1. Physical address match by CAM port.
  // 2. release or nc_with_data is set.
  // 3. Younger than current load instruction.

  paddrModule.io.releaseViolationMdata(0) := genPartialPAddr(latestRelease.bits.paddr)
  val released = paddrModule.io.releaseViolationMmask(0)
  // What to check?
  val check = allocated.asUInt & Mux(tsoActive, pending.asUInt, released.asUInt & pending.asUInt)
  // FIXME: unset pending before committing, can save a lot of flushing

  // Find oldest check
  val lastIdx = io.ldWbPtr.value.tail(1)
  val checkPresent = check.orR && latestRelease.valid
  def findOldest(mask: UInt): UInt = {
    val front = PriorityEncoderOH(mask)
    val back = PriorityEncoderOH(mask & (-(lastIdx << 1).asUInt).tail(1))
    Mux(back.orR, back, front)
  }
  val checkRobIdx = Mux1H(findOldest(check), uop.map(_.robIdx))
  io.check.bits.older := checkRobIdx
  io.check.bits.released := latestRelease.bits.paddr
  io.check.valid := checkPresent

  // Find oldest victim
  val victim = allocated.asUInt & (~pending.asUInt).asUInt & released.asUInt & (VecInit(uop.map(u => isAfter(u.robIdx, checkRobIdx))).asUInt)
  val victimPresent = victim.orR
  val selectedVictim = Mux1H(findOldest(check), uop)

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
