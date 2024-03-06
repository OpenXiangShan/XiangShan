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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.cache._
import utils._
import utility._

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
    val lqFull = Output(Bool())
  })

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
  //
  val allocated = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(LoadQueueRARSize, new MicroOp))
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PAddrBits.W),
    numEntries = LoadQueueRARSize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = LoadPipelineWidth
  ))
  paddrModule.io := DontCare
  val released = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))
  val bypassPAddr = Reg(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadQueueRARSize,
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = true,
    moduleName = "LoadQueueRAR freelist"
  ))
  freeList.io := DontCare

  // Real-allocation: load_s2
  // PAddr write needs 2 cycles, release signal should delay 1 cycle so that
  // load enqueue can catch release.
  val release1Cycle = io.release
  val release2Cycle = RegNext(io.release)
  val release2Cycle_dup_lsu = RegNext(io.release)

  // LoadQueueRAR enqueue condition:
  // There are still not completed load instructions before the current load instruction.
  // (e.g. "not completed" means that load instruction get the data or
  // exception).
  // pre-Allocate logic
  val s0_preReqs = io.query.map(_.s1_prealloc)
  val s0_hasNotWritebackedLoad = io.query.map(x => isAfter(x.s1_lqIdx, io.ldWbPtr))
  val s0_preEnqs = s0_preReqs.zip(s0_hasNotWritebackedLoad).map { case (v, r) => v && r }
  val s0_canAccepts = Wire(Vec(LoadPipelineWidth, Bool()))
  val s0_enqIdxs = Wire(Vec(LoadPipelineWidth, UInt()))

  for ((req, w) <- io.query.zipWithIndex) {
    freeList.io.allocateReq(w) := true.B

    s0_canAccepts(w) := freeList.io.canAllocate(w)
    s0_enqIdxs(w) := freeList.io.allocateSlot(w)
    req.s1_nack := Mux(s0_hasNotWritebackedLoad(w), !s0_canAccepts(w), false.B)
  }

  // Allocate logic
  val s1_canEnqs = io.query.map(_.s2_alloc)
  val s1_hasNotWritebackedLoad = RegNext(VecInit(s0_preReqs.zip(s0_hasNotWritebackedLoad).map(x => x._1 && x._2)))
  val s1_cancel = io.query.map(x => {
    val x_next = RegNext(x.s1_robIdx)
    x_next.needFlush(RegNext(io.redirect)) || x_next.needFlush(io.redirect)
  })
  val s1_needEnqs = s1_canEnqs.zip(s1_hasNotWritebackedLoad.zip(s1_cancel)).map { case (v, x) => v && x._1 && !x._2 }
  val s1_canAccepts = RegNext(s0_canAccepts)
  val s1_enqIdxs = RegNext(s0_enqIdxs)
  val s1_accepts = Wire(Vec(LoadPipelineWidth, Bool()))
  val s1_offset = Wire(Vec(LoadPipelineWidth, UInt()))

  for ((enq, w) <- io.query.zipWithIndex) {
    s1_accepts(w) := false.B
    paddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    //  Allocate ready
    val offset = PopCount(s1_needEnqs.take(w))
    val canAccept = s1_canAccepts(offset)
    val enqIndex = s1_enqIdxs(offset)
    s1_offset(w) := offset

    when (s1_needEnqs(w) && canAccept) {
      s1_accepts(w) := true.B

      val debug_robIdx = enq.s2_uop.robIdx.asUInt
      XSError(allocated(enqIndex), p"LoadQueueRAR: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")

      freeList.io.doAllocate(w) := true.B

      //  Allocate new entry
      allocated(enqIndex) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := enqIndex
      paddrModule.io.wdata(w) := enq.s2_paddr
      bypassPAddr(w) := enq.s2_paddr

      //  Fill info
      uop(enqIndex) := enq.s2_uop
      released(enqIndex) :=
        !enq.s2_dataInvalid &&
        (release2Cycle.valid &&
        enq.s2_paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset) ||
        release1Cycle.valid &&
        enq.s2_paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
    }
  }

  //  LoadQueueRAR deallocate
  val freeMaskVec = Wire(Vec(LoadQueueRARSize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // when the loads that "older than" current load were writebacked,
  // current load will be released.
  for (i <- 0 until LoadQueueRARSize) {
    val deqNotBlock = !isBefore(io.ldWbPtr, uop(i).lqIdx)
    val needFlush = uop(i).robIdx.needFlush(io.redirect) ||
                    uop(i).robIdx.needFlush(RegNext(io.redirect))

    when (allocated(i) && (deqNotBlock || needFlush)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
    }
  }

  // if need replay revoke entry
  val s2_accepts = RegNext(s1_accepts)
  val s2_enqIdxs = RegNext(VecInit(s1_offset.map(x => s1_enqIdxs(x))))
  for ((revoke, w) <- io.query.map(_.s3_revoke).zipWithIndex) {
    val s2_accept = s2_accepts(w)
    val s2_enqIdx = s2_enqIdxs(w)
    val revokeValid = revoke && s2_accept
    val revokeIndex = s2_enqIdx

    when (allocated(revokeIndex) && revokeValid) {
      allocated(revokeIndex) := false.B
      freeMaskVec(revokeIndex) := true.B
    }
  }

  freeList.io.free := freeMaskVec.asUInt

  // LoadQueueRAR Query
  // Load-to-Load violation check condition:
  // 1. Physical address match by CAM port.
  // 2. release is set.
  // 3. Younger than current load instruction.
  val ldLdViolation = Wire(Vec(LoadPipelineWidth, Bool()))
  val allocatedUInt = RegNext(allocated.asUInt)
  for ((query, w) <- io.query.zipWithIndex) {
    ldLdViolation(w) := false.B
    paddrModule.io.releaseViolationMdata(w) := query.s2_paddr

    // Generate real violation mask
    val robIdxMask = VecInit(uop.map(_.robIdx).map(isAfter(_, query.s2_uop.robIdx)))
    val matchMask = (0 until LoadQueueRARSize).map(i => {
                      RegNext(allocated(i) &
                      paddrModule.io.releaseViolationMmask(w)(i) &
                      robIdxMask(i) &&
                      released(i))
                    })
    //  Load-to-Load violation check result
    val ldLdViolationMask = VecInit(matchMask)
    ldLdViolationMask.suggestName("ldLdViolationMask_" + w)
    query.s3_nuke := ParallelORR(ldLdViolationMask)
  }


  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle
  val releaseVioMask = Reg(Vec(LoadQueueRARSize, Bool()))
  when (release1Cycle.valid) {
    paddrModule.io.releaseMdata.takeRight(1)(0) := release1Cycle.bits.paddr
  }

  val lastAllocIndexOH = s2_enqIdxs.map(UIntToOH(_))
  val lastReleasePAddrMatch = VecInit((0 until LoadPipelineWidth).map(i => {
    (bypassPAddr(i)(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
  }))
  (0 until LoadQueueRARSize).map(i => {
    val bypassMatch = VecInit((0 until LoadPipelineWidth).map(j => s2_accepts(j) && lastAllocIndexOH(j)(i) && lastReleasePAddrMatch(j))).asUInt.orR
    when (RegNext((paddrModule.io.releaseMmask.takeRight(1)(0)(i) || bypassMatch) && allocated(i) && release1Cycle.valid)) {
      // Note: if a load has missed in dcache and is waiting for refill in load queue,
      // its released flag still needs to be set as true if addr matches.
      released(i) := true.B
    }
  })

  io.lqFull := freeList.io.empty

  // perf cnt
  val canEnqCount = PopCount(s1_accepts)
  val validCount = freeList.io.validCount
  val allowEnqueue = validCount <= (LoadQueueRARSize - LoadPipelineWidth).U
  val ldLdViolationCount = PopCount(io.query.map(resp => RegNext(resp.s2_alloc) && resp.s3_nuke))

  QueuePerf(LoadQueueRARSize, validCount, !allowEnqueue)
  XSPerfAccumulate("enq", canEnqCount)
  XSPerfAccumulate("ld_ld_violation", ldLdViolationCount)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", canEnqCount),
    ("ld_ld_violation", ldLdViolationCount)
  )
  generatePerfEvent()
  // End
}