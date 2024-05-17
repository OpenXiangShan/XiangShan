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
import xiangshan.backend.Bundles.DynInst

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
  // class STD_CLKGT_func extends BlackBox with HasBlackBoxResource {
  //  val io = IO(new Bundle {
  //   val TE = Input(Bool())
  //   val E  = Input(Bool())
  //   val CK = Input(Clock())
  //   val Q  = Output(Clock())
  // })

  //  addResource("/STD_CLKGT_func.v")
  // }

  val allocated = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(LoadQueueRARSize, new DynInst))
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
  // val release2Cycle = RegNext(io.release)
  // val release2Cycle_dup_lsu = RegNext(io.release)
  val release2Cycle = RegEnable(io.release, io.release.valid)
  val release2Cycle_dup_lsu = RegEnable(io.release, io.release.valid)

  // LoadQueueRAR enqueue condition:
  // There are still not completed load instructions before the current load instruction.
  // (e.g. "not completed" means that load instruction get the data or exception).
  val canEnqueue = io.query.map(_.req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val hasNotWritebackedLoad = io.query.map(_.req.bits.uop.lqIdx).map(lqIdx => isAfter(lqIdx, io.ldWbPtr))
  val needEnqueue = canEnqueue.zip(hasNotWritebackedLoad).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }
  
  // Allocate logic
  val acceptedVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt()))
  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    acceptedVec(w) := false.B
    paddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocate ready
    val offset = PopCount(needEnqueue.take(w))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex = freeList.io.allocateSlot(offset)
    enq.ready := Mux(needEnqueue(w), canAccept, true.B)
    enqIndexVec(w) := enqIndex
    when (needEnqueue(w) && enq.ready) {
      acceptedVec(w) := true.B

      val debug_robIdx = enq.bits.uop.robIdx.asUInt
      XSError(allocated(enqIndex), p"LoadQueueRAR: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")

      freeList.io.doAllocate(w) := true.B
     
      //  Allocate new entry
      allocated(enqIndex) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := enqIndex
      paddrModule.io.wdata(w) := enq.bits.paddr
      bypassPAddr(w) := enq.bits.paddr

      //  Fill info
      uop(enqIndex) := enq.bits.uop
      released(enqIndex) :=
        enq.bits.data_valid &&
        (release2Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset) ||
        release1Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
    }
  }
  // val needEnqueue_valid = Wire(Vec(LoadPipelineWidth, Bool()))
  // needEnqueue_valid := (0 until LoadPipelineWidth).map{ i => needEnqueue(i)}
  // val clkGate = Module(new STD_CLKGT_func)
  //  clkGate.io.TE := false.B
  //  clkGate.io.E := needEnqueue_valid.asUInt.orR || (freeList.io.validCount =/= 0.U)
  //  clkGate.io.CK := clock
  // val gate_clock = clkGate.io.Q
  // paddrModule.clock := gate_clock
 
  //  LoadQueueRAR deallocate
  val freeMaskVec = Wire(Vec(LoadQueueRARSize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // when the loads that "older than" current load were writebacked,
  // current load will be released.
  for (i <- 0 until LoadQueueRARSize) {
    val deqNotBlock = !isBefore(io.ldWbPtr, uop(i).lqIdx)
    val needFlush = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needFlush)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
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
  //val allocatedUInt = RegNext(allocated.asUInt)
  for ((query, w) <- io.query.zipWithIndex) {
    ldLdViolation(w) := false.B
    paddrModule.io.releaseViolationMdata(w) := query.req.bits.paddr

    query.resp.valid := RegNext(query.req.valid)
    // Generate real violation mask
    val robIdxMask = VecInit(uop.map(_.robIdx).map(isAfter(_, query.req.bits.uop.robIdx)))
    val matchMaskReg = Wire(Vec(LoadQueueRARSize, Bool()))
    for(i <- 0 until LoadQueueRARSize) {
      matchMaskReg(i) := (allocated(i) &
                         paddrModule.io.releaseViolationMmask(w)(i) &
                         robIdxMask(i) &&
                         released(i))
      }
    val matchMask = GatedValidRegNext(matchMaskReg)
    //  Load-to-Load violation check result
    // val ldLdViolationMask = VecInit(matchMask)
    val ldLdViolationMask = matchMask
    ldLdViolationMask.suggestName("ldLdViolationMask_" + w)
    query.resp.bits.rep_frm_fetch := ParallelORR(ldLdViolationMask)
  }


  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle
  val releaseVioMask = Reg(Vec(LoadQueueRARSize, Bool()))
  when (release1Cycle.valid) {
    paddrModule.io.releaseMdata.takeRight(1)(0) := release1Cycle.bits.paddr
  }

  val lastAllocIndexOH = lastAllocIndex.map(UIntToOH(_))
  val lastReleasePAddrMatch = VecInit((0 until LoadPipelineWidth).map(i => {
    (bypassPAddr(i)(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
  }))
  (0 until LoadQueueRARSize).map(i => {
    val bypassMatch = VecInit((0 until LoadPipelineWidth).map(j => lastCanAccept(j) && lastAllocIndexOH(j)(i) && lastReleasePAddrMatch(j))).asUInt.orR
    when (RegNext((paddrModule.io.releaseMmask.takeRight(1)(0)(i) || bypassMatch) && allocated(i) && release1Cycle.valid)) {
      // Note: if a load has missed in dcache and is waiting for refill in load queue,
      // its released flag still needs to be set as true if addr matches.
      // releasedEnable(i) := true.B
      released(i) := true.B

    }
  })

  io.lqFull := freeList.io.empty

  // perf cnt
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
  generatePerfEvent()
  // End
}