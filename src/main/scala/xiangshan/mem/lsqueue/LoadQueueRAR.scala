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
import chipsalliance.rocketchip.config._
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
    val redirect = Flipped(Valid(new Redirect))
    val query = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
    val deallocate = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
    val release = Flipped(Valid(new Release))
    val ldIssuePtr = Input(new LqPtr)
  })
  println("LoadQueueRAR: size: " + LoadQueueRARSize)
  //  LoadQueueRAR field
  //  +-------+-------+-------+----------+-----------+---------+
  //  | Valid |  Uop  | PAddr | Released | Datavalid |  Miss   |
  //  +-------+-------+-------+----------+-----------+---------+
  //  
  //  Field descriptions:
  //  Allocated   : entry is valid.
  //  MicroOp     : Micro-op
  //  PAddr       : physical address.
  //  Released    : DCache released. 
  //  Datavalid   : data valid
  //  Miss        : DCache miss
  //
  val allocated = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(LoadQueueRARSize, new MicroOp))
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PAddrBits.W),
    numEntries = LoadQueueRARSize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numCamPort = LoadPipelineWidth
  ))
  paddrModule.io := DontCare
  val released = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))
  val datavalid = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))
  val miss = RegInit(VecInit(List.fill(LoadQueueRARSize)(false.B)))

  //  LoadQueueRAR enqueue
  val freeNums = LoadQueueRARSize.U - PopCount(allocated)
  val canAcceptVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val release1Cycle = io.release
  val release2Cycle = RegNext(io.release)
  val release2Cycle_dup_lsu = RegNext(io.release)

  val canEnqueue = io.query.map(_.req).map(req => req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val hasNotWritebackedLoad = io.query.map(_.req.bits.uop.lqIdx).map(lqIdx => isAfter(lqIdx, io.ldIssuePtr))
  val needEnqueue = canEnqueue.zip(hasNotWritebackedLoad).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }

  // select LoadPipelineWidth valid index.
  val selectMask = ~allocated.asUInt
  val select0IndexOH = PriorityEncoderOH(selectMask)
  val select1IndexOH = Reverse(PriorityEncoderOH(Reverse(selectMask)))
  val selectIndexOH = VecInit(Seq(select0IndexOH, select1IndexOH))

  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    paddrModule.io.wen(w) := false.B

    //  Allocate ready
    val allocateNums = if (w == 0) 0.U else PopCount(needEnqueue.take(w))
    canAcceptVec(w) := freeNums > allocateNums

    val enqIdx = OHToUInt(selectIndexOH(w))
    enq.ready := Mux(needEnqueue(w), canAcceptVec(w), true.B)

    when (needEnqueue(w) && canAcceptVec(w)) {

      //  Allocate new entry
      allocated(enqIdx) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := enqIdx 
      paddrModule.io.wdata(w) := enq.bits.paddr

      //  Fill info
      uop(enqIdx) := enq.bits.uop
      released(enqIdx) := 
        release2Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset) || 
        release1Cycle.valid &&
        enq.bits.paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset)        
      datavalid(enqIdx) := enq.bits.datavalid
      miss(enqIdx) := enq.bits.miss
    }
  }

  //  LoadQueueRAR Query
  val ldLdViolation = Wire(Vec(LoadPipelineWidth, Bool()))
  val allocatedUInt = RegNext(allocated.asUInt)
  for ((query, w) <- io.query.zipWithIndex) {
    ldLdViolation(w) := false.B
    paddrModule.io.releaseViolationMdata(w) := query.req.bits.paddr

    query.resp.valid := RegNext(query.req.valid)
    // Generate real violation mask
    val robIdxMask = VecInit(uop.map(_.robIdx).map(isAfter(_, query.req.bits.uop.robIdx)))
    val matchMask = allocatedUInt &
                    RegNext(paddrModule.io.releaseViolationMmask(w).asUInt) &
                      RegNext(robIdxMask.asUInt)    
    //  Released
    val ldLdViolationMask = WireInit(matchMask & RegNext(released.asUInt & (datavalid.asUInt | miss.asUInt)))
    dontTouch(ldLdViolationMask)
    ldLdViolationMask.suggestName("ldLdViolationMask_"+w)
    query.resp.bits.replayFromFetch := ldLdViolationMask.orR || RegNext(ldLdViolation(w))
  }

  (0 until LoadPipelineWidth).map(w => {
    ldLdViolation(w) := (release1Cycle.valid && io.query(w).req.bits.paddr(PAddrBits-1, DCacheLineOffset) === release1Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset)) ||
                        (release2Cycle.valid && io.query(w).req.bits.paddr(PAddrBits-1, DCacheLineOffset) === release2Cycle.bits.paddr(PAddrBits-1, DCacheLineOffset))
  })

  //  LoadQueueRAR deallocate
  for (i <- 0 until LoadQueueRARSize) {
    val deqNotBlock = !isBefore(io.ldIssuePtr, uop(i).lqIdx) 
    val needFlush = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needFlush)) {
      allocated(i) := false.B
    }
  }

  // if need replay deallocate entry
  val lastCanAccept = RegNext(VecInit(needEnqueue.zip(canAcceptVec).map(x => x._1 && x._2)))
  val lastAllocIndex = RegNext(VecInit(selectIndexOH.map(x => OHToUInt(x))))
  for ((dealloc, i) <- io.deallocate.zipWithIndex) {
    val deallocValid = dealloc.valid && lastCanAccept(i)
    val deallocIndex = lastAllocIndex(i)
    val needReplay = dealloc.bits.replayInfo.needReplay()
    val replayInst = dealloc.bits.uop.ctrl.replayInst

    when (deallocValid && (needReplay || replayInst)) { 
      allocated(deallocIndex) := false.B
    }
  }

  // When io.release.valid (release1cycle.valid), it uses the last ld-ld paddr cam port to
  // update release flag in 1 cycle
  val releaseVioMask = Reg(Vec(LoadQueueRARSize, Bool()))
  when (release1Cycle.valid) {
    paddrModule.io.releaseMdata.takeRight(1)(0) := release1Cycle.bits.paddr
  }

  (0 until LoadQueueRARSize).map(i => {
    when (RegNext(paddrModule.io.releaseMmask.takeRight(1)(0)(i) && allocated(i) && release1Cycle.valid)) {
      // Note: if a load has missed in dcache and is waiting for refill in load queue,
      // its released flag still needs to be set as true if addr matches. 
      released(i) := true.B   
    }
  })  

  // perf cnt
  val canEnqCount = PopCount(io.query.map(_.req.fire))
  val full = freeNums === 0.U
  val ldLdViolationCount = PopCount(io.query.map(_.resp).map(resp => resp.valid && resp.bits.replayFromFetch))
  XSPerfAccumulate("enq", canEnqCount)
  XSPerfAccumulate("full", full)
  XSPerfAccumulate("ld_ld_violation", ldLdViolationCount)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", canEnqCount),
    ("full", full),
    ("ld_ld_violation", ldLdViolationCount)
  )
  generatePerfEvent()  
  // End
}