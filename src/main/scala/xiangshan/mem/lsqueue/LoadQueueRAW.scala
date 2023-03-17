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
import xiangshan.frontend.FtqPtr
import xiangshan.mem.mdp._
import utils._
import utility._

class LoadQueueRAW(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper 
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val query = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val rollback = Output(Valid(new Redirect)) 
    val correctTableUpdate = Valid(new CorrectTableUpdate) 
    val stAddrReadySqPtr = Input(new SqPtr)
    val ldIssuePtr = Input(new LqPtr)
    val lqEmpty = Input(Bool())
    val sqEmpty = Input(Bool())
  })
  println("LoadQueueRAWSize: size " + LoadQueueRAWSize)
  //  LoadQueueRAW field
  //  +-------+--------+-------+-------+-----------+
  //  | Valid |  uop   |PAddr  | Mask  | Datavalid |
  //  +-------+--------+-------+-------+-----------+
  //
  //  Field descriptions:
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  PAddr       : physical address.
  //  Mask        : data mask
  //  Datavalid   : data valid
  //
  val allocated = RegInit(VecInit(List.fill(LoadQueueRAWSize)(false.B))) // The control signals need to explicitly indicate the initial value
  val uop = Reg(Vec(LoadQueueRAWSize, new MicroOp))
  val paddrModule = Module(new LqPAddrModule(
    gen = UInt(PAddrBits.W),
    numEntries = LoadQueueRAWSize,
    numRead = LoadPipelineWidth, 
    numWrite = LoadPipelineWidth, 
    numWBank = LoadQueueNWriteBanks,
    numCamPort = StorePipelineWidth
  ))
  paddrModule.io := DontCare
  val maskModule = Module(new LqMaskModule(
    gen = UInt(8.W),
    numEntries = LoadQueueRAWSize, 
    numRead = LoadPipelineWidth, 
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numCamPort = StorePipelineWidth
  ))
  maskModule.io := DontCare
  val datavalid = RegInit(VecInit(List.fill(LoadQueueRAWSize)(false.B)))

  //  LoadQueueRAW enqueue
  val freeNums = LoadQueueRAWSize.U - PopCount(allocated)
  val canEnqVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val canAcceptVec = Wire(Vec(LoadPipelineWidth, Bool()))

  val canEnqueue = io.query.map(_.req).map(req => req.valid && !req.bits.allocated)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect)) 
  val hasAddrInvalidStore = io.query.map(_.req.bits.uop.sqIdx).map(sqIdx => !isAfter(io.stAddrReadySqPtr, sqIdx) && io.stAddrReadySqPtr =/= sqIdx)
  val needEnqueue = canEnqueue.zip(hasAddrInvalidStore).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }
  val enqIdxOH = SelectFirstN(~allocated.asUInt, LoadPipelineWidth, canEnqVec.asUInt)

  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    canEnqVec(w) := false.B
    paddrModule.io.wen(w) := false.B
    maskModule.io.wen(w) := false.B

    //  Allocate ready 
    val allocateNums = if (w == 0) 0.U else PopCount(needEnqueue.take(w)) 
    canAcceptVec(w) := freeNums > allocateNums

    when (needEnqueue(w) && canAcceptVec(w)) {
      canEnqVec(w) := true.B

      val enqIdx = OHToUInt(enqIdxOH(w))
      //  Allocate new entry
      allocated(enqIdx) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B 
      paddrModule.io.waddr(w) := enqIdx 
      paddrModule.io.wdata(w) := enq.bits.paddr

      //  Write mask
      maskModule.io.wen(w) := true.B 
      maskModule.io.waddr(w) := enqIdx 
      maskModule.io.wdata(w) := enq.bits.mask

      //  Fill info 
      uop(enqIdx) := enq.bits.uop
      datavalid(enqIdx) := false.B
    }

    // update datavalid
    val lastEnqValid = RegNext(enq.valid && enq.bits.allocated) && (uop(RegNext(enq.bits.index)).robIdx === RegNext(enq.bits.uop.robIdx)) || canEnqVec(w)
    val lastEnqBits = RegNext(enq.bits)
    val lastEnqIndex = Wire(UInt(log2Up(LoadQueueRAWSize).W))
    lastEnqIndex := Mux(lastEnqBits.allocated, lastEnqBits.index, RegNext(OHToUInt(enqIdxOH(w))))
    val schedError = VecInit((0 until StorePipelineWidth).map(i => 
      io.storeIn(i).valid &&
      !io.storeIn(i).bits.miss &&
      isAfter(lastEnqBits.uop.robIdx, io.storeIn(i).bits.uop.robIdx) &&
      (lastEnqBits.paddr(PAddrBits-1,3) === io.storeIn(i).bits.paddr(PAddrBits-1, 3)) &&
      (lastEnqBits.mask & io.storeIn(i).bits.mask).orR)).asUInt.orR
    
    when (lastEnqValid) {
      datavalid(lastEnqIndex) := lastEnqBits.datavalid && !schedError
    }
  }

  for ((query, w) <- io.query.map(_.resp).zipWithIndex) {
    query.valid := RegNext(io.query(w).req.valid)
    query.bits.canAccept := RegNext(Mux(needEnqueue(w), canEnqVec(w), true.B))
    query.bits.allocated := RegNext(canEnqVec(w))
    query.bits.replayFromFetch := RegNext(false.B)
    query.bits.index := RegNext(Mux(io.query(w).req.bits.allocated, io.query(w).req.bits.index, OHToUInt(enqIdxOH(w))))
  }


  //  LoadQueueRAW deallocate
  for (i <- 0 until LoadQueueRAWSize) {
    val deqNotBlock = !isBefore(io.stAddrReadySqPtr, uop(i).sqIdx)
    val needFlush = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needFlush)) {
      allocated(i) := false.B
    }
  }

  //  LoadQueueRAW violation check
  //  Scheme 1(Current scheme): like old load queue.
  //  Scheme 2                : re-fetch instructions from the first instruction after the store instruction.
  /**
    * Store-Load Memory violation detection
    *
    * When store writes back, it searches LoadQueue for younger load instructions
    * with the same load physical address. They loaded wrong data and need re-execution.
    *
    * Cycle 0: Store Writeback
    *   Generate match vector for store address with rangeMask(stPtr, enqPtr).
    * Cycle 1: Redirect Generation
    *   There're up to 2 possible redirect requests.
    *   Choose the oldest load (part 1). 
    * Cycle 2: Redirect Fire
    *   Choose the oldest load (part 2).
    *   Prepare redirect request according to the detected violation.
    *   Fire redirect request (if valid)
    */

  // stage 0:        lq                 lq
  //                 |                  |  (paddr match)
  // stage 1:        lq                 lq
  //                 |                  |
  //                 |                  |
  //                 |                  |
  // stage 2:        lq                 lq
  //                 |                  |
  //                 --------------------
  //                          |
  //                      rollback req
  def getFirstOne(mask: Vec[Bool], startMask: UInt) = {
    val length = mask.length
    val highBits = (0 until length).map(i => mask(i) & ~startMask(i))
    val highBitsUint = Cat(highBits.reverse)
    PriorityEncoder(Mux(highBitsUint.orR(), highBitsUint, mask.asUInt))
  }

  def getOldest[T <: XSBundleWithMicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    assert(isPow2(valid.length))
    if (valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).uop.robIdx, bits(1).uop.robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = getOldest(valid.take(valid.length / 2), bits.take(valid.length / 2))
      val right = getOldest(valid.takeRight(valid.length / 2), bits.takeRight(valid.length / 2))
      getOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  def detectRollback(i: Int) = {
    paddrModule.io.violationMdata(i) := io.storeIn(i).bits.paddr
    maskModule.io.violationMdata(i) := io.storeIn(i).bits.mask

    val addrMaskMatch = RegNext(paddrModule.io.violationMmask(i).asUInt & maskModule.io.violationMmask(i).asUInt)
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueRAWSize).map(j => {
      allocated(j) && isAfter(uop(j).robIdx, io.storeIn(i).bits.uop.robIdx) && datavalid(j)
    })))
    val lqViolationVec = VecInit((0 until LoadQueueRAWSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))
    val lqViolation = lqViolationVec.asUInt.orR && RegNext(!io.storeIn(i).bits.miss)
    val lqViolationUopExts = uop.map(op => {
      val opExt = Wire(new XSBundleWithMicroOp)
      opExt.uop := op
      opExt
    })
    val lqViolationUop = getOldest(lqViolationVec, lqViolationUopExts)._2(0).uop

    XSDebug(lqViolation, p"${Binary(Cat(lqViolationVec))}\n")

    XSDebug(
      lqViolation,
      "need rollback (ld wb before store) pc %x robidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.robIdx.asUInt, lqViolationUop.robIdx.asUInt
    )

    (lqViolation, lqViolationUop)
  } 

  val lastCycleRedirect = RegNext(io.redirect)
  val lastlastCycleRedirect = RegNext(lastCycleRedirect)

  // S2: select rollback (part1) and generate rollback request
  // rollback check
  // Lq rollback seq check is done in s3 (next stage), as getting rollbackLq MicroOp is slow
  val rollbackLq = Wire(Vec(StorePipelineWidth, Valid(new MicroOpRbExt)))
  val stFtqIdxS2 = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffsetS2 = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (w <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(w)
    rollbackLq(w).valid := detectedRollback._1 && RegNext(io.storeIn(w).valid)
    rollbackLq(w).bits.uop := detectedRollback._2
    rollbackLq(w).bits.flag := w.U
    stFtqIdxS2(w) := RegNext(io.storeIn(w).bits.uop.cf.ftqPtr)
    stFtqOffsetS2(w) := RegNext(io.storeIn(w).bits.uop.cf.ftqOffset)
  }
 
  val rollbackLqVReg = rollbackLq.map(x => RegNext(x.valid)) 
  val rollbackLqReg = rollbackLq.map(x => RegEnable(x.bits, x.valid))

  // S3: select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  val lqs = getOldest(rollbackLqVReg, rollbackLqReg)
  val rollbackUopExt = lqs._2(0)
  val stFtqIdxS3 = RegNext(stFtqIdxS2)
  val stFtqOffsetS3 = RegNext(stFtqOffsetS2)
  val rollbackUop = rollbackUopExt.uop
  val rollbackStFtqIdx = stFtqIdxS3(rollbackUopExt.flag)
  val rollbackStFtqOffset = stFtqOffsetS3(rollbackUopExt.flag)
 
  // check if rollback request is still valid in parallel
  io.rollback.bits.robIdx := rollbackUop.robIdx
  io.rollback.bits.ftqIdx := rollbackUop.cf.ftqPtr
  io.rollback.bits.stFtqIdx := rollbackStFtqIdx
  io.rollback.bits.ftqOffset := rollbackUop.cf.ftqOffset
  io.rollback.bits.stFtqOffset := rollbackStFtqOffset
  io.rollback.bits.level := RedirectLevel.flush
  io.rollback.bits.interrupt := DontCare
  io.rollback.bits.cfiUpdate := DontCare
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  io.rollback.bits.debug_runahead_checkpoint_id := rollbackUop.debugInfo.runahead_checkpoint_id
  // io.rollback.bits.pc := DontCare

  io.rollback.valid := rollbackLqVReg.reduce(_|_) &&
                        (!lastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastCycleRedirect.bits.robIdx)) && 
                        (!lastlastCycleRedirect.valid || isBefore(rollbackUop.robIdx, lastlastCycleRedirect.bits.robIdx))

  //  update mdp
  io.correctTableUpdate.valid := rollbackLqVReg.reduce(_|_)
  io.correctTableUpdate.bits.addr := rollbackUop.cf.foldpc
  io.correctTableUpdate.bits.strict := true.B
  io.correctTableUpdate.bits.violation := true.B

  XSPerfAccumulate("enqs", PopCount(canEnqVec))
  XSPerfAccumulate("full", freeNums === 0.U)
  XSPerfAccumulate("rollback", io.rollback.valid) 
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enqs", PopCount(canEnqVec)),
    ("full", freeNums === 0.U),
    ("rollback         ", io.rollback.valid),
  )
  generatePerfEvent()   
  // end
}