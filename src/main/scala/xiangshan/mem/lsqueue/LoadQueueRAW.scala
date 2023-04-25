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
    val deallocate = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val rollback = Output(Valid(new Redirect)) 
    val stAddrReadySqPtr = Input(new SqPtr)
    val stIssuePtr = Input(new SqPtr)
    val ldIssuePtr = Input(new LqPtr)
    val sqEmpty = Input(Bool())
    val lqFull = Output(Bool())
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
  val canAcceptVec = Wire(Vec(LoadPipelineWidth, Bool()))

  val canEnqueue = io.query.map(_.req).map(req => req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect)) 
  val allAddrCheck = io.stIssuePtr === io.stAddrReadySqPtr
  val hasAddrInvalidStore = io.query.map(_.req.bits.uop.sqIdx).map(sqIdx => {
    Mux(!allAddrCheck, isBefore(io.stAddrReadySqPtr, sqIdx), false.B) 
  })
  val needEnqueue = canEnqueue.zip(hasAddrInvalidStore).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }

  // select LoadPipelineWidth valid index.
  val selectMask = ~allocated.asUInt
  val select0IndexOH = PriorityEncoderOH(selectMask)
  val select1IndexOH = Reverse(PriorityEncoderOH(Reverse(selectMask)))
  val selectIndexOH = VecInit(Seq(select0IndexOH, select1IndexOH))
 
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
    * Cycle 1: Select oldest load from select group.
    * Cycle x: Redirect Fire
    *   Choose the oldest load from LoadPipelineWidth oldest loads.
    *   Prepare redirect request according to the detected violation.
    *   Fire redirect request (if valid)
    */
  //              SelectGroup 0         SelectGroup 1          SelectGroup y
  // stage 0:       lq  lq  lq  ......    lq  lq  lq  .......    lq  lq  lq
  //                |   |   |             |   |   |              |   |   | (paddr match)
  // stage 1:       lq  lq  lq  ......    lq  lq  lq  .......    lq  lq  lq 
  //                 \  |  /    ......     \  |  /    .......     \  |  / 
  // stage 2:           lq                    lq                     lq
  //                     \  |  /  .......  \  |  /   ........  \  |  /
  // stage 3:               lq                lq                  lq
  //                                          ...
  //                                          ...
  //                                           |
  // stage x:                                  lq
  //                                           |
  //                                       rollback req
  // select logic
  val SelectGroupSize = RollbackGroupSize
  val lgSelectGroupSize = log2Ceil(SelectGroupSize)
  val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1

  def selectPartialOldest[T <: XSBundleWithMicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
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
      val left = selectPartialOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectPartialOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectPartialOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  def selectOldest[T <: XSBundleWithMicroOp](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    val numSelectGroups = scala.math.ceil(valid.length.toFloat / SelectGroupSize).toInt

    // group info
    val selectValidGroups = 
      if (valid.length <= SelectGroupSize) {
        Seq(valid)
      } else {
        (0 until numSelectGroups).map(g => {
          if (valid.length < g * SelectGroupSize) {
            valid.takeRight(valid.length - (g - 1) * SelectGroupSize)
          } else {
            (0 until SelectGroupSize).map(j => valid(g * SelectGroupSize + j))
          }
        })
      }
    val selectBitsGroups = 
      if (bits.length <= SelectGroupSize) {
        Seq(bits)
      } else {
        (0 until numSelectGroups).map(g => {
          if (bits.length < g * SelectGroupSize) {
            bits.takeRight(bits.length - (g - 1) * SelectGroupSize)
          } else {
            (0 until SelectGroupSize).map(j => bits(g * SelectGroupSize + j))
          }
        })
      }

    // select logic 
    if (valid.length <= SelectGroupSize) {
      val (selValid, selBits) = selectPartialOldest(valid, bits)
      (Seq(RegNext(selValid(0) && !selBits(0).uop.robIdx.needFlush(io.redirect))), Seq(RegNext(selBits(0))))
    } else {
      val select = (0 until numSelectGroups).map(g => {
        val (selValid, selBits) = selectPartialOldest(selectValidGroups(g), selectBitsGroups(g))
        (RegNext(selValid(0) && !selBits(0).uop.robIdx.needFlush(io.redirect)), RegNext(selBits(0)))
      })
      selectOldest(select.map(_._1), select.map(_._2))
    }
  }

  def detectRollback(i: Int) = {
    paddrModule.io.violationMdata(i) := io.storeIn(i).bits.paddr
    maskModule.io.violationMdata(i) := io.storeIn(i).bits.mask

    val addrMaskMatch = RegNext(paddrModule.io.violationMmask(i).asUInt & maskModule.io.violationMmask(i).asUInt)
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueRAWSize).map(j => {
      allocated(j) && isAfter(uop(j).robIdx, io.storeIn(i).bits.uop.robIdx) && datavalid(j) && !uop(j).robIdx.needFlush(io.redirect)
    })))
    val lqViolationSelVec = VecInit((0 until LoadQueueRAWSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))

    (0 until LoadQueueRAWSize).map(j => {
      when (allocated(j) && RegNext(io.storeIn(i).valid) && !RegNext(io.storeIn(i).bits.miss) && lqViolationSelVec(j)) {
        allocated(j) := false.B
      }
    })

    val lqViolationSelUopExts = uop.map(op => {
      val opExt = Wire(new XSBundleWithMicroOp)
      opExt.uop := op
      opExt
    })

    // select logic
    val lqSelect = selectOldest(lqViolationSelVec, lqViolationSelUopExts)

    // select one inst
    val lqViolation = lqSelect._1(0)
    val lqViolationUop = lqSelect._2(0).uop

    XSDebug(
      lqViolation,
      "need rollback (ld wb before store) pc %x robidx %d target %x\n",
      io.storeIn(i).bits.uop.cf.pc, io.storeIn(i).bits.uop.robIdx.asUInt, lqViolationUop.robIdx.asUInt
    )

    (lqViolation, lqViolationUop)
  } 

  // select rollback (part1) and generate rollback request
  // rollback check
  // Lq rollback seq check is done in s3 (next stage), as getting rollbackLq MicroOp is slow
  val rollbackLq = Wire(Vec(StorePipelineWidth, Valid(new MicroOpRbExt)))
  val stFtqIdx = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffset = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (w <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(w)
    rollbackLq(w).valid := detectedRollback._1 && DelayN(io.storeIn(w).valid && !io.storeIn(w).bits.miss, TotalSelectCycles)
    rollbackLq(w).bits.uop := detectedRollback._2
    rollbackLq(w).bits.flag := w.U
    stFtqIdx(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqPtr, TotalSelectCycles)
    stFtqOffset(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqOffset, TotalSelectCycles)
  }
 
  val rollbackLqValid = rollbackLq.map(x => x.valid && !x.bits.uop.robIdx.needFlush(io.redirect))
  val rollbackLqBits = rollbackLq.map(x => x.bits)

  // select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  val lqs = selectPartialOldest(rollbackLqValid, rollbackLqBits)
  val rollbackUopExt = lqs._2(0)
  val rollbackUop = rollbackUopExt.uop
  val rollbackStFtqIdx = stFtqIdx(rollbackUopExt.flag)
  val rollbackStFtqOffset = stFtqOffset(rollbackUopExt.flag)
 
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

  io.rollback.valid := VecInit(rollbackLqValid).asUInt.orR

  // <------- DANGEROUS: Don't change sequence here ! ------->

  // Enqueue
  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    paddrModule.io.wen(w) := false.B
    maskModule.io.wen(w) := false.B

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

      //  Write mask
      maskModule.io.wen(w) := true.B 
      maskModule.io.waddr(w) := enqIdx 
      maskModule.io.wdata(w) := enq.bits.mask

      //  Fill info 
      uop(enqIdx) := enq.bits.uop
      datavalid(enqIdx) := enq.bits.datavalid
    }
  }

  for ((query, w) <- io.query.map(_.resp).zipWithIndex) {
    query.valid := RegNext(io.query(w).req.valid)
    query.bits.replayFromFetch := RegNext(false.B)
  }


  //  LoadQueueRAW deallocate
  for (i <- 0 until LoadQueueRAWSize) {
    val deqNotBlock = Mux(!allAddrCheck, !isBefore(io.stAddrReadySqPtr, uop(i).sqIdx), true.B)
    val needCancel = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needCancel)) {
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

  // perf cnt
  val full = freeNums === 0.U
  io.lqFull := full

  XSPerfAccumulate("enqs", PopCount(io.query.map(_.req.fire)))
  XSPerfAccumulate("full", full)
  XSPerfAccumulate("rollback", io.rollback.valid) 
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq ", PopCount(io.query.map(_.req.fire))),
    ("full ", full),
    ("rollback", io.rollback.valid),
  )
  generatePerfEvent()   
  // end
}