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
    // control
    val redirect = Flipped(ValidIO(new Redirect))

    // violation query
    val query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO))

    // from store unit s1
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))

    // global rollback flush
    val rollback = Output(Valid(new Redirect))

    // to LoadQueueReplay
    val stAddrReadySqPtr = Input(new SqPtr)
    val stIssuePtr       = Input(new SqPtr)
    val lqFull           = Output(Bool())
  })

  println("LoadQueueRAW: size " + LoadQueueRAWSize)
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
    numWDelay = 2,
    numCamPort = StorePipelineWidth
  ))
  paddrModule.io := DontCare
  val maskModule = Module(new LqMaskModule(
    gen = UInt((VLEN/8).W),
    numEntries = LoadQueueRAWSize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = StorePipelineWidth
  ))
  maskModule.io := DontCare
  val datavalid = RegInit(VecInit(List.fill(LoadQueueRAWSize)(false.B)))

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadQueueRAWSize,
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = true,
    moduleName = "LoadQueueRAW freelist"
  ))
  freeList.io := DontCare

  //  LoadQueueRAW enqueue
  val canEnqueue = io.query.map(_.req.valid)
  val cancelEnqueue = io.query.map(_.req.bits.uop.robIdx.needFlush(io.redirect))
  val allAddrCheck = io.stIssuePtr === io.stAddrReadySqPtr
  val hasAddrInvalidStore = io.query.map(_.req.bits.uop.sqIdx).map(sqIdx => {
    Mux(!allAddrCheck, isBefore(io.stAddrReadySqPtr, sqIdx), false.B)
  })
  val needEnqueue = canEnqueue.zip(hasAddrInvalidStore).zip(cancelEnqueue).map { case ((v, r), c) => v && r && !c }
  val bypassPAddr = Reg(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))
  val bypassMask = Reg(Vec(LoadPipelineWidth, UInt((VLEN/8).W)))

  // Allocate logic
  val acceptedVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt()))

  // Enqueue
  for ((enq, w) <- io.query.map(_.req).zipWithIndex) {
    acceptedVec(w) := false.B
    paddrModule.io.wen(w) := false.B
    maskModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocate ready
    val offset = PopCount(needEnqueue.take(w))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex = freeList.io.allocateSlot(offset)
    enq.ready := Mux(needEnqueue(w), canAccept(w), true.B)

    enqIndexVec(w) := enqIndex
    when (needEnqueue(w) && enq.ready) {
      acceptedVec(w) := true.B

      val debug_robIdx = enq.bits.uop.robIdx.asUInt
      XSError(allocated(enqIndex), p"LoadQueueRAW: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")

      freeList.io.doAllocate(w) := true.B

      //  Allocate new entry
      allocated(enqIndex) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := enqIndex
      paddrModule.io.wdata(w) := enq.bits.paddr
      bypassPAddr(w) := enq.bits.paddr

      //  Write mask
      maskModule.io.wen(w) := true.B
      maskModule.io.waddr(w) := enqIndex
      maskModule.io.wdata(w) := enq.bits.mask
      bypassMask(w) := enq.bits.mask

      //  Fill info
      uop(enqIndex) := enq.bits.uop
      datavalid(enqIndex) := enq.bits.data_valid
    }
  }

  for ((query, w) <- io.query.map(_.resp).zipWithIndex) {
    query.valid := RegNext(io.query(w).req.valid)
    query.bits.rep_frm_fetch := RegNext(false.B)
  }

  //  LoadQueueRAW deallocate
  val freeMaskVec = Wire(Vec(LoadQueueRAWSize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // when the stores that "older than" current load address were ready.
  // current load will be released.
  for (i <- 0 until LoadQueueRAWSize) {
    val deqNotBlock = Mux(!allAddrCheck, !isBefore(io.stAddrReadySqPtr, uop(i).sqIdx), true.B)
    val needCancel = uop(i).robIdx.needFlush(io.redirect)

    when (allocated(i) && (deqNotBlock || needCancel)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
    }
  }

  // if need replay deallocate entry
  val lastCanAccept = RegNext(acceptedVec)
  val lastAllocIndex = RegNext(enqIndexVec)

  for ((revoke, w) <- io.query.map(_.revoke).zipWithIndex) {
    val revokeValid = revoke && lastCanAccept(w)
    val revokeIndex = lastAllocIndex(w)

    when (allocated(revokeIndex) && revokeValid) {
      allocated(revokeIndex) := false.B
      freeMaskVec(revokeIndex) := true.B
    }
  }
  freeList.io.free := freeMaskVec.asUInt

  io.lqFull := freeList.io.empty

  /**
    * Store-Load Memory violation detection
    * Scheme 1(Current scheme): flush the pipeline then re-fetch from the load instruction (like old load queue).
    * Scheme 2                : re-fetch instructions from the first instruction after the store instruction.
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
  //                |   |   |             |   |   |              |   |   |
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
          if (valid.length < (g + 1) * SelectGroupSize) {
            valid.takeRight(valid.length - g * SelectGroupSize)
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
          if (bits.length < (g + 1) * SelectGroupSize) {
            bits.takeRight(bits.length - g * SelectGroupSize)
          } else {
            (0 until SelectGroupSize).map(j => bits(g * SelectGroupSize + j))
          }
        })
      }

    // select logic
    if (valid.length <= SelectGroupSize) {
      val (selValid, selBits) = selectPartialOldest(valid, bits)
      val selValidNext = RegNext(selValid(0))
      val selBitsNext = RegNext(selBits(0))
      (Seq(selValidNext && !selBitsNext.uop.robIdx.needFlush(io.redirect) && !selBitsNext.uop.robIdx.needFlush(RegNext(io.redirect))), Seq(selBitsNext))
    } else {
      val select = (0 until numSelectGroups).map(g => {
        val (selValid, selBits) = selectPartialOldest(selectValidGroups(g), selectBitsGroups(g))
        val selValidNext = RegNext(selValid(0))
        val selBitsNext = RegNext(selBits(0))
        (selValidNext && !selBitsNext.uop.robIdx.needFlush(io.redirect) && !selBitsNext.uop.robIdx.needFlush(RegNext(io.redirect)), selBitsNext)
      })
      selectOldest(select.map(_._1), select.map(_._2))
    }
  }

  def detectRollback(i: Int) = {
    paddrModule.io.violationMdata(i) := io.storeIn(i).bits.paddr
    maskModule.io.violationMdata(i) := io.storeIn(i).bits.mask

    val bypassPaddrMask = RegNext(VecInit((0 until LoadPipelineWidth).map(j => bypassPAddr(j)(PAddrBits-1, DCacheVWordOffset) === io.storeIn(i).bits.paddr(PAddrBits-1, DCacheVWordOffset))))
    val bypassMMask = RegNext(VecInit((0 until LoadPipelineWidth).map(j => (bypassMask(j) & io.storeIn(i).bits.mask).orR)))
    val bypassMaskUInt = (0 until LoadPipelineWidth).map(j =>
      Fill(LoadQueueRAWSize, RegNext(RegNext(io.query(j).req.fire))) & Mux(bypassPaddrMask(j) && bypassMMask(j), UIntToOH(RegNext(RegNext(enqIndexVec(j)))), 0.U(LoadQueueRAWSize))
    ).reduce(_|_)

    val addrMaskMatch = RegNext(paddrModule.io.violationMmask(i).asUInt & maskModule.io.violationMmask(i).asUInt) | bypassMaskUInt
    val entryNeedCheck = RegNext(VecInit((0 until LoadQueueRAWSize).map(j => {
      allocated(j) && isAfter(uop(j).robIdx, io.storeIn(i).bits.uop.robIdx) && datavalid(j) && !uop(j).robIdx.needFlush(io.redirect)
    })))
    val lqViolationSelVec = VecInit((0 until LoadQueueRAWSize).map(j => {
      addrMaskMatch(j) && entryNeedCheck(j)
    }))

    val lqViolationSelUopExts = uop.map(uop => {
      val wrapper = Wire(new XSBundleWithMicroOp)
      wrapper.uop := uop
      wrapper
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
  val rollbackLqWb = Wire(Vec(StorePipelineWidth, Valid(new MicroOpRbExt)))
  val stFtqIdx = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffset = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (w <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(w)
    rollbackLqWb(w).valid     := detectedRollback._1 && DelayN(io.storeIn(w).valid && !io.storeIn(w).bits.miss, TotalSelectCycles)
    rollbackLqWb(w).bits.uop  := detectedRollback._2
    rollbackLqWb(w).bits.flag := w.U
    stFtqIdx(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqPtr, TotalSelectCycles)
    stFtqOffset(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqOffset, TotalSelectCycles)
  }

  val rollbackLqWbValid = rollbackLqWb.map(x => x.valid && !x.bits.uop.robIdx.needFlush(io.redirect))
  val rollbackLqWbBits = rollbackLqWb.map(x => x.bits)

  // select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  val lqs = selectPartialOldest(rollbackLqWbValid, rollbackLqWbBits)
  val rollbackUopExt = lqs._2(0)
  val rollbackUop = rollbackUopExt.uop
  val rollbackStFtqIdx = stFtqIdx(rollbackUopExt.flag)
  val rollbackStFtqOffset = stFtqOffset(rollbackUopExt.flag)

  // check if rollback request is still valid in parallel
  io.rollback.bits             := DontCare
  io.rollback.bits.robIdx      := rollbackUop.robIdx
  io.rollback.bits.ftqIdx      := rollbackUop.cf.ftqPtr
  io.rollback.bits.stFtqIdx    := rollbackStFtqIdx
  io.rollback.bits.ftqOffset   := rollbackUop.cf.ftqOffset
  io.rollback.bits.stFtqOffset := rollbackStFtqOffset
  io.rollback.bits.level       := RedirectLevel.flush
  io.rollback.bits.interrupt   := DontCare
  io.rollback.bits.cfiUpdate   := DontCare
  io.rollback.bits.cfiUpdate.target := rollbackUop.cf.pc
  io.rollback.bits.debug_runahead_checkpoint_id := rollbackUop.debugInfo.runahead_checkpoint_id
  // io.rollback.bits.pc := DontCare

  io.rollback.valid := VecInit(rollbackLqWbValid).asUInt.orR

  // perf cnt
  val canEnqCount = PopCount(io.query.map(_.req.fire))
  val validCount = freeList.io.validCount
  val allowEnqueue = validCount <= (LoadQueueRAWSize - LoadPipelineWidth).U

  QueuePerf(LoadQueueRAWSize, validCount, !allowEnqueue)
  XSPerfAccumulate("enqs", canEnqCount)
  XSPerfAccumulate("stld_rollback", io.rollback.valid)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq ", canEnqCount),
    ("stld_rollback", io.rollback.valid),
  )
  generatePerfEvent()
  // end
}