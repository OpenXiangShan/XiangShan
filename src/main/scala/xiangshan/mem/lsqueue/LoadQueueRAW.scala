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
    val storeNuke = Vec(StorePipelineWidth, Output(Bool()))
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
  // pre-Allocate logic
  val s1_preReqs = io.query.map(_.s1_prealloc)
  val s1_allAddrCheck = io.stIssuePtr === io.stAddrReadySqPtr
  val s1_hasAddrInvalidStore = io.query.map(x => {
    Mux(!s1_allAddrCheck, isBefore(io.stAddrReadySqPtr, x.s1_sqIdx), false.B)
  })
  val s1_preEnqs = s1_preReqs.zip(s1_hasAddrInvalidStore).map { case (v, r) => v && r }
  val s1_canAccepts = Wire(Vec(LoadPipelineWidth, Bool()))
  val s1_enqIdxs = Wire(Vec(LoadPipelineWidth, UInt()))

  for ((req, w) <- io.query.zipWithIndex) {
    freeList.io.allocateReq(w) := true.B

    s1_canAccepts(w) := freeList.io.canAllocate(w)
    s1_enqIdxs(w) := freeList.io.allocateSlot(w)
    req.s1_nack := Mux(s1_hasAddrInvalidStore(w), !s1_canAccepts(w), false.B)
  }

  // Allocate logic
  val s2_canEnqs = io.query.map(_.s2_alloc)
  val s2_hasAddrInvalidStore = RegNext(VecInit(s1_preReqs.zip(s1_hasAddrInvalidStore).map(x => x._1 && x._2)))
  val s2_cancel = io.query.map(x => {
    val x_next = RegNext(x.s1_robIdx)
    x_next.needFlush(RegNext(io.redirect)) || x_next.needFlush(io.redirect)
  })
  val s2_needEnqs = s2_canEnqs.zip(s2_hasAddrInvalidStore.zip(s2_cancel)).map { case (v, x) => v && x._1 && !x._2 }
  val s2_canAccepts = RegNext(s1_canAccepts)
  val s2_enqIdxs = RegNext(s1_enqIdxs)
  val s2_accepts = Wire(Vec(LoadPipelineWidth, Bool()))
  val s2_offset = Wire(Vec(LoadPipelineWidth, UInt()))
  val s2_bypassPAddr = Reg(Vec(LoadPipelineWidth, UInt(PAddrBits.W)))
  val s2_bypassMask = Reg(Vec(LoadPipelineWidth, UInt((VLEN/8).W)))


  for ((enq, w) <- io.query.zipWithIndex) {
    s2_accepts(w) := false.B
    paddrModule.io.wen(w) := false.B
    maskModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    //  Allocate ready
    val offset = PopCount(s2_needEnqs.take(w))
    val canAccept = s2_canAccepts(offset)
    val enqIndex = s2_enqIdxs(offset)
    s2_offset(w) := offset

    when (s2_needEnqs(w) && canAccept) {
      s2_accepts(w) := true.B

      val debug_robIdx = enq.s2_uop.robIdx.asUInt
      XSError(allocated(enqIndex), p"LoadQueueRAW: You can not write an valid entry! check: ldu $w, robIdx $debug_robIdx")

      freeList.io.doAllocate(w) := true.B

      //  Allocate new entry
      allocated(enqIndex) := true.B

      //  Write paddr
      paddrModule.io.wen(w) := true.B
      paddrModule.io.waddr(w) := enqIndex
      paddrModule.io.wdata(w) := enq.s2_paddr
      s2_bypassPAddr(w) := enq.s2_paddr

      //  Write mask
      maskModule.io.wen(w) := true.B
      maskModule.io.waddr(w) := enqIndex
      maskModule.io.wdata(w) := enq.s2_mask
      s2_bypassMask(w) := enq.s2_mask

      //  Fill info
      uop(enqIndex) := enq.s2_uop
      datavalid(enqIndex) := !enq.s2_dataInvalid
    }
  }

  for ((query, w) <- io.query.zipWithIndex) {
    query.s3_nuke := RegNext(false.B)
  }

  //  LoadQueueRAW deallocate
  val freeMaskVec = Wire(Vec(LoadQueueRAWSize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // when the stores that "older than" current load address were ready.
  // current load will be released.
  for (i <- 0 until LoadQueueRAWSize) {
    val deqNotBlock = Mux(!s1_allAddrCheck, !isBefore(io.stAddrReadySqPtr, uop(i).sqIdx), true.B)
    val needCancel = uop(i).robIdx.needFlush(io.redirect) ||
                     uop(i).robIdx.needFlush(RegNext(io.redirect))

    when (allocated(i) && (deqNotBlock || needCancel)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
    }
  }

  // if need replay deallocate entry
  val s3_accepts = RegNext(s2_accepts)
  val s3_enqIdxs = RegNext(VecInit(s2_offset.map(x => s2_enqIdxs(x))))
  val s3_cancel = io.query.map(x => {
    val x_next = RegNext(RegNext(x.s1_robIdx))
    x_next.needFlush(io.redirect)
  })
  for ((revoke, w) <- io.query.map(_.s3_revoke).zipWithIndex) {
    val s3_accept = s3_accepts(w)
    val s3_enqIdx = s3_enqIdxs(w)
    val revokeValid = revoke && s3_accept
    val revokeIndex = s3_enqIdx

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

    val s3_bypassPaddrMask = RegNext(VecInit((0 until LoadPipelineWidth).map(j => s2_bypassPAddr(j)(PAddrBits-1, DCacheVWordOffset) === io.storeIn(i).bits.paddr(PAddrBits-1, DCacheVWordOffset))))
    val s3_bypassMMask = RegNext(VecInit((0 until LoadPipelineWidth).map(j => (s2_bypassMask(j) & io.storeIn(i).bits.mask).orR)))
    val s3_bypassMaskUInt = (0 until LoadPipelineWidth).map(j =>
      Fill(LoadQueueRAWSize, RegNext(s3_accepts(j) && !s3_cancel(j))) & Mux(s3_bypassPaddrMask(j) && s3_bypassMMask(j), UIntToOH(RegNext(s3_enqIdxs(j))), 0.U(LoadQueueRAWSize.W))
    ).reduce(_|_)

    val addrMaskMatch = RegNext(paddrModule.io.violationMmask(i).asUInt & maskModule.io.violationMmask(i).asUInt) | s3_bypassMaskUInt
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
    io.storeNuke(i) := ParallelORR(lqViolationSelVec)
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
  val rollbackLqWb = Wire(Vec(StorePipelineWidth, Valid(new MicroOp)))
  val stFtqIdx = Wire(Vec(StorePipelineWidth, new FtqPtr))
  val stFtqOffset = Wire(Vec(StorePipelineWidth, UInt(log2Up(PredictWidth).W)))
  for (w <- 0 until StorePipelineWidth) {
    val detectedRollback = detectRollback(w)
    rollbackLqWb(w).valid := detectedRollback._1 && DelayN(io.storeIn(w).valid && !io.storeIn(w).bits.miss, TotalSelectCycles)
    rollbackLqWb(w).bits  := detectedRollback._2
    stFtqIdx(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqPtr, TotalSelectCycles)
    stFtqOffset(w) := DelayN(io.storeIn(w).bits.uop.cf.ftqOffset, TotalSelectCycles)
  }

  // select rollback (part2), generate rollback request, then fire rollback request
  // Note that we use robIdx - 1.U to flush the load instruction itself.
  // Thus, here if last cycle's robIdx equals to this cycle's robIdx, it still triggers the redirect.

  // select uop in parallel
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }
  val allRedirect = (0 until StorePipelineWidth).map(i => {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := rollbackLqWb(i).valid
    redirect.bits             := DontCare
    redirect.bits.isRVC       := rollbackLqWb(i).bits.cf.pd.isRVC
    redirect.bits.robIdx      := rollbackLqWb(i).bits.robIdx
    redirect.bits.ftqIdx      := rollbackLqWb(i).bits.cf.ftqPtr
    redirect.bits.ftqOffset   := rollbackLqWb(i).bits.cf.ftqOffset
    redirect.bits.stFtqIdx    := stFtqIdx(i)
    redirect.bits.stFtqOffset := stFtqOffset(i)
    redirect.bits.level       := RedirectLevel.flush
    redirect.bits.cfiUpdate.target := rollbackLqWb(i).bits.cf.pc
    redirect.bits.debug_runahead_checkpoint_id := rollbackLqWb(i).bits.debugInfo.runahead_checkpoint_id
    redirect
  })
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)
  io.rollback := oldestRedirect

  // perf cnt
  val canEnqCount = PopCount(s2_accepts)
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