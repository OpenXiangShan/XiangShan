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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.rob.RobLsqIO
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.mem.mdp._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput, MemMicroOpRbExt}
import xiangshan.backend.rob.RobPtr

class LqPtr(implicit p: Parameters) extends CircularQueuePtr[LqPtr](
  p => p(XSCoreParamsKey).VirtualLoadQueueSize
){
}

object LqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): LqPtr = {
    val ptr = Wire(new LqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

trait HasLoadHelper { this: XSModule =>
  def rdataHelper(uop: DynInst, rdata: UInt): UInt = {
    val fpWen = uop.fpWen
    LookupTree(uop.fuOpType, List(
      LSUOpType.lb   -> SignExt(rdata(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdata(15, 0), XLEN),
      /*
          riscv-spec-20191213: 12.2 NaN Boxing of Narrower Values
          Any operation that writes a narrower result to an f register must write
          all 1s to the uppermost FLEN−n bits to yield a legal NaN-boxed value.
      */
      LSUOpType.lw   -> Mux(fpWen, FPU.box(rdata, FPU.S), SignExt(rdata(31, 0), XLEN)),
      LSUOpType.ld   -> Mux(fpWen, FPU.box(rdata, FPU.D), SignExt(rdata(63, 0), XLEN)),
      LSUOpType.lbu  -> ZeroExt(rdata(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdata(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdata(31, 0), XLEN),
    ))
  }
}

class LqEnqIO(implicit p: Parameters) extends XSBundle {
  private val LsExuCnt = backendParams.StaCnt + backendParams.LduCnt
  val canAccept = Output(Bool())
  val sqCanAccept = Input(Bool())
  val needAlloc = Vec(LsExuCnt, Input(Bool()))
  val req = Vec(LsExuCnt, Flipped(ValidIO(new DynInst)))
  val resp = Vec(LsExuCnt, Output(new LqPtr))
}

class LqTriggerIO(implicit p: Parameters) extends XSBundle {
  val hitLoadAddrTriggerHitVec = Input(Vec(3, Bool()))
  val lqLoadAddrTriggerHitVec = Output(Vec(3, Bool()))
}

class LoadQueueTopDownIO(implicit p: Parameters) extends XSBundle {
  val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  val robHeadTlbReplay = Output(Bool())
  val robHeadTlbMiss = Output(Bool())
  val robHeadLoadVio = Output(Bool())
  val robHeadLoadMSHR = Output(Bool())
  val robHeadMissInDTlb = Input(Bool())
  val robHeadOtherReplay = Output(Bool())
}

class LoadQueue(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(Valid(new Redirect))
    val enq = new LqEnqIO
    val ldu = new Bundle() {
        val stld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldin         = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle))) // from load_s3
    }
    val sta = new Bundle() {
      val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // from store_s1
    }
    val std = new Bundle() {
      val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput))) // from store_s0, store data, send to sq from rs
    }
    val sq = new Bundle() {
      val stAddrReadySqPtr = Input(new SqPtr)
      val stAddrReadyVec   = Input(Vec(StoreQueueSize, Bool()))
      val stDataReadySqPtr = Input(new SqPtr)
      val stDataReadyVec   = Input(Vec(StoreQueueSize, Bool()))
      val stIssuePtr       = Input(new SqPtr)
      val sqEmpty          = Input(Bool())
    }
    val ldout = Vec(LoadPipelineWidth, DecoupledIO(new MemExuOutput))
    val ld_raw_data = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill))
    val tl_d_channel  = Input(new DcacheToLduForwardIO)
    val release = Flipped(Valid(new Release))
    val rollback = Output(Valid(new Redirect))
    val rob = Flipped(new RobLsqIO)
    val uncache = new UncacheWordIO
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val exceptionAddr = new ExceptionAddrIO
    val lqFull = Output(Bool())
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))
    val lq_rep_full = Output(Bool())
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val debugTopDown = new LoadQueueTopDownIO
  })

  val loadQueueRAR = Module(new LoadQueueRAR)  //  read-after-read violation
  val loadQueueRAW = Module(new LoadQueueRAW)  //  read-after-write violation
  val loadQueueReplay = Module(new LoadQueueReplay)  //  enqueue if need replay
  val virtualLoadQueue = Module(new VirtualLoadQueue)  //  control state
  val exceptionBuffer = Module(new LqExceptionBuffer) // exception buffer
  val uncacheBuffer = Module(new UncacheBuffer) // uncache buffer

  /**
   * LoadQueueRAR
   */
  loadQueueRAR.io.redirect <> io.redirect
  loadQueueRAR.io.release  <> io.release
  loadQueueRAR.io.ldWbPtr  <> virtualLoadQueue.io.ldWbPtr
  for (w <- 0 until LoadPipelineWidth) {
    loadQueueRAR.io.query(w).req    <> io.ldu.ldld_nuke_query(w).req // from load_s1
    loadQueueRAR.io.query(w).resp   <> io.ldu.ldld_nuke_query(w).resp // to load_s2
    loadQueueRAR.io.query(w).revoke := io.ldu.ldld_nuke_query(w).revoke // from load_s3
  }

  /**
   * LoadQueueRAW
   */
  loadQueueRAW.io.redirect         <> io.redirect
  loadQueueRAW.io.storeIn          <> io.sta.storeAddrIn
  loadQueueRAW.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueRAW.io.stIssuePtr       <> io.sq.stIssuePtr
  for (w <- 0 until LoadPipelineWidth) {
    loadQueueRAW.io.query(w).req    <> io.ldu.stld_nuke_query(w).req // from load_s1
    loadQueueRAW.io.query(w).resp   <> io.ldu.stld_nuke_query(w).resp // to load_s2
    loadQueueRAW.io.query(w).revoke := io.ldu.stld_nuke_query(w).revoke // from load_s3
  }

  /**
   * VirtualLoadQueue
   */
  virtualLoadQueue.io.redirect    <> io.redirect
  virtualLoadQueue.io.enq         <> io.enq
  virtualLoadQueue.io.ldin        <> io.ldu.ldin // from load_s3
  virtualLoadQueue.io.lqFull      <> io.lqFull
  virtualLoadQueue.io.lqDeq       <> io.lqDeq
  virtualLoadQueue.io.lqCancelCnt <> io.lqCancelCnt

  /**
   * Load queue exception buffer
   */
  exceptionBuffer.io.redirect <> io.redirect
  for ((buff, w) <- exceptionBuffer.io.req.zipWithIndex) {
    buff.valid := io.ldu.ldin(w).valid // from load_s3
    buff.bits := io.ldu.ldin(w).bits
  }
  io.exceptionAddr <> exceptionBuffer.io.exceptionAddr

  /**
   * Load uncache buffer
   */
  uncacheBuffer.io.redirect   <> io.redirect
  uncacheBuffer.io.ldout      <> io.ldout
  uncacheBuffer.io.ld_raw_data  <> io.ld_raw_data
  uncacheBuffer.io.rob        <> io.rob
  uncacheBuffer.io.uncache    <> io.uncache
  uncacheBuffer.io.trigger    <> io.trigger
  for ((buff, w) <- uncacheBuffer.io.req.zipWithIndex) {
    buff.valid := io.ldu.ldin(w).valid // from load_s3
    buff.bits := io.ldu.ldin(w).bits // from load_s3
  }

  // rollback
  def selectOldest[T <: Redirect](valid: Seq[Bool], bits: Seq[T]): (Seq[Bool], Seq[T]) = {
    assert(valid.length == bits.length)
    if (valid.length == 0 || valid.length == 1) {
      (valid, bits)
    } else if (valid.length == 2) {
      val res = Seq.fill(2)(Wire(ValidIO(chiselTypeOf(bits(0)))))
      for (i <- res.indices) {
        res(i).valid := valid(i)
        res(i).bits := bits(i)
      }
      val oldest = Mux(valid(0) && valid(1), Mux(isAfter(bits(0).robIdx, bits(1).robIdx), res(1), res(0)), Mux(valid(0) && !valid(1), res(0), res(1)))
      (Seq(oldest.valid), Seq(oldest.bits))
    } else {
      val left = selectOldest(valid.take(valid.length / 2), bits.take(bits.length / 2))
      val right = selectOldest(valid.takeRight(valid.length - (valid.length / 2)), bits.takeRight(bits.length - (bits.length / 2)))
      selectOldest(left._1 ++ right._1, left._2 ++ right._2)
    }
  }

  val (rollbackSelV, rollbackSelBits) = selectOldest(
                                          Seq(loadQueueRAW.io.rollback.valid, uncacheBuffer.io.rollback.valid),
                                          Seq(loadQueueRAW.io.rollback.bits, uncacheBuffer.io.rollback.bits)
                                        )
  io.rollback.valid := rollbackSelV.head
  io.rollback.bits := rollbackSelBits.head

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  /**
   * LoadQueueReplay
   */
  loadQueueReplay.io.redirect         <> io.redirect
  loadQueueReplay.io.enq              <> io.ldu.ldin // from load_s3
  loadQueueReplay.io.storeAddrIn      <> io.sta.storeAddrIn // from store_s1
  loadQueueReplay.io.storeDataIn      <> io.std.storeDataIn // from store_s0
  loadQueueReplay.io.replay           <> io.replay
  loadQueueReplay.io.refill           <> io.refill
  loadQueueReplay.io.tl_d_channel     <> io.tl_d_channel
  loadQueueReplay.io.stAddrReadySqPtr <> io.sq.stAddrReadySqPtr
  loadQueueReplay.io.stAddrReadyVec   <> io.sq.stAddrReadyVec
  loadQueueReplay.io.stDataReadySqPtr <> io.sq.stDataReadySqPtr
  loadQueueReplay.io.stDataReadyVec   <> io.sq.stDataReadyVec
  loadQueueReplay.io.sqEmpty          <> io.sq.sqEmpty
  loadQueueReplay.io.lqFull           <> io.lq_rep_full
  loadQueueReplay.io.ldWbPtr          <> virtualLoadQueue.io.ldWbPtr
  loadQueueReplay.io.rarFull          <> loadQueueRAR.io.lqFull
  loadQueueReplay.io.rawFull          <> loadQueueRAW.io.lqFull
  loadQueueReplay.io.l2_hint          <> io.l2_hint
  loadQueueReplay.io.tlbReplayDelayCycleCtrl <> io.tlbReplayDelayCycleCtrl

  loadQueueReplay.io.debugTopDown <> io.debugTopDown

  val full_mask = Cat(loadQueueRAR.io.lqFull, loadQueueRAW.io.lqFull, loadQueueReplay.io.lqFull)
  XSPerfAccumulate("full_mask_000", full_mask === 0.U)
  XSPerfAccumulate("full_mask_001", full_mask === 1.U)
  XSPerfAccumulate("full_mask_010", full_mask === 2.U)
  XSPerfAccumulate("full_mask_011", full_mask === 3.U)
  XSPerfAccumulate("full_mask_100", full_mask === 4.U)
  XSPerfAccumulate("full_mask_101", full_mask === 5.U)
  XSPerfAccumulate("full_mask_110", full_mask === 6.U)
  XSPerfAccumulate("full_mask_111", full_mask === 7.U)
  XSPerfAccumulate("rollback", io.rollback.valid)

  // perf cnt
  val perfEvents = Seq(virtualLoadQueue, loadQueueRAR, loadQueueRAW, loadQueueReplay).flatMap(_.getPerfEvents) ++
  Seq(
    ("full_mask_000", full_mask === 0.U),
    ("full_mask_001", full_mask === 1.U),
    ("full_mask_010", full_mask === 2.U),
    ("full_mask_011", full_mask === 3.U),
    ("full_mask_100", full_mask === 4.U),
    ("full_mask_101", full_mask === 5.U),
    ("full_mask_110", full_mask === 6.U),
    ("full_mask_111", full_mask === 7.U),
    ("rollback", io.rollback.valid)
  )
  generatePerfEvent()
  // end
}