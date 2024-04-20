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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.cache.mmu.{TlbRequestIO, TlbHintIO}
import xiangshan.mem._
import xiangshan.backend._
import xiangshan.backend.rob.RobLsqIO

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
  val gpaddr = Output(UInt(GPAddrBits.W))
}

class FwdEntry extends Bundle {
  val validFast = Bool() // validFast is generated the same cycle with query
  val valid = Bool() // valid is generated 1 cycle after query request
  val data = UInt(8.W) // data is generated 1 cycle after query request
}

// inflight miss block reqs
class InflightBlockInfo(implicit p: Parameters) extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

class LsqEnqIO(implicit p: Parameters) extends MemBlockBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(LSQEnqWidth, Input(UInt(2.W)))
  val req       = Vec(LSQEnqWidth, Flipped(ValidIO(new DynInst)))
  val resp      = Vec(LSQEnqWidth, Output(new LSIdx))
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrapper(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(hartIdLen.W))
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val enq = new LsqEnqIO
    val ldu = new Bundle() {
        val stld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldld_nuke_query = Vec(LoadPipelineWidth, Flipped(new LoadNukeQueryIO)) // from load_s2
        val ldin = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle))) // from load_s3
    }
    val sta = new Bundle() {
      val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // from store_s0, store mask, send to sq from rs
      val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // from store_s1
      val storeAddrInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle())) // from store_s2
      val vecStoreAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) //from store_s2
      val vecStoreFlowAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle))) // from vsFlowQueue last element issue
    }
    val std = new Bundle() {
      val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput))) // from store_s0, store data, send to sq from rs
    }
    val ldout = Vec(LoadPipelineWidth, DecoupledIO(new MemExuOutput))
    val ld_raw_data = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val sbuffer = Vec(EnsbufferWidth, Decoupled(new DCacheWordReqWithVaddrAndPfFlag))
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val rob = Flipped(new RobLsqIO)
    val nuke_rollback = Output(Valid(new Redirect))
    val nack_rollback = Output(Valid(new Redirect))
    val release = Flipped(Valid(new Release))
   // val refill = Flipped(Valid(new Refill))
    val tl_d_channel  = Input(new DcacheToLduForwardIO)
    val uncacheOutstanding = Input(Bool())
    val uncache = new UncacheWordIO
    val mmioStout = DecoupledIO(new MemExuOutput) // writeback uncached store
    val sqEmpty = Output(Bool())
    val lq_rep_full = Output(Bool())
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize+1).W))
    val lqCancelCnt = Output(UInt(log2Up(VirtualLoadQueueSize+1).W))
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val lqCanAccept = Output(Bool())
    val sqCanAccept = Output(Bool())
    val lqDeqPtr = Output(new LqPtr)
    val sqDeqPtr = Output(new SqPtr)
    val exceptionAddr = new ExceptionAddrIO
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val issuePtrExt = Output(new SqPtr)
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val tlb_hint = Flipped(new TlbHintIO)
    val force_write = Output(Bool())
    val lqEmpty = Output(Bool())

    // vector
    val vecWriteback = Flipped(ValidIO(new MemExuOutput(isVector = true)))
    val vecStoreRetire = Flipped(ValidIO(new SqPtr))
    val vecMMIOReplay = Vec(VecLoadPipelineWidth, DecoupledIO(new LsPipelineBundle()))

    // top-down
    val debugTopDown = new LoadQueueTopDownIO
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  storeQueue.io.hartId := io.hartId
  storeQueue.io.uncacheOutstanding := io.uncacheOutstanding


  dontTouch(loadQueue.io.tlbReplayDelayCycleCtrl)
  // Todo: imm
  val tlbReplayDelayCycleCtrl = WireInit(VecInit(Seq(14.U(ReSelectLen.W), 0.U(ReSelectLen.W), 125.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  loadQueue.io.tlbReplayDelayCycleCtrl := tlbReplayDelayCycleCtrl

  // io.enq logic
  // LSQ: send out canAccept when both load queue and store queue are ready
  // Dispatch: send instructions to LSQ only when they are ready
  io.enq.canAccept := loadQueue.io.enq.canAccept && storeQueue.io.enq.canAccept
  io.lqCanAccept := loadQueue.io.enq.canAccept
  io.sqCanAccept := storeQueue.io.enq.canAccept
  loadQueue.io.enq.sqCanAccept := storeQueue.io.enq.canAccept
  storeQueue.io.enq.lqCanAccept := loadQueue.io.enq.canAccept
  io.lqDeqPtr := loadQueue.io.lqDeqPtr
  io.sqDeqPtr := storeQueue.io.sqDeqPtr
  for (i <- io.enq.req.indices) {
    loadQueue.io.enq.needAlloc(i)      := io.enq.needAlloc(i)(0)
    loadQueue.io.enq.req(i).valid      := io.enq.needAlloc(i)(0) && io.enq.req(i).valid
    loadQueue.io.enq.req(i).bits       := io.enq.req(i).bits
    loadQueue.io.enq.req(i).bits.sqIdx := storeQueue.io.enq.resp(i)

    storeQueue.io.enq.needAlloc(i)      := io.enq.needAlloc(i)(1)
    storeQueue.io.enq.req(i).valid      := io.enq.needAlloc(i)(1) && io.enq.req(i).valid
    storeQueue.io.enq.req(i).bits       := io.enq.req(i).bits
    storeQueue.io.enq.req(i).bits       := io.enq.req(i).bits
    storeQueue.io.enq.req(i).bits.lqIdx := loadQueue.io.enq.resp(i)

    io.enq.resp(i).lqIdx := loadQueue.io.enq.resp(i)
    io.enq.resp(i).sqIdx := storeQueue.io.enq.resp(i)
  }

  // store queue wiring
  storeQueue.io.brqRedirect <> io.brqRedirect
  storeQueue.io.storeAddrIn <> io.sta.storeAddrIn // from store_s1
  storeQueue.io.vecStoreAddrIn  <> io.sta.vecStoreFlowAddrIn // from VsFlowQueue inactivative element isuue
  storeQueue.io.storeAddrInRe <> io.sta.storeAddrInRe // from store_s2
  storeQueue.io.storeDataIn <> io.std.storeDataIn // from store_s0
  storeQueue.io.storeMaskIn <> io.sta.storeMaskIn // from store_s0
  storeQueue.io.sbuffer     <> io.sbuffer
  storeQueue.io.mmioStout   <> io.mmioStout
  storeQueue.io.rob         <> io.rob
  storeQueue.io.exceptionAddr.isStore := DontCare
  storeQueue.io.sqCancelCnt <> io.sqCancelCnt
  storeQueue.io.sqDeq       <> io.sqDeq
  storeQueue.io.sqEmpty     <> io.sqEmpty
  storeQueue.io.sqFull      <> io.sqFull
  storeQueue.io.forward     <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE
  storeQueue.io.force_write <> io.force_write
  storeQueue.io.vecStoreRetire <> io.vecStoreRetire

  /* <------- DANGEROUS: Don't change sequence here ! -------> */

  //  load queue wiring
  loadQueue.io.redirect            <> io.brqRedirect
  loadQueue.io.ldu                 <> io.ldu
  loadQueue.io.ldout               <> io.ldout
  loadQueue.io.ld_raw_data         <> io.ld_raw_data
  loadQueue.io.rob                 <> io.rob
  loadQueue.io.nuke_rollback       <> io.nuke_rollback
  loadQueue.io.nack_rollback       <> io.nack_rollback
  loadQueue.io.replay              <> io.replay
 // loadQueue.io.refill              <> io.refill
  loadQueue.io.tl_d_channel        <> io.tl_d_channel
  loadQueue.io.release             <> io.release
  loadQueue.io.trigger             <> io.trigger
  loadQueue.io.exceptionAddr.isStore := DontCare
  loadQueue.io.lqCancelCnt         <> io.lqCancelCnt
  loadQueue.io.sq.stAddrReadySqPtr <> storeQueue.io.stAddrReadySqPtr
  loadQueue.io.sq.stAddrReadyVec   <> storeQueue.io.stAddrReadyVec
  loadQueue.io.sq.stDataReadySqPtr <> storeQueue.io.stDataReadySqPtr
  loadQueue.io.sq.stDataReadyVec   <> storeQueue.io.stDataReadyVec
  loadQueue.io.sq.stIssuePtr       <> storeQueue.io.stIssuePtr
  loadQueue.io.sq.sqEmpty          <> storeQueue.io.sqEmpty
  loadQueue.io.sta.storeAddrIn     <> io.sta.storeAddrIn // store_s1
  loadQueue.io.sta.vecStoreAddrIn  <> io.sta.vecStoreAddrIn // store_s1
  loadQueue.io.std.storeDataIn     <> io.std.storeDataIn // store_s0
  loadQueue.io.lqFull              <> io.lqFull
  loadQueue.io.lq_rep_full         <> io.lq_rep_full
  loadQueue.io.lqDeq               <> io.lqDeq
  loadQueue.io.l2_hint             <> io.l2_hint
  loadQueue.io.tlb_hint            <> io.tlb_hint
  loadQueue.io.lqEmpty             <> io.lqEmpty
  loadQueue.io.vecWriteback        <> io.vecWriteback
  loadQueue.io.vecMMIOReplay       <> io.vecMMIOReplay

  // rob commits for lsq is delayed for two cycles, which causes the delayed update for deqPtr in lq/sq
  // s0: commit
  // s1:               exception find
  // s2:               exception triggered
  // s3: ptr updated & new address
  // address will be used at the next cycle after exception is triggered
  io.exceptionAddr.vaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)
  io.exceptionAddr.gpaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.gpaddr, loadQueue.io.exceptionAddr.gpaddr)
  io.issuePtrExt := storeQueue.io.stAddrReadySqPtr

  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val pendingstate = RegInit(s_idle)

  switch(pendingstate){
    is(s_idle){
      when(io.uncache.req.fire){
        pendingstate := Mux(loadQueue.io.uncache.req.valid, s_load,
                          Mux(io.uncacheOutstanding, s_idle, s_store))
      }
    }
    is(s_load){
      when(io.uncache.resp.fire){
        pendingstate := s_idle
      }
    }
    is(s_store){
      when(io.uncache.resp.fire){
        pendingstate := s_idle
      }
    }
  }

  loadQueue.io.uncache := DontCare
  storeQueue.io.uncache := DontCare
  loadQueue.io.uncache.req.ready := false.B
  storeQueue.io.uncache.req.ready := false.B
  loadQueue.io.uncache.resp.valid := false.B
  storeQueue.io.uncache.resp.valid := false.B
  when(loadQueue.io.uncache.req.valid){
    io.uncache.req <> loadQueue.io.uncache.req
  }.otherwise{
    io.uncache.req <> storeQueue.io.uncache.req
  }
  when (io.uncacheOutstanding) {
    io.uncache.resp <> loadQueue.io.uncache.resp
  } .otherwise {
    when(pendingstate === s_load){
      io.uncache.resp <> loadQueue.io.uncache.resp
    }.otherwise{
      io.uncache.resp <> storeQueue.io.uncache.resp
    }
  }

  loadQueue.io.debugTopDown <> io.debugTopDown

  assert(!(loadQueue.io.uncache.req.valid && storeQueue.io.uncache.req.valid))
  assert(!(loadQueue.io.uncache.resp.valid && storeQueue.io.uncache.resp.valid))
  when (!io.uncacheOutstanding) {
    assert(!((loadQueue.io.uncache.resp.valid || storeQueue.io.uncache.resp.valid) && pendingstate === s_idle))
  }


  val perfEvents = Seq(loadQueue, storeQueue).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class LsqEnqCtrl(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    // to dispatch
    val enq = new LsqEnqIO
    // from `memBlock.io.lqDeq
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    // from `memBlock.io.sqDeq`
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
    // from/tp lsq
    val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val enqLsq = Flipped(new LsqEnqIO)
  })

  val lqPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val sqPtr = RegInit(0.U.asTypeOf(new SqPtr))
  val lqCounter = RegInit(VirtualLoadQueueSize.U(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCounter = RegInit(StoreQueueSize.U(log2Up(StoreQueueSize + 1).W))
  val canAccept = RegInit(false.B)

  val loadEnqVec = io.enq.req.zip(io.enq.needAlloc).map(x => x._1.valid && x._2(0))
  val storeEnqVec = io.enq.req.zip(io.enq.needAlloc).map(x => x._1.valid && x._2(1))
  val loadEnqNumber = PopCount(loadEnqVec)
  val storeEnqNumber = PopCount(storeEnqVec)
  val isLastUopVec = io.enq.req.map(_.bits.lastUop)
  val lqAllocNumber = PopCount(loadEnqVec.zip(isLastUopVec).map(x => x._1 && x._2))
  val sqAllocNumber = PopCount(storeEnqVec.zip(isLastUopVec).map(x => x._1 && x._2))

  // How to update ptr and counter:
  // (1) by default, updated according to enq/commit
  // (2) when redirect and dispatch queue is empty, update according to lsq
  val t1_redirect = RegNext(io.redirect.valid)
  val t2_redirect = RegNext(t1_redirect)
  val t2_update = t2_redirect && !VecInit(io.enq.needAlloc.map(_.orR)).asUInt.orR
  val t3_update = RegNext(t2_update)
  val t3_lqCancelCnt = RegNext(io.lqCancelCnt)
  val t3_sqCancelCnt = RegNext(io.sqCancelCnt)
  when (t3_update) {
    lqPtr := lqPtr - t3_lqCancelCnt
    lqCounter := lqCounter + io.lcommit + t3_lqCancelCnt
    sqPtr := sqPtr - t3_sqCancelCnt
    sqCounter := sqCounter + io.scommit + t3_sqCancelCnt
  }.elsewhen (!io.redirect.valid && io.enq.canAccept) {
    lqPtr := lqPtr + lqAllocNumber
    lqCounter := lqCounter + io.lcommit - lqAllocNumber
    sqPtr := sqPtr + sqAllocNumber
    sqCounter := sqCounter + io.scommit - sqAllocNumber
  }.otherwise {
    lqCounter := lqCounter + io.lcommit
    sqCounter := sqCounter + io.scommit
  }


  val lqMaxAllocate = LSQLdEnqWidth
  val sqMaxAllocate = LSQStEnqWidth
  val maxAllocate = lqMaxAllocate max sqMaxAllocate
  val ldCanAccept = lqCounter >= lqAllocNumber +& lqMaxAllocate.U
  val sqCanAccept = sqCounter >= sqAllocNumber +& sqMaxAllocate.U
  // It is possible that t3_update and enq are true at the same clock cycle.
  // For example, if redirect.valid lasts more than one clock cycle,
  // after the last redirect, new instructions may enter but previously redirect
  // has not been resolved (updated according to the cancel count from LSQ).
  // To solve the issue easily, we block enqueue when t3_update, which is RegNext(t2_update).
  io.enq.canAccept := RegNext(ldCanAccept && sqCanAccept && !t2_update)
  val lqOffset = Wire(Vec(io.enq.resp.length, UInt(log2Up(maxAllocate + 1).W)))
  val sqOffset = Wire(Vec(io.enq.resp.length, UInt(log2Up(maxAllocate + 1).W)))
  for ((resp, i) <- io.enq.resp.zipWithIndex) {
    lqOffset(i) := PopCount(io.enq.needAlloc.zip(isLastUopVec).take(i).map(x => x._1(0) && x._2))
    resp.lqIdx := lqPtr + lqOffset(i)
    sqOffset(i) := PopCount(io.enq.needAlloc.zip(isLastUopVec).take(i).map(x => x._1(1) && x._2))
    resp.sqIdx := sqPtr + sqOffset(i)
  }

  io.enqLsq.needAlloc := RegNext(VecInit(io.enq.needAlloc.zip(io.enq.req).map(x => x._1 & Fill(2, x._2.bits.lastUop))))
  io.enqLsq.req.zip(io.enq.req).zip(io.enq.resp).foreach{ case ((toLsq, enq), resp) =>
    val do_enq = enq.valid && !io.redirect.valid && io.enq.canAccept && enq.bits.lastUop
    toLsq.valid := RegNext(do_enq)
    toLsq.bits := RegEnable(enq.bits, do_enq)
    toLsq.bits.lqIdx := RegEnable(resp.lqIdx, do_enq)
    toLsq.bits.sqIdx := RegEnable(resp.sqIdx, do_enq)
  }

}