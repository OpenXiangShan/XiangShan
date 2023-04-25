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
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.cache.mmu.{TlbRequestIO}
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.backend.rob.RobLsqIO

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
}

class FwdEntry(implicit p: Parameters) extends XSBundle {
  val validFast = Bool() // validFast is generated the same cycle with query
  val valid = Bool() // valid is generated 1 cycle after query request
  val data = UInt(8.W) // data is generated 1 cycle after query request
}

// inflight miss block reqs
class InflightBlockInfo(implicit p: Parameters) extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

class LsqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(UInt(2.W)))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new LSIdx))
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrapper(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val enq = new LsqEnqIO
    val ldu = new Bundle() {
        val s2 = new Bundle() {
          val storeLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
          val loadLoadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
          val replay = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
        }
      val s3 = new Bundle() {
        val loadIn = Vec(StorePipelineWidth, Flipped(Valid(new LqWriteBundle)))
      }
    }
    val sta = new Bundle() {
      val s0 = new Bundle() {
        val storeMaskIn = Vec(StorePipelineWidth, Flipped(Valid(new StoreMaskBundle))) // store mask, send to sq from rs
      }
      val s1 = new Bundle() {
        val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
      }
      val s2 = new Bundle() {
        val storeAddrInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle()))
      }
    }
    val std = new Bundle() {
      val s0 = new Bundle() {
        val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput))) // store data, send to sq from rs
      }
    }
    val loadOut = Vec(LoadPipelineWidth, DecoupledIO(new ExuOutput))
    val ldRawDataOut = Vec(LoadPipelineWidth, Output(new LoadDataFromLQBundle))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle)) 
    val sbuffer = Vec(EnsbufferWidth, Decoupled(new DCacheWordReqWithVaddr)) 
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val rob = Flipped(new RobLsqIO)
    val rollback = Output(Valid(new Redirect))
    val release = Flipped(Valid(new Release))
    val refill = Flipped(Valid(new Refill))
    val uncacheOutstanding = Input(Bool())
    val uncache = new UncacheWordIO
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val sqEmpty = Output(Bool())
    val lqReplayFull = Output(Bool())
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
    val lqReplayCanAccept = Output(Vec(LoadPipelineWidth, Bool()))
    val sqCancelCnt = Output(UInt(log2Up(StoreQueueSize+1).W))
    val lqCancelCnt = Output(UInt(log2Up(LoadQueueFlagSize+1).W))
    val lqDeq = Output(UInt(log2Up(CommitWidth + 1).W))
    val sqDeq = Output(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val exceptionAddr = new ExceptionAddrIO
    val trigger = Vec(LoadPipelineWidth, new LqTriggerIO)
    val issuePtrExt = Output(new SqPtr)
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  storeQueue.io.hartId := io.hartId
  storeQueue.io.uncacheOutstanding := io.uncacheOutstanding
  

  dontTouch(loadQueue.io.tlbReplayDelayCycleCtrl)
  val tlbReplayDelayCycleCtrl = WireInit(VecInit(Seq(15.U(ReSelectLen.W), 0.U(ReSelectLen.W), 126.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  loadQueue.io.tlbReplayDelayCycleCtrl := tlbReplayDelayCycleCtrl

  // io.enq logic
  // LSQ: send out canAccept when both load queue and store queue are ready
  // Dispatch: send instructions to LSQ only when they are ready
  io.enq.canAccept := loadQueue.io.enq.canAccept && storeQueue.io.enq.canAccept
  loadQueue.io.enq.sqCanAccept := storeQueue.io.enq.canAccept
  storeQueue.io.enq.lqCanAccept := loadQueue.io.enq.canAccept
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
  storeQueue.io.storeAddrIn <> io.sta.s1.storeAddrIn
  storeQueue.io.storeAddrInRe <> io.sta.s2.storeAddrInRe
  storeQueue.io.storeDataIn <> io.std.s0.storeDataIn
  storeQueue.io.storeMaskIn <> io.sta.s0.storeMaskIn
  storeQueue.io.sbuffer <> io.sbuffer
  storeQueue.io.mmioStout <> io.mmioStout
  storeQueue.io.rob <> io.rob
  storeQueue.io.exceptionAddr.isStore := DontCare
  storeQueue.io.sqCancelCnt <> io.sqCancelCnt
  storeQueue.io.sqDeq <> io.sqDeq
  storeQueue.io.sqEmpty <> io.sqEmpty
  storeQueue.io.sqFull <> io.sqFull
  storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE
  storeQueue.io.sqEmpty <> io.sqEmpty

  // <------- DANGEROUS: Don't change sequence here ! ------->

  //  load queue wiring
  loadQueue.io.redirect <> io.brqRedirect
  loadQueue.io.ldu <> io.ldu
  loadQueue.io.loadOut <> io.loadOut
  loadQueue.io.ldRawDataOut <> io.ldRawDataOut
  loadQueue.io.rob <> io.rob
  loadQueue.io.rollback <> io.rollback
  loadQueue.io.replay <> io.replay
  loadQueue.io.refill <> io.refill
  loadQueue.io.release <> io.release
  loadQueue.io.trigger <> io.trigger
  loadQueue.io.exceptionAddr.isStore := DontCare
  loadQueue.io.lqCancelCnt <> io.lqCancelCnt 
  loadQueue.io.sq.stAddrReadySqPtr <> storeQueue.io.stAddrReadySqPtr
  loadQueue.io.sq.stAddrReadyVec <> storeQueue.io.stAddrReadyVec
  loadQueue.io.sq.stDataReadySqPtr <> storeQueue.io.stDataReadySqPtr
  loadQueue.io.sq.stDataReadyVec <> storeQueue.io.stDataReadyVec
  loadQueue.io.sq.stIssuePtr <> storeQueue.io.stIssuePtr
  loadQueue.io.sq.sqEmpty <> storeQueue.io.sqEmpty
  loadQueue.io.sta.s1.storeAddrIn <> io.sta.s1.storeAddrIn
  loadQueue.io.std.s0.storeDataIn <> io.std.s0.storeDataIn
  loadQueue.io.lqFlagFull <> io.lqFull
  loadQueue.io.lqReplayFull <> io.lqReplayFull
  loadQueue.io.lqReplayCanAccept <> io.lqReplayCanAccept
  loadQueue.io.lqDeq <> io.lqDeq

  // rob commits for lsq is delayed for two cycles, which causes the delayed update for deqPtr in lq/sq
  // s0: commit
  // s1:               exception find
  // s2:               exception triggered
  // s3: ptr updated & new address
  // address will be used at the next cycle after exception is triggered
  io.exceptionAddr.vaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)
  io.issuePtrExt := storeQueue.io.stAddrReadySqPtr

  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val pendingstate = RegInit(s_idle)

  switch(pendingstate){
    is(s_idle){
      when(io.uncache.req.fire() && !io.uncacheOutstanding){
        pendingstate := Mux(loadQueue.io.uncache.req.valid, s_load, 
                          Mux(io.uncacheOutstanding, s_idle, s_store))
      }
    }
    is(s_load){
      when(io.uncache.resp.fire()){
        pendingstate := s_idle
      }
    }
    is(s_store){
      when(io.uncache.resp.fire()){
        pendingstate := s_idle
      }
    }
  }

  loadQueue.io.uncache := DontCare
  storeQueue.io.uncache := DontCare
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
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueFlagSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val enqLsq = Flipped(new LsqEnqIO)
  })

  val lqPtr = RegInit(0.U.asTypeOf(new LqPtr))
  val sqPtr = RegInit(0.U.asTypeOf(new SqPtr))
  val lqCounter = RegInit(LoadQueueFlagSize.U(log2Up(LoadQueueFlagSize + 1).W))
  val sqCounter = RegInit(StoreQueueSize.U(log2Up(StoreQueueSize + 1).W))
  val canAccept = RegInit(false.B)

  val loadEnqNumber = PopCount(io.enq.req.zip(io.enq.needAlloc).map(x => x._1.valid && x._2(0)))
  val storeEnqNumber = PopCount(io.enq.req.zip(io.enq.needAlloc).map(x => x._1.valid && x._2(1)))

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
    lqPtr := lqPtr + loadEnqNumber
    lqCounter := lqCounter + io.lcommit - loadEnqNumber
    sqPtr := sqPtr + storeEnqNumber
    sqCounter := sqCounter + io.scommit - storeEnqNumber
  }.otherwise {
    lqCounter := lqCounter + io.lcommit
    sqCounter := sqCounter + io.scommit
  }


  val maxAllocate = Seq(exuParameters.LduCnt, exuParameters.StuCnt).max
  val ldCanAccept = lqCounter >= loadEnqNumber +& maxAllocate.U
  val sqCanAccept = sqCounter >= storeEnqNumber +& maxAllocate.U
  // It is possible that t3_update and enq are true at the same clock cycle.
  // For example, if redirect.valid lasts more than one clock cycle,
  // after the last redirect, new instructions may enter but previously redirect
  // has not been resolved (updated according to the cancel count from LSQ).
  // To solve the issue easily, we block enqueue when t3_update, which is RegNext(t2_update).
  io.enq.canAccept := RegNext(ldCanAccept && sqCanAccept && !t2_update)
  val lqOffset = Wire(Vec(io.enq.resp.length, UInt(log2Up(maxAllocate + 1).W)))
  val sqOffset = Wire(Vec(io.enq.resp.length, UInt(log2Up(maxAllocate + 1).W)))
  for ((resp, i) <- io.enq.resp.zipWithIndex) {
    lqOffset(i) := PopCount(io.enq.needAlloc.take(i).map(a => a(0)))
    resp.lqIdx := lqPtr + lqOffset(i)
    sqOffset(i) := PopCount(io.enq.needAlloc.take(i).map(a => a(1)))
    resp.sqIdx := sqPtr + sqOffset(i)
  }

  io.enqLsq.needAlloc := RegNext(io.enq.needAlloc)
  io.enqLsq.req.zip(io.enq.req).zip(io.enq.resp).foreach{ case ((toLsq, enq), resp) =>
    val do_enq = enq.valid && !io.redirect.valid && io.enq.canAccept
    toLsq.valid := RegNext(do_enq)
    toLsq.bits := RegEnable(enq.bits, do_enq)
    toLsq.bits.lqIdx := RegEnable(resp.lqIdx, do_enq)
    toLsq.bits.sqIdx := RegEnable(resp.sqIdx, do_enq)
  }

}