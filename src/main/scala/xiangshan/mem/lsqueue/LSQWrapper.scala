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
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, MemoryOpConstants}
import xiangshan.cache.mmu.{TlbRequestIO}
import xiangshan.mem._
import xiangshan.backend.rob.RobLsqIO

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
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

class LsqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(exuParameters.LsExuCnt, Input(UInt(2.W)))
  val req = Vec(exuParameters.LsExuCnt, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(exuParameters.LsExuCnt, Output(new LSIdx))
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrappper(implicit p: Parameters) extends XSModule with HasDCacheParameters with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    val enq = new LsqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeInRe = Vec(StorePipelineWidth, Input(new LsPipelineBundle()))
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput))) // store data, send to sq from rs
    val loadDataForwarded = Vec(LoadPipelineWidth, Input(Bool()))
    val dcacheRequireReplay = Vec(LoadPipelineWidth, Input(Bool()))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReqWithVaddr))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback int load
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new PipeLoadForwardQueryIO))
    val loadViolationQuery = Vec(LoadPipelineWidth, Flipped(new LoadViolationQueryIO))
    val rob = Flipped(new RobLsqIO)
    val rollback = Output(Valid(new Redirect))
    val dcache = Flipped(ValidIO(new Refill))
    val release = Flipped(ValidIO(new Release))
    val uncache = new DCacheWordIO
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr)
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  storeQueue.io.hartId := io.hartId

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

  // load queue wiring
  loadQueue.io.brqRedirect <> io.brqRedirect
  loadQueue.io.loadIn <> io.loadIn
  loadQueue.io.storeIn <> io.storeIn
  loadQueue.io.loadDataForwarded <> io.loadDataForwarded
  loadQueue.io.dcacheRequireReplay <> io.dcacheRequireReplay
  loadQueue.io.ldout <> io.ldout
  loadQueue.io.rob <> io.rob
  loadQueue.io.rollback <> io.rollback
  loadQueue.io.dcache <> io.dcache
  loadQueue.io.release <> io.release
  loadQueue.io.exceptionAddr.isStore := DontCare

  // store queue wiring
  // storeQueue.io <> DontCare
  storeQueue.io.brqRedirect <> io.brqRedirect
  storeQueue.io.storeIn <> io.storeIn
  storeQueue.io.storeInRe <> io.storeInRe
  storeQueue.io.storeDataIn <> io.storeDataIn
  storeQueue.io.sbuffer <> io.sbuffer
  storeQueue.io.mmioStout <> io.mmioStout
  storeQueue.io.rob <> io.rob
  storeQueue.io.exceptionAddr.isStore := DontCare
  storeQueue.io.issuePtrExt <> io.issuePtrExt

  loadQueue.io.load_s1 <> io.forward
  storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE

  loadQueue.io.loadViolationQuery <> io.loadViolationQuery

  storeQueue.io.sqempty <> io.sqempty

  // rob commits for lsq is delayed for two cycles, which causes the delayed update for deqPtr in lq/sq
  // s0: commit
  // s1:               exception find
  // s2:               exception triggered
  // s3: ptr updated & new address
  // address will be used at the next cycle after exception is triggered
  io.exceptionAddr.vaddr := Mux(RegNext(io.exceptionAddr.isStore), storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)

  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val pendingstate = RegInit(s_idle)

  switch(pendingstate){
    is(s_idle){
      when(io.uncache.req.fire()){
        pendingstate := Mux(loadQueue.io.uncache.req.valid, s_load, s_store)
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
  when(pendingstate === s_load){
    io.uncache.resp <> loadQueue.io.uncache.resp
  }.otherwise{
    io.uncache.resp <> storeQueue.io.uncache.resp
  }

  assert(!(loadQueue.io.uncache.req.valid && storeQueue.io.uncache.req.valid))
  assert(!(loadQueue.io.uncache.resp.valid && storeQueue.io.uncache.resp.valid))
  assert(!((loadQueue.io.uncache.resp.valid || storeQueue.io.uncache.resp.valid) && pendingstate === s_idle))

  io.lqFull := loadQueue.io.lqFull
  io.sqFull := storeQueue.io.sqFull

  val perfEvents = Seq(loadQueue, storeQueue).flatMap(_.getPerfEvents)
  generatePerfEvent()
}
