package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.mem._
import xiangshan.backend.roq.RoqLsqIO

class ExceptionAddrIO(implicit p: Parameters) extends XSBundle {
  val lsIdx = Input(new LSIdx)
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
}

class FwdEntry extends Bundle {
  val valid = Bool()
  val data = UInt(8.W)
}

// inflight miss block reqs
class InflightBlockInfo(implicit p: Parameters) extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

class LsqEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val needAlloc = Vec(RenameWidth, Input(UInt(2.W)))
  val req = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
  val resp = Vec(RenameWidth, Output(new LSIdx))
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrappper(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle() {
    val enq = new LsqEnqIO
    val brqRedirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val loadDataForwarded = Vec(LoadPipelineWidth, Input(Bool()))
    val needReplayFromRS = Vec(LoadPipelineWidth, Input(Bool()))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback int load
    val mmioStout = DecoupledIO(new ExuOutput) // writeback uncached store
    val forward = Vec(LoadPipelineWidth, Flipped(new MaskedLoadForwardQueryIO))
    val roq = Flipped(new RoqLsqIO)
    val rollback = Output(Valid(new Redirect))
    val dcache = Flipped(ValidIO(new Refill))
    val uncache = new DCacheWordIO
    val exceptionAddr = new ExceptionAddrIO
    val sqempty = Output(Bool())
    val issuePtrExt = Output(new SqPtr)
    val storeIssue = Vec(StorePipelineWidth, Flipped(Valid(new ExuInput)))
    val sqFull = Output(Bool())
    val lqFull = Output(Bool())
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  // io.enq logic
  // LSQ: send out canAccept when both load queue and store queue are ready
  // Dispatch: send instructions to LSQ only when they are ready
  io.enq.canAccept := loadQueue.io.enq.canAccept && storeQueue.io.enq.canAccept
  loadQueue.io.enq.sqCanAccept := storeQueue.io.enq.canAccept
  storeQueue.io.enq.lqCanAccept := loadQueue.io.enq.canAccept
  for (i <- 0 until RenameWidth) {
    loadQueue.io.enq.needAlloc(i) := io.enq.needAlloc(i)(0)
    loadQueue.io.enq.req(i).valid := io.enq.needAlloc(i)(0) && io.enq.req(i).valid
    loadQueue.io.enq.req(i).bits  := io.enq.req(i).bits

    storeQueue.io.enq.needAlloc(i) := io.enq.needAlloc(i)(1)
    storeQueue.io.enq.req(i).valid := io.enq.needAlloc(i)(1) && io.enq.req(i).valid
    storeQueue.io.enq.req(i).bits  := io.enq.req(i).bits

    io.enq.resp(i).lqIdx := loadQueue.io.enq.resp(i)
    io.enq.resp(i).sqIdx := storeQueue.io.enq.resp(i)
  }

  // load queue wiring
  loadQueue.io.brqRedirect <> io.brqRedirect
  loadQueue.io.flush <> io.flush
  loadQueue.io.loadIn <> io.loadIn
  loadQueue.io.storeIn <> io.storeIn
  loadQueue.io.loadDataForwarded <> io.loadDataForwarded
  loadQueue.io.needReplayFromRS <> io.needReplayFromRS
  loadQueue.io.ldout <> io.ldout
  loadQueue.io.roq <> io.roq
  loadQueue.io.rollback <> io.rollback
  loadQueue.io.dcache <> io.dcache
  loadQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  loadQueue.io.exceptionAddr.isStore := DontCare

  // store queue wiring
  // storeQueue.io <> DontCare
  storeQueue.io.brqRedirect <> io.brqRedirect
  storeQueue.io.flush <> io.flush
  storeQueue.io.storeIn <> io.storeIn
  storeQueue.io.sbuffer <> io.sbuffer
  storeQueue.io.mmioStout <> io.mmioStout
  storeQueue.io.roq <> io.roq
  storeQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  storeQueue.io.exceptionAddr.isStore := DontCare
  storeQueue.io.issuePtrExt <> io.issuePtrExt
  storeQueue.io.storeIssue <> io.storeIssue

  loadQueue.io.load_s1 <> io.forward
  storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE

  storeQueue.io.sqempty <> io.sqempty

  io.exceptionAddr.vaddr := Mux(io.exceptionAddr.isStore, storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)

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
}
