package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._
import xiangshan.backend.roq.RoqPtr

class ExceptionAddrIO extends XSBundle {
  val lsIdx = Input(new LSIdx)
  val isStore = Input(Bool())
  val vaddr = Output(UInt(VAddrBits.W))
}


class LsqEntry extends XSBundle {
  val vaddr = UInt(VAddrBits.W) // TODO: need opt
  val paddr = UInt(PAddrBits.W)
  val op = UInt(6.W)
  val mask = UInt(8.W)
  val data = UInt(XLEN.W)
  val exception = UInt(16.W) // TODO: opt size
  val mmio = Bool()
  val fwdMask = Vec(8, Bool())
  val fwdData = Vec(8, UInt(8.W))
}

// inflight miss block reqs
class InflightBlockInfo extends XSBundle {
  val block_addr = UInt(PAddrBits.W)
  val valid = Bool()
}

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
class LsqWrappper extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle() {
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val lsIdxs = Output(Vec(RenameWidth, new LSIdx))
    val brqRedirect = Input(Valid(new Redirect))
    val loadIn = Vec(LoadPipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val storeIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))
    val sbuffer = Vec(StorePipelineWidth, Decoupled(new DCacheWordReq))
    val ldout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val stout = Vec(2, DecoupledIO(new ExuOutput)) // writeback store
    val forward = Vec(LoadPipelineWidth, Flipped(new LoadForwardQueryIO))
    val commits = Flipped(Vec(CommitWidth, Valid(new RoqCommit)))
    val rollback = Output(Valid(new Redirect))
    val dcache = new DCacheLineIO
    val uncache = new DCacheWordIO
    val roqDeqPtr = Input(new RoqPtr)
    val oldestStore = Output(Valid(new RoqPtr))
    val exceptionAddr = new ExceptionAddrIO
  })

  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  // load queue wiring
  loadQueue.io.dp1Req <> io.dp1Req
  loadQueue.io.brqRedirect <> io.brqRedirect
  loadQueue.io.loadIn <> io.loadIn
  loadQueue.io.storeIn <> io.storeIn
  loadQueue.io.ldout <> io.ldout
  loadQueue.io.commits <> io.commits
  loadQueue.io.rollback <> io.rollback
  loadQueue.io.dcache <> io.dcache
  loadQueue.io.roqDeqPtr <> io.roqDeqPtr
  loadQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  loadQueue.io.exceptionAddr.isStore := DontCare

  // store queue wiring
  // storeQueue.io <> DontCare
  storeQueue.io.dp1Req <> io.dp1Req
  storeQueue.io.brqRedirect <> io.brqRedirect
  storeQueue.io.storeIn <> io.storeIn
  storeQueue.io.sbuffer <> io.sbuffer
  storeQueue.io.stout <> io.stout
  storeQueue.io.commits <> io.commits
  storeQueue.io.roqDeqPtr <> io.roqDeqPtr
  storeQueue.io.oldestStore <> io.oldestStore
  storeQueue.io.exceptionAddr.lsIdx := io.exceptionAddr.lsIdx
  storeQueue.io.exceptionAddr.isStore := DontCare

  loadQueue.io.forward <> io.forward
  storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE

  io.exceptionAddr.vaddr := Mux(io.exceptionAddr.isStore, storeQueue.io.exceptionAddr.vaddr, loadQueue.io.exceptionAddr.vaddr)

  // naive uncache arbiter
  val s_idle :: s_load :: s_store :: Nil = Enum(3)
  val uncacheState = RegInit(s_idle)

  switch(uncacheState){
    is(s_idle){
      when(io.uncache.req.fire()){
        uncacheState := Mux(loadQueue.io.uncache.req.valid, s_load, s_store)
      }
    }
    is(s_load){
      when(io.uncache.resp.fire()){
        uncacheState := s_idle
      }
    }
    is(s_store){
      when(io.uncache.resp.fire()){
        uncacheState := s_idle
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
  when(uncacheState === s_load){
    io.uncache.resp <> loadQueue.io.uncache.resp
  }.otherwise{
    io.uncache.resp <> storeQueue.io.uncache.resp
  }

  assert(!(loadQueue.io.uncache.req.valid && storeQueue.io.uncache.req.valid))
  assert(!(loadQueue.io.uncache.resp.valid && storeQueue.io.uncache.resp.valid))
  assert(!((loadQueue.io.uncache.resp.valid || storeQueue.io.uncache.resp.valid) && uncacheState === s_idle))

  // fix valid, allocate lq / sq index
  (0 until RenameWidth).map(i => {
    val isStore = CommitType.lsInstIsStore(io.dp1Req(i).bits.ctrl.commitType)
    val prevCanIn = if (i == 0) true.B else Cat((0 until i).map(i => io.dp1Req(i).ready)).andR
    loadQueue.io.dp1Req(i).valid := !isStore && io.dp1Req(i).valid && prevCanIn
    storeQueue.io.dp1Req(i).valid := isStore && io.dp1Req(i).valid && prevCanIn
    loadQueue.io.lqIdxs(i) <> io.lsIdxs(i).lqIdx
    storeQueue.io.sqIdxs(i) <> io.lsIdxs(i).sqIdx
    io.dp1Req(i).ready := storeQueue.io.dp1Req(i).ready && loadQueue.io.dp1Req(i).ready
  })
}
