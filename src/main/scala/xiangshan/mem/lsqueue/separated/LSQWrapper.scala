package xiangshan.mem

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.cache._
import xiangshan.cache.{DCacheWordIO, DCacheLineIO, TlbRequestIO, MemoryOpConstants}
import xiangshan.backend.LSUOpType
import xiangshan.mem._

// Load / Store Queue Wrapper for XiangShan Out of Order LSU
//
// By using this Wrapper, interface of unified lsroq and ldq / stq are the same 
class LsqWrappper extends XSModule with HasDCacheParameters with NeedImpl {
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
    val roqDeqPtr = Input(UInt(RoqIdxWidth.W))
  })
  
  if(EnableUnifiedLSQ){
    val lsroq = Module(new Lsroq)

    lsroq.io.dp1Req <> io.dp1Req
    lsroq.io.brqRedirect <> io.brqRedirect
    lsroq.io.loadIn <> io.loadIn
    lsroq.io.storeIn <> io.storeIn
    lsroq.io.sbuffer <> io.sbuffer
    lsroq.io.ldout <> io.ldout
    lsroq.io.stout <> io.stout
    lsroq.io.forward <> io.forward
    lsroq.io.commits <> io.commits
    lsroq.io.rollback <> io.rollback
    lsroq.io.dcache <> io.dcache
    lsroq.io.uncache <> io.uncache
    lsroq.io.roqDeqPtr <> io.roqDeqPtr
    (0 until RenameWidth).map(i => {
      io.lsIdxs(i).lsroqIdx := lsroq.io.lsroqIdxs(i)
    })
  } else {
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
    
    // store queue wiring
    // storeQueue.io <> DontCare
    storeQueue.io.dp1Req <> io.dp1Req
    storeQueue.io.brqRedirect <> io.brqRedirect
    storeQueue.io.storeIn <> io.storeIn
    storeQueue.io.sbuffer <> io.sbuffer
    storeQueue.io.stout <> io.stout
    storeQueue.io.commits <> io.commits
    storeQueue.io.roqDeqPtr <> io.roqDeqPtr
    
    loadQueue.io.forward <> io.forward
    storeQueue.io.forward <> io.forward // overlap forwardMask & forwardData, DO NOT CHANGE SEQUENCE

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
      val isStore = LSUOpType.isStore(io.dp1Req(i).bits.ctrl.fuOpType)
      loadQueue.io.dp1Req(i).valid := !isStore && io.dp1Req(i).valid
      storeQueue.io.dp1Req(i).valid := isStore && io.dp1Req(i).valid
      io.lsIdxs(i) := DontCare
      loadQueue.io.lqIdxs(i) <> io.lsIdxs(i).lqIdx
      storeQueue.io.sqIdxs(i) <> io.lsIdxs(i).sqIdx
      io.lsIdxs(i).instIsLoad := !isStore
      io.dp1Req(i).ready := Mux(isStore, storeQueue.io.dp1Req(i).ready, loadQueue.io.dp1Req(i).ready)
    })
  }
}
