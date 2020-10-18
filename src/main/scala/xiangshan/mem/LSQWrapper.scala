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
    val lsroqIdxs = Output(Vec(RenameWidth, UInt(LsroqIdxWidth.W)))
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
  
  val loadQueue = Module(new LoadQueue)
  val storeQueue = Module(new StoreQueue)

  // load queue wiring
  loadQueue.io.dp1Req <> io.dp1Req
  loadQueue.io.brqRedirect <> io.brqRedirect
  loadQueue.io.loadIn <> io.loadIn
  loadQueue.io.storeIn <> io.storeIn
  loadQueue.io.ldout <> io.ldout
  loadQueue.io.forward <> io.forward
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
  storeQueue.io.forward <> io.forward
  storeQueue.io.commits <> io.commits
  storeQueue.io.rollback <> io.rollback
  storeQueue.io.roqDeqPtr <> io.roqDeqPtr

  // uncache arbiter
  val uncacheArb = Module(new Arbiter(new DCacheWordIO, 2))
  uncacheArb.io.in(0) <> loadQueue.io.uncache
  uncacheArb.io.in(1) <> storeQueue.io.uncache
  uncacheArb.io.out <> io.uncache

  (0 until RenameWidth).map(i => {
    loadQueue.io.lqIdxs(i) <> io.lsroqIdxs(i).lqIdx
    storeQueue.io.sqIdxs(i) <> io.lsroqIdxs(i).sqIdx
    io.lsroqIdxs(i).lsIdxType := DontCare 
  })
}
