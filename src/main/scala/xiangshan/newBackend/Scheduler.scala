//package xiangshan.newBackend
//
//import chipsalliance.rocketchip.config.Parameters
//import freechips.rocketchip.diplomacy._
//import chisel3._
//import chisel3.util._
//import utils.{HasPerfEvents, RegNextWithEnable}
//import xiangshan._
//import xiangshan.backend.regfile.{VfRfReadAddr, VfRfReadData}
//import xiangshan.mem.{LsqEnqIO, MemWaitUpdateReq, SqPtr}
//
//class IntScheduler()(implicit p: Parameters) extends LazyModule {
//  val numIntRfWritePorts = 8
//  val numVfpRfWritePorts = 8
//  val numIntDispatch = 4
//  val numLsDispatch = 4
//  val numDispatch = numIntDispatch + numLsDispatch
//  val vfpRfReadPorts = 2 // Todo: should be num of store units
//  val numIssue = 10 // Todo: should be num of issue pipelines
//  lazy val module = new IntSchedulerImp(this)
//
//}
//
//class IntSchedulerImp(outer: IntScheduler)(implicit p: Parameters) extends LazyModuleImp(outer)
//  with HasXSParameter
//{
//  private val intRfWritePorts = outer.numIntRfWritePorts
//  private val vfpRfWritePorts = outer.numVfpRfWritePorts
//  private val numDispatch = outer.numDispatch
//  private val vfpRfReadPorts = outer.vfpRfReadPorts
//  private val numIssuePorts = outer.numIssue
//  class IntSchedulerIOBundle(implicit p: Parameters) extends XSBundle {
//    val fromTop = new Bundle {
//      val hartId = Input(UInt(8.W))
//    }
//    val fromRob = new Bundle {
//      val redirect = Flipped(ValidIO(new Redirect))
//      val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
//    }
//    val fromDispatch = new Bundle {
//      val allocPregs = Vec(RenameWidth, Input(new ResetPregStateReq()))
//      val uopIn = Vec(dpParams.IntDqDeqWidth + dpParams.LsDqDeqWidth, Flipped(DecoupledIO(new DynInst())))
//      val iqReady = Vec(numDispatch, Output(Bool()))
//    }
//    val fromWb = new Bundle {
//      val writeback = Vec(intRfWritePorts + vfpRfWritePorts, Flipped(ValidIO(new ExuOutput)))
//    }
//    val toFu = new Bundle {
//      val uops = Vec(numIssuePorts, DecoupledIO(new ExuInput))
//    }
//    val fromVfScheduler = new Bundle {
//      val wakeUp = Vec(intRfWritePorts + vfpRfWritePorts, Flipped(ValidIO(new DynInst)))
//      val vfpRfReadData = VfRfReadData(p)
//    }
//    val toVfScheduler = new Bundle {
//      val vfpRfReadAddr = VfRfReadAddr()
//    }
//    val fromCtrlBlock = new Bundle {
//      val jumpPc = Input(UInt(VAddrBits.W))
//      val jalrTarget = Input(UInt(VAddrBits.W))
//    }
//    val fromSq = new Bundle {
//      val stIssuePtr = Input(new SqPtr())
//      val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
//    }
//    val lsq = new Bundle {
//      val enqLsq = Flipped(new LsqEnqIO)
//      val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
//      val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
//      val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
//    }
//    // debug
//    val debug = new Bundle {
//      val intRat = Vec(32, Input(UInt(IntPregIdxWidth.W)))
//      val vfRat = Vec(64, Input(VfPregIdxWidth.W))
//    }
//  }
//
//  val io = IO(new IntSchedulerIOBundle)
//
//  // To reduce fanout, we add registers here for redirect.
//  val redirect = RegNextWithEnable(io.redirect)
//
//}
