package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.PipelineConnect
import xiangshan._
import xiangshan.backend.CtrlBlock
import xiangshan.backend.exu.FenceIO
import xiangshan.backend.fu.{CSRFileIO, FenceToSbuffer}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.frontend.FtqRead
import xiangshan.mem.{LsqEnqIO, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, MemExuInput, MemExuOutput}

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  for (exuCfg <- params.allExuParams) {
    val fuConfigs = exuCfg.fuConfigs
    val wbPortConfigs = exuCfg.wbPortConfigs
    val immType = exuCfg.immType
    println(s"exu: ${fuConfigs.map(_.name)}, wb: ${wbPortConfigs}, imm: ${immType}")
    require(wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
      fuConfigs.map(_.writeIntRf).reduce(_ || _),
      "int wb port has no priority" )
    require(wbPortConfigs.collectFirst { case x: VecWB => x }.nonEmpty ==
      fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
      "vec wb port has no priority" )
  }

  println(s"Function Unit: Alu(${params.AluCnt}), Brh(${params.BrhCnt}), Jmp(${params.JmpCnt}), " +
    s"Ldu(${params.LduCnt}), Stu(${params.StuCnt})")


  val numMemRsEntryMax = 16 // Todo: fix it

  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
//  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
//  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))

  lazy val module = new BackendImp(this)
}

class BackendImp(override val wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
//  implicit val numMemRsEntryMax = wrapper.numMemRsEntryMax
  implicit private val params = wrapper.params
  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intScheduler = wrapper.intScheduler.get.module
//  private val vfScheduler = wrapper.vfScheduler.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
//  private val fpExuBlock = wrapper.vfExuBlock.get.module
  private val wbDataPath = Module(new WbDataPath(params))

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromWB.wbData <> wbDataPath.io.toCtrlBlock.writeback
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.csrCtrl <> intExuBlock.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intExuBlock.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intExuBlock.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intExuBlock.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intExuBlock.io.csrio.get.wfi_event
  ctrlBlock.io.robio.lsq <> io.mem.lsq

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromCtrlBlock.pcVec := ctrlBlock.io.toIssueBlock.pcVec
  intScheduler.io.fromCtrlBlock.targetVec := ctrlBlock.io.toIssueBlock.targetVec
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.writeback := wbDataPath.io.toIntPreg.map(_.toWakeUpBundle)

//  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
//  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.redirect
//  vfScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
//  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.uops
//  vfScheduler.io.writeback := wbDataPath.io.toVfPreg.map(_.toWakeUpBundle)

  dataPath.io.flush := ctrlBlock.io.toDataPath.flush

  for (i <- 0 until dataPath.io.fromIntIQ.length) {
    for (j <- 0 until dataPath.io.fromIntIQ(i).length) {
      PipelineConnect(intScheduler.io.toDataPath(i)(j), dataPath.io.fromIntIQ(i)(j), dataPath.io.fromIntIQ(i)(j).fire,
        intScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.redirect))
    }
  }

  dataPath.io.fromVfIQ <> 0.U.asTypeOf(dataPath.io.fromVfIQ)
  dataPath.io.fromMemIQ <> 0.U.asTypeOf(dataPath.io.fromMemIQ)
  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromFpWb := wbDataPath.io.toVfPreg
  dataPath.io.debugIntRat := ctrlBlock.io.debug_int_rat
  dataPath.io.debugFpRat := ctrlBlock.io.debug_fp_rat
  dataPath.io.debugVecRat := ctrlBlock.io.debug_vec_rat

  intExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until intExuBlock.io.in.length) {
    for (j <- 0 until intExuBlock.io.in(i).length) {
      PipelineConnect(dataPath.io.toIntExu(i)(j), intExuBlock.io.in(i)(j), intExuBlock.io.in(i)(j).fire,
        dataPath.io.toIntExu(i)(j).bits.robIdx.needFlush(ctrlBlock.io.redirect))
    }
  }

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrio.perf.perfEventsCtrl <> ctrlBlock.getPerf
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> 0.U.asTypeOf(csrio.vpu) // Todo
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionVAddr
  csrio.externalInterrupt := io.csr.externalInterrupt
  csrio.distributedUpdate := io.csr.distributedUpdate
  private val fenceio = intExuBlock.io.fenceio.get
  fenceio.disableSfence := csrio.disableSfence
  io.fenceio <> fenceio

//  fpExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
//  fpExuBlock.io.in := dataPath.io.toFpExu
//  fpExuBlock.io.frm.get := intExuBlock.io.csrio.get.fpu.frm

  wbDataPath.io.flush := ctrlBlock.io.redirect
  wbDataPath.io.fromIntExu <> intExuBlock.io.out
  wbDataPath.io.fromVfExu <> 0.U.asTypeOf(wbDataPath.io.fromVfExu) // fpExuBlock.io.out
  wbDataPath.io.fromMemExu <> 0.U.asTypeOf(wbDataPath.io.fromMemExu) // Todo

  io.mem.loadFastMatch := DontCare
  io.mem.loadFastImm := DontCare
  io.mem.flush := ctrlBlock.io.redirect
  io.mem.lsq <> ctrlBlock.io.robio.lsq
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  io.mem.tlbCsr := csrio.tlb
  io.mem.sfence := fenceio.sfence
  io.mem.toSbuffer <> fenceio.sbuffer

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl
}

class BackendMemIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val rsParameters = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = 16 // Todo
    )
  })

  val writeBack = Vec(params.LduCnt + params.StuCnt * 2, Flipped(DecoupledIO(new MemExuOutput())))

  val s3_delayed_load_error = Input(Vec(LoadPipelineWidth, Bool()))
  val stIn = Input(Vec(params.StuCnt, ValidIO(new DynInst())))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val exceptionVAddr = Input(UInt(VAddrBits.W))
  val enqLsq = Flipped(new LsqEnqIO)
  val sqDeq = Input(UInt(params.StuCnt.W))

  val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  val otherFastWakeup = Flipped(Vec(params.LduCnt + 2 * params.StuCnt, ValidIO(new DynInst)))
  val loadPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  val issueUops = Vec(params.LduCnt + 2 * params.StuCnt, DecoupledIO(new MemExuInput()))
  val loadFastMatch = Vec(params.LduCnt, Output(UInt(params.LduCnt.W)))
  val loadFastImm   = Vec(params.LduCnt, Output(UInt(12.W))) // Imm_I

  val stIssuePtr = Input(new SqPtr()) // where is valid signal
  val flush = ValidIO(new Redirect)   // rob flush MemBlock
  val rsFeedBack = Vec(params.StuCnt, Flipped(new MemRSFeedbackIO()(rsParameters)))
  val lsq = new RobLsqIO
  val tlbCsr = Output(new TlbCsrBundle)
  val csrCtrl = Output(new CustomCSRCtrlIO)
  val sfence = Output(new SfenceBundle)
  val toSbuffer = new FenceToSbuffer
  val isStoreException = Output(Bool())
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }

  val toTop = new Bundle {
    val cpuHalted = Output(Bool())
  }

  val csr = new CSRFileIO
  val fenceio = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val frontend = Flipped(new FrontendToCtrlIO)
  val frontendSfence = Output(new SfenceBundle)
  val frontendCsrCtrl = Output(new CustomCSRCtrlIO)
  val frontendTlbCsr = Output(new TlbCsrBundle)
  val mem = new BackendMemIO

  val redirect = ValidIO(new Redirect)
}
