package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.OptionWrapper
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{PipelineConnect, ZeroExt}
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.ctrlblock.CtrlBlock
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{DataPath, NewPipelineConnect, WbDataPath, WbFuBusyTable}
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, PerfCounterIO}
import xiangshan.backend.issue.{Scheduler, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.frontend.{FtqPtr, FtqRead}
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  /* Only update the idx in mem-scheduler here
   * Idx in other schedulers can be updated the same way if needed
   *
   * Also note that we filter out the 'stData issue-queues' when counting
   */
  for ((ibp, idx) <- params.memSchdParams.get.issueBlockParams.filter(iq => iq.StdCnt == 0).zipWithIndex) {
    ibp.updateIdx(idx)
  }

  println(params.iqWakeUpParams)

  for ((schdCfg, i) <- params.allSchdParams.zipWithIndex) {
    schdCfg.bindBackendParam(params)
  }

  for ((iqCfg, i) <- params.allIssueParams.zipWithIndex) {
    iqCfg.bindBackendParam(params)
  }

  for ((exuCfg, i) <- params.allExuParams.zipWithIndex) {
    exuCfg.updateIQWakeUpConfigs(params.iqWakeUpParams)
    exuCfg.updateExuIdx(i)
    exuCfg.bindBackendParam(params)
  }

  println("[Backend] ExuConfigs:")
  for (exuCfg <- params.allExuParams) {
    val fuConfigs = exuCfg.fuConfigs
    val wbPortConfigs = exuCfg.wbPortConfigs
    val immType = exuCfg.immType

    println("[Backend]   " +
      s"${exuCfg.name}: " +
      s"${ fuConfigs.map(_.name).mkString("fu(s): {", ",", "}") }, " +
      s"${ wbPortConfigs.mkString("wb: {", ",", "}") }, " +
      s"${ immType.map(SelImm.mkString(_)).mkString("imm: {", "," , "}") }, " +
      s"latMax(${exuCfg.latencyValMax}), ${exuCfg.fuLatancySet.mkString("lat: {",",","}")}, ")
    require(wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
      fuConfigs.map(_.writeIntRf).reduce(_ || _),
      "int wb port has no priority" )
    require(wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
      fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
      "vec wb port has no priority" )
  }

  for (cfg <- FuConfig.allConfigs) {
    println(s"[Backend] $cfg")
  }

  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val memScheduler = params.memSchdParams.map(x => LazyModule(new Scheduler(x)))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val wbFuBusyTable = LazyModule(new WbFuBusyTable(params))

  lazy val module = new BackendImp(this)
}

class BackendImp(override val wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter{
  implicit private val params = wrapper.params
  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intScheduler = wrapper.intScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val wbDataPath = Module(new WbDataPath(params))
  private val wbFuBusyTable = wrapper.wbFuBusyTable.module

  private val iqWakeUpMappedBundle: Map[String, ValidIO[Bundles.IssueQueueWakeUpBundle]] = (
    intScheduler.io.toSchedulers.wakeupVec ++
    vfScheduler.io.toSchedulers.wakeupVec ++
    memScheduler.io.toSchedulers.wakeupVec
  ).map(x => (x.bits.wakeupSource, x)).toMap

  println(s"[Backend] iq wake up keys: ${iqWakeUpMappedBundle.keys}")

  wbFuBusyTable.io.in.intSchdBusyTable := intScheduler.io.wbFuBusyTable
  wbFuBusyTable.io.in.vfSchdBusyTable := vfScheduler.io.wbFuBusyTable
  wbFuBusyTable.io.in.memSchdBusyTable := memScheduler.io.wbFuBusyTable
  intScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.intRespRead
  vfScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.vfRespRead
  memScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.memRespRead
  dataPath.io.wbConfictRead := wbFuBusyTable.io.out.wbConflictRead

  wbDataPath.io.fromIntExu.flatten.filter(x => x.bits.params.writeIntRf)

  private val vconfig = dataPath.io.vconfigReadPort.data

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
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO
  ctrlBlock.io.fromDataPath.vtype := vconfig(7, 0).asTypeOf(new VType)

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromCtrlBlock.pcVec := ctrlBlock.io.toIssueBlock.pcVec
  intScheduler.io.fromCtrlBlock.targetVec := ctrlBlock.io.toIssueBlock.targetVec
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)
  intScheduler.io.fromDataPath := dataPath.io.toIntIQ
  intScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.wakeupSource) }

  memScheduler.io.fromTop.hartId := io.fromTop.hartId
  memScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  memScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  memScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.memUops
  memScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  memScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  memScheduler.io.fromMem.get.scommit := io.mem.sqDeq
  memScheduler.io.fromMem.get.lcommit := io.mem.lqDeq
  memScheduler.io.fromMem.get.sqCancelCnt := io.mem.sqCancelCnt
  memScheduler.io.fromMem.get.lqCancelCnt := io.mem.lqCancelCnt
  memScheduler.io.fromMem.get.stIssuePtr := io.mem.stIssuePtr
  memScheduler.io.fromMem.get.memWaitUpdateReq.staIssue.zip(io.mem.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.uop := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.uop.robIdx := source.bits.robIdx
  }
  memScheduler.io.fromDataPath := dataPath.io.toMemIQ
  memScheduler.io.fromMem.get.ldaFeedback := io.mem.ldaIqFeedback
  memScheduler.io.fromMem.get.staFeedback := io.mem.staIqFeedback
  memScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.wakeupSource) }

  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  vfScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.vfUops
  vfScheduler.io.intWriteBack := 0.U.asTypeOf(vfScheduler.io.intWriteBack)
  vfScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  vfScheduler.io.fromDataPath := dataPath.io.toVfIQ
  vfScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.wakeupSource) }

  dataPath.io.flush := ctrlBlock.io.toDataPath.flush
  dataPath.io.vconfigReadPort.addr := ctrlBlock.io.toDataPath.vtypeAddr

  for (i <- 0 until dataPath.io.fromIntIQ.length) {
    for (j <- 0 until dataPath.io.fromIntIQ(i).length) {
      NewPipelineConnect(intScheduler.io.toDataPath(i)(j), dataPath.io.fromIntIQ(i)(j), dataPath.io.fromIntIQ(i)(j).valid,
        intScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush), Option("intScheduler2DataPathPipe"))
    }
  }

  for (i <- 0 until dataPath.io.fromVfIQ.length) {
    for (j <- 0 until dataPath.io.fromVfIQ(i).length) {
      NewPipelineConnect(vfScheduler.io.toDataPath(i)(j), dataPath.io.fromVfIQ(i)(j), dataPath.io.fromVfIQ(i)(j).valid,
        vfScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush), Option("vfScheduler2DataPathPipe"))
    }
  }

  for (i <- 0 until dataPath.io.fromMemIQ.length) {
    for (j <- 0 until dataPath.io.fromMemIQ(i).length) {
      NewPipelineConnect(memScheduler.io.toDataPath(i)(j), dataPath.io.fromMemIQ(i)(j), dataPath.io.fromMemIQ(i)(j).valid,
        memScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush), Option("memScheduler2DataPathPipe"))
    }
  }

  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  println(s"[Backend] wbDataPath.io.toVfPreg: ${wbDataPath.io.toVfPreg.size}, dataPath.io.fromFpWb: ${dataPath.io.fromVfWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
  dataPath.io.debugIntRat := ctrlBlock.io.debug_int_rat
  dataPath.io.debugFpRat := ctrlBlock.io.debug_fp_rat
  dataPath.io.debugVecRat := ctrlBlock.io.debug_vec_rat
  dataPath.io.debugVconfigRat := ctrlBlock.io.debug_vconfig_rat

  intExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until intExuBlock.io.in.length) {
    for (j <- 0 until intExuBlock.io.in(i).length) {
      NewPipelineConnect(dataPath.io.toIntExu(i)(j), intExuBlock.io.in(i)(j), intExuBlock.io.in(i)(j).fire,
        Mux(dataPath.io.toIntExu(i)(j).fire,
          dataPath.io.toIntExu(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          intExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)))
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

  val debugVconfig = dataPath.io.debugVconfig.asTypeOf(new VConfig)
  val debugVtype = VType.toVtypeStruct(debugVconfig.vtype).asUInt
  val debugVl = debugVconfig.vl
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vstart.bits := 0.U
  csrio.vpu.set_vtype.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vtype.bits := ZeroExt(debugVtype, XLEN)
  csrio.vpu.set_vl.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vl.bits := ZeroExt(debugVl, XLEN)
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionVAddr
  csrio.externalInterrupt := io.fromTop.externalInterrupt
  csrio.distributedUpdate(0) := io.mem.csrDistributedUpdate
  csrio.distributedUpdate(1) := io.frontendCsrDistributedUpdate
  csrio.perf <> io.perf
  private val fenceio = intExuBlock.io.fenceio.get
  fenceio.disableSfence := csrio.disableSfence
  io.fenceio <> fenceio

  vfExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until vfExuBlock.io.in.size) {
    for (j <- 0 until vfExuBlock.io.in(i).size) {
      NewPipelineConnect(dataPath.io.toFpExu(i)(j), vfExuBlock.io.in(i)(j), vfExuBlock.io.in(i)(j).fire,
        Mux(dataPath.io.toFpExu(i)(j).fire,
          dataPath.io.toFpExu(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          vfExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)))
    }
  }
  vfExuBlock.io.frm.foreach(_ := csrio.fpu.frm)

  wbDataPath.io.flush := ctrlBlock.io.redirect
  wbDataPath.io.fromTop.hartId := io.fromTop.hartId
  wbDataPath.io.fromIntExu <> intExuBlock.io.out
  wbDataPath.io.fromVfExu <> vfExuBlock.io.out
  wbDataPath.io.fromMemExu.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.data   := source.bits.data
    sink.bits.pdest  := source.bits.uop.pdest
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
    sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
    sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
    sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
    sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
    sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
    sink.bits.debug := source.bits.debug
    sink.bits.debugInfo := 0.U.asTypeOf(sink.bits.debugInfo)
    sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
    sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
    sink.bits.ftqIdx.foreach(_ := source.bits.uop.ftqPtr)
    sink.bits.ftqOffset.foreach(_ := source.bits.uop.ftqOffset)
  }

  // to mem
  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(dataPath.io.toMemExu.flatten).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx         := source.bits.iqIdx
    sink.bits.isFirstIssue  := source.bits.isFirstIssue
    sink.bits.uop           := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src           := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r}
    sink.bits.uop.fuType    := source.bits.fuType
    sink.bits.uop.fuOpType  := source.bits.fuOpType
    sink.bits.uop.imm       := source.bits.imm
    sink.bits.uop.robIdx    := source.bits.robIdx
    sink.bits.uop.pdest     := source.bits.pdest
    sink.bits.uop.rfWen     := source.bits.rfWen.getOrElse(false.B)
    sink.bits.uop.fpWen     := source.bits.fpWen.getOrElse(false.B)
    sink.bits.uop.vecWen    := source.bits.vecWen.getOrElse(false.B)
    sink.bits.uop.flushPipe := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc        := source.bits.pc.getOrElse(0.U)
    sink.bits.uop.lqIdx     := source.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
    sink.bits.uop.sqIdx     := source.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    sink.bits.uop.ftqPtr    := source.bits.ftqIdx.getOrElse(0.U.asTypeOf(new FtqPtr))
    sink.bits.uop.ftqOffset := source.bits.ftqOffset.getOrElse(0.U)
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  require(io.mem.loadPcRead.size == params.LduCnt)
  io.mem.loadPcRead.zipWithIndex.foreach { case (loadPcRead, i) =>
    loadPcRead.data := ctrlBlock.io.memLdPcRead(i).data
    ctrlBlock.io.memLdPcRead(i).ptr := loadPcRead.ptr
    ctrlBlock.io.memLdPcRead(i).offset := loadPcRead.offset
  }
  // mem io
  io.mem.lsqEnqIO <> memScheduler.io.memIO.get.lsqEnqIO
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq
  io.mem.toSbuffer <> fenceio.sbuffer

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  dontTouch(memScheduler.io)
  dontTouch(io.mem)
  dontTouch(dataPath.io.toMemExu)
  dontTouch(wbDataPath.io.fromMemExu)
}

class BackendMemIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize
  // In/Out // Todo: split it into one-direction bundle
  val lsqEnqIO = Flipped(new LsqEnqIO)
  val robLsqIO = new RobLsqIO
  val toSbuffer = new FenceToSbuffer
  val ldaIqFeedback = Vec(params.LduCnt, Flipped(new MemRSFeedbackIO))
  val staIqFeedback = Vec(params.StaCnt, Flipped(new MemRSFeedbackIO))
  val loadPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  // Input
  val writeBack = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(Flipped(DecoupledIO(new MemExuOutput()))) ++ Seq.fill(params.VlduCnt)(Flipped(DecoupledIO(new MemExuOutput(true)))))

  val s3_delayed_load_error = Input(Vec(LoadPipelineWidth, Bool()))
  val stIn = Input(Vec(params.StaCnt, ValidIO(new DynInst())))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val exceptionVAddr = Input(UInt(VAddrBits.W))
  val sqDeq = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Input(UInt(log2Up(CommitWidth + 1).W))

  val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  val otherFastWakeup = Flipped(Vec(params.LduCnt + 2 * params.StaCnt, ValidIO(new DynInst)))
  val stIssuePtr = Input(new SqPtr())

  val csrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  // Output
  val redirect = ValidIO(new Redirect)   // rob flush MemBlock
  val issueUops = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(DecoupledIO(new MemExuInput())) ++ Seq.fill(params.VlduCnt)(DecoupledIO(new MemExuInput(true))))
  val loadFastMatch = Vec(params.LduCnt, Output(UInt(params.LduCnt.W)))
  val loadFastImm   = Vec(params.LduCnt, Output(UInt(12.W))) // Imm_I

  val tlbCsr = Output(new TlbCsrBundle)
  val csrCtrl = Output(new CustomCSRCtrlIO)
  val sfence = Output(new SfenceBundle)
  val isStoreException = Output(Bool())
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
    val externalInterrupt = new ExternalInterruptIO
  }

  val toTop = new Bundle {
    val cpuHalted = Output(Bool())
  }

  val fenceio = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val frontend = Flipped(new FrontendToCtrlIO)
  val frontendSfence = Output(new SfenceBundle)
  val frontendCsrCtrl = Output(new CustomCSRCtrlIO)
  val frontendTlbCsr = Output(new TlbCsrBundle)
  // distributed csr write
  val frontendCsrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  val mem = new BackendMemIO

  val perf = Input(new PerfCounterIO)

  val tlb = Output(new TlbCsrBundle)

  val csrCustomCtrl = Output(new CustomCSRCtrlIO)
}
