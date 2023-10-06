package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.ZeroExt
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, IssueQueueIQWakeUpBundle, MemExuInput, MemExuOutput, LoadShouldCancel}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.datapath.DataConfig.{IntData, VecData}
import xiangshan.backend.datapath.RdConfig.{IntRD, VfRD}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath._
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, PerfCounterIO}
import xiangshan.backend.issue.{CancelNetwork, Scheduler}
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
      s"${fuConfigs.map(_.name).mkString("fu(s): {", ",", "}")}, " +
      s"${wbPortConfigs.mkString("wb: {", ",", "}")}, " +
      s"${immType.map(SelImm.mkString(_)).mkString("imm: {", ",", "}")}, " +
      s"latMax(${exuCfg.latencyValMax}), ${exuCfg.fuLatancySet.mkString("lat: {", ",", "}")}, "
    )
    require(
      wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
        fuConfigs.map(_.writeIntRf).reduce(_ || _),
      "int wb port has no priority"
    )
    require(
      wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
        fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
      "vec wb port has no priority"
    )
  }

  println(s"[Backend] all fu configs")
  for (cfg <- FuConfig.allConfigs) {
    println(s"[Backend]   $cfg")
  }

  println(s"[Backend] Int RdConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getRdPortParams(IntData())) {
    println(s"[Backend]   port($port): ${seq.map(x => params.getExuName(x._1) + "(" + x._2.toString + ")").mkString(",")}")
  }

  println(s"[Backend] Int WbConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getWbPortParams(IntData())) {
    println(s"[Backend]   port($port): ${seq.map(x => params.getExuName(x._1) + "(" + x._2.toString + ")").mkString(",")}")
  }

  println(s"[Backend] Vf RdConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getRdPortParams(VecData())) {
    println(s"[Backend]   port($port): ${seq.map(x => params.getExuName(x._1) + "(" + x._2.toString + ")").mkString(",")}")
  }

  println(s"[Backend] Vf WbConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getWbPortParams(VecData())) {
    println(s"[Backend]   port($port): ${seq.map(x => params.getExuName(x._1) + "(" + x._2.toString + ")").mkString(",")}")
  }

  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val pcTargetMem = LazyModule(new PcTargetMem(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val memScheduler = params.memSchdParams.map(x => LazyModule(new Scheduler(x)))
  val cancelNetwork = LazyModule(new CancelNetwork(params))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val wbFuBusyTable = LazyModule(new WbFuBusyTable(params))

  lazy val module = new BackendImp(this)
}

class BackendImp(override val wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter {
  implicit private val params = wrapper.params

  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val pcTargetMem = wrapper.pcTargetMem.module
  private val intScheduler = wrapper.intScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val cancelNetwork = wrapper.cancelNetwork.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val bypassNetwork = Module(new BypassNetwork)
  private val wbDataPath = Module(new WbDataPath(params))
  private val wbFuBusyTable = wrapper.wbFuBusyTable.module

  private val iqWakeUpMappedBundle: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] = (
    intScheduler.io.toSchedulers.wakeupVec ++
      vfScheduler.io.toSchedulers.wakeupVec ++
      memScheduler.io.toSchedulers.wakeupVec
    ).map(x => (x.bits.exuIdx, x)).toMap

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
  private val og1CancelVec: Vec[Bool] = dataPath.io.og1CancelVec
  private val og0CancelVecFromDataPath: Vec[Bool] = dataPath.io.og0CancelVec
  private val og0CancelVecFromCancelNet: Vec[Bool] = cancelNetwork.io.out.og0CancelVec
  private val og0CancelVecFromFinalIssue: Vec[Bool] = Wire(chiselTypeOf(dataPath.io.og0CancelVec))
  private val og0CancelVec: Seq[Bool] = og0CancelVecFromDataPath.zip(og0CancelVecFromCancelNet).zip(og0CancelVecFromFinalIssue).map(x => x._1._1 | x._1._2 | x._2)
  private val cancelToBusyTable = dataPath.io.cancelToBusyTable

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromWB.wbData <> wbDataPath.io.toCtrlBlock.writeback
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.lqCanAccept := io.mem.lqCanAccept
  ctrlBlock.io.sqCanAccept := io.mem.sqCanAccept
  ctrlBlock.io.csrCtrl <> intExuBlock.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intExuBlock.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intExuBlock.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intExuBlock.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intExuBlock.io.csrio.get.wfi_event
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO
  ctrlBlock.io.robio.lsTopdownInfo <> io.mem.lsTopdownInfo
  ctrlBlock.io.robio.debug_ls <> io.mem.debugLS
  ctrlBlock.io.fromDataPath.vtype := vconfig(7, 0).asTypeOf(new VType)

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromCtrlBlock.pcVec := ctrlBlock.io.toIssueBlock.pcVec
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)
  intScheduler.io.fromDataPath.resp := dataPath.io.toIntIQ
  intScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  intScheduler.io.fromDataPath.og0Cancel := og0CancelVec
  intScheduler.io.fromDataPath.og1Cancel := og1CancelVec
  intScheduler.io.ldCancel := io.mem.ldCancel
  intScheduler.io.fromDataPath.cancelToBusyTable := cancelToBusyTable

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
  memScheduler.io.fromMem.get.memWaitUpdateReq.robIdx.zip(io.mem.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits  := source.bits.robIdx
  }
  memScheduler.io.fromMem.get.memWaitUpdateReq.sqIdx := DontCare // TODO
  memScheduler.io.fromDataPath.resp := dataPath.io.toMemIQ
  memScheduler.io.fromMem.get.ldaFeedback := io.mem.ldaIqFeedback
  memScheduler.io.fromMem.get.staFeedback := io.mem.staIqFeedback
  memScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  memScheduler.io.fromDataPath.og0Cancel := og0CancelVec
  memScheduler.io.fromDataPath.og1Cancel := og1CancelVec
  memScheduler.io.ldCancel := io.mem.ldCancel
  memScheduler.io.fromDataPath.cancelToBusyTable := cancelToBusyTable

  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  vfScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.vfUops
  vfScheduler.io.intWriteBack := 0.U.asTypeOf(vfScheduler.io.intWriteBack)
  vfScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  vfScheduler.io.fromDataPath.resp := dataPath.io.toVfIQ
  vfScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  vfScheduler.io.fromDataPath.og0Cancel := og0CancelVec
  vfScheduler.io.fromDataPath.og1Cancel := og1CancelVec
  vfScheduler.io.ldCancel := io.mem.ldCancel
  vfScheduler.io.fromDataPath.cancelToBusyTable := cancelToBusyTable

  cancelNetwork.io.in.int <> intScheduler.io.toDataPath
  cancelNetwork.io.in.vf  <> vfScheduler.io.toDataPath
  cancelNetwork.io.in.mem <> memScheduler.io.toDataPath
  cancelNetwork.io.in.og0CancelVec := og0CancelVecFromDataPath.zip(og0CancelVecFromFinalIssue).map(x => x._1 || x._2)
  cancelNetwork.io.in.og1CancelVec := og1CancelVec
  intScheduler.io.fromCancelNetwork <> cancelNetwork.io.out.int
  vfScheduler.io.fromCancelNetwork <> cancelNetwork.io.out.vf
  memScheduler.io.fromCancelNetwork <> cancelNetwork.io.out.mem

  dataPath.io.flush := ctrlBlock.io.toDataPath.flush
  dataPath.io.vconfigReadPort.addr := ctrlBlock.io.toDataPath.vtypeAddr

  dataPath.io.fromIntIQ <> intScheduler.io.toDataPathAfterDelay
  dataPath.io.fromVfIQ <> vfScheduler.io.toDataPathAfterDelay
  dataPath.io.fromMemIQ <> memScheduler.io.toDataPathAfterDelay

  dataPath.io.ldCancel := io.mem.ldCancel

  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  println(s"[Backend] wbDataPath.io.toVfPreg: ${wbDataPath.io.toVfPreg.size}, dataPath.io.fromFpWb: ${dataPath.io.fromVfWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
  dataPath.io.debugIntRat    .foreach(_ := ctrlBlock.io.debug_int_rat.get)
  dataPath.io.debugFpRat     .foreach(_ := ctrlBlock.io.debug_fp_rat.get)
  dataPath.io.debugVecRat    .foreach(_ := ctrlBlock.io.debug_vec_rat.get)
  dataPath.io.debugVconfigRat.foreach(_ := ctrlBlock.io.debug_vconfig_rat.get)

  bypassNetwork.io.fromDataPath.int <> dataPath.io.toIntExu
  bypassNetwork.io.fromDataPath.vf <> dataPath.io.toFpExu
  bypassNetwork.io.fromDataPath.mem <> dataPath.io.toMemExu
  bypassNetwork.io.fromExus.connectExuOutput(_.int)(intExuBlock.io.out)
  bypassNetwork.io.fromExus.connectExuOutput(_.vf)(vfExuBlock.io.out)
  bypassNetwork.io.fromExus.mem.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.pdest := source.bits.uop.pdest
    sink.bits.data := source.bits.data
  }


  intExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until intExuBlock.io.in.length) {
    for (j <- 0 until intExuBlock.io.in(i).length) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.int(i)(j).bits.loadDependency, io.mem.ldCancel)
      NewPipelineConnect(
        bypassNetwork.io.toExus.int(i)(j), intExuBlock.io.in(i)(j), intExuBlock.io.in(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.int(i)(j).fire,
          bypassNetwork.io.toExus.int(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          intExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        )
      )
    }
  }

  pcTargetMem.io.fromFrontendFtq := io.frontend.fromFtq
  pcTargetMem.io.fromDataPathFtq := bypassNetwork.io.toExus.int.flatten.filter(_.bits.params.hasPredecode).map(_.bits.ftqIdx.get)
  intExuBlock.io.in.flatten.filter(_.bits.params.hasPredecode).map(_.bits.predictInfo.get.target).zipWithIndex.foreach {
    case (sink, i) =>
      sink := pcTargetMem.io.toExus(i)
  }

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> 0.U.asTypeOf(csrio.vpu) // Todo

  val debugVconfig = dataPath.io.debugVconfig.get.asTypeOf(new VConfig)
  val debugVtype = VType.toVtypeStruct(debugVconfig.vtype).asUInt
  val debugVl = debugVconfig.vl
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vstart.bits := 0.U
  csrio.vpu.set_vtype.valid := ctrlBlock.io.robio.csr.vcsrFlag
  //Todo here need change design
  csrio.vpu.set_vtype.bits := ZeroExt(debugVtype, XLEN)
  csrio.vpu.set_vl.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vl.bits := ZeroExt(debugVl, XLEN)
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionVAddr
  csrio.externalInterrupt := io.fromTop.externalInterrupt
  csrio.distributedUpdate(0) := io.mem.csrDistributedUpdate
  csrio.distributedUpdate(1) := io.frontendCsrDistributedUpdate
  csrio.perf <> io.perf
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrio.perf.perfEventsCtrl <> ctrlBlock.getPerf
  private val fenceio = intExuBlock.io.fenceio.get
  fenceio.disableSfence := csrio.disableSfence
  io.fenceio <> fenceio

  vfExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until vfExuBlock.io.in.size) {
    for (j <- 0 until vfExuBlock.io.in(i).size) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.vf(i)(j).bits.loadDependency, io.mem.ldCancel)
      NewPipelineConnect(
        bypassNetwork.io.toExus.vf(i)(j), vfExuBlock.io.in(i)(j), vfExuBlock.io.in(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.vf(i)(j).fire,
          bypassNetwork.io.toExus.vf(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          vfExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        )
      )

      vfExuBlock.io.in(i)(j).bits.vpu.foreach(_.vstart := csrio.vpu.vstart)
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
    sink.bits.debugInfo := source.bits.uop.debugInfo
    sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
    sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
  }

  // to mem
  private val memIssueParams = params.memSchdParams.get.issueBlockParams
  private val memExuBlocksHasLDU = memIssueParams.map(_.exuBlockParams.map(_.fuConfigs.contains(FuConfig.LduCfg)))
  private val toMem = Wire(bypassNetwork.io.toExus.mem.cloneType)
  for (i <- toMem.indices) {
    for (j <- toMem(i).indices) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.mem(i)(j).bits.loadDependency, io.mem.ldCancel)
      val issueTimeout =
        if (memExuBlocksHasLDU(i)(j))
          Counter(0 until 16, toMem(i)(j).valid && !toMem(i)(j).fire, bypassNetwork.io.toExus.mem(i)(j).fire)._2
        else
          false.B

      if (memScheduler.io.loadFinalIssueResp(i).nonEmpty) {
        memScheduler.io.loadFinalIssueResp(i)(j).valid := issueTimeout
        memScheduler.io.loadFinalIssueResp(i)(j).bits.dataInvalidSqIdx := DontCare
        memScheduler.io.loadFinalIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.loadFinalIssueResp(i)(j).bits.respType := RSFeedbackType.fuBusy
        memScheduler.io.loadFinalIssueResp(i)(j).bits.rfWen := toMem(i)(j).bits.rfWen.getOrElse(false.B)
        memScheduler.io.loadFinalIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
      }

      NewPipelineConnect(
        bypassNetwork.io.toExus.mem(i)(j), toMem(i)(j), toMem(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.mem(i)(j).fire,
          bypassNetwork.io.toExus.mem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          toMem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || issueTimeout
        )
      )
    }
  }

  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(toMem.flatten).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx         := source.bits.iqIdx
    sink.bits.isFirstIssue  := source.bits.isFirstIssue
    sink.bits.uop           := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src           := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r}
    sink.bits.deqPortIdx    := source.bits.deqPortIdx.getOrElse(0.U)
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
    sink.bits.uop.debugInfo := source.bits.perfDebugInfo
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  require(io.mem.loadPcRead.size == params.LduCnt)
  io.mem.loadPcRead.zipWithIndex.foreach { case (loadPcRead, i) =>
    loadPcRead := ctrlBlock.io.memLdPcRead(i).data
    ctrlBlock.io.memLdPcRead(i).ptr := io.mem.issueUops(i).bits.uop.ftqPtr
    ctrlBlock.io.memLdPcRead(i).offset := io.mem.issueUops(i).bits.uop.ftqOffset
    require(toMem.head(i).bits.ftqIdx.isDefined && toMem.head(i).bits.ftqOffset.isDefined)
  }

  ctrlBlock.io.robio.robHeadLsIssue := io.mem.issueUops.map(deq => deq.fire && deq.bits.uop.robIdx === ctrlBlock.io.robio.robDeqPtr).reduce(_ || _)

  // mem io
  io.mem.lsqEnqIO <> memScheduler.io.memIO.get.lsqEnqIO
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq

  private val intFinalIssueBlock = intExuBlock.io.in.flatten.map(_ => false.B)
  private val vfFinalIssueBlock = vfExuBlock.io.in.flatten.map(_ => false.B)
  private val memFinalIssueBlock = io.mem.issueUops zip memExuBlocksHasLDU.flatten map {
    case (out, isLdu) =>
      if (isLdu) RegNext(out.valid && !out.ready, false.B)
      else false.B
  }
  og0CancelVecFromFinalIssue := intFinalIssueBlock ++ vfFinalIssueBlock ++ memFinalIssueBlock

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  io.toTop.cpuHalted := false.B // TODO: implement cpu halt

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
  val ldaIqFeedback = Vec(params.LduCnt, Flipped(new MemRSFeedbackIO))
  val staIqFeedback = Vec(params.StaCnt, Flipped(new MemRSFeedbackIO))
  val ldCancel = Vec(params.LduCnt, Flipped(new LoadCancelIO))
  val loadPcRead = Vec(params.LduCnt, Output(UInt(VAddrBits.W)))

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

  val lqCanAccept = Input(Bool())
  val sqCanAccept = Input(Bool())

  val otherFastWakeup = Flipped(Vec(params.LduCnt + 2 * params.StaCnt, ValidIO(new DynInst)))
  val stIssuePtr = Input(new SqPtr())

  val csrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  val debugLS = Flipped(Output(new DebugLSIO))

  val lsTopdownInfo = Vec(params.LduCnt, Flipped(Output(new LsTopdownInfo)))
  // Output
  val redirect = ValidIO(new Redirect)   // rob flush MemBlock
  val issueUops = MixedVec(Seq.fill(params.LduCnt + params.StaCnt + params.StdCnt)(DecoupledIO(new MemExuInput())) ++ Seq.fill(params.VlduCnt)(DecoupledIO(new MemExuInput(true))))
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
