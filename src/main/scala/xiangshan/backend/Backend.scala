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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Robert. M. Tomasulo. "[An efficient algorithm for exploiting multiple arithmetic units.]
* (https://doi.org/10.1147/rd.111.0025)" IBM Journal of Research and Development (IBMJ) 11.1: 25-33. 1967.
***************************************************************************************/

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import system.HasSoCParameter
import utility._
import utility.sram.SramBroadcastBundle
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, IssueQueueIQWakeUpBundle, LoadShouldCancel, MemExuInput, MemExuOutput, VPUCtrlSignals}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.datapath.DataConfig.{IntData, VecData, FpData}
import xiangshan.backend.datapath.RdConfig.{IntRD, VfRD}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath._
import xiangshan.backend.dispatch.CoreDispatchTopDownIO
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, FuType, PerfCounterIO}
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.issue.{Scheduler, SchedulerArithImp, SchedulerImpBase, SchedulerMemImp}
import xiangshan.backend.rob.{RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.frontend.{FtqPtr, FtqRead, PreDecodeInfo}
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}

import scala.collection.mutable

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  val inner = LazyModule(new BackendInlined(params))
  lazy val module = new BackendImp(this)
}

class BackendImp(wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  val io = IO(new BackendIO()(p, wrapper.params))
  io <> wrapper.inner.module.io
  if (p(DebugOptionsKey).ResetGen) {
    ResetGen(ResetGenNode(Seq(ModuleNode(wrapper.inner.module))), reset, sim = false, io.dft_reset)
  }
}

class BackendInlined(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  override def shouldBeInlined: Boolean = true

  // check read & write port config
  params.configChecks

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
    exuCfg.bindBackendParam(params)
    exuCfg.updateIQWakeUpConfigs(params.iqWakeUpParams)
    exuCfg.updateExuIdx(i)
  }

  println("[Backend] ExuConfigs:")
  for (exuCfg <- params.allExuParams) {
    val fuConfigs = exuCfg.fuConfigs
    val wbPortConfigs = exuCfg.wbPortConfigs
    val immType = exuCfg.immType

    println("[Backend]   " +
      s"${exuCfg.name}: " +
      (if (exuCfg.fakeUnit) "fake, " else "") +
      (if (exuCfg.hasLoadFu || exuCfg.hasHyldaFu) s"LdExuIdx(${backendParams.getLdExuIdx(exuCfg)})" else "") +
      s"${fuConfigs.map(_.name).mkString("fu(s): {", ",", "}")}, " +
      s"${wbPortConfigs.mkString("wb: {", ",", "}")}, " +
      s"${immType.map(SelImm.mkString(_)).mkString("imm: {", ",", "}")}, " +
      s"latMax(${exuCfg.latencyValMax}), ${exuCfg.fuLatancySet.mkString("lat: {", ",", "}")}, " +
      s"srcReg(${exuCfg.numRegSrc})"
    )
    require(
      wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
        fuConfigs.map(_.writeIntRf).reduce(_ || _),
      s"${exuCfg.name} int wb port has no priority"
    )
    require(
      wbPortConfigs.collectFirst { case x: FpWB => x }.nonEmpty ==
        fuConfigs.map(x => x.writeFpRf).reduce(_ || _),
      s"${exuCfg.name} fp wb port has no priority"
    )
    require(
      wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
        fuConfigs.map(x => x.writeVecRf).reduce(_ || _),
      s"${exuCfg.name} vec wb port has no priority"
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

  println(s"[Backend] Fp RdConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getRdPortParams(FpData())) {
    println(s"[Backend]   port($port): ${seq.map(x => params.getExuName(x._1) + "(" + x._2.toString + ")").mkString(",")}")
  }

  println(s"[Backend] Fp WbConfigs: ExuName(Priority)")
  for ((port, seq) <- params.getWbPortParams(FpData())) {
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

  println(s"[Backend] Dispatch Configs:")
  println(s"[Backend] Load IQ enq width(${params.numLoadDp}), Store IQ enq width(${params.numStoreDp})")
  println(s"[Backend] Load DP width(${LSQLdEnqWidth}), Store DP width(${LSQStEnqWidth})")

  params.updateCopyPdestInfo
  println(s"[Backend] copyPdestInfo ${params.copyPdestInfo}")
  params.allExuParams.map(_.copyNum)
  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
  val fpScheduler = params.fpSchdParams.map(x => LazyModule(new Scheduler(x)))
  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val memScheduler = params.memSchdParams.map(x => LazyModule(new Scheduler(x)))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val fpExuBlock = params.fpSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val wbFuBusyTable = LazyModule(new WbFuBusyTable(params))

  lazy val module = new BackendInlinedImp(this)
}

class BackendInlinedImp(override val wrapper: BackendInlined)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasPerfEvents
  with HasCriticalErrors {
  implicit private val params: BackendParams = wrapper.params

  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intScheduler: SchedulerImpBase = wrapper.intScheduler.get.module
  private val fpScheduler = wrapper.fpScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val fpExuBlock = wrapper.fpExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val og2ForVector = Module(new Og2ForVector(params))
  private val bypassNetwork = Module(new BypassNetwork)
  private val wbDataPath = Module(new WbDataPath(params))
  private val wbFuBusyTable = wrapper.wbFuBusyTable.module
  private val vecExcpMod = Module(new VecExcpDataMergeModule)

  private val iqWakeUpMappedBundle: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] = (
    intScheduler.io.toSchedulers.wakeupVec ++
      fpScheduler.io.toSchedulers.wakeupVec ++
      vfScheduler.io.toSchedulers.wakeupVec ++
      memScheduler.io.toSchedulers.wakeupVec
    ).map(x => (x.bits.exuIdx, x)).toMap

  private val iqWakeUpMappedBundleDelayed: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] = (
    intScheduler.io.toSchedulers.wakeupVec ++
      fpScheduler.io.toSchedulers.wakeupVec ++
      vfScheduler.io.toSchedulers.wakeupVec ++
      memScheduler.io.toSchedulers.wakeupVec
    ).map{ case x =>
    val delayed = Wire(chiselTypeOf(x))
    // TODO: add clock gate use Wen, remove issuequeue wakeupToIQ logic Wen = Wen && valid
    delayed := RegNext(x)
    (x.bits.exuIdx, delayed)
  }.toMap

  println(s"[Backend] iq wake up keys: ${iqWakeUpMappedBundle.keys}")

  wbFuBusyTable.io.in.intSchdBusyTable := intScheduler.io.wbFuBusyTable
  wbFuBusyTable.io.in.fpSchdBusyTable := fpScheduler.io.wbFuBusyTable
  wbFuBusyTable.io.in.vfSchdBusyTable := vfScheduler.io.wbFuBusyTable
  wbFuBusyTable.io.in.memSchdBusyTable := memScheduler.io.wbFuBusyTable
  intScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.intRespRead
  fpScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.fpRespRead
  vfScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.vfRespRead
  memScheduler.io.fromWbFuBusyTable.fuBusyTableRead := wbFuBusyTable.io.out.memRespRead
  dataPath.io.wbConfictRead := wbFuBusyTable.io.out.wbConflictRead

  private val og1Cancel = dataPath.io.og1Cancel
  private val og0Cancel = dataPath.io.og0Cancel
  private val vlFromIntIsZero = intExuBlock.io.vlIsZero.get
  private val vlFromIntIsVlmax = intExuBlock.io.vlIsVlmax.get
  private val vlFromVfIsZero = vfExuBlock.io.vlIsZero.get
  private val vlFromVfIsVlmax = vfExuBlock.io.vlIsVlmax.get

  private val backendCriticalError = Wire(Bool())

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromCSR.toDecode := intExuBlock.io.csrToDecode.get
  ctrlBlock.io.fromCSR.traceCSR := intExuBlock.io.csrio.get.traceCSR
  ctrlBlock.io.fromCSR.instrAddrTransType := RegNext(intExuBlock.io.csrio.get.instrAddrTransType)
  ctrlBlock.io.fromWB.wbData <> wbDataPath.io.toCtrlBlock.writeback
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.lqCanAccept := io.mem.lqCanAccept
  ctrlBlock.io.sqCanAccept := io.mem.sqCanAccept

  io.mem.wfi <> ctrlBlock.io.toMem.wfi

  io.mem.lsqEnqIO <> ctrlBlock.io.toMem.lsqEnqIO
  ctrlBlock.io.fromMemToDispatch.scommit := io.mem.sqDeq
  ctrlBlock.io.fromMemToDispatch.lcommit := io.mem.lqDeq
  ctrlBlock.io.fromMemToDispatch.sqDeqPtr := io.mem.sqDeqPtr
  ctrlBlock.io.fromMemToDispatch.lqDeqPtr := io.mem.lqDeqPtr
  ctrlBlock.io.fromMemToDispatch.sqCancelCnt := io.mem.sqCancelCnt
  ctrlBlock.io.fromMemToDispatch.lqCancelCnt := io.mem.lqCancelCnt
  ctrlBlock.io.toDispatch.wakeUpInt := intScheduler.io.toSchedulers.wakeupVec
  ctrlBlock.io.toDispatch.wakeUpFp  := fpScheduler.io.toSchedulers.wakeupVec
  ctrlBlock.io.toDispatch.wakeUpVec := vfScheduler.io.toSchedulers.wakeupVec
  ctrlBlock.io.toDispatch.wakeUpMem := memScheduler.io.toSchedulers.wakeupVec
  ctrlBlock.io.toDispatch.IQValidNumVec := intScheduler.io.IQValidNumVec ++ fpScheduler.io.IQValidNumVec ++ vfScheduler.io.IQValidNumVec ++ memScheduler.io.IQValidNumVec
  ctrlBlock.io.toDispatch.ldCancel := io.mem.ldCancel
  ctrlBlock.io.toDispatch.og0Cancel := og0Cancel
  ctrlBlock.io.toDispatch.wbPregsInt.zip(wbDataPath.io.toIntPreg).map(x => {
    x._1.valid := x._2.wen && x._2.intWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsFp.zip(wbDataPath.io.toFpPreg).map(x => {
    x._1.valid := x._2.wen && x._2.fpWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsVec.zip(wbDataPath.io.toVfPreg).map(x => {
    x._1.valid := x._2.wen && x._2.vecWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsV0.zip(wbDataPath.io.toV0Preg).map(x => {
    x._1.valid := x._2.wen && x._2.v0Wen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsVl.zip(wbDataPath.io.toVlPreg).map(x => {
    x._1.valid := x._2.wen && x._2.vlWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromIntIsZero := vlFromIntIsZero
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromIntIsVlmax := vlFromIntIsVlmax
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromVfIsZero := vlFromVfIsZero
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromVfIsVlmax := vlFromVfIsVlmax
  ctrlBlock.io.csrCtrl <> intExuBlock.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intExuBlock.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intExuBlock.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intExuBlock.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intExuBlock.io.csrio.get.wfi_event
  ctrlBlock.io.robio.csr.criticalErrorState := intExuBlock.io.csrio.get.criticalErrorState
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO
  ctrlBlock.io.robio.lsTopdownInfo <> io.mem.lsTopdownInfo
  ctrlBlock.io.robio.debug_ls <> io.mem.debugLS
  ctrlBlock.io.debugEnqLsq.canAccept := io.mem.lsqEnqIO.canAccept
  ctrlBlock.io.debugEnqLsq.resp := io.mem.lsqEnqIO.resp
  ctrlBlock.io.debugEnqLsq.req := ctrlBlock.io.toMem.lsqEnqIO.req
  ctrlBlock.io.debugEnqLsq.needAlloc := ctrlBlock.io.toMem.lsqEnqIO.needAlloc
  ctrlBlock.io.debugEnqLsq.iqAccept := ctrlBlock.io.toMem.lsqEnqIO.iqAccept
  ctrlBlock.io.fromVecExcpMod.busy := vecExcpMod.o.status.busy

  val intWriteBackDelayed = Wire(chiselTypeOf(wbDataPath.io.toIntPreg))
  intWriteBackDelayed.zip(wbDataPath.io.toIntPreg).map{ case (sink, source) =>
    sink := DontCare
    sink.wen := RegNext(source.wen)
    sink.intWen := RegNext(source.intWen)
    sink.addr := RegEnable(source.addr, source.wen)
  }
  val fpWriteBackDelayed = Wire(chiselTypeOf(wbDataPath.io.toFpPreg))
  fpWriteBackDelayed.zip(wbDataPath.io.toFpPreg).map { case (sink, source) =>
    sink := DontCare
    sink.wen := RegNext(source.wen)
    sink.fpWen := RegNext(source.fpWen)
    sink.addr := RegEnable(source.addr, source.wen)
  }
  val vfWriteBackDelayed = Wire(chiselTypeOf(wbDataPath.io.toVfPreg))
  vfWriteBackDelayed.zip(wbDataPath.io.toVfPreg).map { case (sink, source) =>
    sink := DontCare
    sink.wen := RegNext(source.wen)
    sink.vecWen := RegNext(source.vecWen)
    sink.addr := RegEnable(source.addr, source.wen)
  }
  val v0WriteBackDelayed = Wire(chiselTypeOf(wbDataPath.io.toV0Preg))
  v0WriteBackDelayed.zip(wbDataPath.io.toV0Preg).map { case (sink, source) =>
    sink := DontCare
    sink.wen := RegNext(source.wen)
    sink.v0Wen := RegNext(source.v0Wen)
    sink.addr := RegEnable(source.addr, source.wen)
  }
  val vlWriteBackDelayed = Wire(chiselTypeOf(wbDataPath.io.toVlPreg))
  vlWriteBackDelayed.zip(wbDataPath.io.toVlPreg).map { case (sink, source) =>
    sink := DontCare
    sink.wen := RegNext(source.wen)
    sink.vlWen := RegNext(source.vlWen)
    sink.addr := RegEnable(source.addr, source.wen)
  }
  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.fpWriteBack := 0.U.asTypeOf(intScheduler.io.fpWriteBack)
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)
  intScheduler.io.v0WriteBack := 0.U.asTypeOf(intScheduler.io.v0WriteBack)
  intScheduler.io.vlWriteBack := 0.U.asTypeOf(intScheduler.io.vlWriteBack)
  intScheduler.io.intWriteBackDelayed := intWriteBackDelayed
  intScheduler.io.fpWriteBackDelayed := 0.U.asTypeOf(intScheduler.io.fpWriteBackDelayed)
  intScheduler.io.vfWriteBackDelayed := 0.U.asTypeOf(intScheduler.io.vfWriteBackDelayed)
  intScheduler.io.v0WriteBackDelayed := 0.U.asTypeOf(intScheduler.io.v0WriteBackDelayed)
  intScheduler.io.vlWriteBackDelayed := 0.U.asTypeOf(intScheduler.io.vlWriteBackDelayed)
  intScheduler.io.fromDataPath.resp := dataPath.io.toIntIQ
  intScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  intScheduler.io.fromSchedulers.wakeupVecDelayed.foreach { wakeup => wakeup := iqWakeUpMappedBundleDelayed(wakeup.bits.exuIdx) }
  intScheduler.io.fromDataPath.og0Cancel := og0Cancel
  intScheduler.io.fromDataPath.og1Cancel := og1Cancel
  intScheduler.io.ldCancel := io.mem.ldCancel
  intScheduler.io.fromDataPath.replaceRCIdx.get := dataPath.io.toWakeupQueueRCIdx.take(params.getIntExuRCWriteSize)
  intScheduler.io.vlWriteBackInfo.vlFromIntIsZero := false.B
  intScheduler.io.vlWriteBackInfo.vlFromIntIsVlmax := false.B
  intScheduler.io.vlWriteBackInfo.vlFromVfIsZero := false.B
  intScheduler.io.vlWriteBackInfo.vlFromVfIsVlmax := false.B

  fpScheduler.io.fromTop.hartId := io.fromTop.hartId
  fpScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  fpScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.fpUops
  fpScheduler.io.intWriteBack := 0.U.asTypeOf(fpScheduler.io.intWriteBack)
  fpScheduler.io.fpWriteBack := wbDataPath.io.toFpPreg
  fpScheduler.io.vfWriteBack := 0.U.asTypeOf(fpScheduler.io.vfWriteBack)
  fpScheduler.io.v0WriteBack := 0.U.asTypeOf(fpScheduler.io.v0WriteBack)
  fpScheduler.io.vlWriteBack := 0.U.asTypeOf(fpScheduler.io.vlWriteBack)
  fpScheduler.io.intWriteBackDelayed := 0.U.asTypeOf(intWriteBackDelayed)
  fpScheduler.io.fpWriteBackDelayed := fpWriteBackDelayed
  fpScheduler.io.vfWriteBackDelayed := 0.U.asTypeOf(intScheduler.io.vfWriteBackDelayed)
  fpScheduler.io.v0WriteBackDelayed := 0.U.asTypeOf(intScheduler.io.v0WriteBackDelayed)
  fpScheduler.io.vlWriteBackDelayed := 0.U.asTypeOf(intScheduler.io.vlWriteBackDelayed)
  fpScheduler.io.fromDataPath.resp := dataPath.io.toFpIQ
  fpScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  fpScheduler.io.fromSchedulers.wakeupVecDelayed.foreach { wakeup => wakeup := iqWakeUpMappedBundleDelayed(wakeup.bits.exuIdx) }
  fpScheduler.io.fromDataPath.og0Cancel := og0Cancel
  fpScheduler.io.fromDataPath.og1Cancel := og1Cancel
  fpScheduler.io.ldCancel := io.mem.ldCancel
  fpScheduler.io.vlWriteBackInfo.vlFromIntIsZero := false.B
  fpScheduler.io.vlWriteBackInfo.vlFromIntIsVlmax := false.B
  fpScheduler.io.vlWriteBackInfo.vlFromVfIsZero := false.B
  fpScheduler.io.vlWriteBackInfo.vlFromVfIsVlmax := false.B

  memScheduler.io.fromTop.hartId := io.fromTop.hartId
  memScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  memScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.memUops
  memScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  memScheduler.io.fpWriteBack := wbDataPath.io.toFpPreg
  memScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  memScheduler.io.v0WriteBack := wbDataPath.io.toV0Preg
  memScheduler.io.vlWriteBack := wbDataPath.io.toVlPreg
  memScheduler.io.intWriteBackDelayed := intWriteBackDelayed
  memScheduler.io.fpWriteBackDelayed := fpWriteBackDelayed
  memScheduler.io.vfWriteBackDelayed := vfWriteBackDelayed
  memScheduler.io.v0WriteBackDelayed := v0WriteBackDelayed
  memScheduler.io.vlWriteBackDelayed := vlWriteBackDelayed
  memScheduler.io.fromMem.get.scommit := io.mem.sqDeq
  memScheduler.io.fromMem.get.lcommit := io.mem.lqDeq
  memScheduler.io.fromMem.get.wakeup := io.mem.wakeup
  memScheduler.io.fromMem.get.sqDeqPtr := io.mem.sqDeqPtr
  memScheduler.io.fromMem.get.lqDeqPtr := io.mem.lqDeqPtr
  memScheduler.io.fromMem.get.sqCancelCnt := io.mem.sqCancelCnt
  memScheduler.io.fromMem.get.lqCancelCnt := io.mem.lqCancelCnt
  memScheduler.io.fromMem.get.stIssuePtr := io.mem.stIssuePtr
  require(memScheduler.io.fromMem.get.memWaitUpdateReq.robIdx.length == io.mem.stIn.length)
  memScheduler.io.fromMem.get.memWaitUpdateReq.robIdx.zip(io.mem.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits  := source.bits.robIdx
  }
  memScheduler.io.fromMem.get.memWaitUpdateReq.sqIdx := DontCare // TODO
  memScheduler.io.fromDataPath.resp := dataPath.io.toMemIQ
  memScheduler.io.fromMem.get.ldaFeedback := io.mem.ldaIqFeedback
  memScheduler.io.fromMem.get.staFeedback := io.mem.staIqFeedback
  memScheduler.io.fromMem.get.hyuFeedback := io.mem.hyuIqFeedback
  memScheduler.io.fromMem.get.vstuFeedback := io.mem.vstuIqFeedback
  memScheduler.io.fromMem.get.vlduFeedback := io.mem.vlduIqFeedback
  memScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  memScheduler.io.fromSchedulers.wakeupVecDelayed.foreach { wakeup => wakeup := iqWakeUpMappedBundleDelayed(wakeup.bits.exuIdx) }
  memScheduler.io.fromDataPath.og0Cancel := og0Cancel
  memScheduler.io.fromDataPath.og1Cancel := og1Cancel
  memScheduler.io.ldCancel := io.mem.ldCancel
  memScheduler.io.fromDataPath.replaceRCIdx.get := dataPath.io.toWakeupQueueRCIdx.takeRight(params.getMemExuRCWriteSize)
  memScheduler.io.vlWriteBackInfo.vlFromIntIsZero := vlFromIntIsZero
  memScheduler.io.vlWriteBackInfo.vlFromIntIsVlmax := vlFromIntIsVlmax
  memScheduler.io.vlWriteBackInfo.vlFromVfIsZero := vlFromVfIsZero
  memScheduler.io.vlWriteBackInfo.vlFromVfIsVlmax := vlFromVfIsVlmax
  memScheduler.io.fromOg2Resp.get := og2ForVector.io.toMemIQOg2Resp

  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.vfUops
  vfScheduler.io.intWriteBack := 0.U.asTypeOf(vfScheduler.io.intWriteBack)
  vfScheduler.io.fpWriteBack := 0.U.asTypeOf(vfScheduler.io.fpWriteBack)
  vfScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  vfScheduler.io.v0WriteBack := wbDataPath.io.toV0Preg
  vfScheduler.io.vlWriteBack := wbDataPath.io.toVlPreg
  vfScheduler.io.intWriteBackDelayed := 0.U.asTypeOf(intWriteBackDelayed)
  vfScheduler.io.fpWriteBackDelayed := 0.U.asTypeOf(fpWriteBackDelayed)
  vfScheduler.io.vfWriteBackDelayed := vfWriteBackDelayed
  vfScheduler.io.v0WriteBackDelayed := v0WriteBackDelayed
  vfScheduler.io.vlWriteBackDelayed := vlWriteBackDelayed
  vfScheduler.io.fromDataPath.resp := dataPath.io.toVfIQ
  vfScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  vfScheduler.io.fromSchedulers.wakeupVecDelayed.foreach { wakeup => wakeup := iqWakeUpMappedBundleDelayed(wakeup.bits.exuIdx) }
  vfScheduler.io.fromDataPath.og0Cancel := og0Cancel
  vfScheduler.io.fromDataPath.og1Cancel := og1Cancel
  vfScheduler.io.ldCancel := io.mem.ldCancel
  vfScheduler.io.vlWriteBackInfo.vlFromIntIsZero := vlFromIntIsZero
  vfScheduler.io.vlWriteBackInfo.vlFromIntIsVlmax := vlFromIntIsVlmax
  vfScheduler.io.vlWriteBackInfo.vlFromVfIsZero := vlFromVfIsZero
  vfScheduler.io.vlWriteBackInfo.vlFromVfIsVlmax := vlFromVfIsVlmax
  vfScheduler.io.fromOg2Resp.get := og2ForVector.io.toVfIQOg2Resp

  dataPath.io.hartId := io.fromTop.hartId
  dataPath.io.flush := ctrlBlock.io.toDataPath.flush

  dataPath.io.fromIntIQ <> intScheduler.io.toDataPathAfterDelay
  dataPath.io.fromFpIQ <> fpScheduler.io.toDataPathAfterDelay
  dataPath.io.fromVfIQ <> vfScheduler.io.toDataPathAfterDelay
  dataPath.io.fromMemIQ <> memScheduler.io.toDataPathAfterDelay

  dataPath.io.ldCancel := io.mem.ldCancel

  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  println(s"[Backend] wbDataPath.io.toVfPreg: ${wbDataPath.io.toVfPreg.size}, dataPath.io.fromFpWb: ${dataPath.io.fromVfWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromFpWb := wbDataPath.io.toFpPreg
  dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
  dataPath.io.fromV0Wb := wbDataPath.io.toV0Preg
  dataPath.io.fromVlWb := wbDataPath.io.toVlPreg
  dataPath.io.diffIntRat.foreach(_ := ctrlBlock.io.diff_int_rat.get)
  dataPath.io.diffFpRat .foreach(_ := ctrlBlock.io.diff_fp_rat.get)
  dataPath.io.diffVecRat.foreach(_ := ctrlBlock.io.diff_vec_rat.get)
  dataPath.io.diffV0Rat .foreach(_ := ctrlBlock.io.diff_v0_rat.get)
  dataPath.io.diffVlRat .foreach(_ := ctrlBlock.io.diff_vl_rat.get)
  dataPath.io.fromBypassNetwork := bypassNetwork.io.toDataPath
  dataPath.io.fromVecExcpMod.r := vecExcpMod.o.toVPRF.r
  dataPath.io.fromVecExcpMod.w := vecExcpMod.o.toVPRF.w
  dataPath.io.topDownInfo.lqEmpty := DelayN(io.topDownInfo.lqEmpty, 2)
  dataPath.io.topDownInfo.sqEmpty := DelayN(io.topDownInfo.sqEmpty, 2)
  dataPath.io.topDownInfo.l1Miss := RegNext(io.topDownInfo.l1Miss)
  dataPath.io.topDownInfo.l2TopMiss.l2Miss := io.topDownInfo.l2TopMiss.l2Miss
  dataPath.io.topDownInfo.l2TopMiss.l3Miss := io.topDownInfo.l2TopMiss.l3Miss

  og2ForVector.io.flush := ctrlBlock.io.toDataPath.flush
  og2ForVector.io.ldCancel := io.mem.ldCancel
  og2ForVector.io.fromOg1VfArith <> dataPath.io.toVecExu
  og2ForVector.io.fromOg1VecMem.zip(dataPath.io.toMemExu.zip(params.memSchdParams.get.issueBlockParams).filter(_._2.needOg2Resp).map(_._1))
    .foreach {
      case (og1Mem, datapathMem) => og1Mem <> datapathMem
    }
  og2ForVector.io.fromOg1ImmInfo := dataPath.io.og1ImmInfo.zip(params.allExuParams).filter(_._2.needOg2).map(_._1)

  println(s"[Backend] BypassNetwork OG1 Mem Size: ${bypassNetwork.io.fromDataPath.mem.zip(params.memSchdParams.get.issueBlockParams).filterNot(_._2.needOg2Resp).size}")
  println(s"[Backend] BypassNetwork OG2 Mem Size: ${bypassNetwork.io.fromDataPath.mem.zip(params.memSchdParams.get.issueBlockParams).filter(_._2.needOg2Resp).size}")
  println(s"[Backend] bypassNetwork.io.fromDataPath.mem: ${bypassNetwork.io.fromDataPath.mem.size}, dataPath.io.toMemExu: ${dataPath.io.toMemExu.size}")
  bypassNetwork.io.fromDataPath.int <> dataPath.io.toIntExu
  bypassNetwork.io.fromDataPath.fp <> dataPath.io.toFpExu
  bypassNetwork.io.fromDataPath.vf <> og2ForVector.io.toVfArithExu
  bypassNetwork.io.fromDataPath.mem.lazyZip(params.memSchdParams.get.issueBlockParams).lazyZip(dataPath.io.toMemExu).filterNot(_._2.needOg2Resp)
    .map(x => (x._1, x._3)).foreach {
      case (bypassMem, datapathMem) => bypassMem <> datapathMem
    }
  bypassNetwork.io.fromDataPath.mem.zip(params.memSchdParams.get.issueBlockParams).filter(_._2.needOg2Resp).map(_._1)
    .zip(og2ForVector.io.toVecMemExu).foreach {
      case (bypassMem, og2Mem) => bypassMem <> og2Mem
    }
  bypassNetwork.io.fromDataPath.immInfo := dataPath.io.og1ImmInfo
  bypassNetwork.io.fromDataPath.immInfo.zip(params.allExuParams).filter(_._2.needOg2).map(_._1)
    .zip(og2ForVector.io.toBypassNetworkImmInfo).foreach {
      case (immInfo, og2ImmInfo) => immInfo := og2ImmInfo
    }
  bypassNetwork.io.fromDataPath.rcData := dataPath.io.toBypassNetworkRCData
  bypassNetwork.io.fromExus.connectExuOutput(_.int)(intExuBlock.io.out)
  bypassNetwork.io.fromExus.connectExuOutput(_.fp)(fpExuBlock.io.out)
  bypassNetwork.io.fromExus.connectExuOutput(_.vf)(vfExuBlock.io.out)

  require(bypassNetwork.io.fromExus.mem.flatten.size == io.mem.writeBack.size,
    s"bypassNetwork.io.fromExus.mem.flatten.size(${bypassNetwork.io.fromExus.mem.flatten.size}: ${bypassNetwork.io.fromExus.mem.map(_.size)}, " +
    s"io.mem.writeback(${io.mem.writeBack.size})"
  )
  bypassNetwork.io.fromExus.mem.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.intWen := source.bits.uop.rfWen && source.bits.isFromLoadUnit
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
        ),
        Option("bypassNetwork2intExuBlock")
      )
    }
  }

  ctrlBlock.io.toDataPath.pcToDataPathIO <> dataPath.io.fromPcTargetMem

  private val csrin = intExuBlock.io.csrin.get
  csrin.hartId := io.fromTop.hartId
  csrin.msiInfo.valid := RegNext(io.fromTop.msiInfo.valid)
  csrin.msiInfo.bits := RegEnable(io.fromTop.msiInfo.bits, io.fromTop.msiInfo.valid)
  csrin.clintTime.valid := RegNext(io.fromTop.clintTime.valid)
  csrin.clintTime.bits := RegEnable(io.fromTop.clintTime.bits, io.fromTop.clintTime.valid)
  csrin.l2FlushDone := RegNext(io.fromTop.l2FlushDone)
  csrin.trapInstInfo := ctrlBlock.io.toCSR.trapInstInfo
  csrin.fromVecExcpMod.busy := vecExcpMod.o.status.busy
  csrin.criticalErrorState := backendCriticalError

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> WireDefault(0.U.asTypeOf(csrio.vpu)) // Todo

  val fromIntExuVsetVType = intExuBlock.io.vtype.getOrElse(0.U.asTypeOf((Valid(new VType))))
  val fromVfExuVsetVType = vfExuBlock.io.vtype.getOrElse(0.U.asTypeOf((Valid(new VType))))
  val fromVsetVType = Mux(fromIntExuVsetVType.valid, fromIntExuVsetVType.bits, fromVfExuVsetVType.bits)
  val vsetvlVType = RegEnable(fromVsetVType, 0.U.asTypeOf(new VType), fromIntExuVsetVType.valid || fromVfExuVsetVType.valid)
  ctrlBlock.io.toDecode.vsetvlVType := vsetvlVType

  val commitVType = ctrlBlock.io.robio.commitVType.vtype
  val hasVsetvl = ctrlBlock.io.robio.commitVType.hasVsetvl
  val vtype = VType.toVtypeStruct(Mux(hasVsetvl, vsetvlVType, commitVType.bits)).asUInt

  // csr not store the value of vl, so when using difftest we assign the value of vl to debugVl
  val debugVl_s0 = WireInit(UInt(VlData().dataWidth.W), 0.U)
  val debugVl_s1 = WireInit(UInt(VlData().dataWidth.W), 0.U)
  debugVl_s0 := dataPath.io.diffVl.getOrElse(0.U.asTypeOf(UInt(VlData().dataWidth.W)))
  debugVl_s1 := RegNext(debugVl_s0)
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vstart.valid
  csrio.vpu.set_vstart.bits := ctrlBlock.io.robio.csr.vstart.bits
  ctrlBlock.io.toDecode.vstart := csrio.vpu.vstart
  //Todo here need change design
  csrio.vpu.set_vtype.valid := commitVType.valid
  csrio.vpu.set_vtype.bits := ZeroExt(vtype, XLEN)
  csrio.vpu.vl := ZeroExt(debugVl_s1, XLEN)
  csrio.vpu.dirty_vs := ctrlBlock.io.robio.csr.dirty_vs
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.robDeqPtr := ctrlBlock.io.robio.robDeqPtr
  csrio.memExceptionVAddr := io.mem.exceptionAddr.vaddr
  csrio.memExceptionGPAddr := io.mem.exceptionAddr.gpaddr
  csrio.memExceptionIsForVSnonLeafPTE := io.mem.exceptionAddr.isForVSnonLeafPTE
  csrio.externalInterrupt := RegNext(io.fromTop.externalInterrupt)
  csrio.perf <> io.perf
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  private val fenceio = intExuBlock.io.fenceio.get
  io.fenceio <> fenceio

  // to fpExuBlock
  fpExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until fpExuBlock.io.in.length) {
    for (j <- 0 until fpExuBlock.io.in(i).length) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.fp(i)(j).bits.loadDependency, io.mem.ldCancel)
      NewPipelineConnect(
        bypassNetwork.io.toExus.fp(i)(j), fpExuBlock.io.in(i)(j), fpExuBlock.io.in(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.fp(i)(j).fire,
          bypassNetwork.io.toExus.fp(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          fpExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)
        ),
        Option("bypassNetwork2fpExuBlock")
      )
    }
  }

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
        ),
        Option("bypassNetwork2vfExuBlock")
      )

    }
  }

  intExuBlock.io.frm.foreach(_ := csrio.fpu.frm)
  fpExuBlock.io.frm.foreach(_ := csrio.fpu.frm)
  fpExuBlock.io.vxrm.foreach(_ := csrio.vpu.vxrm)
  vfExuBlock.io.frm.foreach(_ := csrio.fpu.frm)
  vfExuBlock.io.vxrm.foreach(_ := csrio.vpu.vxrm)

  wbDataPath.io.flush := ctrlBlock.io.redirect
  wbDataPath.io.fromTop.hartId := io.fromTop.hartId
  wbDataPath.io.fromIntExu <> intExuBlock.io.out
  wbDataPath.io.fromFpExu <> fpExuBlock.io.out
  wbDataPath.io.fromVfExu <> vfExuBlock.io.out
  wbDataPath.io.fromMemExu.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.data   := VecInit(Seq.fill(sink.bits.params.wbPathNum)(source.bits.data))
    sink.bits.pdest  := source.bits.uop.pdest
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
    sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
    sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
    sink.bits.v0Wen.foreach(_ := source.bits.uop.v0Wen)
    sink.bits.vlWen.foreach(_ := source.bits.uop.vlWen)
    sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
    sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
    sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
    sink.bits.debug := source.bits.debug
    sink.bits.debugInfo := source.bits.uop.debugInfo
    sink.bits.debug_seqNum := source.bits.uop.debug_seqNum
    sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
    sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
    sink.bits.predecodeInfo.foreach(_ := source.bits.uop.preDecodeInfo)
    sink.bits.vls.foreach(x => {
      x.vdIdx := source.bits.vdIdx.get
      x.vdIdxInField := source.bits.vdIdxInField.get
      x.vpu   := source.bits.uop.vpu
      x.oldVdPsrc := source.bits.uop.psrc(2)
      x.isIndexed := VlduType.isIndexed(source.bits.uop.fuOpType)
      x.isMasked := VlduType.isMasked(source.bits.uop.fuOpType)
      x.isStrided := VlduType.isStrided(source.bits.uop.fuOpType)
      x.isWhole := VlduType.isWhole(source.bits.uop.fuOpType)
      x.isVecLoad := VlduType.isVecLd(source.bits.uop.fuOpType)
      x.isVlm := VlduType.isMasked(source.bits.uop.fuOpType) && VlduType.isVecLd(source.bits.uop.fuOpType)
    })
    sink.bits.trigger.foreach(_ := source.bits.uop.trigger)
  }
  wbDataPath.io.fromCSR.vstart := csrio.vpu.vstart

  vecExcpMod.i.fromExceptionGen := ctrlBlock.io.toVecExcpMod.excpInfo
  vecExcpMod.i.fromRab.logicPhyRegMap := ctrlBlock.io.toVecExcpMod.logicPhyRegMap
  vecExcpMod.i.fromRat := ctrlBlock.io.toVecExcpMod.ratOldPest
  vecExcpMod.i.fromVprf := dataPath.io.toVecExcpMod

  // to mem
  private val memIssueParams = params.memSchdParams.get.issueBlockParams
  private val memExuBlocksHasLDU = memIssueParams.map(_.exuBlockParams.map(x => x.hasLoadFu || x.hasHyldaFu))
  private val memExuBlocksHasVecLoad = memIssueParams.map(_.exuBlockParams.map(x => x.hasVLoadFu))
  println(s"[Backend] memExuBlocksHasLDU: $memExuBlocksHasLDU")
  println(s"[Backend] memExuBlocksHasVecLoad: $memExuBlocksHasVecLoad")

  private val toMem = Wire(bypassNetwork.io.toExus.mem.cloneType)
  for (i <- toMem.indices) {
    for (j <- toMem(i).indices) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.mem(i)(j).bits.loadDependency, io.mem.ldCancel)
      val needIssueTimeout = memExuBlocksHasVecLoad(i)(j)
      val olderUopComing =
        if (needIssueTimeout && bypassNetwork.io.toExus.mem(i)(j).bits.lqIdx.nonEmpty)
          bypassNetwork.io.toExus.mem(i)(j).valid && toMem(i)(j).valid && !toMem(i)(j).fire &&
            (bypassNetwork.io.toExus.mem(i)(j).bits.lqIdx.get < toMem(i)(j).bits.lqIdx.get || bypassNetwork.io.toExus.mem(i)(j).bits.sqIdx.get < toMem(i)(j).bits.sqIdx.get) // Older inst come from iq.
        else
          false.B

      val issueTimeout =
        if (needIssueTimeout)
          Counter(0 until 14, toMem(i)(j).valid && !toMem(i)(j).fire, bypassNetwork.io.toExus.mem(i)(j).fire)._2
        else
          false.B

      if (memScheduler.io.loadFinalIssueResp(i).nonEmpty && memExuBlocksHasLDU(i)(j)) {
        memScheduler.io.loadFinalIssueResp(i)(j).valid := issueTimeout || olderUopComing
        memScheduler.io.loadFinalIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.loadFinalIssueResp(i)(j).bits.resp := RespType.block
        memScheduler.io.loadFinalIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
        memScheduler.io.loadFinalIssueResp(i)(j).bits.uopIdx.foreach(_ := toMem(i)(j).bits.vpu.get.vuopIdx)
        memScheduler.io.loadFinalIssueResp(i)(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
        memScheduler.io.loadFinalIssueResp(i)(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
      }

      if (memScheduler.io.vecLoadFinalIssueResp(i).nonEmpty && memExuBlocksHasVecLoad(i)(j)) {
        memScheduler.io.vecLoadFinalIssueResp(i)(j).valid := issueTimeout || olderUopComing
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.resp := RespType.block
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.uopIdx.foreach(_ := toMem(i)(j).bits.vpu.get.vuopIdx)
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
        memScheduler.io.vecLoadFinalIssueResp(i)(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
      }

      NewPipelineConnect(
        bypassNetwork.io.toExus.mem(i)(j), toMem(i)(j), toMem(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.mem(i)(j).fire,
          bypassNetwork.io.toExus.mem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          toMem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || issueTimeout
        ),
        Option("bypassNetwork2toMemExus"),
        isOlder = olderUopComing
      )

      if (memScheduler.io.memAddrIssueResp(i).nonEmpty && memExuBlocksHasLDU(i)(j)) {
        memScheduler.io.memAddrIssueResp(i)(j).valid := toMem(i)(j).fire && FuType.isLoad(toMem(i)(j).bits.fuType)
        memScheduler.io.memAddrIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.memAddrIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
        memScheduler.io.memAddrIssueResp(i)(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
        memScheduler.io.memAddrIssueResp(i)(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
        memScheduler.io.memAddrIssueResp(i)(j).bits.resp := RespType.success // for load inst, firing at toMem means issuing successfully
      }

      if (memScheduler.io.vecLoadIssueResp(i).nonEmpty && memExuBlocksHasVecLoad(i)(j)) {
        memScheduler.io.vecLoadIssueResp(i)(j) match {
          case resp =>
            resp.valid := toMem(i)(j).fire && VlduType.isVecLd(toMem(i)(j).bits.fuOpType)
            resp.bits.fuType := toMem(i)(j).bits.fuType
            resp.bits.robIdx := toMem(i)(j).bits.robIdx
            resp.bits.uopIdx.get := toMem(i)(j).bits.vpu.get.vuopIdx
            resp.bits.sqIdx.get := toMem(i)(j).bits.sqIdx.get
            resp.bits.lqIdx.get := toMem(i)(j).bits.lqIdx.get
            resp.bits.resp := RespType.success
        }
        if (backendParams.debugEn){
          dontTouch(memScheduler.io.vecLoadIssueResp(i)(j))
        }
      }
    }
  }

  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(toMem.flatten).foreach { case (sink, source) =>
    val enableMdp = Constantin.createRecord("EnableMdp", true)
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx              := source.bits.iqIdx
    sink.bits.isFirstIssue       := source.bits.isFirstIssue
    sink.bits.uop                := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src                := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r}
    sink.bits.uop.fuType         := source.bits.fuType
    sink.bits.uop.fuOpType       := source.bits.fuOpType
    sink.bits.uop.imm            := source.bits.imm
    sink.bits.uop.robIdx         := source.bits.robIdx
    sink.bits.uop.pdest          := source.bits.pdest
    sink.bits.uop.rfWen          := source.bits.rfWen.getOrElse(false.B)
    sink.bits.uop.fpWen          := source.bits.fpWen.getOrElse(false.B)
    sink.bits.uop.vecWen         := source.bits.vecWen.getOrElse(false.B)
    sink.bits.uop.v0Wen          := source.bits.v0Wen.getOrElse(false.B)
    sink.bits.uop.vlWen          := source.bits.vlWen.getOrElse(false.B)
    sink.bits.uop.flushPipe      := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc             := source.bits.pc.getOrElse(0.U) + (source.bits.ftqOffset.getOrElse(0.U) << instOffsetBits)
    sink.bits.uop.loadWaitBit    := Mux(enableMdp, source.bits.loadWaitBit.getOrElse(false.B), false.B)
    sink.bits.uop.waitForRobIdx  := Mux(enableMdp, source.bits.waitForRobIdx.getOrElse(0.U.asTypeOf(new RobPtr)), 0.U.asTypeOf(new RobPtr))
    sink.bits.uop.storeSetHit    := Mux(enableMdp, source.bits.storeSetHit.getOrElse(false.B), false.B)
    sink.bits.uop.loadWaitStrict := Mux(enableMdp, source.bits.loadWaitStrict.getOrElse(false.B), false.B)
    sink.bits.uop.ssid           := Mux(enableMdp, source.bits.ssid.getOrElse(0.U(SSIDWidth.W)), 0.U(SSIDWidth.W))
    sink.bits.uop.lqIdx          := source.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
    sink.bits.uop.sqIdx          := source.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    sink.bits.uop.ftqPtr         := source.bits.ftqIdx.getOrElse(0.U.asTypeOf(new FtqPtr))
    sink.bits.uop.ftqOffset      := source.bits.ftqOffset.getOrElse(0.U)
    sink.bits.uop.debugInfo      := source.bits.perfDebugInfo
    sink.bits.uop.debug_seqNum   := source.bits.debug_seqNum
    sink.bits.uop.vpu            := source.bits.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals))
    sink.bits.uop.preDecodeInfo  := source.bits.preDecode.getOrElse(0.U.asTypeOf(new PreDecodeInfo))
    sink.bits.uop.numLsElem      := source.bits.numLsElem.getOrElse(0.U) // Todo: remove this bundle, keep only the one below
    sink.bits.flowNum.foreach(_  := source.bits.numLsElem.get)
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  io.mem.isVlsException := ctrlBlock.io.robio.exception.bits.vls

  io.mem.storePcRead.zipWithIndex.foreach { case (storePcRead, i) =>
    storePcRead := ctrlBlock.io.memStPcRead(i).data
    ctrlBlock.io.memStPcRead(i).valid := io.mem.issueSta(i).valid
    ctrlBlock.io.memStPcRead(i).ptr := io.mem.issueSta(i).bits.uop.ftqPtr
    ctrlBlock.io.memStPcRead(i).offset := io.mem.issueSta(i).bits.uop.ftqOffset
  }

  io.mem.hyuPcRead.zipWithIndex.foreach( { case (hyuPcRead, i) =>
    hyuPcRead := ctrlBlock.io.memHyPcRead(i).data
    ctrlBlock.io.memHyPcRead(i).valid := io.mem.issueHylda(i).valid
    ctrlBlock.io.memHyPcRead(i).ptr := io.mem.issueHylda(i).bits.uop.ftqPtr
    ctrlBlock.io.memHyPcRead(i).offset := io.mem.issueHylda(i).bits.uop.ftqOffset
  })

  ctrlBlock.io.robio.robHeadLsIssue := io.mem.issueUops.map(deq => deq.fire && deq.bits.uop.robIdx === ctrlBlock.io.robio.robDeqPtr).reduce(_ || _)

  // mem io
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq
  io.mem.storeDebugInfo <> ctrlBlock.io.robio.storeDebugInfo

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  io.toTop.cpuHalted := ctrlBlock.io.toTop.cpuHalt

  io.traceCoreInterface <> ctrlBlock.io.traceCoreInterface

  io.debugTopDown.fromRob := ctrlBlock.io.debugTopDown.fromRob
  ctrlBlock.io.debugTopDown.fromCore := io.debugTopDown.fromCore

  io.debugRolling := ctrlBlock.io.debugRolling

  io.topDownInfo.noUopsIssued := RegNext(dataPath.io.topDownInfo.noUopsIssued)

  private val cg = ClockGate.genTeSrc
  dontTouch(cg)
  if(hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }

  if(backendParams.debugEn) {
    dontTouch(memScheduler.io)
    dontTouch(dataPath.io.toMemExu)
    dontTouch(wbDataPath.io.fromMemExu)
  }

  // reset tree
  if (p(DebugOptionsKey).ResetGen) {
    val rightResetTree = ResetGenNode(Seq(
      ModuleNode(dataPath),
      ModuleNode(intExuBlock),
      ModuleNode(fpExuBlock),
      ModuleNode(vfExuBlock),
      ModuleNode(bypassNetwork),
      ModuleNode(wbDataPath)
    ))
    val leftResetTree = ResetGenNode(Seq(
      ModuleNode(intScheduler),
      ModuleNode(fpScheduler),
      ModuleNode(vfScheduler),
      ModuleNode(memScheduler),
      ModuleNode(og2ForVector),
      ModuleNode(wbFuBusyTable),
      ResetGenNode(Seq(
        ModuleNode(ctrlBlock),
        // ResetGenNode(Seq(
          CellNode(io.frontendReset)
        // ))
      ))
    ))
    ResetGen(leftResetTree, reset, sim = false, io.dft_reset)
    ResetGen(rightResetTree, reset, sim = false, io.dft_reset)
  } else {
    io.frontendReset := DontCare
  }

  // perf events
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := RegNext(csrio.customCtrl.distribute_csr)
  val csrevents = pfevent.io.hpmevent.slice(8,16)

  val ctrlBlockPerf    = ctrlBlock.getPerfEvents
  val intSchedulerPerf = intScheduler.asInstanceOf[SchedulerArithImp].getPerfEvents
  val fpSchedulerPerf  = fpScheduler.asInstanceOf[SchedulerArithImp].getPerfEvents
  val vecSchedulerPerf = vfScheduler.asInstanceOf[SchedulerArithImp].getPerfEvents
  val memSchedulerPerf = memScheduler.asInstanceOf[SchedulerMemImp].getPerfEvents
  val dataPathPerf = dataPath.getPerfEvents

  val perfBackend  = Seq()
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ ctrlBlockPerf  ++ dataPathPerf ++
    intSchedulerPerf ++ fpSchedulerPerf ++ vecSchedulerPerf ++ memSchedulerPerf ++ perfBackend


  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("backend perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  csrio.perf.perfEventsBackend := VecInit(perfEvents.map(_._2.asTypeOf(new PerfEvent)))

  val ctrlBlockError = ctrlBlock.getCriticalErrors
  val intExuBlockError = intExuBlock.getCriticalErrors
  val criticalErrors = ctrlBlockError ++ intExuBlockError

  if (printCriticalError) {
    for (((name, error), _) <- criticalErrors.zipWithIndex) {
      XSError(error, s"critical error: $name \n")
    }
  }

  // expand to collect frontend/memblock/L2 critical errors
  backendCriticalError := criticalErrors.map(_._2).reduce(_ || _)

  io.toTop.cpuCriticalError := csrio.criticalErrorState
  io.toTop.msiAck := csrio.msiAck
}

class BackendMemIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // Since fast load replay always use load unit 0, Backend flips two load port to avoid conflicts
  val flippedLda = true
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize
  // In/Out // Todo: split it into one-direction bundle
  val lsqEnqIO = Flipped(new LsqEnqIO)
  val robLsqIO = new RobLsqIO
  val ldaIqFeedback = Vec(params.LduCnt, Flipped(new MemRSFeedbackIO))
  val staIqFeedback = Vec(params.StaCnt, Flipped(new MemRSFeedbackIO))
  val hyuIqFeedback = Vec(params.HyuCnt, Flipped(new MemRSFeedbackIO))
  val vstuIqFeedback = Flipped(Vec(params.VstuCnt, new MemRSFeedbackIO(isVector = true)))
  val vlduIqFeedback = Flipped(Vec(params.VlduCnt, new MemRSFeedbackIO(isVector = true)))
  val ldCancel = Vec(params.LdExuCnt, Input(new LoadCancelIO))
  val wakeup = Vec(params.LdExuCnt, Flipped(Valid(new DynInst)))
  val storePcRead = Vec(params.StaCnt, Output(UInt(VAddrBits.W)))
  val hyuPcRead = Vec(params.HyuCnt, Output(UInt(VAddrBits.W)))
  // Input
  val writebackLda = Vec(params.LduCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackSta = Vec(params.StaCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackStd = Vec(params.StdCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackHyuLda = Vec(params.HyuCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackHyuSta = Vec(params.HyuCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackVldu = Vec(params.VlduCnt, Flipped(DecoupledIO(new MemExuOutput(true))))

  val s3_delayed_load_error = Input(Vec(LoadPipelineWidth, Bool()))
  val stIn = Input(Vec(params.StaExuCnt, ValidIO(new DynInst())))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val exceptionAddr = Input(new Bundle {
    val vaddr = UInt(XLEN.W)
    val gpaddr = UInt(XLEN.W)
    val isForVSnonLeafPTE = Bool()
  })
  val sqDeq = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Input(UInt(log2Up(CommitWidth + 1).W))
  val sqDeqPtr = Input(new SqPtr)
  val lqDeqPtr = Input(new LqPtr)

  val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  val lqCanAccept = Input(Bool())
  val sqCanAccept = Input(Bool())

  val otherFastWakeup = Flipped(Vec(params.LduCnt + params.HyuCnt, ValidIO(new DynInst)))
  val stIssuePtr = Input(new SqPtr())

  val debugLS = Flipped(Output(new DebugLSIO))

  val lsTopdownInfo = Vec(params.LduCnt + params.HyuCnt, Flipped(Output(new LsTopdownInfo)))
  // Output
  val redirect = ValidIO(new Redirect)   // rob flush MemBlock
  val issueLda = MixedVec(Seq.fill(params.LduCnt)(DecoupledIO(new MemExuInput())))
  val issueSta = MixedVec(Seq.fill(params.StaCnt)(DecoupledIO(new MemExuInput())))
  val issueStd = MixedVec(Seq.fill(params.StdCnt)(DecoupledIO(new MemExuInput())))
  val issueHylda = MixedVec(Seq.fill(params.HyuCnt)(DecoupledIO(new MemExuInput())))
  val issueHysta = MixedVec(Seq.fill(params.HyuCnt)(DecoupledIO(new MemExuInput())))
  val issueVldu = MixedVec(Seq.fill(params.VlduCnt)(DecoupledIO(new MemExuInput(true))))

  val loadFastMatch = Vec(params.LduCnt, Output(UInt(params.LduCnt.W)))
  val loadFastImm   = Vec(params.LduCnt, Output(UInt(12.W))) // Imm_I

  val tlbCsr = Output(new TlbCsrBundle)
  val csrCtrl = Output(new CustomCSRCtrlIO)
  val sfence = Output(new SfenceBundle)
  val isStoreException = Output(Bool())
  val isVlsException = Output(Bool())

  val wfi = new WfiReqBundle
  // ATTENTION: The issue ports' sequence order should be the same as IQs' deq config
  private [backend] def issueUops: Seq[DecoupledIO[MemExuInput]] = {
    issueSta ++
      issueHylda ++ issueHysta ++
      issueLda ++
      issueVldu ++
      issueStd
  }.toSeq

  // ATTENTION: The writeback ports' sequence order should be the same as IQs' deq config
  private [backend] def writeBack: Seq[DecoupledIO[MemExuOutput]] = {
    writebackSta ++
      writebackHyuLda ++ writebackHyuSta ++
      writebackLda ++
      writebackVldu ++
      writebackStd
  }

  // store event difftest information
  val storeDebugInfo = Vec(EnsbufferWidth, new Bundle {
    val robidx = Input(new RobPtr)
    val pc     = Output(UInt(VAddrBits.W))
  })
}

class TopToBackendBundle(implicit p: Parameters) extends XSBundle with HasSoCParameter {
  val hartId            = Output(UInt(hartIdLen.W))
  val externalInterrupt = Output(new ExternalInterruptIO)
  val msiInfo           = Output(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
  val clintTime         = Output(ValidIO(UInt(64.W)))
  val l2FlushDone       = Output(Bool())
}

class BackendToTopBundle extends Bundle {
  val cpuHalted = Output(Bool())
  val cpuCriticalError = Output(Bool())
  val msiAck = Output(Bool())
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle with HasSoCParameter {
  val fromTop = Flipped(new TopToBackendBundle)

  val toTop = new BackendToTopBundle

  val traceCoreInterface = new TraceCoreInterface(hasOffset = true)
  val fenceio = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val frontend = Flipped(new FrontendToCtrlIO)
  val frontendSfence = Output(new SfenceBundle)
  val frontendCsrCtrl = Output(new CustomCSRCtrlIO)
  val frontendTlbCsr = Output(new TlbCsrBundle)
  val frontendReset = Output(Reset())

  val mem = new BackendMemIO

  val perf = Input(new PerfCounterIO)

  val tlb = Output(new TlbCsrBundle)

  val csrCustomCtrl = Output(new CustomCSRCtrlIO)

  val debugTopDown = new Bundle {
    val fromRob = new RobCoreTopDownIO
    val fromCore = new CoreDispatchTopDownIO
  }
  val debugRolling = new RobDebugRollingIO
  val topDownInfo = new TopDownInfo
  val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
  val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
}
