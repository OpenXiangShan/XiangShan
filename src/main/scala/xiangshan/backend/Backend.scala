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

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{Constantin, ZeroExt}
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, IssueQueueIQWakeUpBundle, LoadShouldCancel, MemExuInput, MemExuOutput, VPUCtrlSignals}
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.datapath.DataConfig.{IntData, VecData}
import xiangshan.backend.datapath.RdConfig.{IntRD, VfRD}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath._
import xiangshan.backend.dispatch.CoreDispatchTopDownIO
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, FuType, PerfCounterIO}
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.issue.{CancelNetwork, Scheduler, SchedulerImpBase}
import xiangshan.backend.rob.{RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.frontend.{FtqPtr, FtqRead, PreDecodeInfo}
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import scala.collection.mutable

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  override def shouldBeInlined: Boolean = false

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
      wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
        fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
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
  val pcTargetMem = LazyModule(new PcTargetMem(params))
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
  with HasXSParameter {
  implicit private val params = wrapper.params

  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val pcTargetMem = wrapper.pcTargetMem.module
  private val intScheduler: SchedulerImpBase = wrapper.intScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val og2ForVector = Module(new Og2ForVector(params))
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

  private val og1CancelOH: UInt = dataPath.io.og1CancelOH
  private val og0CancelOH: UInt = dataPath.io.og0CancelOH
  private val cancelToBusyTable = dataPath.io.cancelToBusyTable

  ctrlBlock.io.IQValidNumVec := intScheduler.io.IQValidNumVec
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
  ctrlBlock.perfinfo := DontCare // TODO: Implement backend hpm
  ctrlBlock.io.debugEnqLsq.canAccept := io.mem.lsqEnqIO.canAccept
  ctrlBlock.io.debugEnqLsq.resp := io.mem.lsqEnqIO.resp
  ctrlBlock.io.debugEnqLsq.req := memScheduler.io.memIO.get.lsqEnqIO.req
  ctrlBlock.io.debugEnqLsq.needAlloc := memScheduler.io.memIO.get.lsqEnqIO.needAlloc

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)
  intScheduler.io.fromDataPath.resp := dataPath.io.toIntIQ
  intScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  intScheduler.io.fromDataPath.og0Cancel := og0CancelOH
  intScheduler.io.fromDataPath.og1Cancel := og1CancelOH
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
  memScheduler.io.fromSchedulers.wakeupVec.foreach { wakeup => wakeup := iqWakeUpMappedBundle(wakeup.bits.exuIdx) }
  memScheduler.io.fromDataPath.og0Cancel := og0CancelOH
  memScheduler.io.fromDataPath.og1Cancel := og1CancelOH
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
  vfScheduler.io.fromDataPath.og0Cancel := og0CancelOH
  vfScheduler.io.fromDataPath.og1Cancel := og1CancelOH
  vfScheduler.io.ldCancel := io.mem.ldCancel
  vfScheduler.io.fromDataPath.cancelToBusyTable := cancelToBusyTable
  vfScheduler.io.fromOg2.get := og2ForVector.io.toVfIQ

  dataPath.io.hartId := io.fromTop.hartId
  dataPath.io.flush := ctrlBlock.io.toDataPath.flush

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

  og2ForVector.io.flush := ctrlBlock.io.toDataPath.flush
  og2ForVector.io.ldCancel := io.mem.ldCancel
  og2ForVector.io.fromOg1NoReg <> dataPath.io.toFpExu

  bypassNetwork.io.fromDataPath.int <> dataPath.io.toIntExu
  bypassNetwork.io.fromDataPath.vf <> og2ForVector.io.toVfExu
  bypassNetwork.io.fromDataPath.mem <> dataPath.io.toMemExu
  bypassNetwork.io.fromDataPath.immInfo := dataPath.io.og1ImmInfo
  bypassNetwork.io.fromExus.connectExuOutput(_.int)(intExuBlock.io.out)
  bypassNetwork.io.fromExus.connectExuOutput(_.vf)(vfExuBlock.io.out)

  require(bypassNetwork.io.fromExus.mem.flatten.size == io.mem.writeBack.size,
    s"bypassNetwork.io.fromExus.mem.flatten.size(${bypassNetwork.io.fromExus.mem.flatten.size}: ${bypassNetwork.io.fromExus.mem.map(_.size)}, " +
    s"io.mem.writeback(${io.mem.writeBack.size})"
  )
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
        ),
        Option("intExuBlock2bypassNetwork")
      )
    }
  }

  pcTargetMem.io.fromFrontendFtq := io.frontend.fromFtq
  pcTargetMem.io.toDataPath <> dataPath.io.fromPcTargetMem

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> 0.U.asTypeOf(csrio.vpu) // Todo

  val vsetvlVType = intExuBlock.io.vtype.getOrElse(0.U.asTypeOf(new VType))
  ctrlBlock.io.robio.vsetvlVType := vsetvlVType

  val debugVconfig = dataPath.io.debugVconfig match {
    case Some(x) => dataPath.io.debugVconfig.get.asTypeOf(new VConfig)
    case None => 0.U.asTypeOf(new VConfig)
  }
  val commitVType = ctrlBlock.io.robio.commitVType.vtype
  val hasVsetvl = ctrlBlock.io.robio.commitVType.hasVsetvl
  val vtype = VType.toVtypeStruct(Mux(hasVsetvl, vsetvlVType, commitVType.bits)).asUInt
  val debugVl = debugVconfig.vl
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vstart.valid
  csrio.vpu.set_vstart.bits := ctrlBlock.io.robio.csr.vstart.bits
  csrio.vpu.set_vtype.valid := ctrlBlock.io.robio.csr.vcsrFlag
  //Todo here need change design
  csrio.vpu.set_vtype.valid := commitVType.valid
  csrio.vpu.set_vtype.bits := ZeroExt(vtype, XLEN)
  csrio.vpu.set_vl.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vl.bits := ZeroExt(debugVl, XLEN)
  csrio.vpu.dirty_vs := ctrlBlock.io.robio.csr.dirty_vs
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionAddr.vaddr
  csrio.memExceptionGPAddr := io.mem.exceptionAddr.gpaddr
  csrio.externalInterrupt := io.fromTop.externalInterrupt
  csrio.distributedUpdate(0) := io.mem.csrDistributedUpdate
  csrio.distributedUpdate(1) := io.frontendCsrDistributedUpdate
  csrio.perf <> io.perf
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrio.perf.perfEventsCtrl <> ctrlBlock.getPerf
  private val fenceio = intExuBlock.io.fenceio.get
  io.fenceio <> fenceio
  fenceio.disableSfence := csrio.disableSfence
  fenceio.disableHfenceg := csrio.disableHfenceg
  fenceio.disableHfencev := csrio.disableHfencev
  fenceio.virtMode := csrio.customCtrl.virtMode

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
        Option("vfExuBlock2bypassNetwork")
      )

      vfExuBlock.io.in(i)(j).bits.vpu.foreach(_.vstart := csrio.vpu.vstart)
    }
  }

  intExuBlock.io.frm.foreach(_ := csrio.fpu.frm)
  vfExuBlock.io.frm.foreach(_ := csrio.fpu.frm)
  vfExuBlock.io.vxrm.foreach(_ := csrio.vpu.vxrm)

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
    sink.bits.predecodeInfo.foreach(_ := source.bits.uop.preDecodeInfo)
    sink.bits.vls.foreach(x => {
      x.vdIdx := source.bits.vdIdx.get
      x.vdIdxInField := source.bits.vdIdxInField.get
      x.vpu   := source.bits.uop.vpu
      x.oldVdPsrc := source.bits.uop.psrc(2)
      x.isIndexed := VlduType.isIndexed(source.bits.uop.fuOpType)
      x.isMasked := VlduType.isMasked(source.bits.uop.fuOpType)
    })
    sink.bits.trigger.foreach(_ := source.bits.uop.trigger)
  }

  // to mem
  private val memIssueParams = params.memSchdParams.get.issueBlockParams
  private val memExuBlocksHasLDU = memIssueParams.map(_.exuBlockParams.map(x => x.hasLoadFu || x.hasHyldaFu))
  println(s"[Backend] memExuBlocksHasLDU: $memExuBlocksHasLDU")

  private val toMem = Wire(bypassNetwork.io.toExus.mem.cloneType)
  for (i <- toMem.indices) {
    for (j <- toMem(i).indices) {
      val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.mem(i)(j).bits.loadDependency, io.mem.ldCancel)
      val issueTimeout =
        if (memExuBlocksHasLDU(i)(j))
          Counter(0 until 16, toMem(i)(j).valid && !toMem(i)(j).fire, bypassNetwork.io.toExus.mem(i)(j).fire)._2
        else
          false.B

      if (memScheduler.io.loadFinalIssueResp(i).nonEmpty && memExuBlocksHasLDU(i)(j)) {
        memScheduler.io.loadFinalIssueResp(i)(j).valid := issueTimeout
        memScheduler.io.loadFinalIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.loadFinalIssueResp(i)(j).bits.resp := RespType.block
        memScheduler.io.loadFinalIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
        memScheduler.io.loadFinalIssueResp(i)(j).bits.uopIdx.foreach(_ := toMem(i)(j).bits.vpu.get.vuopIdx)
      }

      NewPipelineConnect(
        bypassNetwork.io.toExus.mem(i)(j), toMem(i)(j), toMem(i)(j).fire,
        Mux(
          bypassNetwork.io.toExus.mem(i)(j).fire,
          bypassNetwork.io.toExus.mem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || shouldLdCancel,
          toMem(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush) || issueTimeout
        ),
        Option("bypassNetwork2toMemExus")
      )

      if (memScheduler.io.memAddrIssueResp(i).nonEmpty && memExuBlocksHasLDU(i)(j)) {
        memScheduler.io.memAddrIssueResp(i)(j).valid := toMem(i)(j).fire && FuType.isLoad(toMem(i)(j).bits.fuType)
        memScheduler.io.memAddrIssueResp(i)(j).bits.fuType := toMem(i)(j).bits.fuType
        memScheduler.io.memAddrIssueResp(i)(j).bits.robIdx := toMem(i)(j).bits.robIdx
        memScheduler.io.memAddrIssueResp(i)(j).bits.resp := RespType.success // for load inst, firing at toMem means issuing successfully
      }
    }
  }

  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(toMem.flatten).foreach { case (sink, source) =>
    val enableMdp = Constantin.createRecord("EnableMdp", true.B)(0)
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
    sink.bits.uop.flushPipe      := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc             := source.bits.pc.getOrElse(0.U)
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
    sink.bits.uop.vpu            := source.bits.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals))
    sink.bits.uop.preDecodeInfo  := source.bits.preDecode.getOrElse(0.U.asTypeOf(new PreDecodeInfo))
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  io.mem.isVlsException := ctrlBlock.io.robio.exception.bits.vls
  require(io.mem.loadPcRead.size == params.LduCnt)
  io.mem.loadPcRead.zipWithIndex.foreach { case (loadPcRead, i) =>
    loadPcRead := ctrlBlock.io.memLdPcRead(i).data
    ctrlBlock.io.memLdPcRead(i).vld := io.mem.issueLda(i).valid
    ctrlBlock.io.memLdPcRead(i).ptr := io.mem.issueLda(i).bits.uop.ftqPtr
    ctrlBlock.io.memLdPcRead(i).offset := io.mem.issueLda(i).bits.uop.ftqOffset
  }

  io.mem.storePcRead.zipWithIndex.foreach { case (storePcRead, i) =>
    storePcRead := ctrlBlock.io.memStPcRead(i).data
    ctrlBlock.io.memStPcRead(i).vld := io.mem.issueSta(i).valid
    ctrlBlock.io.memStPcRead(i).ptr := io.mem.issueSta(i).bits.uop.ftqPtr
    ctrlBlock.io.memStPcRead(i).offset := io.mem.issueSta(i).bits.uop.ftqOffset
  }

  io.mem.hyuPcRead.zipWithIndex.foreach( { case (hyuPcRead, i) =>
    hyuPcRead := ctrlBlock.io.memHyPcRead(i).data
    ctrlBlock.io.memHyPcRead(i).vld := io.mem.issueHylda(i).valid
    ctrlBlock.io.memHyPcRead(i).ptr := io.mem.issueHylda(i).bits.uop.ftqPtr
    ctrlBlock.io.memHyPcRead(i).offset := io.mem.issueHylda(i).bits.uop.ftqOffset
  })

  ctrlBlock.io.robio.robHeadLsIssue := io.mem.issueUops.map(deq => deq.fire && deq.bits.uop.robIdx === ctrlBlock.io.robio.robDeqPtr).reduce(_ || _)

  // mem io
  io.mem.lsqEnqIO <> memScheduler.io.memIO.get.lsqEnqIO
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  io.toTop.cpuHalted := false.B // TODO: implement cpu halt

  io.debugTopDown.fromRob := ctrlBlock.io.debugTopDown.fromRob
  ctrlBlock.io.debugTopDown.fromCore := io.debugTopDown.fromCore

  io.debugRolling := ctrlBlock.io.debugRolling

  if(backendParams.debugEn) {
    dontTouch(memScheduler.io)
    dontTouch(dataPath.io.toMemExu)
    dontTouch(wbDataPath.io.fromMemExu)
  }
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
  val ldCancel = Vec(params.LdExuCnt, Flipped(new LoadCancelIO))
  val wakeup = Vec(params.LdExuCnt, Flipped(Valid(new DynInst)))
  val loadPcRead = Vec(params.LduCnt, Output(UInt(VAddrBits.W)))
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
    val vaddr = UInt(VAddrBits.W)
    val gpaddr = UInt(GPAddrBits.W)
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

  val csrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

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
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
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

  val debugTopDown = new Bundle {
    val fromRob = new RobCoreTopDownIO
    val fromCore = new CoreDispatchTopDownIO
  }
  val debugRolling = new RobDebugRollingIO
}
