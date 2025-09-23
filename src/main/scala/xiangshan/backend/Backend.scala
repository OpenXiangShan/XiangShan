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
import xiangshan.backend.Bundles._
import xiangshan.backend.ctrlblock.{DebugLSIO, LsTopdownInfo}
import xiangshan.backend.datapath.DataConfig.{IntData, VecData, FpData}
import xiangshan.backend.datapath.RdConfig.{IntRD, VfRD}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath._
import xiangshan.backend.dispatch.CoreDispatchTopDownIO
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, FuConfig, FuType, PerfCounterIO}
import xiangshan.backend.fu.NewCSR.PFEvent
import xiangshan.backend.issue.EntryBundles._
import xiangshan.backend.issue.Region
import xiangshan.backend.rob.{RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.frontend.{PreDecodeInfo}
import xiangshan.frontend.ftq.{FtqPtr, FtqRead}
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

  println(s"[Backend] debugEn:${backendParams.debugEn}")
  println(s"[Backend] basicDebugEn:${backendParams.basicDebugEn}")
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

  lazy val module = new BackendInlinedImp(this)
}

class BackendInlinedImp(override val wrapper: BackendInlined)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasPerfEvents
  with HasCriticalErrors {
  implicit private val params: BackendParams = wrapper.params

  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intRegion = Module(new Region(params.intSchdParams.get))
  private val fpRegion = Module(new Region(params.fpSchdParams.get))
  private val vecRegion = Module(new Region(params.vecSchdParams.get))
  private val vecExcpMod = Module(new VecExcpDataMergeModule)


  private val vlFromIntIsZero = intRegion.io.vlWriteBackInfoOut.vlFromIntIsZero
  private val vlFromIntIsVlmax = intRegion.io.vlWriteBackInfoOut.vlFromIntIsVlmax
  private val vlFromVfIsZero = vecRegion.io.vlWriteBackInfoOut.vlFromVfIsZero
  private val vlFromVfIsVlmax = vecRegion.io.vlWriteBackInfoOut.vlFromVfIsVlmax

  private val backendCriticalError = Wire(Bool())

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromBJUResolve := intRegion.io.toFrontendBJUResolve.get
  ctrlBlock.io.fromCSR.toDecode := intRegion.io.csrToDecode.get
  ctrlBlock.io.fromCSR.traceCSR := intRegion.io.csrio.get.traceCSR
  ctrlBlock.io.fromCSR.instrAddrTransType := RegNext(intRegion.io.csrio.get.instrAddrTransType)
  val wbDataPathToCtrlBlock = intRegion.io.wbDataPathToCtrlBlock.writeback ++
    fpRegion.io.wbDataPathToCtrlBlock.writeback ++
    vecRegion.io.wbDataPathToCtrlBlock.writeback
  println(s"[Backend] intRegion.io.wbDataPathToCtrlBlock.writeback.size = ${intRegion.io.wbDataPathToCtrlBlock.writeback.size}")
  println(s"[Backend] fpRegion.io.wbDataPathToCtrlBlock.writeback.size = ${fpRegion.io.wbDataPathToCtrlBlock.writeback.size}")
  println(s"[Backend] vecRegion.io.wbDataPathToCtrlBlock.writeback.size = ${vecRegion.io.wbDataPathToCtrlBlock.writeback.size}")
  println(s"[Backend] ctrlBlock.io.fromWB.wbData.size = ${ctrlBlock.io.fromWB.wbData.size}, wbDataPathToCtrlBlock.size = ${wbDataPathToCtrlBlock.size}")
  assert(ctrlBlock.io.fromWB.wbData.size == wbDataPathToCtrlBlock.size, "ctrlBlock.io.fromWB.wbData.size == wbDataPathToCtrlBlock.size")
  ctrlBlock.io.fromWB.wbData.zip(wbDataPathToCtrlBlock).map(x => x._1 := x._2)
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.lqCanAccept := io.mem.lqCanAccept
  ctrlBlock.io.sqCanAccept := io.mem.sqCanAccept

  io.mem.wfi <> ctrlBlock.io.toMem.wfi
  io.mem.loadFastMatch := 0.U.asTypeOf(io.mem.loadFastMatch)
  io.mem.loadFastImm := 0.U.asTypeOf(io.mem.loadFastImm)

  io.mem.lsqEnqIO <> ctrlBlock.io.toMem.lsqEnqIO
  ctrlBlock.io.fromMemToDispatch.scommit := io.mem.sqDeq
  ctrlBlock.io.fromMemToDispatch.lcommit := io.mem.lqDeq
  ctrlBlock.io.fromMemToDispatch.sqDeqPtr := io.mem.sqDeqPtr
  ctrlBlock.io.fromMemToDispatch.lqDeqPtr := io.mem.lqDeqPtr
  ctrlBlock.io.fromMemToDispatch.sqCancelCnt := io.mem.sqCancelCnt
  ctrlBlock.io.fromMemToDispatch.lqCancelCnt := io.mem.lqCancelCnt
  ctrlBlock.io.toDispatch.wakeUpInt := intRegion.io.wakeUpToDispatch
  ctrlBlock.io.toDispatch.wakeUpFp  := fpRegion.io.wakeUpToDispatch
  ctrlBlock.io.toDispatch.wakeUpVec := vecRegion.io.wakeUpToDispatch
  ctrlBlock.io.toDispatch.IQValidNumVec := intRegion.io.IQValidNumVec ++ fpRegion.io.IQValidNumVec ++ vecRegion.io.IQValidNumVec
  ctrlBlock.io.toDispatch.ldCancel := io.mem.ldCancel
  val og0Cancel = (intRegion.io.og0Cancel.asUInt | fpRegion.io.og0Cancel.asUInt | vecRegion.io.og0Cancel.asUInt).asBools
  ctrlBlock.io.toDispatch.og0Cancel := og0Cancel
  ctrlBlock.io.toDispatch.wbPregsInt.zip(intRegion.io.toIntPreg).map(x => {
    x._1.valid := x._2.wen && x._2.intWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsFp.zip(fpRegion.io.toFpPreg).map(x => {
    x._1.valid := x._2.wen && x._2.fpWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsVec.zip(vecRegion.io.toVfPreg).map(x => {
    x._1.valid := x._2.wen && x._2.vecWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsV0.zip(vecRegion.io.toV0Preg).map(x => {
    x._1.valid := x._2.wen && x._2.v0Wen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.wbPregsVl.zip(vecRegion.io.toVlPreg).map(x => {
    x._1.valid := x._2.wen && x._2.vlWen
    x._1.bits := x._2.addr
  })
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromIntIsZero := vlFromIntIsZero
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromIntIsVlmax := vlFromIntIsVlmax
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromVfIsZero := vlFromVfIsZero
  ctrlBlock.io.toDispatch.vlWriteBackInfo.vlFromVfIsVlmax := vlFromVfIsVlmax
  ctrlBlock.io.csrCtrl <> intRegion.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intRegion.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intRegion.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intRegion.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intRegion.io.csrio.get.wfi_event
  ctrlBlock.io.robio.csr.criticalErrorState := intRegion.io.csrio.get.criticalErrorState
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO
  ctrlBlock.io.robio.lsTopdownInfo <> io.mem.lsTopdownInfo
  ctrlBlock.io.robio.debug_ls <> io.mem.debugLS
  ctrlBlock.io.debugEnqLsq.canAccept := io.mem.lsqEnqIO.canAccept
  ctrlBlock.io.debugEnqLsq.resp := io.mem.lsqEnqIO.resp
  ctrlBlock.io.debugEnqLsq.req := ctrlBlock.io.toMem.lsqEnqIO.req
  ctrlBlock.io.debugEnqLsq.needAlloc := ctrlBlock.io.toMem.lsqEnqIO.needAlloc
  ctrlBlock.io.debugEnqLsq.iqAccept := ctrlBlock.io.toMem.lsqEnqIO.iqAccept
  ctrlBlock.io.fromVecExcpMod.busy := vecExcpMod.o.status.busy

  intRegion.io.hartId := io.fromTop.hartId
  intRegion.io.flush := ctrlBlock.io.toIssueBlock.flush
  intRegion.io.fromDispatch.flatten.zip(ctrlBlock.io.toIssueBlock.intUops).map { case (sink, source) => {
    sink.valid := source.valid
    connectSamePort(sink.bits, source.bits)
    source.ready := sink.ready
  }
  }
  println(s"[Backend] intRegion.io.fromMemExuOutput.size = ${intRegion.io.fromMemExuOutput.size}")
  val allWriteBack = io.mem.writebackLda ++ io.mem.writebackSta ++ io.mem.writebackStd
  intRegion.io.fromMemExuOutput.zip(allWriteBack).map{ case(sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
    source.ready := sink.ready
  }
  fpRegion.io.fromLduOutput.get.zip(io.mem.writebackLda).map { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
  }
  intRegion.io.wakeUpFromFp.foreach(x => x := fpRegion.io.wakeUpToDispatch)
  intRegion.io.wakeupFromF2I.foreach(x => x := fpRegion.io.F2IWakeupOut.get)
  fpRegion.io.wakeUpFromInt.foreach(x => x := intRegion.io.wakeUpToDispatch)
  fpRegion.io.I2FWakeupIn.foreach(x => x := intRegion.io.I2FWakeupOut.get)
  fpRegion.io.wakeupFromI2F.foreach(x => x := intRegion.io.I2FWakeupOut.get)
  intRegion.io.F2IWakeupIn.foreach(x => x := fpRegion.io.F2IWakeupOut.get)
  intRegion.io.wakeupFromLDU.foreach(x => x := io.mem.wakeup)
  intRegion.io.staFeedback.foreach(x => x := io.mem.staIqFeedback)
  vecRegion.io.vstuFeedback.foreach(x => x := io.mem.vstuIqFeedback)
  intRegion.io.ldCancel := io.mem.ldCancel
  intRegion.io.vlWriteBackInfoIn := 0.U.asTypeOf(intRegion.io.vlWriteBackInfoIn)
  val regions = Seq(intRegion, fpRegion, vecRegion)
  regions.map{ case x =>
    x.io.fromIntWb := 0.U.asTypeOf(x.io.fromIntWb)
    x.io.fromFpWb := 0.U.asTypeOf(x.io.fromFpWb)
    x.io.fromVfWb := 0.U.asTypeOf(x.io.fromVfWb)
    x.io.fromV0Wb := 0.U.asTypeOf(x.io.fromV0Wb)
    x.io.fromVlWb := 0.U.asTypeOf(x.io.fromVlWb)
  }
  intRegion.io.fromIntWb := intRegion.io.toIntPreg
  vecRegion.io.fromVfWb := vecRegion.io.toVfPreg
  vecRegion.io.fromV0Wb := vecRegion.io.toV0Preg
  vecRegion.io.fromVlWb := vecRegion.io.toVlPreg

  fpRegion.io.hartId := io.fromTop.hartId
  fpRegion.io.flush := ctrlBlock.io.toIssueBlock.flush
  fpRegion.io.fromDispatch.flatten.zip(ctrlBlock.io.toIssueBlock.fpUops).map{ case (sink, source) => {
    sink.valid := source.valid
    connectSamePort(sink.bits, source.bits)
    source.ready := sink.ready
  }
  }
  fpRegion.io.ldCancel := io.mem.ldCancel
  fpRegion.io.vlWriteBackInfoIn := 0.U.asTypeOf(intRegion.io.vlWriteBackInfoIn)

  vecRegion.io.hartId := io.fromTop.hartId
  vecRegion.io.flush := ctrlBlock.io.toIssueBlock.flush
  vecRegion.io.fromDispatch.flatten.zip(ctrlBlock.io.toIssueBlock.vfUops).map { case (sink, source) => {
    sink.valid := source.valid
    connectSamePort(sink.bits, source.bits)
    source.ready := sink.ready
  }
  }
  vecRegion.io.fromMemExuOutput <> io.mem.writebackVldu
  vecRegion.io.ldCancel := io.mem.ldCancel
  vecRegion.io.vlWriteBackInfoIn.vlFromIntIsZero := vlFromIntIsZero
  vecRegion.io.vlWriteBackInfoIn.vlFromIntIsVlmax := vlFromIntIsVlmax
  vecRegion.io.vlWriteBackInfoIn.vlFromVfIsZero := vlFromVfIsZero
  vecRegion.io.vlWriteBackInfoIn.vlFromVfIsVlmax := vlFromVfIsVlmax
  vecRegion.io.lqDeqPtr.get := io.mem.lqDeqPtr
  vecRegion.io.sqDeqPtr.get := io.mem.sqDeqPtr
  // for wbDatapath wirte regfile
  intRegion.io.fromFpExu.get := fpRegion.io.exuOut
  intRegion.io.fromVecExu.get := vecRegion.io.exuOut
  fpRegion.io.fromIntExu.get := intRegion.io.exuOut
  fpRegion.io.fromVecExu.get := vecRegion.io.exuOut
  vecRegion.io.fromIntExu.get := intRegion.io.exuOut
  vecRegion.io.fromFpExu.get := fpRegion.io.exuOut
  // for fast wakeup data
  intRegion.io.formFpExuBlockOut.get <> fpRegion.io.fpExuBlockOut.get
  intRegion.io.intSchdBusyTable := intRegion.io.wbFuBusyTableWriteOut
  intRegion.io.fpSchdBusyTable := fpRegion.io.wbFuBusyTableWriteOut
  intRegion.io.vfSchdBusyTable := vecRegion.io.wbFuBusyTableWriteOut
  fpRegion.io.intSchdBusyTable := intRegion.io.wbFuBusyTableWriteOut
  fpRegion.io.fpSchdBusyTable := fpRegion.io.wbFuBusyTableWriteOut
  fpRegion.io.vfSchdBusyTable := vecRegion.io.wbFuBusyTableWriteOut
  vecRegion.io.intSchdBusyTable := intRegion.io.wbFuBusyTableWriteOut
  vecRegion.io.fpSchdBusyTable := fpRegion.io.wbFuBusyTableWriteOut
  vecRegion.io.vfSchdBusyTable := vecRegion.io.wbFuBusyTableWriteOut
  // for intIQ read fp regfile
  fpRegion.io.fromIntIQ.get <> intRegion.io.intIQOut.get
  intRegion.io.fpRfRdataIn.get := fpRegion.io.fpRfRdataOut.get
  // for fpIQ write int regfile arbiter
  intRegion.io.fromFpIQ.get <> fpRegion.io.fpIQOut.get

  intRegion.io.diffIntRat.foreach(_ := ctrlBlock.io.diff_int_rat.get)
  fpRegion.io.diffFpRat.foreach(_ := ctrlBlock.io.diff_fp_rat.get)
  vecRegion.io.diffVecRat.foreach(_ := ctrlBlock.io.diff_vec_rat.get)
  vecRegion.io.diffV0Rat.foreach(_ := ctrlBlock.io.diff_v0_rat.get)
  vecRegion.io.diffVlRat.foreach(_ := ctrlBlock.io.diff_vl_rat.get)
  vecRegion.io.fromVecExcpMod.get.r := vecExcpMod.o.toVPRF.r
  vecRegion.io.fromVecExcpMod.get.w := vecExcpMod.o.toVPRF.w

  ctrlBlock.io.toDataPath.pcToDataPathIO <> intRegion.io.fromPcTargetMem.get
  val toMem = intRegion.io.toMemExu.get ++ vecRegion.io.toMemExu.get
  io.mem.issueUops.zip(toMem.flatten).foreach { case (sink, source) =>
    val enableMdp = Constantin.createRecord("EnableMdp", true)
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx := source.bits.iqIdx
    sink.bits.isFirstIssue := source.bits.isFirstIssue
    sink.bits.uop := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r }
    sink.bits.uop.fuType := source.bits.fuType
    sink.bits.uop.fuOpType := source.bits.fuOpType
    sink.bits.uop.imm := source.bits.imm
    sink.bits.uop.robIdx := source.bits.robIdx
    sink.bits.uop.pdest := source.bits.pdest
    sink.bits.uop.rfWen := source.bits.rfWen.getOrElse(false.B)
    sink.bits.uop.fpWen := source.bits.fpWen.getOrElse(false.B)
    sink.bits.uop.vecWen := source.bits.vecWen.getOrElse(false.B)
    sink.bits.uop.v0Wen := source.bits.v0Wen.getOrElse(false.B)
    sink.bits.uop.vlWen := source.bits.vlWen.getOrElse(false.B)
    sink.bits.uop.flushPipe := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc := source.bits.pc.getOrElse(0.U) + (source.bits.ftqOffset.getOrElse(0.U) << instOffsetBits)
    sink.bits.uop.loadWaitBit := Mux(enableMdp, source.bits.loadWaitBit.getOrElse(false.B), false.B)
    sink.bits.uop.waitForRobIdx := Mux(enableMdp, source.bits.waitForRobIdx.getOrElse(0.U.asTypeOf(new RobPtr)), 0.U.asTypeOf(new RobPtr))
    sink.bits.uop.storeSetHit := Mux(enableMdp, source.bits.storeSetHit.getOrElse(false.B), false.B)
    sink.bits.uop.loadWaitStrict := Mux(enableMdp, source.bits.loadWaitStrict.getOrElse(false.B), false.B)
    sink.bits.uop.ssid := Mux(enableMdp, source.bits.ssid.getOrElse(0.U(SSIDWidth.W)), 0.U(SSIDWidth.W))
    sink.bits.uop.lqIdx := source.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
    sink.bits.uop.sqIdx := source.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    sink.bits.uop.ftqPtr := source.bits.ftqIdx.getOrElse(0.U.asTypeOf(new FtqPtr))
    sink.bits.uop.ftqOffset := source.bits.ftqOffset.getOrElse(0.U)
    sink.bits.uop.debugInfo := source.bits.perfDebugInfo
    sink.bits.uop.debug_seqNum := source.bits.debug_seqNum
    sink.bits.uop.vpu := source.bits.vpu.getOrElse(0.U.asTypeOf(new VPUCtrlSignals))
    sink.bits.uop.preDecodeInfo := source.bits.preDecode.getOrElse(0.U.asTypeOf(new PreDecodeInfo))
    sink.bits.uop.numLsElem := source.bits.numLsElem.getOrElse(0.U) // Todo: remove this bundle, keep only the one below
    sink.bits.flowNum.foreach(_ := source.bits.numLsElem.get)
  }
  private val csrin = intRegion.io.csrin.get
  csrin.hartId := io.fromTop.hartId
  csrin.msiInfo.valid := RegNext(io.fromTop.msiInfo.valid)
  csrin.msiInfo.bits := RegEnable(io.fromTop.msiInfo.bits, io.fromTop.msiInfo.valid)
  csrin.clintTime.valid := RegNext(io.fromTop.clintTime.valid)
  csrin.clintTime.bits := RegEnable(io.fromTop.clintTime.bits, io.fromTop.clintTime.valid)
  csrin.l2FlushDone := RegNext(io.fromTop.l2FlushDone)
  csrin.trapInstInfo := ctrlBlock.io.toCSR.trapInstInfo
  csrin.fromVecExcpMod.busy := vecExcpMod.o.status.busy
  csrin.criticalErrorState := backendCriticalError

  private val csrio = intRegion.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> WireDefault(0.U.asTypeOf(csrio.vpu)) // Todo

  val fromIntExuVsetVType = intRegion.io.vtype.getOrElse(0.U.asTypeOf((Valid(new VType))))
  val fromVfExuVsetVType = vecRegion.io.vtype.getOrElse(0.U.asTypeOf((Valid(new VType))))
  val fromVsetVType = Mux(fromIntExuVsetVType.valid, fromIntExuVsetVType.bits, fromVfExuVsetVType.bits)
  val vsetvlVType = RegEnable(fromVsetVType, 0.U.asTypeOf(new VType), fromIntExuVsetVType.valid || fromVfExuVsetVType.valid)
  ctrlBlock.io.toDecode.vsetvlVType := vsetvlVType

  val commitVType = ctrlBlock.io.robio.commitVType.vtype
  val hasVsetvl = ctrlBlock.io.robio.commitVType.hasVsetvl
  val vtype = VType.toVtypeStruct(Mux(hasVsetvl, vsetvlVType, commitVType.bits)).asUInt

  // csr not store the value of vl, so when using difftest we assign the value of vl to debugVl
  val debugVl_s0 = WireInit(UInt(VlData().dataWidth.W), 0.U)
  val debugVl_s1 = WireInit(UInt(VlData().dataWidth.W), 0.U)
  debugVl_s0 := vecRegion.io.diffVl.getOrElse(0.U.asTypeOf(UInt(VlData().dataWidth.W)))
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
  private val fenceio = intRegion.io.fenceio.get
  io.fenceio <> fenceio

  fpRegion.io.I2FDataIn.get := intRegion.io.I2FDataOut.get
  intRegion.io.F2IDataIn.get := fpRegion.io.F2IDataOut.get


  intRegion.io.frm := csrio.fpu.frm
  intRegion.io.vxrm := csrio.vpu.vxrm
  fpRegion.io.frm := csrio.fpu.frm
  fpRegion.io.vxrm := csrio.vpu.vxrm
  vecRegion.io.frm := csrio.fpu.frm
  vecRegion.io.vxrm := csrio.vpu.vxrm
  vecRegion.io.vstart.get := csrio.vpu.vstart

  vecExcpMod.i.fromExceptionGen := ctrlBlock.io.toVecExcpMod.excpInfo
  vecExcpMod.i.fromRab.logicPhyRegMap := ctrlBlock.io.toVecExcpMod.logicPhyRegMap
  vecExcpMod.i.fromRat := ctrlBlock.io.toVecExcpMod.ratOldPest
  vecExcpMod.i.fromVprf := vecRegion.io.toVecExcpMod.get

  io.mem.redirect := ctrlBlock.io.redirect
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

  io.topDownInfo.noUopsIssued := false.B

  private val cg = ClockGate.genTeSrc
  dontTouch(cg)
  if(hasMbist) {
    cg.cgen := io.dft.get.cgen
  } else {
    cg.cgen := false.B
  }
  // reset tree
  if (p(DebugOptionsKey).ResetGen) {
    val rightResetTree = ResetGenNode(Seq(
      ModuleNode(intRegion),
      ModuleNode(fpRegion),
    ))
    val leftResetTree = ResetGenNode(Seq(
      ModuleNode(vecRegion),
      ModuleNode(vecExcpMod),
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

  // TODO fix perf events and topDown
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := RegNext(csrio.customCtrl.distribute_csr)
  val csrevents = pfevent.io.hpmevent.slice(8,16)

  val ctrlBlockPerf    = ctrlBlock.getPerfEvents

  val perfBackend  = Seq()
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ ctrlBlockPerf ++ perfBackend


  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("backend perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  csrio.perf.perfEventsBackend := VecInit(perfEvents.map(_._2.asTypeOf(new PerfEvent)))

  val ctrlBlockError = ctrlBlock.getCriticalErrors
  val intExuBlockError = intRegion.getCriticalErrors
  val criticalErrors = ctrlBlockError ++ intExuBlockError

  for (((name, error), _) <- criticalErrors.zipWithIndex) {
    println(s"[Backend] critical error: $name \n")
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
  val wakeup = Vec(params.LdExuCnt, Flipped(Valid(new MemWakeUpBundle)))
  val storePcRead = Vec(params.StaCnt, Output(UInt(VAddrBits.W)))
  val hyuPcRead = Vec(params.HyuCnt, Output(UInt(VAddrBits.W)))
  // Input
  val writebackLda = Vec(params.LduCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackSta = Vec(params.StaCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackStd = Vec(params.StdCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackHyuLda = Vec(params.HyuCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackHyuSta = Vec(params.HyuCnt, Flipped(DecoupledIO(new MemExuOutput)))
  val writebackVldu = Vec(params.VlduCnt, Flipped(DecoupledIO(new MemExuOutput(true))))

  val stIn = Input(Vec(params.StaExuCnt, ValidIO(new StoreUnitToLFST)))

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
      issueLda ++
      issueHylda ++ issueHysta ++
      issueSta ++
      issueStd ++
      issueVldu
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
