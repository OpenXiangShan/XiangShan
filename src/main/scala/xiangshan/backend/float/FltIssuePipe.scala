package xiangshan.backend.float

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.PerfCCT
import xiangshan.backend.datapath.DataConfig.{FpData, IntData, VecData}
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.Vxrm
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.VecIssueQueue
import xiangshan.backend.vector.datapath.VecImmExtractor
import xiangshan.backend.vector.ExuParam
import xiangshan.backend.vector.fu.VecFuConfig
import xiangshan.backend.float.FltIssueQueue.FltWakeUpBundle
import xiangshan.backend.float.FltIssueQueue.FltRespBundle
import xiangshan.backend.vector.Exu
import xiangshan.backend.vector.IssuePipe.{RfReadAddrBundle, RfReadDataBundle}
import xiangshan.backend.vector.VecIssueQueue.{BypassDelay, RespBundle, WakeUpBundle}
import xiangshan.mem.StoreQueueDataWrite
import xiangshan.{HasXSParameter, LoadCancelIO, Redirect, XSBundle}
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{Frm => VecFrm}

class FltIssuePipe(
  override val wrapper: FltIssuePipe.LazyMod
)(implicit val param: ExuParam, p: Parameters) extends LazyModuleImp(wrapper) with HasXSParameter {
  override def desiredName: String = param.getDefinitionNameOfPipe

  val readPortCfgs: Seq[Set[RdConfig]] = param.readPortCfgs.map(_.toSet)
  val vlReadPortCfgs: VlRD = param.vlRD

  val in = IO(Input(new FltIssuePipe.In(param)))
  val out = IO(Output(new FltIssuePipe.Out(param)))

  dontTouch(in)
  dontTouch(out)

  val is0Next: ValidIO[VecIssueQueue.Deq] = in.is0Next
  val is0    : ValidIO[VecIssueQueue.Deq] = RegInit(chiselTypeOf(is0Next).Lit(_.valid -> false.B))
  val is1Next: ValidIO[VecIssueQueue.Deq] = Wire(chiselTypeOf(is0Next))
  val is1    : ValidIO[VecIssueQueue.Deq] = RegInit(chiselTypeOf(is0Next).Lit(_.valid -> false.B))
  val ex0Next: ValidIO[FltExu.InUop]         = Wire(ValidIO(new FltExu.InUop(param)))
  val ex0    : ValidIO[FltExu.InUop]         = RegInit(ValidIO(new FltExu.InUop(param)).Lit(_.valid -> false.B))

  dontTouch(is0Next)
  dontTouch(is0)
  dontTouch(is1Next)
  dontTouch(is1)
  dontTouch(ex0Next)
  dontTouch(ex0)

  val is0Resp = out.is0Resp
  val is1Resp = out.is1Resp
  val ex0Resp = out.ex0Resp

  is0.bits.debug.foreach(x => PerfCCT.updateInstPos(x.seqNum, PerfCCT.InstPos.AtIssueArb.id.U, is0.valid, clock, reset))
  is1.bits.debug.foreach(x => PerfCCT.updateInstPos(x.seqNum, PerfCCT.InstPos.AtIssueReadReg.id.U, is1.valid, clock, reset))

   private def hasScalarRfRead(gpRen: Vec[Bool], fpRen: Vec[Bool], bypassDelay: Vec[UInt]): Bool = {
    val gpRfRead = gpRen.zip(bypassDelay).map {
      case (ren, delay) => ren && delay >= BypassDelay.delay2
    }
    val fpRfRead = fpRen.zip(bypassDelay).map {
      case (ren, delay) => ren && delay >= BypassDelay.delay2
    }
    (gpRfRead ++ fpRfRead).foldLeft(false.B)(_ || _)
  }

  /**
   * is0 stage
   */
  val is0FlushNext: Bool = is0Next.bits.robIdx.needFlush(in.flush)
  val i2fWbArbiterFail = in.busyTableI2F.getOrElse(0.U(3.W))(2) && is0.valid && is0.bits.fpWen && is0.bits.latency === 3.U
  val is0Failed: Bool = in.is0RdFail || is0Resp.srcCancel.asUInt.orR || i2fWbArbiterFail
  is0Resp.fail := is0.valid && is0Failed
  is0Resp.success := false.B // Todo
  val is1RespSrcCancel = RegInit(0.U.asTypeOf(Vec(param.numRegSrc, Bool())))
  val is1RespSrcCancelNext = Wire(Vec(param.numRegSrc, Bool()))
  val is1RespFail = RegInit(false.B)
  val is1RespSuccess = RegInit(false.B)
  val is1Flush: Bool = is1.bits.robIdx.needFlush(in.flush)
  is1RespSrcCancel := is1RespSrcCancelNext
  is1RespFail := is0.valid && is1RespSrcCancelNext.reduce(_ || _)
  is1RespSuccess := false.B // TODO
  is1Resp.fail := is1RespFail
  is1Resp.success := is1RespSuccess
  is1Resp.srcCancel := is1RespSrcCancel
  val ex0RespSrcCancel = RegInit(0.U.asTypeOf(Vec(param.numRegSrc, Bool())))
  val ex0RespSrcCancelNext = Wire(Vec(param.numRegSrc, Bool()))
  val ex0RespFail = RegInit(false.B)
  val ex0RespSuccess = RegInit(false.B)
  ex0RespSrcCancel := ex0RespSrcCancelNext
  ex0RespFail := is1.valid && ex0RespSrcCancelNext.reduce(_ || _)
  out.ex0RespFailLat1Next := is1.valid && is1.bits.latency === 1.U && ex0RespSrcCancelNext.reduce(_ || _)
  ex0RespSuccess := ex0Next.valid && !ex0RespSrcCancelNext.reduce(_ || _)
  val exu: FltExu = Module(new FltExu(param))
  val fdivEx0Fail = if (exu.out.outFuBusy.nonEmpty) ex0.valid && FuType.isFdiv(ex0.bits.ctrl.fuType) && exu.out.outFuBusy.get.head else false.B
  ex0Resp.fail := ex0RespFail || fdivEx0Fail
  ex0Resp.success := ex0RespSuccess && !fdivEx0Fail
  ex0Resp.srcCancel := ex0RespSrcCancel
  val ldCancelVec = VecInit(in.ldCancel.map(_.ld2Cancel))
  val ldCancelVecRegNext = RegNext(ldCancelVec) // for issue and ldCancel at same cycle in IQ
  val ldWBPort = param.backendParams.getIntRegionParam.issueParams.filter(_.hasLdu).map(_.fpWbPortIds.head)
  println(s"ldWBPort = $ldWBPort")
  val fltExuWBPort = param.backendParams.getFltRegionParam.issueParams.map(_.fpWbPortIds.head)
  println(s"fltExuWBPort = $fltExuWBPort")
  is0Resp.srcCancel.zipWithIndex.map { case (srcCancel, srcIdx) =>
    srcCancel := fltExuWBPort.zip(in.ex0RespFailLat1).map { case (wbIdx, ex0RespFail) =>
      is0.bits.bypassSource(srcIdx).idx === wbIdx.U && is0.bits.bypassDelay(srcIdx) === BypassDelay.delay0 && ex0RespFail
    }.reduce(_ || _) ||
    is0.valid && ldWBPort.zip(ldCancelVecRegNext).map { case (wbIdx, ldCancel) =>
      is0.bits.bypassSource(srcIdx).idx === wbIdx.U && is0.bits.bypassDelay(srcIdx) === BypassDelay.delay2 && ldCancel
    }.reduce(_ || _)
  }
  is1RespSrcCancelNext.zipWithIndex.map { case (srcCancel, srcIdx) =>
    srcCancel := is0.valid && ldWBPort.zip(ldCancelVec).map { case (wbIdx, ldCancel) =>
      is0.bits.bypassSource(srcIdx).idx === wbIdx.U && is0.bits.bypassDelay(srcIdx) === BypassDelay.delay1 && ldCancel
    }.reduce(_ || _)
  }
  ex0RespSrcCancelNext.zipWithIndex.map { case (srcCancel, srcIdx) =>
    srcCancel := is1.valid && ldWBPort.zip(ldCancelVec).map { case (wbIdx, ldCancel) =>
      is1.bits.bypassSource(srcIdx).idx === wbIdx.U && is1.bits.bypassDelay(srcIdx) === BypassDelay.delay0 && ldCancel
    }.reduce(_ || _)
  }

  is0.valid := is0Next.valid && !is0FlushNext
  when (is0Next.valid) {
    is0.bits := is0Next.bits
  }

  /**
   * is1 stage
   */
  val is0FpRdAddrReqSrcIdx: Seq[Int] = is0.bits.exuParam.readPortCfgs.zipWithIndex collect {
    case (readCfg, srcIdx) if readCfg.exists(_.getDataConfig == FpData()) => srcIdx
  }

  val is0Flush: Bool = is0.bits.robIdx.needFlush(in.flush)

  out.is0FpRdAddr.zip(is0FpRdAddrReqSrcIdx).foreach {
    case (readBundle, srcIdx) =>
      val readRf = is1Next.bits.bypassDelay(srcIdx) >= BypassDelay.delay2
      readBundle.ren := is1Next.valid && is1Next.bits.fpRen(srcIdx) && readRf
      readBundle.addr := is1Next.bits.psrc(srcIdx)
      readBundle.robIdx := is1Next.bits.robIdx
  }

  is1Next.valid := is0.valid && !is0Flush && !is0Failed && !is1RespSrcCancelNext.reduce(_ || _)
  is1Next.bits := is0.bits
  is1Next.bits.loadDependency.get.zip(is0.bits.loadDependency.get).foreach( x => x._1.zip(x._2).map(xx => xx._1 := (xx._2 << 1)))

  is1.valid := is1Next.valid
  when (is1Next.valid) {
    is1.bits := is1Next.bits
  }


  val is2ImmNext: Option[UInt] = Option.when(is1.bits.imm.nonEmpty)(VecImmExtractor(
    VLEN, param.immTypes
  )(
    is1.bits.imm.get,
    is1.bits.immType.get,
    is1.bits.vtype.get.vsew
  ))

  ex0Next.valid := is1.valid && !is1Flush && !ex0RespSrcCancelNext.reduce(_ || _)
  ex0Next.bits.ctrl.fromIssueDeq(is1.bits)
  ex0Next.bits.ctrl.frm.foreach(_ := Mux(is1.bits.frm.get === VecFrm.DYN, in.frm.get, is1.bits.frm.get))
  ex0Next.bits.data.imm.foreach(_ := is1.bits.imm.get)
  ex0Next.bits.data.pc.foreach(_ := ???)
  ex0Next.bits.bypassCtrl.fromIssueDeq(is1.bits)
  ex0Next.bits.debug.foreach(_ := is1.bits.debug.get)

  ex0Next.bits.data.src.zipWithIndex.foreach {
    case (src, srcIdx) =>
      val fpKV: Seq[(Bool, UInt)] = Option.when(readPortCfgs(srcIdx).exists(_.isInstanceOf[FpRD]))(
        is1.bits.fpRen(srcIdx) -> in.is1FpRdDataNext.find(_.srcIdx == srcIdx).get.data
      ).toSeq
      src := Mux1H(fpKV)
  }

  // Todo: ExuBypass
  ex0.valid := ex0Next.valid
  when (ex0Next.valid) {
    ex0.bits := ex0Next.bits
  }

  exu.in.flush := in.flush
  exu.in.uop := ex0Next
  exu.in.busyTableEmpty.foreach(_ := in.busyTableEmpty.get)
  exu.in.frm.zip(in.frm).foreach { case (sink, source) => sink := source }
  for ((rdCfgs, srcIdx) <- param.readPortCfgs.zipWithIndex) {
    exu.in.fpRdData(srcIdx) := Mux(
      ex0Next.bits.bypassCtrl.fpRen(srcIdx),
      in.is1FpRdDataNext.find(_.srcIdx == srcIdx).get.data,
      0.U,
    )
  }
  exu.in.fpWb0Next := in.fpWb0Next
  exu.in.fpWb0 := in.fpWb0
  exu.in.fpWb1 := in.fpWb1

  out.gpWbNext.foreach(_ := exu.out.uop.bits.toGpRf.get)
  out.fpWbNext.foreach(_ := exu.out.uop.bits.toFpRf.get)
  out.robWbNext.valid := exu.out.uop.valid
  out.robWbNext.bits := exu.out.uop.bits.toRob

  out.ex0 := ex0

  private val is0FixedLatFpWen = is0.bits.fpWen && !FuType.FuTypeOrR(is0.bits.fuType, FuType.fDivSqrt)
  private val is1FixedLatFpWen = is1.bits.fpWen && !FuType.FuTypeOrR(is1.bits.fuType, FuType.fDivSqrt)
  private val ex0FixedLatFpWen =
    ex0.bits.ctrl.fpWen.getOrElse(false.B) && !FuType.FuTypeOrR(ex0.bits.ctrl.fuType, FuType.fDivSqrt)
  private val ex0FixedLatGpWen = ex0.bits.ctrl.gpWen.getOrElse(false.B)

  private val ex0Flush: Bool = ex0.bits.ctrl.robIdx.needFlush(in.flush)
  private val is0WakeupValid: Bool =
    is1Next.valid && is0FixedLatFpWen && is0.bits.latency === 1.U
  private val is1WakeupValid: Bool =
    ex0Next.valid && is1FixedLatFpWen && is1.bits.latency === 2.U
  private val ex0WakeupValid: Bool =
    ex0.valid && !ex0Flush && ex0FixedLatFpWen && ex0.bits.ctrl.latency === 3.U
  private val ex0F2IWakeupValid: Bool =
    ex0.valid && !ex0Flush && ex0FixedLatGpWen && ex0.bits.ctrl.latency === 3.U

  private val nonFixedLatWakeUp = Wire(new FltWakeUpBundle(backendParams.fpPregParams))
  if (exu.out.outFuWakeUp.isEmpty) {
    nonFixedLatWakeUp := 0.U.asTypeOf(nonFixedLatWakeUp)
  }
  else {
    exu.out.outFuWakeUp.foreach { wakeups =>
      nonFixedLatWakeUp.wen := wakeups.map(_.wen).reduce(_ || _) && in.busyTableEmpty.get
      nonFixedLatWakeUp.pdest := Mux1H(wakeups.map(wakeup => wakeup.wen -> wakeup.pdest))
      nonFixedLatWakeUp.loadDependency := 0.U.asTypeOf(nonFixedLatWakeUp.loadDependency)
    }
  }
  private val fixedLatWakeupValid = Seq(
    is0WakeupValid,
    is1WakeupValid,
    ex0WakeupValid,
  ).reduce(_ || _)

  val fpWbM2Wakeup = Reg(new FltWakeUpBundle(backendParams.fpPregParams))
  val fpWbM2WakeupIs1Lat = Reg(Bool())
  out.fpWbM2Wakeup := fpWbM2Wakeup
  out.fpWbM2WakeupIs1Lat := fpWbM2WakeupIs1Lat
  out.wakeupF2I.foreach{ x =>
    x.wen := ex0F2IWakeupValid
    x.pdest := ex0.bits.ctrl.pdest
    x.loadDependency := 0.U.asTypeOf(x.loadDependency)
  }
  out.busyTableF2I.foreach{ x =>
    val busyTableF2I = RegInit(0.U(3.W))
    busyTableF2I := Cat(
      in.is0Next.valid && in.is0Next.bits.gpWen,
      is0.valid && is0.bits.gpWen,
      is1.valid && is1.bits.gpWen,
    )
    x := busyTableF2I
  }
  fpWbM2Wakeup.wen := fixedLatWakeupValid || nonFixedLatWakeUp.wen

  private val fixedLatWakeupPdest = Mux1H(Seq(
    is0WakeupValid -> is0.bits.pdest,
    is1WakeupValid -> is1.bits.pdest,
    ex0WakeupValid -> ex0.bits.ctrl.pdest,
  ))
  fpWbM2Wakeup.pdest := Mux(nonFixedLatWakeUp.wen, nonFixedLatWakeUp.pdest, fixedLatWakeupPdest)
  fpWbM2Wakeup.loadDependency := 0.U.asTypeOf(fpWbM2Wakeup.loadDependency)
  fpWbM2WakeupIs1Lat := is0WakeupValid

  out.outFuLat.zip(exu.out.outFuLat).foreach {
    case (sink, source) => sink <> source
  }
}

object FltIssuePipe {
  class LazyMod(val param: ExuParam)(implicit p: Parameters) extends LazyModule with HasXSParameter {
    override def shouldBeInlined: Boolean = false

    lazy val module = new FltIssuePipe(this)(param, p)
  }

  class In(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val is0Next: ValidIO[VecIssueQueue.Deq] = ValidIO(new VecIssueQueue.Deq(param))
    val is0RdFail = Bool()
    val is0WtFail = Bool()
    val ldCancel = Vec(backendParams.LdExuCnt, Input(new LoadCancelIO))
    val ex0RespFailLat1 = Vec(backendParams.getFltRegionParam.getFpWriteSize, Bool())
    val is1FpRdDataNext: MixedVec[RfReadDataBundle] = param.genRfRdDataBundle(backendParams.fpPregParams)
    val fpWb0Next = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb0 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb1 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val frm = Option.when(param.readFrm)(Frm())
    val busyTableEmpty = Option.when(param.fuConfigs.find(_.isFdiv).nonEmpty)(Bool())
    val busyTableI2F = Option.when(param.fuConfigs.contains(VecFuConfig.FcvtCfg))(UInt(3.W)) // i2f latency = 3
  }

  class Out(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val is0Resp = new FltRespBundle()(p, param.getIssueParam())
    val is1Resp = new FltRespBundle()(p, param.getIssueParam())
    val ex0Resp = new FltRespBundle()(p, param.getIssueParam())
    val ex0RespFailLat1Next = Bool()
    val is0FpRdAddr: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.fpPregParams)
    val fpWbM2Wakeup = new FltWakeUpBundle(backendParams.fpPregParams)
    val fpWbM2WakeupIs1Lat = Bool()
    val wakeupF2I = Option.when(param.needGpWen)(new FltWakeUpBundle(backendParams.gpPregParams))
    val busyTableF2I = Option.when(param.needGpWen)(UInt(3.W)) // f2i latency = 3
    val ex0 = param.genExuInputBundle(ValidIO(_))
    val outFuLat = Option.when(param.hasNonFixedLatFu)(Vec(param.numNonFixedLatFu, Valid(UInt(WbFuBusyTable.NonFixedLatencyWidth.W))))
    val fpWbNext: Option[Exu.ToRf] = param.writePortCfgs.find(_.writeFp).map(x => new Exu.ToRf(x, backendParams.fpPregParams))
    val gpWbNext: Option[Exu.ToRf] = param.writePortCfgs.find(_.writeInt).map(x => new Exu.ToRf(x, backendParams.intPregParams))
    val robWbNext: ValidIO[Exu.ToRob] = ValidIO(new Exu.ToRob(param))
  }

}
