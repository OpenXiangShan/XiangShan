package xiangshan.backend.vector

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.PerfCCT
import xiangshan.backend.datapath.DataConfig.{FpData, IntData, VecData}
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.Vxrm
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.vector.VecIssueQueue.RespBundle
import xiangshan.backend.vector.datapath.VecImmExtractor
import xiangshan.{HasXSParameter, Redirect, XSBundle}

class IssuePipe(
  override val wrapper: IssuePipe.LazyMod
)(implicit val param: ExuParam, p: Parameters) extends LazyModuleImp(wrapper) with HasXSParameter {
  override def desiredName: String = param.getDefinitionNameOfPipe

  val readPortCfgs: Seq[Set[RdConfig]] = param.readPortCfgs.map(_.toSet)
  val vlReadPortCfgs: VlRD = param.vlRD

  val in = IO(Input(new IssuePipe.In(param)))
  val out = IO(Output(new IssuePipe.Out(param)))

  dontTouch(in)
  dontTouch(out)

  val is0Next: ValidIO[VecIssueQueue.Deq] = in.is0Next
  val is0    : ValidIO[VecIssueQueue.Deq] = RegInit(chiselTypeOf(is0Next).Lit(_.valid -> false.B))
  val is1Next: ValidIO[VecIssueQueue.Deq] = Wire(chiselTypeOf(is0Next))
  val is1    : ValidIO[VecIssueQueue.Deq] = RegInit(chiselTypeOf(is0Next).Lit(_.valid -> false.B))
  val is2Next: ValidIO[Exu.InUop]         = Wire(ValidIO(new Exu.InUop(param)))
  val is2    : ValidIO[Exu.InUop]         = RegInit(ValidIO(new Exu.InUop(param)).Lit(_.valid -> false.B))
  val ex0Next: ValidIO[Exu.InUop]         = Wire(ValidIO(new Exu.InUop(param)))
  val ex0    : ValidIO[Exu.InUop]         = RegInit(ValidIO(new Exu.InUop(param)).Lit(_.valid -> false.B))

  dontTouch(is0Next)
  dontTouch(is0)
  dontTouch(is1Next)
  dontTouch(is1)
  dontTouch(is2Next)
  dontTouch(is2)
  dontTouch(ex0Next)
  dontTouch(ex0)

  val is0Resp = out.is0Resp
  val is1Resp = out.is1Resp

  is0.bits.debug.foreach(x => PerfCCT.updateInstPos(x.seqNum, PerfCCT.InstPos.AtIssueArb.id.U, is0.valid, clock, reset))
  is1.bits.debug.foreach(x => PerfCCT.updateInstPos(x.seqNum, PerfCCT.InstPos.AtIssueReadReg.id.U, is1.valid, clock, reset))
  is2.bits.debug.foreach(x => PerfCCT.updateInstPos(x.seqNum, PerfCCT.InstPos.AtIssueReadReg.id.U, is2.valid, clock, reset))

  /**
   * is0 stage
   */
  val is0Failed: Bool = in.is0RdFail && in.is0LdCancel
  is0Resp.fail := is0.valid && is0Failed
  is0Resp.success := false.B // Todo

  is0.valid := is0Next.valid
  when (is0Next.valid) {
    is0.bits := is0Next.bits
  }

  /**
   * is1 stage
   */
  val is1GpRdAddrReqSrcIdx: Seq[Int] = is1Next.bits.exuParam.readPortCfgs.zipWithIndex collect {
    case (readCfg, srcIdx) if readCfg.exists(_.getDataConfig == IntData()) => srcIdx
  }

  val is1FpRdAddrReqSrcIdx: Seq[Int] = is1Next.bits.exuParam.readPortCfgs.zipWithIndex collect {
    case (readCfg, srcIdx) if readCfg.exists(_.getDataConfig == FpData()) => srcIdx
  }

  val is1VpRdAddrReqSrcIdx: Seq[Int] = is1Next.bits.exuParam.readPortCfgs.zipWithIndex collect {
    case (readCfg, srcIdx) if readCfg.exists(_.getDataConfig == VecData()) => srcIdx
  }

  val is1FlushNext: Bool = is0.bits.robIdx.needFlush(in.flush)

  out.is1GpRdAddrNext.zip(is1GpRdAddrReqSrcIdx).foreach {
    case (readBundle, srcIdx) =>
      readBundle.ren := is0Next.valid && is0Next.bits.gpRen(srcIdx)
      readBundle.addr := is0Next.bits.psrc(srcIdx)
  }

  out.is1FpRdAddrNext.zip(is1FpRdAddrReqSrcIdx).foreach {
    case (readBundle, srcIdx) =>
      readBundle.ren := is1Next.valid && is1Next.bits.fpRen(srcIdx)
      readBundle.addr := is1Next.bits.psrc(srcIdx)
  }

  out.is1VpRdAddrNext.zip(is1VpRdAddrReqSrcIdx).foreach {
    case (readBundle, srcIdx) =>
      readBundle.ren := is1Next.valid && is1Next.bits.vpRen(srcIdx)
      readBundle.addr := is1Next.bits.psrc(srcIdx)
  }

  out.is1V0RdAddrNext.zip(is1Next.bits.psrcV0).foreach {
    case (readBundle, psrc) =>
      readBundle.ren := is1Next.valid && psrc.valid
      readBundle.addr := psrc.bits
  }

  out.is1VlRdAddrNext.zip(is1Next.bits.psrcVl).foreach {
    case (readBundle, psrc) =>
      readBundle.ren := is1Next.valid && psrc.valid
      readBundle.addr := psrc.bits
  }

  is1Next.valid := is0.valid && !is1FlushNext && !is0Failed
  is1Next.bits := is0.bits

  is1.valid := is1Next.valid
  when (is1Next.valid) {
    is1.bits := is1Next.bits
  }

  is1Resp.fail := false.B
  is1Resp.success := is2Next.valid

  /**
   * is2 stage
   */
  val is1Flush: Bool = is1.bits.robIdx.needFlush(in.flush)

  val is2GpRdFail: Vec[Bool] = VecInit(is1.bits.psrc.indices.map {
    srcIdx =>
      val idx = in.is2GpRdDataNext.map(_.srcIdx).indexWhere(_ == srcIdx)
      if (idx != -1)
        in.is2GpRdFailNext(idx)
      else
        false.B
  })
  val is2FpRdFail: Vec[Bool] = VecInit(is1.bits.psrc.indices.map {
    srcIdx =>
      val idx = in.is2FpRdDataNext.map(_.srcIdx).indexWhere(_ == srcIdx)
      if (idx != -1)
        in.is2FpRdFailNext(idx)
      else
        false.B
  })

  val is2ImmNext: Option[UInt] = Option.when(is1.bits.imm.nonEmpty)(VecImmExtractor(
    VLEN, param.immTypes
  )(
    is1.bits.imm.get,
    is1.bits.immType.get,
    is1.bits.vtype.get.vsew
  ))

  is2Next.valid := is1.valid && !is1Flush && !is2GpRdFail.asUInt.orR && !is2FpRdFail.asUInt.orR
  is2Next.bits.ctrl.fromIssueDeq(is1.bits)
  is2Next.bits.data.imm.foreach(_ := is1.bits.imm.get)
  is2Next.bits.data.pc.foreach(_ := ???)
  is2Next.bits.debug.foreach(_ := is1.bits.debug.get)

  is2Next.bits.data.src.zipWithIndex.foreach {
    case (src, srcIdx) =>
      val gpKV: Seq[(Bool, UInt)] = Option.when(readPortCfgs(srcIdx).exists(_.isInstanceOf[IntRD]))(
        is1.bits.gpRen(srcIdx) -> in.is2GpRdDataNext.find(_.srcIdx == srcIdx).get.data
      ).toSeq
      val fpKV: Seq[(Bool, UInt)] = Option.when(readPortCfgs(srcIdx).exists(_.isInstanceOf[FpRD]))(
        is1.bits.fpRen(srcIdx) -> in.is2FpRdDataNext.find(_.srcIdx == srcIdx).get.data
      ).toSeq
      val vpKV: Seq[(Bool, UInt)] = Option.when(readPortCfgs(srcIdx).exists(_.isInstanceOf[VfRD]))(
        is1.bits.vpRen(srcIdx) -> in.is2VpRdDataNext.find(_.srcIdx == srcIdx).get.data
      ).toSeq
      val immKV: Seq[(Bool, UInt)] = is2ImmNext.map { imm =>
        !Mux1H(Seq(
          is1.bits.gpRen(srcIdx) -> readPortCfgs(srcIdx).exists(_.isInstanceOf[IntRD]).B,
          is1.bits.fpRen(srcIdx) -> readPortCfgs(srcIdx).exists(_.isInstanceOf[FpRD]).B,
          is1.bits.vpRen(srcIdx) -> readPortCfgs(srcIdx).exists(_.isInstanceOf[VfRD]).B,
        )) -> imm
      }.toSeq

      src := Mux1H(Seq(
        gpKV,
        fpKV,
        vpKV,
        immKV,
      ).reduce(_ ++ _))
  }

  is2Next.bits.data.v0.foreach(_ := 0.U)
  is2Next.bits.data.vl.foreach(_ := in.is2VlRdDataNext.head.data)

  is2.valid := is2Next.valid
  when(is2Next.valid) {
    is2.bits := is2Next.bits
  }

  /**
   * ex0 stage
   */
  val is2Flush: Bool = is2.bits.ctrl.robIdx.needFlush(in.flush)

  ex0Next.valid := is2.valid && !is2Flush
  ex0Next.bits := is2.bits

  // Todo: ExuBypass
  ex0.valid := ex0Next.valid
  when (ex0Next.valid) {
    ex0.bits := ex0Next.bits
  }

  val exu: Option[Exu] = Option.when(!param.hasVStd)(Module(new Exu(param)))

  exu.foreach {
    exu =>
      exu.in.flush := in.flush
      exu.in.uop := ex0Next
      exu.in.frm.foreach(_ := in.frm.get)
      exu.in.vxrm.foreach(_ := in.vxrm.get)
  }

  out.gpWbNext.foreach(_ := exu.get.out.uop.bits.toGpRf.get)
  out.fpWbNext.foreach(_ := exu.get.out.uop.bits.toFpRf.get)
  out.vpWbNext.foreach(_ := exu.get.out.uop.bits.toVpRf.get)
  out.v0WbNext.foreach(_ := exu.get.out.uop.bits.toV0Rf.get)
  if (exu.nonEmpty) {
    out.robWbNext.valid := exu.get.out.uop.valid
    out.robWbNext.bits := exu.get.out.uop.bits.toRob
  } else {
    out.robWbNext.valid := ex0Next.valid
    out.robWbNext.bits :<#= ex0Next.bits
  }

  out.ex0 := ex0
}

object IssuePipe {
  class LazyMod(val param: ExuParam)(implicit p: Parameters) extends LazyModule with HasXSParameter {
    override def shouldBeInlined: Boolean = false

    lazy val module = new IssuePipe(this)(param, p)
  }

  class In(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val flush = ValidIO(new Redirect)
    val is0Next: ValidIO[VecIssueQueue.Deq] = ValidIO(new VecIssueQueue.Deq(param))
    val is0RdFail = Bool()
    val is0WtFail = Bool()
    val is0LdCancel = Bool()
    val is2VpRdDataNext: MixedVec[RfReadDataBundle] = param.genRfRdDataBundle(backendParams.vfPregParams)
    val is2VlRdDataNext: MixedVec[RfReadDataBundle] = param.genRfRdDataBundle(backendParams.vlPregParams)

    val is2GpRdDataNext: MixedVec[RfReadDataBundle] = param.genRfRdDataBundle(backendParams.intPregParams)
    val is2FpRdDataNext: MixedVec[RfReadDataBundle] = param.genRfRdDataBundle(backendParams.fpPregParams)

    val is2GpRdFailNext: Vec[Bool] = param.genRfRdFailBundle(backendParams.intPregParams)
    val is2FpRdFailNext: Vec[Bool] = param.genRfRdFailBundle(backendParams.fpPregParams)

    val frm = Option.when(param.readFrm)(Frm())
    val vxrm = Option.when(param.readVxrm)(Vxrm())
  }

  class Out(val param: ExuParam)(implicit p: Parameters) extends XSBundle {
    val is0Resp: RespBundle = new RespBundle()(p, param.getIssueParam())
    val is1Resp: RespBundle = new RespBundle()(p, param.getIssueParam())
    val is1GpRdAddrNext: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.gpPregParams)
    val is1FpRdAddrNext: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.fpPregParams)
    val is1VpRdAddrNext: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.vpPregParams)
    val is1V0RdAddrNext: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.v0PregParams)
    val is1VlRdAddrNext: MixedVec[RfReadAddrBundle] = param.genRfRdAddrBundle(backendParams.vlPregParams)

    val ex0 = param.genExuInputBundle(ValidIO(_))

    val gpWbNext: Option[Exu.ToRf] = param.writePortCfgs.find(_.writeInt).map(x => new Exu.ToRf(x, backendParams.intPregParams))
    val fpWbNext: Option[Exu.ToRf] = param.writePortCfgs.find(_.writeFp).map(x => new Exu.ToRf(x, backendParams.fpPregParams))
    val vpWbNext: Option[Exu.ToRf] = param.writePortCfgs.find(_.writeVec).map(x => new Exu.ToRf(x, backendParams.vfPregParams))
    val v0WbNext: Option[Exu.ToRf] = Option.when(param.v0WB != null)(new Exu.ToRf(param.v0WB, backendParams.v0PregParams))
    val vlWbNext: Option[Exu.ToRf] = Option.when(param.vlWB != null)(new Exu.ToRf(param.vlWB, backendParams.vlPregParams))
    val robWbNext: ValidIO[Exu.ToRob] = ValidIO(new Exu.ToRob(param))
  }

  class RfReadAddrBundle(
    val rdConfig  : RdConfig,
    val srcIdx    : Int,
    val pregParams: PregParams,
  ) extends Bundle {
    val ren = Bool()
    val addr = UInt(pregParams.addrWidth.W)
  }

  class RfReadDataBundle(
    val rdConfig  : RdConfig,
    val srcIdx    : Int,
    val pregParams: PregParams,
  ) extends Bundle {
    val data = UInt(pregParams.dataCfg.dataWidth.W)
  }
}
