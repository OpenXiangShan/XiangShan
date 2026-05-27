package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.datapath.RdConfig.{FpRD, IntRD}
import xiangshan.{HasXSParameter, XSBundle}
import xiangshan.backend.vector.Exu.InBypassCtrl
import xiangshan.backend.vector.VecIssueQueue.BypassDelay
import yunsuan.vector.Common.SewOH
import yunsuan.vector.Common._

class BypassNetwork(implicit val param: ExuParam, val p: Parameters) extends Module with HasXSParameter {
  val in = IO(Input(new BypassNetwork.In))
  val out = IO(Output(new BypassNetwork.Out))

  private val gpRen = in.bypassCtrl.gpRen
  private val fpRen = in.bypassCtrl.fpRen
  private val vpRen = in.bypassCtrl.vpRen
  private val source = in.bypassCtrl.bypassSource
  private val delay = in.bypassCtrl.bypassDelay

  private def scalarToVector(data: UInt): UInt = Mux1H(Seq(
    in.sewOH.is8  -> Fill(VLEN /  8, data.take( 8)),
    in.sewOH.is16 -> Fill(VLEN / 16, data.take(16)),
    in.sewOH.is32 -> Fill(VLEN / 32, data.take(32)),
    in.sewOH.is64 -> Fill(VLEN / 64, data.take(64)),
  ))

  val gpDataOption: Seq[Option[UInt]] = param.readPortCfgs.zipWithIndex.map {
    case (cfg, i) =>
      Option.when(cfg.exists(_.isInstanceOf[IntRD]))(in.gpWb1(source(i).idx))
  }
  val fpDataOption: Seq[Option[UInt]] = param.readPortCfgs.zipWithIndex.map {
    case (cfg, i) =>
      Option.when(cfg.exists(_.isInstanceOf[FpRD]))(in.fpWb1(source(i).idx))
  }

  for (i <- out.src.indices) {
    val gpSeq: Seq[(Bool, UInt)] = Option.when(param.readPortCfgs(i).exists(_.isInstanceOf[IntRD])){
      val gpData = gpDataOption(i).get
      Seq(
        (gpRen(i) && delay(i) === BypassDelay.delay1) -> scalarToVector(gpData),
        (gpRen(i) && delay(i) >= BypassDelay.delay2) -> scalarToVector(in.gpRdData(i)),
      )
    }.getOrElse(Seq())

    val fpSeq: Seq[(Bool, UInt)] = Option.when(param.readPortCfgs(i).exists(_.isInstanceOf[FpRD])){
      val fpData = fpDataOption(i).get
      Seq(
        (fpRen(i) && delay(i) === BypassDelay.delay1) -> scalarToVector(fpData),
        (fpRen(i) && delay(i) >= BypassDelay.delay2) -> scalarToVector(in.fpRdData(i)),
      )
    }.getOrElse(Seq())

    val vpSeq: Seq[(Bool, UInt)] = Seq(
      (vpRen(i) && delay(i) === BypassDelay.delay0) -> in.vpWb0(source(i).idx),
      (vpRen(i) && delay(i) === BypassDelay.delay1) -> in.vpWb1(source(i).idx),
      (vpRen(i) && delay(i) === BypassDelay.delay2) -> in.vpWb2(source(i).idx),
      (vpRen(i) && delay(i) === BypassDelay.delay3) -> in.vpRdData(i),
    )

    val isRegSrc = gpRen(i) || fpRen(i) || vpRen(i)
    out.src(i) := Mux1H((!isRegSrc -> in.vpRdData(i)) +: (gpSeq ++ fpSeq ++ vpSeq))
  }
}

object BypassNetwork {
  class In(implicit val param: ExuParam, p: Parameters) extends XSBundle {
    val sewOH = SewOH()
    val bypassCtrl = new InBypassCtrl(param)
    val gpRdData = Vec(param.numRegSrc, UInt(XLEN.W))
    val fpRdData = Vec(param.numRegSrc, UInt(XLEN.W))
    val vpRdData = Vec(param.numRegSrc, UInt(VLEN.W))
    val gpWb1 = Vec(backendParams.getIntRfWriteSize, UInt(XLEN.W))
    val fpWb1 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val vpWb0 = Vec(backendParams.getWbPortIndices(backendParams.vpPregParams.dataCfg).size, UInt(VLEN.W))
    val vpWb1, vpWb2 = Vec(backendParams.getVfRfWriteSize, UInt(VLEN.W))
  }

  class Out(implicit val param: ExuParam, p: Parameters) extends XSBundle {
    val src = Vec(param.numRegSrc, UInt(param.srcDataBitsMax.W))
  }
}
