package xiangshan.backend.float

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.datapath.RdConfig.{FpRD, IntRD}
import xiangshan.backend.vector.Exu.InBypassCtrl
import xiangshan.backend.vector.VecIssueQueue.BypassDelay
import xiangshan.backend.vector.ExuParam
import xiangshan.{HasXSParameter, XSBundle}
import yunsuan.vector.Common.SewOH

class FltBypassNetwork(implicit val param: ExuParam, val p: Parameters) extends Module with HasXSParameter {
  val in = IO(Input(new FltBypassNetwork.In))
  val out = IO(Output(new FltBypassNetwork.Out))

  private val fpRen = in.bypassCtrl.fpRen
  private val source = in.bypassCtrl.bypassSource
  private val delay = in.bypassCtrl.bypassDelay

  for (i <- out.src.indices) {
    val fpSeq: Seq[(Bool, UInt)] = Option.when(param.readPortCfgs(i).exists(_.isInstanceOf[FpRD])){
      Seq(
        (fpRen(i) && delay(i) === BypassDelay.delay0) -> in.fpWb0Next(source(i).idx),
        (fpRen(i) && delay(i) === BypassDelay.delay1) -> in.fpWb0(source(i).idx),
        (fpRen(i) && delay(i) >=  BypassDelay.delay2) -> in.fpRdData(i),
      )
    }.getOrElse(Seq())
    val isRegSrc = fpRen(i)
    out.src(i) := Mux1H((!isRegSrc -> in.fpRdData(i)) +: fpSeq)
  }
}

object FltBypassNetwork {
  class In(implicit val param: ExuParam, p: Parameters) extends XSBundle {
    val sewOH = SewOH()
    val bypassCtrl = new InBypassCtrl(param)
    val fpRdData = Vec(param.numRegSrc, UInt(XLEN.W))
    val fpWb0Next = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb0 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    val fpWb1 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
  }

  class Out(implicit val param: ExuParam, p: Parameters) extends XSBundle {
    val src = Vec(param.numRegSrc, UInt(param.srcDataBitsMax.W))
  }
}
