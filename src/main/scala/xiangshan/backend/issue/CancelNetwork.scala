package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuOH, IssueQueueIssueBundle}

class CancelNetworkIO(backendParams: BackendParams)(implicit p: Parameters) extends Bundle {
  private val numExu = backendParams.numExu

  val in = new Bundle {
    val int = Flipped(MixedVec(backendParams.intSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val vf  = Flipped(MixedVec(backendParams.vfSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val mem = Flipped(MixedVec(backendParams.memSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val og0CancelOH = Input(ExuOH(numExu))
    // Todo: remove this when no uop would be canceled at og1
    val og1CancelOH = Input(ExuOH(numExu))

    def allIssue: Seq[DecoupledIO[IssueQueueIssueBundle]] = (Seq() :+ int :+ vf :+ mem).flatten.flatten
  }
  val out = new Bundle {
    val int = MixedVec(backendParams.intSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val vf  = MixedVec(backendParams.vfSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val mem = MixedVec(backendParams.memSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val og0CancelOH = Output(ExuOH(numExu))
    def allIssue: Seq[DecoupledIO[IssueQueueIssueBundle]] = (Seq() :+ int :+ vf :+ mem).flatten.flatten
  }
}

class CancelNetwork(backendParams: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new CancelNetworkImp(backendParams, this)
}

class CancelNetworkImp(backendParams: BackendParams, override val wrapper: LazyModule)(implicit p: Parameters) extends LazyModuleImp(wrapper) {
  private val numExu = backendParams.numExu
  private val allExuParams = backendParams.allExuParams

  val io = IO(new CancelNetworkIO(backendParams))

  private val og0CancelOH = Wire(ExuOH(numExu))
  private val og1CancelOH = WireInit(io.in.og1CancelOH)
  private val transferredCancelOH = RegInit(0.U(numExu.W))

  private val isInferWakeUpVec = WireInit(VecInit(allExuParams.map(_.isIQWakeUpSink.B)))
  dontTouch(isInferWakeUpVec)

  og0CancelOH := io.in.og0CancelOH | transferredCancelOH

  transferredCancelOH := VecInit(io.in.allIssue.zip(io.out.allIssue).map(x => x._1.fire && !x._2.fire)).asUInt

  io.out.allIssue.zip(io.in.allIssue).zipWithIndex.foreach { case ((out, in), i) =>
    out.valid := in.valid && !in.bits.common.needCancel(og0CancelOH, og1CancelOH)
    out.bits := in.bits
    in.ready := out.ready
  }

  io.out.og0CancelOH := transferredCancelOH
}
