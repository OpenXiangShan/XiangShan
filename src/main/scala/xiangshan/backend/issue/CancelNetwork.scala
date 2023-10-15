package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuVec, IssueQueueIssueBundle}

class CancelNetworkIO(backendParams: BackendParams)(implicit p: Parameters) extends Bundle {
  private val numExu = backendParams.numExu

  val in = new Bundle {
    val int = Flipped(MixedVec(backendParams.intSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val vf  = Flipped(MixedVec(backendParams.vfSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val mem = Flipped(MixedVec(backendParams.memSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle)))
    val og0CancelVec = Input(ExuVec(numExu))
    // Todo: remove this when no uop would be canceled at og1
    val og1CancelVec = Input(ExuVec(numExu))

    def allIssue: Seq[DecoupledIO[IssueQueueIssueBundle]] = (Seq() :+ int :+ vf :+ mem).flatten.flatten
  }
  val out = new Bundle {
    val int = MixedVec(backendParams.intSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val vf  = MixedVec(backendParams.vfSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val mem = MixedVec(backendParams.memSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))
    val og0CancelVec = Output(ExuVec(numExu))
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

  private val og0CancelVec = Wire(ExuVec(numExu))
  private val og1CancelVec = WireInit(io.in.og1CancelVec)
  private val transferredCancelVec = RegInit(VecInit(Seq.fill(numExu)(false.B)))

  private val isInferWakeUpVec = WireInit(VecInit(allExuParams.map(_.isIQWakeUpSink.B)))
  dontTouch(isInferWakeUpVec)

  og0CancelVec.zipWithIndex.foreach { case (og0Cancel, i) =>
    og0Cancel := io.in.og0CancelVec(i) || transferredCancelVec(i)
  }

  transferredCancelVec.zipWithIndex.foreach { case (transferredCancel, i) =>
    transferredCancel := io.in.allIssue(i).fire && !io.out.allIssue(i).fire
  }

  io.out.allIssue.zip(io.in.allIssue).zipWithIndex.foreach { case ((out, in), i) =>
    out.valid := in.valid && !in.bits.common.needCancel(og0CancelVec, og1CancelVec)
    out.bits := in.bits
    in.ready := out.ready
  }

  io.out.og0CancelVec := transferredCancelVec
}
