package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{Arbiter, DecoupledIO, RRArbiter, Valid}
import utils.SeqUtils.{MixedVec3, Seq3}
import utils.{OptionWrapper, SeqUtils}
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig.{IntData, VecData}
import xiangshan.backend.datapath.RdConfig.{IntRD, NoRD, RdConfig, VfRD}
import xiangshan.backend.regfile.PregParams

case class RFRdArbParams(
  inRdCfgs: Seq3[RdConfig],
  pregParams: PregParams,
) {
  require(inRdCfgs.nonEmpty)

  def genInputBundle: MixedVec3[DecoupledIO[RFArbiterBundle]] = {
    val pregWidth = pregParams.addrWidth
    SeqUtils.mapToMixedVec3(this.inRdCfgs, (rd: RdConfig) => DecoupledIO(new RFArbiterBundle(rd, pregWidth)))
  }

  def portMax: Int = inRdCfgs.flatten.flatten.map(_.port).max
}

class RFArbiterBundle(var rdCfg: Option[RdConfig], pregWidth: Int) extends Bundle {
  val addr = UInt(pregWidth.W)

  def this(rdCfg_ : RdConfig, pregWidth_ : Int) = this(Some(rdCfg_), pregWidth_)

  def this(pregWidth_ :Int) = this(None, pregWidth_)
}

class RFReadArbiterIO(params: RFRdArbParams)(implicit p: Parameters) extends Bundle {
  private val pregWidth = params.pregParams.addrWidth
  val in = Flipped(params.genInputBundle)
  val out = Vec(params.portMax + 1, Valid(new RFArbiterBundle(pregWidth)))
}

abstract class RFReadArbiterBase(val params: RFRdArbParams)(implicit p: Parameters) extends Module {
  protected def portRange: Range

  val io = IO(new RFReadArbiterIO(params))
  dontTouch(io)

  protected val pregParams = params.pregParams
  protected val pregWidth = pregParams.addrWidth

  protected val inGroup: Map[Int, Seq[DecoupledIO[RFArbiterBundle]]] = io.in
    .flatten.flatten
    .groupBy(_.bits.rdCfg.get.port)
    .map(x => (x._1, x._2.sortBy(_.bits.rdCfg.get.priority).toSeq))

  protected val arbiters: Seq[Option[RRArbiter[RFArbiterBundle]]] = portRange.map { portIdx =>
    OptionWrapper(
      inGroup.isDefinedAt(portIdx),
      Module(new RRArbiter(
        new RFArbiterBundle(pregWidth),
        inGroup(portIdx).size
      ))
    )
  }

  arbiters.zipWithIndex.foreach { case (arbiter, portIdx) =>
    if (arbiter.nonEmpty) {
      arbiter.get.io.in.zip(inGroup(portIdx)).foreach { case (arbiterIn, ioIn) =>
        arbiterIn <> ioIn
      }
    }
  }

  // connection of NoRD
  io.in.map(_.map(_.map(x =>
    if (x.bits.rdCfg.get.isInstanceOf[NoRD]) {
      x.ready := true.B
    }
  )))

  for (portIdx <- io.out.indices) {
    val arb = arbiters(portIdx)
    val out = io.out(portIdx)
    if (arb.nonEmpty) {
      val arbOut = arb.get.io.out
      arbOut.ready := true.B
      out.valid := arbOut.valid
      out.bits := arbOut.bits
    } else {
      out := 0.U.asTypeOf(out)
    }
  }
}

class IntRFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getRdCfgs[IntRD], backendParams.intPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(IntData()).max
}

class VfRFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getRdCfgs[VfRD], backendParams.vfPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(VecData()).max
}

