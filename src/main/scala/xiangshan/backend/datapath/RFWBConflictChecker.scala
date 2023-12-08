package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.OptionWrapper
import utils.SeqUtils.MixedVec2
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig.{IntData, VecData}
import xiangshan.backend.datapath.WbConfig.{NoWB, PregWB}
import xiangshan.backend.regfile.PregParams

case class RFWBCollideCheckerParams (
  inWbCfgs: Seq[Seq[Set[PregWB]]],
  pregParams: PregParams,
) {
  def genInputBundle: MixedVec2[DecoupledIO[RFWBCollideCheckerBundle]] = {
    val pregWidth = pregParams.addrWidth
    utils.SeqUtils.mapToMixedVec2(this.filteredCfgs, (wb: PregWB) => DecoupledIO(new RFWBCollideCheckerBundle(wb, pregWidth)))
  }

  def filteredCfgs: Seq[Seq[PregWB]] = inWbCfgs.map(_.map(x =>
    if (x.map(_.dataCfg).contains(pregParams.dataCfg))
      x.find(x => x.dataCfg == pregParams.dataCfg).get
    else
      NoWB()
  ))

  def portMax = filteredCfgs.flatten.map(_.port).max
}

class RFWBCollideCheckerBundle(var wbCfg: Option[PregWB], pregWidth: Int) extends Bundle {

  def this(wbCfg_ : PregWB, pregWidth_ : Int) = this(Some(wbCfg_), pregWidth_)

  def this(pregWidth_ : Int) = this(None, pregWidth_)
}

class RFWBCollideCheckerIO(val params: RFWBCollideCheckerParams)(implicit p: Parameters) extends Bundle {
  private val pregWidth = params.pregParams.addrWidth
  val in: MixedVec2[DecoupledIO[RFWBCollideCheckerBundle]] = Flipped(params.genInputBundle)
  val out = Vec(params.portMax + 1, Valid(new RFWBCollideCheckerBundle(pregWidth)))
}

abstract class RFWBCollideCheckerBase(params: RFWBCollideCheckerParams)(implicit p: Parameters) extends Module {
  protected def portRange: Range

  val io = IO(new RFWBCollideCheckerIO(params))
  dontTouch(io)

  protected val pregParams = params.pregParams
  protected val pregWidth = pregParams.addrWidth

  protected val inGroup = io.in
    .flatten
    .groupBy(_.bits.wbCfg.get.port)
    .map(x => (x._1, x._2.sortBy(_.bits.wbCfg.get.priority)))

  private object ArbiterCtrl {
    def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
      case 0 => Seq()
      case 1 => Seq(true.B)
      case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
    }
  }

  // when io.in.valid is false.B, io.in.ready is true.B
  class WBArbiter[T <: Data](val gen: T, val n: Int) extends Module {
    val io = IO(new ArbiterIO(gen, n))

    io.chosen := (n - 1).asUInt
    io.out.bits := io.in(n - 1).bits
    for (i <- n - 2 to 0 by -1) {
      when(io.in(i).valid) {
        io.chosen := i.asUInt
        io.out.bits := io.in(i).bits
      }
    }

    val grant = ArbiterCtrl(io.in.map(_.valid))
    for ((in, g) <- io.in.zip(grant))
      in.ready := (g || !in.valid) && io.out.ready
    io.out.valid := !grant.last || io.in.last.valid
  }

  protected val arbiters: Seq[Option[WBArbiter[RFWBCollideCheckerBundle]]] = portRange.map { portIdx =>
    OptionWrapper(
      inGroup.isDefinedAt(portIdx),
      Module(new WBArbiter(
        new RFWBCollideCheckerBundle(pregWidth),
        inGroup(portIdx).size
      ))
    )
  }

  // connection of IntWB or VfWB
  arbiters.zipWithIndex.foreach { case (arb, portIdx) =>
    if (arb.nonEmpty) {
      arb.get.io.in.zip(inGroup(portIdx)).foreach { case (arbiterIn, ioIn) =>
        arbiterIn <> ioIn
      }
    }
  }

  // connection of NoWB
  io.in.map(_.map(x =>
    if (x.bits.wbCfg.get.isInstanceOf[NoWB]) {
      x.ready := true.B
    }
  ))

  io.out.zip(arbiters).foreach { case (out, arb) =>
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

class IntRFWBCollideChecker(
  backendParams: BackendParams
)(implicit
  p:Parameters
) extends RFWBCollideCheckerBase(RFWBCollideCheckerParams(backendParams.getAllWbCfgs, backendParams.intPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getWbPortIndices(IntData()).max
}

class VfRFWBCollideChecker(
  backendParams: BackendParams
)(implicit
  p:Parameters
) extends RFWBCollideCheckerBase(RFWBCollideCheckerParams(backendParams.getAllWbCfgs, backendParams.vfPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getWbPortIndices(VecData()).max
}
