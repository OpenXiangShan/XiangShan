package xiangshan.backend.datapath

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.SeqUtils.{MixedVec3, Seq3}
import utils.{OptionWrapper, SeqUtils}
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.regfile.PregParams
import utility._
import xiangshan.backend.rob.RobPtr
import yunsuan.vector.LZD

case class RFRdArbParams(
  inRdCfgs: Seq3[RdConfig],
  pregParams: PregParams,
)(implicit p: Parameters) {
  require(inRdCfgs.nonEmpty)

  def genInputBundle: MixedVec3[DecoupledIO[RFArbiterBundle]] = {
    val pregWidth = pregParams.addrWidth
    SeqUtils.mapToMixedVec3(this.inRdCfgs, (rd: RdConfig) => DecoupledIO(new RFArbiterBundle(rd, pregWidth)))
  }

  def portMax: Int = inRdCfgs.flatten.flatten.map(_.port).max
}

class RFArbiterBundle(var rdCfg: Option[RdConfig], pregWidth: Int)(implicit p: Parameters) extends Bundle {
  val addr       = UInt(pregWidth.W)
  val robIdx     = new RobPtr
  val issueValid = Bool()

  def this(rdCfg_ : RdConfig, pregWidth_ : Int)(implicit p: Parameters) = this(Some(rdCfg_), pregWidth_)

  def this(pregWidth_ :Int)(implicit p: Parameters) = this(None, pregWidth_)
}

class OldestArbiterIO(pregWidth: Int, n: Int)(implicit p: Parameters) extends Bundle {
  val in  = Flipped(Vec(n, Decoupled(new RFArbiterBundle(pregWidth))))
  val out = Valid(new RFArbiterBundle(pregWidth))
}
// use robIdx to check age in T0, and adjustment priority in T2
class OldestArbiter(pregWidth: Int, n: Int)(implicit p: Parameters) extends Module {
  val io = IO(new OldestArbiterIO(pregWidth, n))
  if (n == 1) {
    io.out.valid := io.in.head.valid
    io.out.bits := io.in.head.bits
    io.in.head.ready := true.B
  }
  else {
    val validVec = io.in.map(_.valid)
    val validVecReg = RegNext(VecInit(io.in.map(x => x.valid)), VecInit.tabulate(n)(i => false.B))
    val robIdxVecReg = RegNext(VecInit(io.in.map(x => x.bits.robIdx)))
    val priorityVecReg = RegInit(VecInit((0 until n).map(i => (1 << i).U(n.W))))
    val mergePriority = validVec.zip(priorityVecReg).map(x => Mux(x._1, x._2, 0.U)).reduce(_ | _)
    val lzdMergePriority = LZD(Reverse(mergePriority))
    val lzdHit = Wire(Vec(n, Bool()))
    for (i <- 0 until n) {
      // count valid and older than other num
      val otherValidVec = validVecReg.zipWithIndex.filter(_._2 != i).map(_._1)
      val otherRobIdxVec = robIdxVecReg.zipWithIndex.filter(_._2 != i).map(_._1)
      val indexLowerInvalidVec = validVecReg.zipWithIndex.filter(_._2 < i).map(!_._1)
      val olderThanOtherNum = PopCount(otherRobIdxVec.map(x => robIdxVecReg(i) < x).zip(otherValidVec).map(x => x._1 || !x._2))
      val otherValidNum = PopCount(otherValidVec)
      dontTouch(olderThanOtherNum)
      dontTouch(otherValidNum)
      val indexLowerInvalidNum = if (i == 0) 0.U else PopCount(indexLowerInvalidVec)
//      when(validVecReg(i)){
//        val minPriority = (1.U << (n-1))
//        priorityVecReg(i) := minPriority >> olderThanOtherNum
//      }.otherwise{
//        priorityVecReg(i) := 1.U << (otherValidNum + indexLowerInvalidNum)
//      }
      when(validVecReg(i) && (otherValidNum > 0.U)){
        val minPriority = (1.U << (n - 1))
        priorityVecReg(i) := minPriority >> olderThanOtherNum
      }.otherwise{
        priorityVecReg(i) := 1.U << Mux(otherValidNum > 1.U, otherValidNum + indexLowerInvalidNum, i.U)
      }
      assert(PopCount(priorityVecReg(i)) === 1.U, s"priorityRegVec_$i must be one hot")
      assert(PopCount(priorityVecReg.map(x => x === priorityVecReg(i))) === 1.U, s"priorityVecReg must be different")

      val lzdPriority = LZD(Reverse(priorityVecReg(i)))
      lzdHit(i) := lzdPriority === lzdMergePriority
      io.in(i).ready := !io.in(i).valid || lzdHit(i)
    }
    io.out.valid := validVec.reduce(_ | _)
    io.out.bits := Mux1H(lzdHit, io.in.map(_.bits))
  }
}


class RFReadArbiterIO(params: RFRdArbParams)(implicit p: Parameters) extends Bundle {
  private val pregWidth = params.pregParams.addrWidth
  val in = Flipped(params.genInputBundle)
  val out = Vec(params.portMax + 1, Valid(new RFArbiterBundle(pregWidth)))
}

class RFBankReadArbiterIO(params: RFRdArbParams)(implicit p: Parameters) extends Bundle {
  val in = Flipped(params.genInputBundle)
  val out = Vec(params.portMax + 1, Vec(params.pregParams.numBank, Valid(new RFArbiterBundle(params.pregParams.arbiterAddrWidth))))
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
  protected val arbiters: Seq[Option[OldestArbiter]] = portRange.map { portIdx =>
    Option.when(inGroup.isDefinedAt(portIdx))(
      Module(new OldestArbiter(pregWidth, inGroup(portIdx).size))
    )
  }

  arbiters.zipWithIndex.foreach { case (arbiter, portIdx) =>
    if (arbiter.nonEmpty) {
      arbiter.get.io.in.zip(inGroup(portIdx)).foreach { case (arbiterIn, ioIn) =>
        arbiterIn <> ioIn
      }
    }
  }

  if (params.pregParams.dataCfg.isInstanceOf[IntData]) {
    val arbitersIn = arbiters.filter(_.nonEmpty).map(_.get.io.in)
    val hasConflict = arbitersIn.map { case a =>
      PopCount(a.map(_.valid)) > 1.U
    }
    for (i <- hasConflict.indices) {
      XSPerfAccumulate(s"IntRFReadPort_${i}_Conflict", PopCount(hasConflict(i)))
    }
    val hasRead0 = arbitersIn.map { case a =>
      a.map(x => x.valid && x.bits.addr === 0.U).reduce(_ || _)
    }
    val hasSameAddr = arbitersIn.map { case a =>
      if (a.size == 2) a(0).valid && a(1).valid && a(0).bits.addr === a(1).bits.addr
      else false.B
    }
    val hasRead0Conflict = hasConflict.zip(hasRead0).map(x => x._1 && x._2)
    val hasReadSameAddrConflict = hasConflict.zip(hasSameAddr).map(x => x._1 && x._2)
    XSPerfAccumulate("IntRFRead0_conflict_count", PopCount(hasRead0Conflict))
    XSPerfAccumulate("IntRFReadSameAddr_conflict_count", PopCount(hasReadSameAddrConflict))
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

abstract class RFBankReadArbiterBase(val params: RFRdArbParams)(implicit p: Parameters) extends Module {
  protected def portRange: Range

  val io = IO(new RFBankReadArbiterIO(params))
  dontTouch(io)

  protected val pregParams = params.pregParams
  protected val pregWidth = pregParams.addrWidth
  protected val bankAddrWidth = log2Ceil(pregParams.numBank)

  protected val inGroup: Map[Int, Seq[DecoupledIO[RFArbiterBundle]]] = io.in
    .flatten.flatten
    .groupBy(_.bits.rdCfg.get.port)
    .map(x => (x._1, x._2.sortBy(_.bits.rdCfg.get.priority).toSeq))
  protected val arbiters: Seq[Option[Seq[OldestArbiter]]] = portRange.map { portIdx =>
    Option.when(inGroup.isDefinedAt(portIdx))(
      Seq.fill(params.pregParams.numBank)(Module(new OldestArbiter(pregWidth - bankAddrWidth, inGroup(portIdx).size)))
    )
  }

  arbiters.zipWithIndex.foreach { case (arbiters, portIdx) =>
    if (arbiters.nonEmpty) {
      arbiters.get.zipWithIndex.map{ case (arbiter, i) => {
          arbiter.io.in.zip(inGroup(portIdx)).zipWithIndex.foreach { case ((arbiterIn, ioIn), idx) =>
            arbiterIn.valid := ioIn.valid && (ioIn.bits.addr.head(bankAddrWidth) === i.U)
            arbiterIn.bits := ioIn.bits
            ioIn.ready := arbiters.get.map(x => x.io.in(idx).ready).reduce(_ && _)
          }
        }
      }
      // perfCounter
      val arbitersIn = arbiters.get.map(_.io.in)
      val numReq = arbitersIn.head.length
      if (params.pregParams.dataCfg.isInstanceOf[IntData] && numReq > 1) {
        println(f"numReq = ${numReq}")
        for (conflictNum <- 2 to numReq) {
          val hasConflict = arbitersIn.map { case x =>
            PopCount(x.map(_.valid)) === conflictNum.U
          }
          for (i <- hasConflict.indices) {
            XSPerfAccumulate(s"IntRFReadPort_portIdx_${portIdx}_bankIdx_${i}_Conflict_ReqNumIs${conflictNum}", PopCount(hasConflict(i)))
          }
        }
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
      val arbOut = VecInit(arb.get.map(_.io.out))
      out := arbOut
    } else {
      out := 0.U.asTypeOf(out)
    }
  }
}

class IntRFBankReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFBankReadArbiterBase(RFRdArbParams(backendParams.getRdCfgs[IntRD], backendParams.intPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(IntData()).max
}

class FpRFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getRdCfgs[FpRD], backendParams.fpPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(FpData()).max
}

class VfRFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getRdCfgs[VfRD], backendParams.vfPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(VecData()).max
}

class V0RFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getV0RdCfgs, backendParams.v0PregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(V0Data()).max
}

class VlRFReadArbiter(
  backendParams: BackendParams
)(implicit
  p: Parameters
) extends RFReadArbiterBase(RFRdArbParams(backendParams.getVlRdCfgs, backendParams.vlPregParams)) {
  override protected def portRange: Range = 0 to backendParams.getRdPortIndices(VlData()).max
}
