package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.backend.fu.{CertainLatency, FuConfig, FuOutput, FunctionUnit, HasFuLatency, UncertainLatency}
import utils.ParallelOR
import xiangshan.backend.fu.FunctionUnit._

case class ExuParameters
(
  JmpCnt: Int,
  AluCnt: Int,
  MulCnt: Int,
  MduCnt: Int,
  FmacCnt: Int,
  FmiscCnt: Int,
  FmiscDivSqrtCnt: Int,
  LduCnt: Int,
  StuCnt: Int
) {
  assert(JmpCnt == 1, "Only support 1 JmpUnit now!")

  def IntExuCnt = AluCnt + MulCnt + MduCnt + JmpCnt

  def FpExuCnt = FmacCnt + FmiscCnt + FmiscDivSqrtCnt

  def LsExuCnt = LduCnt + StuCnt

  def ExuCnt = IntExuCnt + FpExuCnt + LduCnt + StuCnt

  def NRFuType = 9

  def FuOpWidth = 7
}

case class ExuConfig
(
  name: String,
  supportedFuncUnits: Seq[FuConfig],
  wbIntPriority: Int,
  wbFpPriority: Int
) {
  def max(in: Seq[Int]): Int = in.reduce((x, y) => if (x > y) x else y)

  val intSrcCnt = max(supportedFuncUnits.map(_.numIntSrc))
  val fpSrcCnt = max(supportedFuncUnits.map(_.numFpSrc))
  val readIntRf = intSrcCnt > 0
  val readFpRf = fpSrcCnt > 0
  val writeIntRf = supportedFuncUnits.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf = supportedFuncUnits.map(_.writeFpRf).reduce(_ || _)
  val hasRedirect = supportedFuncUnits.map(_.hasRedirect).reduce(_ || _)

  val latency: HasFuLatency = {
    val lats = supportedFuncUnits.map(_.latency)
    if (lats.exists(x => x.latencyVal.isEmpty)) {
      UncertainLatency()
    } else {
      val x = lats.head
      for (l <- lats.drop(1)) {
        require(x.latencyVal.get == l.latencyVal.get)
      }
      x
    }
  }
  val hasCertainLatency = latency.latencyVal.nonEmpty
  val hasUncertainlatency = latency.latencyVal.isEmpty

  def canAccept(fuType: UInt): Bool = {
    Cat(supportedFuncUnits.map(_.fuType === fuType)).orR()
  }
}

abstract class Exu[T <: FunctionUnit]
(
  val exuName: String,
  val fuGen: Seq[(() => T, T => Bool)],
  val wbIntPriority: Int,
  val wbFpPriority: Int
) extends XSModule {

  val supportedFunctionUnits = fuGen.map(_._1).map(gen => Module(gen()))

  val fuSel = supportedFunctionUnits.zip(fuGen.map(_._2)).map(x => x._2(x._1))

  def fuConfigs = supportedFunctionUnits.map(_.cfg)

  def config: ExuConfig = {
    ExuConfig(exuName, fuConfigs, wbIntPriority, wbFpPriority)
  }

  require(fuGen.nonEmpty)
  require(!fuConfigs.exists(c => {
    (c.numIntSrc > 0) && (c.numFpSrc > 0)
  }))

  //  val io = IO(new ExuIO)

  val io = IO(new Bundle() {
    val fromInt = if (config.readIntRf) Flipped(DecoupledIO(new ExuInput)) else null
    val fromFp = if (config.readFpRf) Flipped(DecoupledIO(new ExuInput)) else null
    val redirect = Flipped(ValidIO(new Redirect))
    val toInt = if (config.writeIntRf) DecoupledIO(new ExuOutput) else null
    val toFp = if (config.writeFpRf) DecoupledIO(new ExuOutput) else null
  })

  for ((fu, sel) <- supportedFunctionUnits.zip(fuSel)) {

    val in = if (fu.cfg.numIntSrc > 0) {
      assert(fu.cfg.numFpSrc == 0)
      io.fromInt
    } else {
      assert(fu.cfg.numFpSrc > 0)
      io.fromFp
    }

    val src1 = in.bits.src1
    val src2 = in.bits.src2
    val src3 = in.bits.src3

    fu.io.in.valid := in.valid && sel
    fu.io.in.bits.uop := in.bits.uop
    if (fu.cfg.srcCnt > 0) {
      fu.io.in.bits.src(0) := src1
    }
    if (fu.cfg.srcCnt > 1) {
      fu.io.in.bits.src(1) := src2
    }
    if (fu.cfg.srcCnt > 2) {
      fu.io.in.bits.src(2) := src3
    }
    fu.io.redirectIn := io.redirect
  }


  val needArbiter = !(config.latency.latencyVal.nonEmpty && (config.latency.latencyVal.get == 0))

  def writebackArb(in: Seq[DecoupledIO[FuOutput]], out: DecoupledIO[ExuOutput]): Arbiter[FuOutput] = {
    if (needArbiter) {
      val arb = Module(new Arbiter(new FuOutput, in.size))
      arb.io.in <> in
      arb.io.out.ready := out.ready
      out.bits.data := arb.io.out.bits.data
      out.bits.uop := arb.io.out.bits.uop
      out.valid := arb.io.out.valid
      arb
    } else {
      in.foreach(_.ready := out.ready)
      val sel = Mux1H(in.map(x => x.valid -> x))
      out.bits.data := sel.bits.data
      out.bits.uop := sel.bits.uop
      out.valid := sel.valid
      null
    }
  }

  val intArb = if (config.writeIntRf) writebackArb(
    supportedFunctionUnits.filter(_.cfg.writeIntRf).map(_.io.out),
    io.toInt
  ) else null

  val fpArb = if (config.writeFpRf) writebackArb(
    supportedFunctionUnits.filter(_.cfg.writeFpRf).map(_.io.out),
    io.toFp
  ) else null

  val readIntFu = supportedFunctionUnits.zip(fuSel).filter(p => p._1.cfg.numIntSrc > 0)
  val readFpFu = supportedFunctionUnits.zip(fuSel).filter(p => p._1.cfg.numFpSrc > 0)

  def inReady(s: Seq[(T, Bool)]): Bool = {
    if (s.size == 1) {
      s.head._1.io.in.ready
    } else {
      if(needArbiter){
        Cat(s.map(x => x._1.io.in.ready && x._2)).orR()
      } else {
        Cat(s.map(x => x._1.io.in.ready)).andR()
      }
    }
  }


  if (config.readIntRf) {
    io.fromInt.ready := inReady(
      supportedFunctionUnits.zip(fuSel).filter(p => p._1.cfg.numIntSrc > 0)
    )
  }

  if (config.readFpRf) {
    io.fromFp.ready := inReady(
      supportedFunctionUnits.zip(fuSel).filter(p => p._1.cfg.numFpSrc > 0)
    )
  }

  def assignDontCares(out: ExuOutput) = {
    out.brUpdate := DontCare
    out.fflags := DontCare
    out.debug <> DontCare
    out.debug.isMMIO := false.B
    out.redirect <> DontCare
    out.redirectValid := false.B
  }

  if(config.writeFpRf){
    assignDontCares(io.toFp.bits)
  }
  if(config.writeIntRf){
    assignDontCares(io.toInt.bits)
  }
}
