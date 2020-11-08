package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.fpu.divsqrt.DivSqrt
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.fpu.fma.FMA

/*
    XiangShan Function Unit
    A Exu can have one or more function units
 */

trait HasFuLatency {
  val latencyVal: Option[Int]
}

case class CertainLatency(value: Int) extends HasFuLatency{
  override val latencyVal: Option[Int] = Some(value)
}

case class UncertainLatency() extends HasFuLatency {
  override val latencyVal: Option[Int] = None
}



case class FuConfig
(
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  hasRedirect: Boolean,
  latency: HasFuLatency = CertainLatency(0)
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
}



class FuOutput extends XSBundle {
  val data = UInt(XLEN.W)
  val uop = new MicroOp
}


class FunctionUnitIO[TI <: Data, TO <: Data]
(
  cfg: FuConfig,
  len: Int
) extends XSBundle
{
  val in = Flipped(DecoupledIO(new Bundle() {
    val src = Vec(cfg.srcCnt, UInt(len.W))
    val uop = new MicroOp

    def connectToExuInput(exuIn: ExuInput): Unit = {
      val exuSrcIn = Seq(exuIn.src1, exuIn.src2, exuIn.src3)
      src.zip(exuSrcIn).foreach{case (x, y) => x := y}
      uop := exuIn.uop
    }
  }))

  val out = DecoupledIO(new FuOutput)

  val redirectIn = Flipped(ValidIO(new Redirect))

  override def cloneType: FunctionUnitIO.this.type =
    new FunctionUnitIO(cfg, len).asInstanceOf[this.type]
}

abstract class FunctionUnit
(
  val cfg: FuConfig,
  val len: Int = 64
) extends XSModule {

  val io = IO(new FunctionUnitIO(cfg, len))

}

trait HasPipelineReg { this: FunctionUnit =>

  require(cfg.latency.latencyVal.nonEmpty && cfg.latency.latencyVal.get > 0)
  val latency = cfg.latency.latencyVal.get

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new MicroOp))


  val flushVec = uopVec.zip(validVec).map(x => x._2 && x._1.roqIdx.needFlush(io.redirectIn))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(flushVec(i - 1) || rdyVec(i) && !validVec(i - 1)) {
      validVec(i) := false.B
    }.elsewhen(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)) {
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last && !flushVec.last
  io.out.bits.uop := uopVec.last

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    enable = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)
}

object FunctionUnit extends HasXSParameter {

  def multiplier = new ArrayMultiplier(XLEN+1)
  def divider = new Divider(XLEN)
  def alu = new Alu

  def jmp = new Jump
  def fence = new Fence
  def csr = new CSR
  def i2f = new IntToFloatSingleCycle

  def fmac = new FMA
  def fcmp = new FCMP
  def fmv = new FMV(XLEN)
  def f2i = new FloatToInt
  def f32toF64 = new F32toF64
  def f64toF32 = new F64toF32
  def fdivSqrt = new DivSqrt

  def fmiscSel(fu: String)(x: FPUSubModule): Bool = {
    x.io.in.bits.uop.ctrl.fuOpType.head(4) === s"b$fu".U
  }

  val lduCfg =
    FuConfig(FuType.ldu, 1, 0, writeIntRf = true, writeFpRf = true, hasRedirect = false,
      UncertainLatency()
    )

  val stuCfg =
    FuConfig(FuType.stu, 2, 1, writeIntRf = false, writeFpRf = false, hasRedirect = false,
      UncertainLatency()
    )

  val mouCfg = 
    FuConfig(FuType.mou, 2, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
      UncertainLatency()
  )
}
