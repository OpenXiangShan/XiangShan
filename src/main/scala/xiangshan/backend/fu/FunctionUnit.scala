package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.MDUOpType
import xiangshan.backend.fu.fpu.FPUOpType.{FU_D2S, FU_DIVSQRT, FU_F2I, FU_FCMP, FU_FMV, FU_S2D}
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

case class CertainLatency(value: Int) extends HasFuLatency {
  override val latencyVal: Option[Int] = Some(value)
}

case class UncertainLatency() extends HasFuLatency {
  override val latencyVal: Option[Int] = None
}


case class FuConfig
(
  fuGen: () => FunctionUnit,
  fuSel: FunctionUnit => Bool,
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


class FunctionUnitIO(len: Int) extends XSBundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val src = Vec(3, UInt(len.W))
    val uop = new MicroOp
  }))

  val out = DecoupledIO(new FuOutput)

  val redirectIn = Flipped(ValidIO(new Redirect))

  override def cloneType: FunctionUnitIO.this.type =
    new FunctionUnitIO(len).asInstanceOf[this.type]
}

abstract class FunctionUnit(len: Int = 64) extends XSModule {

  val io = IO(new FunctionUnitIO(len))

}

trait HasPipelineReg {
  this: FunctionUnit =>

  def latency: Int

  require(latency > 0)

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

  def divider = new SRT4Divider(XLEN)

  def multiplier = new ArrayMultiplier(XLEN + 1, Seq(0, 2))

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

  def fmiscSel(fu: String)(x: FunctionUnit): Bool = {
    x.io.in.bits.uop.ctrl.fuOpType.head(4) === s"b$fu".U
  }

  val aluCfg = FuConfig(
    fuGen = alu _,
    fuSel = _ => true.B,
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true
  )

  val jmpCfg = FuConfig(
    fuGen = jmp _,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.jmp,
    fuType = FuType.jmp,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true
  )

  val fenceCfg = FuConfig(
    fuGen = fence _,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.fence,
    FuType.fence, 1, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency() // TODO: need rewrite latency structure, not just this value
  )

  val csrCfg = FuConfig(
    fuGen = csr _,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.csr,
    fuType = FuType.csr,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false
  )

  val i2fCfg = FuConfig(
    fuGen = i2f _,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.i2f,
    FuType.i2f,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    hasRedirect = false,
    CertainLatency(0)
  )

  val divCfg = FuConfig(
    fuGen = divider _,
    fuSel = (x: FunctionUnit) => MDUOpType.isDiv(x.io.in.bits.uop.ctrl.fuOpType),
    FuType.div,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false,
    UncertainLatency()
  )

  val mulCfg = FuConfig(
    fuGen = multiplier _,
    fuSel = (x: FunctionUnit) => MDUOpType.isMul(x.io.in.bits.uop.ctrl.fuOpType),
    FuType.mul,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = false,
    CertainLatency(3)
  )

  val fmacCfg = FuConfig(
    fuGen = fmac _,
    fuSel = _ => true.B,
    FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(5)
  )

  val fcmpCfg = FuConfig(
    fuGen = fcmp _,
    fuSel = (x: FunctionUnit) => fmiscSel(FU_FCMP)(x) && x.io.in.bits.uop.ctrl.rfWen,
    FuType.fmisc, 0, 2, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(2)
  )

  val fminCfg = FuConfig(
    fuGen = fcmp _,
    fuSel = (x: FunctionUnit) => fmiscSel(FU_FCMP)(x) && x.io.in.bits.uop.ctrl.fpWen,
    FuType.fmisc, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(2)
  )

  val fsgnjCfg = FuConfig(
    fuGen = fmv _,
    fuSel = (x: FunctionUnit) => fmiscSel(FU_FMV)(x) && x.io.in.bits.uop.ctrl.fpWen,
    FuType.fmisc, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(1)
  )

  val fmvCfg = FuConfig(
    fuGen = fmv _,
    fuSel = (x: FunctionUnit) => fmiscSel(FU_FMV)(x) && x.io.in.bits.uop.ctrl.rfWen,
    FuType.fmisc, 0, 2, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(1)
  )

  val f2iCfg = FuConfig(
    fuGen = f2i _,
    fuSel = fmiscSel(FU_F2I),
    FuType.fmisc, 0, 1, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(2)
  )

  val s2dCfg = FuConfig(
    fuGen = f32toF64 _,
    fuSel = fmiscSel(FU_S2D),
    FuType.fmisc, 0, 1, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(2)
  )

  val d2sCfg = FuConfig(
    fuGen = f64toF32 _,
    fuSel = fmiscSel(FU_D2S),
    FuType.fmisc, 0, 1, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(2)
  )

  val fdivSqrtCfg = FuConfig(
    fuGen = fdivSqrt _,
    fuSel = fmiscSel(FU_DIVSQRT),
    FuType.fDivSqrt, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false, UncertainLatency()
  )

  val lduCfg = FuConfig(
    null, // DontCare
    null,
    FuType.ldu, 1, 0, writeIntRf = true, writeFpRf = true, hasRedirect = false,
    UncertainLatency()
  )

  val stuCfg = FuConfig(
    null,
    null,
    FuType.stu, 2, 1, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency()
  )

  val mouCfg = FuConfig(
    null,
    null,
    FuType.mou, 2, 0, writeIntRf = false, writeFpRf = false, hasRedirect = false,
    UncertainLatency()
  )
}
