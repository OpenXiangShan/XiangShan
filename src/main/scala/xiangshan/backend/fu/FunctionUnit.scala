package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.MDUOpType
import xiangshan.backend.fu.fpu._

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
  latency: HasFuLatency = CertainLatency(0),
) {
  def srcCnt: Int = math.max(numIntSrc, numFpSrc)
}


class FuOutput(val len: Int) extends XSBundle {
  val data = UInt(len.W)
  val uop = new MicroOp
}


class FunctionUnitIO(val len: Int) extends XSBundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val src = Vec(3, UInt(len.W))
    val uop = new MicroOp
  }))

  val out = DecoupledIO(new FuOutput(len))

  val redirectIn = Flipped(ValidIO(new Redirect))
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


  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.roqIdx.needFlush(io.redirectIn))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)){
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)){
      validVec(i) := false.B
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last
  io.out.bits.uop := uopVec.last

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    enable = regEnable(i)
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

  def i2f = new IntToFP

  def fmac = new FMA

  def f2i = new FPToInt

  def f2f = new FPToFP

  def fdivSqrt = new FDivSqrt

  def f2iSel(x: FunctionUnit): Bool = {
    x.io.in.bits.uop.ctrl.rfWen
  }

  def i2fSel(x: FunctionUnit): Bool = {
    x.io.in.bits.uop.ctrl.fpu.fromInt
  }

  def f2fSel(x: FunctionUnit): Bool = {
    val ctrl = x.io.in.bits.uop.ctrl.fpu
    ctrl.fpWen && !ctrl.div && !ctrl.sqrt
  }

  def fdivSqrtSel(x: FunctionUnit): Bool = {
    val ctrl = x.io.in.bits.uop.ctrl.fpu
    ctrl.div || ctrl.sqrt
  }

  val aluCfg = FuConfig(
    fuGen = alu _,
    fuSel = _ => true.B,
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
  )

  val jmpCfg = FuConfig(
    fuGen = jmp _,
    fuSel = (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.jmp,
    fuType = FuType.jmp,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
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
    fuSel = i2fSel,
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
    FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(4)
  )

  val f2iCfg = FuConfig(
    fuGen = f2i _,
    fuSel = f2iSel,
    FuType.fmisc, 0, 1, writeIntRf = true, writeFpRf = false, hasRedirect = false, CertainLatency(2)
  )

  val f2fCfg = FuConfig(
    fuGen = f2f _,
    fuSel = f2fSel,
    FuType.fmisc, 0, 1, writeIntRf = false, writeFpRf = true, hasRedirect = false, CertainLatency(2)
  )

  val fdivSqrtCfg = FuConfig(
    fuGen = fdivSqrt _,
    fuSel = fdivSqrtSel,
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
