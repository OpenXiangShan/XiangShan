package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object MDUOpType {
  def mul  = "b0000".U
  def mulh = "b0001".U
  def div  = "b0100".U
  def divu = "b0101".U
  def rem  = "b0110".U
  def remu = "b0111".U

  def mulw  = "b1000".U
  def divw  = "b1100".U
  def divuw = "b1101".U
  def remw  = "b1110".U
  def remuw = "b1111".U

  def isDiv(op: UInt) = op(2)
  def isSign(op: UInt) = isDiv(op) && !op(0)
  def isW(op: UInt) = op(3)
}

object MDUInstr extends HasInstrType {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")
  def MULW    = BitPat("b0000001_?????_?????_000_?????_0111011")
  def DIVW    = BitPat("b0000001_?????_?????_100_?????_0111011")
  def DIVUW   = BitPat("b0000001_?????_?????_101_?????_0111011")
  def REMW    = BitPat("b0000001_?????_?????_110_?????_0111011")
  def REMUW   = BitPat("b0000001_?????_?????_111_?????_0111011")

  val mulTable = Array(
    MUL            -> List(InstrR, FuType.mdu, MDUOpType.mul),
    MULH           -> List(InstrR, FuType.mdu, MDUOpType.mulh),
    MULW           -> List(InstrR, FuType.mdu, MDUOpType.mulw)
  )
  val divTable = Array(
    DIV            -> List(InstrR, FuType.mdu, MDUOpType.div),
    DIVU           -> List(InstrR, FuType.mdu, MDUOpType.divu),
    REM            -> List(InstrR, FuType.mdu, MDUOpType.rem),
    REMU           -> List(InstrR, FuType.mdu, MDUOpType.remu),
    DIVW           -> List(InstrR, FuType.mdu, MDUOpType.divw),
    DIVUW          -> List(InstrR, FuType.mdu, MDUOpType.divuw),
    REMW           -> List(InstrR, FuType.mdu, MDUOpType.remw),
    REMUW          -> List(InstrR, FuType.mdu, MDUOpType.remuw)
  )
  def table(implicit p: NOOPConfig) = mulTable ++ (if (p.HasDiv) divTable else Nil)
}

class MulDivIO(val len: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val sign = Input(Bool())
  val out = DecoupledIO(Vec(2, Output(UInt(len.W))))
}

class Multiplier(len: Int)(implicit val p: NOOPConfig) extends Module {
  val io = IO(new MulDivIO(len))
  val latency = if (p.HasMExtension) 1 else 0

  def DSPpipe[T <: Data](a: T) = RegNext(a)
  val mulRes = (DSPpipe(io.in.bits(0)).asSInt * DSPpipe(io.in.bits(1)).asSInt).asUInt
  val mulPipeOut = Pipe(DSPpipe(io.in.fire()), mulRes, latency)

  io.out.bits(0) := (if (!p.HasMExtension) 0.U else mulPipeOut.bits(len - 1, 0))
  io.out.bits(1) := (if (!p.HasMExtension) 0.U else mulPipeOut.bits(2 * len - 1, len))

  val busy = RegInit(false.B)
  when (io.in.valid && !busy) { busy := true.B }
  when (mulPipeOut.valid) { busy := false.B }

  io.in.ready := (if (latency == 0) true.B else !busy)
  io.out.valid := mulPipeOut.valid
}

class Divider(len: Int = 64)(implicit val p: NOOPConfig) extends Module {
  val io = IO(new MulDivIO(len))

  val shiftReg = Reg(UInt((1 + len * 2).W))
  val bReg = Reg(UInt(len.W))
  val aSignReg = Reg(Bool())
  val bSignReg = Reg(Bool())

  def abs(a: UInt, sign: Bool): (Bool, UInt) = {
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val next = Wire(Bool())
  val (state, finish) = Counter(next, len + 2)

  io.in.ready := state === 0.U
  val (a, b) = (io.in.bits(0), io.in.bits(1))
  when (state === 0.U && io.in.fire()) {
    val (aSign, aVal) = abs(a, io.sign)
    val (bSign, bVal) = abs(b, io.sign)
    aSignReg := aSign
    bSignReg := bSign
    bReg := bVal
    shiftReg := Cat(0.U(len.W), aVal, 0.U(1.W))
  }

  val hi = shiftReg(len * 2, len)
  val lo = shiftReg(len - 1, 0)
  when (state =/= 0.U) {
    val enough = hi >= bReg
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough)
  }

  next := (state === 0.U && io.in.fire()) || (state =/= 0.U)

  val r = hi(len, 1)
  io.out.bits(0) := (if (p.HasDiv) Mux(aSignReg ^ bSignReg, -lo, lo) else 0.U)
  io.out.bits(1) := (if (p.HasDiv) Mux(aSignReg, -r, r) else 0.U)
  io.out.valid := (if (p.HasDiv) finish else io.in.valid) // FIXME: should deal with ready = 0
}

class MDUIO extends FunctionUnitIO {
}

class MDU(implicit val p: NOOPConfig) extends Module {
  val io = IO(new MDUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val mul = Module(new Multiplier(64))
  val div = Module(new Divider(64))
  val mul32 = Module(new Multiplier(32))
  val div32 = Module(new Divider(32))
  List(mul.io, div.io).map { case x =>
    x.in.bits(0) := src1
    x.in.bits(1) := src2
    x.sign := MDUOpType.isSign(func)
    x.out.ready := io.out.ready
  }
  List(mul32.io, div32.io).map { case x =>
    x.in.bits(0) := src1(31,0)
    x.in.bits(1) := src2(31,0)
    x.sign := MDUOpType.isSign(func)
    x.out.ready := io.out.ready
  }
  val isDiv = MDUOpType.isDiv(func)
  val isW   = MDUOpType.isW(func)
  mul.io.in.valid := io.in.valid && !isDiv && !isW
  div.io.in.valid := io.in.valid && isDiv && !isW
  mul32.io.in.valid := io.in.valid && !isDiv && isW
  div32.io.in.valid := io.in.valid && isDiv && isW

  io.out.bits := LookupTree(func, List(
    MDUOpType.mul  -> mul.io.out.bits(0),
    MDUOpType.mulh -> mul.io.out.bits(1),
    MDUOpType.div  -> div.io.out.bits(0),
    MDUOpType.divu -> div.io.out.bits(0),
    MDUOpType.rem  -> div.io.out.bits(1),
    MDUOpType.remu -> div.io.out.bits(1),

    MDUOpType.mulw -> Cat(Fill(32, mul32.io.out.bits(0)(31)), mul32.io.out.bits(0)),
    MDUOpType.divw -> Cat(Fill(32, div32.io.out.bits(0)(31)), div32.io.out.bits(0)),
    MDUOpType.divuw-> Cat(Fill(32, div32.io.out.bits(0)(31)), div32.io.out.bits(0)),//not sure: spec used "signed ext to describe this inst"
    MDUOpType.remw -> Cat(Fill(32, div32.io.out.bits(1)(31)), div32.io.out.bits(1)),
    MDUOpType.remuw-> Cat(Fill(32, div32.io.out.bits(1)(31)), div32.io.out.bits(1))
  ))

  val isDivReg = Mux(io.in.fire(), isDiv, RegNext(isDiv))
  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)
  io.out.valid := Mux(isDivReg, div.io.out.valid || div32.io.out.valid, mul.io.out.valid || mul32.io.out.valid)

  BoringUtils.addSource(mul.io.out.fire(), "perfCntCondMmulInstr")
}
