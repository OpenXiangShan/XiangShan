package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasMDUOpType {
  val MduOpTypeNum  = 8

  def MduMul  = "b000".U
  def MduMulh = "b001".U
  def MduDiv  = "b100".U
  def MduDivu = "b101".U
  def MduRem  = "b110".U
  def MduRemu = "b111".U

  def isDiv(op: UInt) = op(2)
  def isSign(op: UInt) = isDiv(op) && !op(0)
}

object MDUInstr extends HasDecodeConst {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")

  val mulTable = Array(
    MUL            -> List(InstrR, FuMdu, MduMul),
    MULH           -> List(InstrR, FuMdu, MduMulh)
  )
  val divTable = Array(
    DIV            -> List(InstrR, FuMdu, MduDiv),
    DIVU           -> List(InstrR, FuMdu, MduDivu),
    REM            -> List(InstrR, FuMdu, MduRem),
    REMU           -> List(InstrR, FuMdu, MduRemu)
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

class Divider(len: Int = 32)(implicit val p: NOOPConfig) extends Module {
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

class MDU(implicit val p: NOOPConfig) extends Module with HasMDUOpType {
  val io = IO(new MDUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val mul = Module(new Multiplier(32))
  val div = Module(new Divider(32))
  List(mul.io, div.io).map { case x =>
    x.in.bits(0) := src1
    x.in.bits(1) := src2
    x.sign := isSign(func)
    x.out.ready := io.out.ready
  }
  mul.io.in.valid := io.in.valid && !isDiv(func)
  div.io.in.valid := io.in.valid && isDiv(func)

  io.out.bits := LookupTree(func, 0.U, List(
    MduMul  -> mul.io.out.bits(0),
    MduMulh -> mul.io.out.bits(1),
    MduDiv  -> div.io.out.bits(0),
    MduDivu -> div.io.out.bits(0),
    MduRem  -> div.io.out.bits(1),
    MduRemu -> div.io.out.bits(1)
  ))

  val isDivReg = Mux(io.in.fire(), isDiv(func), RegNext(isDiv(func)))
  io.in.ready := Mux(isDiv(func), div.io.in.ready, mul.io.in.ready)
  io.out.valid := Mux(isDivReg, div.io.out.valid, mul.io.out.valid)

  BoringUtils.addSource(mul.io.out.fire(), "perfCntCondMmulInstr")
}
