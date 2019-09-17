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

class MulDivIO(val len: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val sign = Input(Bool())
  val out = DecoupledIO(Vec(2, Output(UInt(len.W))))
}

class Multiplier(len: Int) extends NOOPModule {
  val io = IO(new MulDivIO(len))
  val latency = if (HasMExtension) 1 else 0

  def DSPpipe[T <: Data](a: T) = RegNext(a)
  val mulRes = (DSPpipe(io.in.bits(0)).asSInt * DSPpipe(io.in.bits(1)).asSInt).asUInt
  val mulPipeOut = Pipe(DSPpipe(io.in.fire()), mulRes, latency)

  io.out.bits(0) := (if (!HasMExtension) 0.U else mulPipeOut.bits(len - 1, 0))
  io.out.bits(1) := (if (!HasMExtension) 0.U else mulPipeOut.bits(2 * len - 1, len))

  val busy = RegInit(false.B)
  when (io.in.valid && !busy) { busy := true.B }
  when (mulPipeOut.valid) { busy := false.B }

  io.in.ready := (if (latency == 0) true.B else !busy)
  io.out.valid := mulPipeOut.valid
}

class Divider(len: Int = 64) extends NOOPModule {
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

  //Division by zero
  val divisionByZero = b === 0.U(len.W)

  //Overflow
  val bit1 = 1.U(1.W)
  val overflow = (a === Fill(len, bit1)) && io.sign 

  val specialResult = divisionByZero || overflow
  val earlyFinish = RegInit(false.B)
  val specialResultDIV = Mux(overflow, Cat(1.U(1.W), 0.U((len-1).W)), Fill(len, bit1))
  val specialResultDIVU = Fill(len, bit1)
  val specialResultREM = Mux(overflow, 0.U(len.W), a)
  val specialResultREMU = a
  val specialResultLo = Reg(UInt(len.W))
  val specialResultR = Reg(UInt(len.W))
  //early finish
  when(state === 0.U && io.in.fire()){
    earlyFinish := specialResult 
    specialResultLo := Mux(io.sign, specialResultDIV, specialResultDIVU)
    specialResultR := Mux(io.sign, specialResultREM, specialResultREMU)
  }
  when(io.out.fire){
    earlyFinish := false.B
  }

  val hi = shiftReg(len * 2, len)
  val lo = shiftReg(len - 1, 0)
  when (state =/= 0.U) {
    val enough = hi >= bReg
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough)
  }

  next := (state === 0.U && io.in.fire()) || (state =/= 0.U)

  val r = hi(len, 1)
  io.out.bits(0) := (if (HasDiv) Mux(earlyFinish, specialResultLo, Mux(aSignReg ^ bSignReg, -lo, lo)) else 0.U)
  io.out.bits(1) := (if (HasDiv) Mux(earlyFinish, specialResultR, Mux(aSignReg, -r, r)) else 0.U)
  io.out.valid := (if (HasDiv) (finish || earlyFinish) else io.in.valid) // FIXME: should deal with ready = 0
}

class MDUIO extends FunctionUnitIO {
}

class MDU extends NOOPModule {
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

    MDUOpType.mulw -> SignExt(mul32.io.out.bits(0), XLEN),
    MDUOpType.divw -> SignExt(div32.io.out.bits(0), XLEN),
    MDUOpType.divuw-> SignExt(div32.io.out.bits(0), XLEN),//not sure: spec used "signed ext to describe this inst"
    MDUOpType.remw -> SignExt(div32.io.out.bits(1), XLEN),
    MDUOpType.remuw-> SignExt(div32.io.out.bits(1), XLEN)
  ))

  val isDivReg = Mux(io.in.fire(), isDiv, RegNext(isDiv))
  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)
  io.out.valid := Mux(isDivReg, div.io.out.valid || div32.io.out.valid, mul.io.out.valid || mul32.io.out.valid)

  BoringUtils.addSource(mul.io.out.fire(), "perfCntCondMmulInstr")
}
