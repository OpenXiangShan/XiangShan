package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object MDUOpType {
  def mul    = "b0000".U
  def mulh   = "b0001".U
  def mulhsu = "b0010".U
  def mulhu  = "b0011".U
  def div    = "b0100".U
  def divu   = "b0101".U
  def rem    = "b0110".U
  def remu   = "b0111".U

  def mulw   = "b1000".U
  def divw   = "b1100".U
  def divuw  = "b1101".U
  def remw   = "b1110".U
  def remuw  = "b1111".U

  def isDiv(op: UInt) = op(2)
  def isDivSign(op: UInt) = isDiv(op) && !op(0)
  def isW(op: UInt) = op(3)
}

class MulDivIO(val len: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val sign = Input(Bool())
  val out = DecoupledIO(Output(UInt((len * 2).W)))
}

class Multiplier(len: Int) extends NOOPModule {
  val io = IO(new MulDivIO(len))
  val latency = 1

  def DSPInPipe[T <: Data](a: T) = RegNext(a)
  def DSPOutPipe[T <: Data](a: T) = RegNext(RegNext(RegNext(a)))
  val mulRes = (DSPInPipe(io.in.bits(0)).asSInt * DSPInPipe(io.in.bits(1)).asSInt)
  io.out.bits := DSPOutPipe(mulRes).asUInt
  io.out.valid := DSPOutPipe(DSPInPipe(io.in.fire()))

  val busy = RegInit(false.B)
  when (io.in.valid && !busy) { busy := true.B }
  when (io.out.valid) { busy := false.B }
  io.in.ready := (if (latency == 0) true.B else !busy)
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

  val stateCnt = Counter(len + 2)
  val busy = stateCnt.value =/= 0.U

  val (a, b) = (io.in.bits(0), io.in.bits(1))

  //Division by zero
  val divisionByZero = b === 0.U(len.W)

  //Overflow
  val bit1 = 1.U(1.W)
  val overflow = (a === Cat(1.U(1.W),0.U((len-1).W))) && (b === Fill(len, bit1)) && io.sign

  val specialResult = divisionByZero || overflow
  val earlyFinish = RegInit(false.B)
  val specialResultDIV = Mux(overflow, Cat(1.U(1.W), 0.U((len-1).W)), Fill(len, bit1))
  val specialResultDIVU = Fill(len, bit1)
  val specialResultREM = Mux(overflow, 0.U(len.W), a)
  val specialResultREMU = a
  val specialResultLo = Reg(UInt(len.W))
  val specialResultR = Reg(UInt(len.W))
  //early finish

  io.in.ready := !busy && !earlyFinish
  val newReqIn = !busy && io.in.fire()
  when(newReqIn){
    earlyFinish := specialResult
    specialResultLo := Mux(io.sign, specialResultDIV, specialResultDIVU)
    specialResultR := Mux(io.sign, specialResultREM, specialResultREMU)
  }
  when(io.out.fire && !newReqIn ){
    earlyFinish := false.B
  }

  when (!busy && io.in.fire() && !specialResult) {
    val (aSign, aVal) = abs(a, io.sign)
    val (bSign, bVal) = abs(b, io.sign)
    aSignReg := aSign
    bSignReg := bSign
    bReg := bVal
    val skipShift = CountLeadingZero(aVal, XLEN)
    shiftReg := Cat(aVal, 0.U(1.W)) << skipShift
    stateCnt.value := skipShift +& 1.U

    // printf(name + " Input %x %x %x\n", io.in.bits(0), io.in.bits(1), specialResult)
    // printf(name + " ABS %x %x \n", aVal, bVal)
  } .elsewhen (busy) {
    stateCnt.inc()
  }

  val hi = shiftReg(len * 2, len)
  val lo = shiftReg(len - 1, 0)
  when (busy) {
    val enough = hi.asUInt >= bReg.asUInt
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough)
    //printf(" DIVing state %d hi %x lo %x earlyFinish %x\n", stateCnt.value, hi, lo, earlyFinish)
  }

  val finish = (stateCnt.value === (stateCnt.n-1).U) && busy

  val r = hi(len, 1)
  val resQ = Mux(earlyFinish, specialResultLo, Mux(aSignReg ^ bSignReg, -lo, lo))
  val resR = Mux(earlyFinish, specialResultR, Mux(aSignReg, -r, r))
  io.out.bits := Cat(resR, resQ)
  io.out.valid := (if (HasDiv) (finish || earlyFinish) else io.in.valid) // FIXME: should deal with ready = 0
}

class MDUIO extends FunctionUnitIO {
}

class MDU extends NOOPModule {
  val io = IO(new MDUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  // when(io.in.fire()){
  //   printf(name + "%x %x\n", src1, src2)
  // }
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isDiv = MDUOpType.isDiv(func)
  val isDivSign = MDUOpType.isDivSign(func)
  val isW = MDUOpType.isW(func)

  val mul = Module(new Multiplier(XLEN + 1))
  val div = Module(new Divider(64))
  List(mul.io, div.io).map { case x =>
    x.sign := isDivSign
    x.out.ready := io.out.ready
  }

  val signext = SignExt(_: UInt, XLEN+1)
  val zeroext = ZeroExt(_: UInt, XLEN+1)
  val mulInputFuncTable = List(
    MDUOpType.mul    -> (zeroext, zeroext),
    MDUOpType.mulh   -> (signext, signext),
    MDUOpType.mulhsu -> (signext, zeroext),
    MDUOpType.mulhu  -> (zeroext, zeroext)
  )
  mul.io.in.bits(0) := LookupTree(func(1,0), mulInputFuncTable.map(p => (p._1(1,0), p._2._1(src1))))
  mul.io.in.bits(1) := LookupTree(func(1,0), mulInputFuncTable.map(p => (p._1(1,0), p._2._2(src2))))

  val divInputFunc = (x: UInt) => Mux(isW, Mux(isDivSign, SignExt(x(31,0), XLEN), ZeroExt(x(31,0), XLEN)), x)
  div.io.in.bits(0) := divInputFunc(src1)
  div.io.in.bits(1) := divInputFunc(src2)

  mul.io.in.valid := io.in.valid && !isDiv
  div.io.in.valid := io.in.valid && isDiv

  val mulRes = Mux(func(1,0) === MDUOpType.mul(1,0), mul.io.out.bits(XLEN-1,0), mul.io.out.bits(2*XLEN-1,XLEN))
  val divRes = Mux(func(1) /* rem */, div.io.out.bits(2*XLEN-1,XLEN), div.io.out.bits(XLEN-1,0))
  val res = Mux(isDiv, divRes, mulRes)
  io.out.bits := Mux(isW, SignExt(res(31,0),XLEN), res)

  val isDivReg = Mux(io.in.fire(), isDiv, RegNext(isDiv))
  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)
  io.out.valid := Mux(isDivReg, div.io.out.valid, mul.io.out.valid)

  BoringUtils.addSource(mul.io.out.fire(), "perfCntCondMmulInstr")
}
