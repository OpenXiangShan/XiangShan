package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{LookupTree, SignExt, ZeroExt, _}
import xiangshan.backend.{MULOpType, MDUOpType}

class Mul extends Exu(FuType.mul.litValue()){
  override def toString: String = "Mul"

  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)

  val mul = Module(new ArrayMultiplier(XLEN+1))

  val signext = SignExt(_: UInt, XLEN+1)
  val zeroext = ZeroExt(_: UInt, XLEN+1)
  val mulInputFuncTable = List(
    MULOpType.mul    -> (zeroext, zeroext),
    MULOpType.mulh   -> (signext, signext),
    MULOpType.mulhsu -> (signext, zeroext),
    MULOpType.mulhu  -> (zeroext, zeroext)
  )

  val isW = MDUOpType.isW(func)

  mul.io.redirect := io.redirect
  mul.io.in.bits.ctrl.uop := io.in.bits.uop
  mul.io.in.bits.ctrl.sign := DontCare //Mul don't use this
  mul.io.in.bits.ctrl.isW := isW
  mul.io.in.bits.ctrl.isHi := func(1,0) =/= MDUOpType.mul(1,0)
  mul.io.in.bits.src1 := LookupTree(
    func(1,0),
    mulInputFuncTable.map(p => (p._1(1,0), p._2._1(src1)))
  )
  mul.io.in.bits.src2 := LookupTree(
    func(1,0),
    mulInputFuncTable.map(p => (p._1(1,0), p._2._2(src2)))
  )
  mul.io.in.valid := io.in.valid
  mul.io.out.ready := io.out.ready

  io.in.ready := mul.io.in.ready
  io.out.valid := mul.io.out.valid
  io.out.bits.uop := mul.io.out.bits.uop
  io.out.bits.data := mul.io.out.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d) brTag:%x\n",
    io.in.valid, io.in.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.in.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )
}

// A wrapper of Divider
class Div extends XSModule {
  val io = IO(new ExuIO)

  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)

  val divider = Module(new Divider(XLEN))

  val isDiv = MDUOpType.isDiv(func)
  val isDivSign = MDUOpType.isDivSign(func)
  val isW = MDUOpType.isW(func)

  val divInputFunc = (x: UInt) => Mux(
    isW,
    Mux(isDivSign,
      SignExt(x(31,0), XLEN),
      ZeroExt(x(31,0), XLEN)
    ),
    x
  )

  divider.io.redirect := io.redirect
  divider.io.in.valid := io.in.valid
  divider.io.in.bits.ctrl.uop := io.in.bits.uop
  divider.io.in.bits.ctrl.sign := isDivSign
  divider.io.in.bits.ctrl.isW := isW
  divider.io.in.bits.ctrl.isHi := func(1)
  divider.io.in.bits.src1 := divInputFunc(src1)
  divider.io.in.bits.src2 := divInputFunc(src2)
  divider.io.out.ready := io.out.ready

  io.in.ready := divider.io.in.ready
  io.out.valid := divider.io.out.valid
  io.out.bits.uop := divider.io.out.bits.uop
  io.out.bits.data := divider.io.out.bits.data
  io.out.bits.redirectValid := false.B
  io.out.bits.redirect <> DontCare
  io.dmem <> DontCare
  io.out.bits.debug <> DontCare

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d) brTag:%x\n",
    io.in.valid, io.in.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.in.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )

}

class Mdu extends Exu(FuType.mdu.litValue()) {
  override def toString: String = "MulDiv"

  val (src1, src2, uop, func) =
    (io.in.bits.src1, io.in.bits.src2, io.in.bits.uop, io.in.bits.uop.ctrl.fuOpType)


  val isDiv = MDUOpType.isDiv(func)

  val mul = Module(new Mul)
  val div = Module(new Div)

  for(x <- Seq(mul.io, div.io)){
    x.scommit <> DontCare
    x.dmem <> DontCare
    x.in.bits := io.in.bits
    x.redirect := io.redirect
  }

  mul.io.in.valid := io.in.valid && !isDiv
  div.io.in.valid := io.in.valid && isDiv

  io.in.ready := Mux(isDiv, div.io.in.ready, mul.io.in.ready)

  val arb = Module(new Arbiter(new ExuOutput, 2))

  arb.io.in(0) <> mul.io.out
  arb.io.in(1) <> div.io.out

  io.out <> arb.io.out

  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d) brTag:%x\n",
    io.in.valid, io.in.ready,
    io.out.valid, io.out.ready,
    io.redirect.valid,
    io.redirect.bits.isException,
    io.redirect.bits.brTag.value
  )
  XSDebug(io.in.valid, "src1:%x src2:%x pc:%x\n", src1, src2, io.in.bits.uop.cf.pc)
  XSDebug(io.out.valid, "Out(%d %d) res:%x pc:%x\n",
    io.out.valid, io.out.ready, io.out.bits.data, io.out.bits.uop.cf.pc
  )

}

class MulDivCtrl extends Bundle{
  val uop = new MicroOp
  val sign = Bool()
  val isW = Bool()
  val isHi = Bool() // return hi bits of result ?
}

class MulDivOutput extends XSBundle {
  val data = UInt(XLEN.W)
  val uop = new MicroOp
}

class MulDivIO(val len: Int) extends XSBundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val src1, src2 = UInt(len.W)
    val ctrl = new MulDivCtrl
  }))
  val out = DecoupledIO(new MulDivOutput)
  val redirect = Flipped(ValidIO(new Redirect))
}

trait HasPipelineReg { this: ArrayMultiplier =>

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = Array.fill(latency)(Wire(Bool())) :+ io.out.ready
  val ctrlVec = io.in.bits.ctrl +: Array.fill(latency)(Reg(new MulDivCtrl))
  val flushVec = ctrlVec.map(_.uop.brTag.needFlush(io.redirect))

  for(i <- 0 until latency){
    rdyVec(i) := !validVec(i+1) || rdyVec(i+1)
  }

  when(io.out.fire()){
    validVec.last := false.B
  }

  for(i <- 1 to latency){
    when(flushVec(i)){
      validVec(i) := false.B
    }

    when(rdyVec(i-1) && validVec(i-1) && !flushVec(i-1)){
      if(i-1 !=0 ) validVec(i-1) := false.B
      validVec(i) := validVec(i-1)
      ctrlVec(i) := ctrlVec(i-1)
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last && !flushVec.last
  io.out.bits.uop := ctrlVec.last.uop

  def PipelineReg[T<:Data](i: Int)(next: T) = RegEnable(next, enable = validVec(i-1) && rdyVec(i-1))

  def S1Reg[T<:Data](next: T):T = PipelineReg[T](1)(next)
  def S2Reg[T<:Data](next: T):T = PipelineReg[T](2)(next)
  def S3Reg[T<:Data](next: T):T = PipelineReg[T](3)(next)
  def S4Reg[T<:Data](next: T):T = PipelineReg[T](4)(next)
  def S5Reg[T<:Data](next: T):T = PipelineReg[T](5)(next)
}


abstract class Multiplier
(
  val len: Int,
  val latency: Int = 3
) extends Module {
  val io = IO(new MulDivIO(len))
}

class ArrayMultiplier
(
  len: Int,
  latency: Int = 3,
  realArray: Boolean = false
) extends Multiplier(len, latency) with HasPipelineReg {

  val mulRes = io.in.bits.src1.asSInt() * io.in.bits.src2.asSInt()

  var dataVec = Seq(mulRes.asUInt())

  for(i <- 1 to latency){
    dataVec = dataVec :+ PipelineReg(i)(dataVec(i-1))
  }

  val xlen = io.out.bits.data.getWidth
  val res = Mux(ctrlVec.last.isHi, dataVec.last.head(xlen), dataVec.last.tail(xlen))
  io.out.bits.data := Mux(ctrlVec.last.isW, SignExt(res(31,0),xlen), res)

//  printf(p"t=${GTimer()} in: v${io.in.valid} r:${io.in.ready}\n")
//  printf(p"t=${GTimer()} out: v:${io.out.valid} r:${io.out.ready} vec:${Binary(Cat(validVec))}\n")
}


class Divider(len: Int) extends Module {
  val io = IO(new MulDivIO(len))

  def abs(a: UInt, sign: Bool): (Bool, UInt) = {
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val s_idle :: s_log2 :: s_shift :: s_compute :: s_finish :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val newReq = (state === s_idle) && io.in.fire()

  val (a, b) = (io.in.bits.src1, io.in.bits.src2)
  val divBy0 = b === 0.U(len.W)

  val shiftReg = Reg(UInt((1 + len * 2).W))
  val hi = shiftReg(len * 2, len)
  val lo = shiftReg(len - 1, 0)

  val (aSign, aVal) = abs(a, io.in.bits.ctrl.sign)
  val (bSign, bVal) = abs(b, io.in.bits.ctrl.sign)
  val aSignReg = RegEnable(aSign, newReq)
  val qSignReg = RegEnable((aSign ^ bSign) && !divBy0, newReq)
  val bReg = RegEnable(bVal, newReq)
  val aValx2Reg = RegEnable(Cat(aVal, "b0".U), newReq)
  val ctrlReg = RegEnable(io.in.bits.ctrl, newReq)

  val cnt = Counter(len)
  when (newReq) {
    state := s_log2
  } .elsewhen (state === s_log2) {
    // `canSkipShift` is calculated as following:
    //   bEffectiveBit = Log2(bVal, XLEN) + 1.U
    //   aLeadingZero = 64.U - aEffectiveBit = 64.U - (Log2(aVal, XLEN) + 1.U)
    //   canSkipShift = aLeadingZero + bEffectiveBit
    //     = 64.U - (Log2(aVal, XLEN) + 1.U) + Log2(bVal, XLEN) + 1.U
    //     = 64.U + Log2(bVal, XLEN) - Log2(aVal, XLEN)
    //     = (64.U | Log2(bVal, XLEN)) - Log2(aVal, XLEN)  // since Log2(bVal, XLEN) < 64.U
    val canSkipShift = (64.U | Log2(bReg)) - Log2(aValx2Reg)
    // When divide by 0, the quotient should be all 1's.
    // Therefore we can not shift in 0s here.
    // We do not skip any shift to avoid this.
    cnt.value := Mux(divBy0, 0.U, Mux(canSkipShift >= (len-1).U, (len-1).U, canSkipShift))
    state := s_shift
  } .elsewhen (state === s_shift) {
    shiftReg := aValx2Reg << cnt.value
    state := s_compute
  } .elsewhen (state === s_compute) {
    val enough = hi.asUInt >= bReg.asUInt
    shiftReg := Cat(Mux(enough, hi - bReg, hi)(len - 1, 0), lo, enough)
    cnt.inc()
    when (cnt.value === (len-1).U) { state := s_finish }
  } .elsewhen (state === s_finish) {
    when(io.out.ready){
      state := s_idle
    }
  }

  when(state=/=s_idle && ctrlReg.uop.brTag.needFlush(io.redirect)){
    state := s_idle
  }

  val r = hi(len, 1)
  val resQ = Mux(qSignReg, -lo, lo)
  val resR = Mux(aSignReg, -r, r)

  val xlen = io.out.bits.data.getWidth
  val res = Mux(ctrlReg.isHi, resR, resQ)
  io.out.bits.data := Mux(ctrlReg.isW, SignExt(res(31,0),xlen), res)
  io.out.bits.uop := ctrlReg.uop

  io.out.valid := state === s_finish
  io.in.ready := state === s_idle


}