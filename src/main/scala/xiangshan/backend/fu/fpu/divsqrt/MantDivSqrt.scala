package xiangshan.backend.fu.fpu.divsqrt

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.fpu.util._
import xiangshan.backend.fu.fpu.util.FPUDebug

class MantDivSqrt(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new Bundle() {
      val a, b = UInt(len.W)
      val isDiv = Bool()
    }))
    val out = DecoupledIO(new Bundle() {
      val quotient = UInt(len.W)
      val isZeroRem = Bool()
    })
  })

  val (a, b) = (io.in.bits.a, io.in.bits.b)
  val isDiv = io.in.bits.isDiv
  val isDivReg = RegEnable(isDiv, io.in.fire())
  val divisor = RegEnable(b, io.in.fire())

  val s_idle :: s_recurrence :: s_recovery :: s_finish :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val cnt_next = Wire(UInt(log2Up((len+1)/2).W))
  val cnt = RegEnable(cnt_next, state===s_idle || state===s_recurrence)
  cnt_next := Mux(state === s_idle, (len/2).U, cnt - 1.U)


  val firstCycle = RegNext(io.in.fire())

  switch(state){
    is(s_idle){
      when(io.in.fire()){ state := s_recurrence }
    }
    is(s_recurrence){
      when(cnt_next === 0.U){ state := s_recovery }
    }
    is(s_recovery){
      state := s_finish
    }
    is(s_finish){
      when(io.out.fire()){ state := s_idle }
    }
  }

  val ws, wc = Reg(UInt((len+4).W))


  val table = Module(new SrtTable)
  val conv = Module(new OnTheFlyConv(len+3))
  val csa = Module(new CSA3_2(len+4))

  // partial square root
  val S = conv.io.Q >> 2
  val s0 :: s1 :: s2 :: s3 :: s4 :: Nil =  S(len-2, len-6).asBools().reverse
  val sqrt_d = Mux(firstCycle, "b101".U(3.W), Mux(s0, "b111".U(3.W), Cat(s2, s3, s4)))
  val div_d = divisor(len-2, len-4)
  val sqrt_y = ws(len+3, len-4) + wc(len+3, len-4)
  val div_y = ws(len+2, len-5) + wc(len+2, len-5)

  table.io.d := Mux(isDivReg, div_d, sqrt_d)
  table.io.y := Mux(isDivReg, div_y, sqrt_y)

  conv.io.resetSqrt := io.in.fire() && !isDiv
  conv.io.resetDiv := io.in.fire() && isDiv
  conv.io.enable := state===s_recurrence
  conv.io.qi := table.io.q

  val dx1, dx2, neg_dx1, neg_dx2 = Wire(UInt((len+4).W))
  dx1 := divisor
  dx2 := divisor << 1
  neg_dx1 := ~dx1
  neg_dx2 := neg_dx1 << 1

  val divCsaIn = MuxLookup(table.io.q.asUInt(), 0.U, Seq(
    -1 -> dx1,
    -2 -> dx2,
    1 -> neg_dx1,
    2 -> neg_dx2
  ).map(m => m._1.S(3.W).asUInt() -> m._2))

  csa.io.in(0) := ws
  csa.io.in(1) := Mux(isDivReg & !table.io.q(2),  wc | table.io.q(1, 0), wc)
  csa.io.in(2) := Mux(isDivReg, divCsaIn, conv.io.F)

  val divWsInit =  a
  val sqrtWsInit = Cat( Cat(0.U(2.W), a) - Cat(1.U(2.W), 0.U(len.W)), 0.U(2.W))

  when(io.in.fire()){
    ws := Mux(isDiv, divWsInit, sqrtWsInit)
    wc := 0.U
  }.elsewhen(state === s_recurrence){
    ws := Mux(cnt_next === 0.U, csa.io.out(0), csa.io.out(0) << 2)
    wc := Mux(cnt_next === 0.U, csa.io.out(1) << 1, csa.io.out(1) << 3)
  }

  val rem = ws + wc

  /** Remainder format:
    * Sqrt:
    * s s x x. x x x ... x
    * Div:
    * s s s x. x x x ... x
    */
  val remSignReg = RegEnable(rem.head(1).asBool(), state===s_recovery)
  val isZeroRemReg = RegEnable(rem===0.U, state===s_recovery)

  io.in.ready := state === s_idle
  io.out.valid := state === s_finish
  io.out.bits.quotient := Mux(remSignReg, conv.io.QM, conv.io.Q) >> !isDivReg
  io.out.bits.isZeroRem := isZeroRemReg

  FPUDebug(){
    when(io.in.fire()){
      printf(p"a:${Hexadecimal(io.in.bits.a)} b:${Hexadecimal(io.in.bits.b)}\n")
    }
    when(io.out.valid) {
      printf(p"Q:${Binary(conv.io.Q)} QM:${Binary(conv.io.QM)} isNegRem:${rem.head(1)}\n" +
        p"rem:${Hexadecimal(rem)}\n")
    }
  }
}

