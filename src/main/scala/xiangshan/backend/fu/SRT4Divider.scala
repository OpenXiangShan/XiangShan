package xiangshan.backend.fu

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import utils.SignExt
import xiangshan.XSModule
import xiangshan.backend.fu.util.CSA3_2

/** A Radix-4 SRT Integer Divider
  *
  * 2 ~ (5 + (len+3)/2) cycles are needed for each division.
  */
class SRT4DividerDataModule(len: Int) extends Module {
  val io = IO(new Bundle() {
    val src1, src2 = Input(UInt(len.W))
    val valid, sign, kill_w, kill_r, isHi, isW = Input(Bool())
    val in_ready = Output(Bool())
    val out_valid = Output(Bool())
    val out_data = Output(UInt(len.W))
    val out_ready = Input(Bool())
  })

  val (a, b, sign, valid, kill_w, kill_r, isHi, isW) =
    (io.src1, io.src2, io.sign, io.valid, io.kill_w, io.kill_r, io.isHi, io.isW)
  val in_fire = valid && io.in_ready
  val out_fire = io.out_ready && io.out_valid

  // s_pad_* is not used
  val s_idle :: s_lzd :: s_normlize :: s_recurrence :: s_recovery_1 :: s_recovery_2 :: s_pad_1 :: s_pad_2 :: s_finish :: Nil = Enum(9)
  require(s_finish.litValue() == 8)

  val state = RegInit(s_idle)
  val finished = state(3).asBool // state === s_finish

  val cnt_next = Wire(UInt(log2Up((len + 3) / 2).W))
  val cnt = RegEnable(cnt_next, state === s_normlize || state === s_recurrence)
  val rec_enough = cnt_next === 0.U
  val newReq = in_fire

  def abs(a: UInt, sign: Bool): (Bool, UInt) = {
    val s = a(len - 1) && sign
    (s, Mux(s, -a, a))
  }

  val (aSign, aVal) = abs(a, sign)
  val (bSign, bVal) = abs(b, sign)
  val aSignReg = RegEnable(aSign, newReq)
  val qSignReg = RegEnable(aSign ^ bSign, newReq)
  val divZero = b === 0.U
  val divZeroReg = RegEnable(divZero, newReq)

  switch(state) {
    is(s_idle) {
      when(in_fire && !kill_w) {
        state := Mux(divZero, s_finish, s_lzd)
      }
    }
    is(s_lzd) { // leading zero detection
      state := s_normlize
    }
    is(s_normlize) { // shift a/b
      state := s_recurrence
    }
    is(s_recurrence) { // (ws[j+1], wc[j+1]) = 4(ws[j],wc[j]) - q(j+1)*d
      when(rec_enough) {
        state := s_recovery_1
      }
    }
    is(s_recovery_1) { // if rem < 0, rem = rem + d
      state := s_recovery_2
    }
    is(s_recovery_2) { // recovery shift
      state := s_finish
    }
    is(s_finish) {
      when(out_fire) {
        state := s_idle
      }
    }
  }
  when(kill_r) {
    state := s_idle
  }

  /** Calculate abs(a)/abs(b) by recurrence
    *
    * ws, wc: partial remainder in carry-save form,
    * in recurrence steps, ws/wc = 4ws[j]/4wc[j];
    * in recovery step, ws/wc = ws[j]/wc[j];
    * in final step, ws = abs(a)/abs(b).
    *
    * d: normlized divisor(1/2<=d<1)
    *
    * wLen = 3 integer bits + (len+1) frac bits
    */
  def wLen = 3 + len + 1

  val ws, wc = Reg(UInt(wLen.W))
  val ws_next, wc_next = Wire(UInt(wLen.W))
  val d = Reg(UInt(wLen.W))

  val aLeadingZeros = RegEnable(
    next = PriorityEncoder(ws(len - 1, 0).asBools().reverse),
    enable = state === s_lzd
  )
  val bLeadingZeros = RegEnable(
    next = PriorityEncoder(d(len - 1, 0).asBools().reverse),
    enable = state === s_lzd
  )
  val diff = Cat(0.U(1.W), bLeadingZeros).asSInt() - Cat(0.U(1.W), aLeadingZeros).asSInt()
  val isNegDiff = diff(diff.getWidth - 1)
  val quotientBits = Mux(isNegDiff, 0.U, diff.asUInt())
  val qBitsIsOdd = quotientBits(0)
  val recoveryShift = RegEnable(len.U - bLeadingZeros, state === s_normlize)
  val a_shifted, b_shifted = Wire(UInt(len.W))
  a_shifted := Mux(isNegDiff,
    ws(len - 1, 0) << bLeadingZeros,
    ws(len - 1, 0) << aLeadingZeros
  )
  b_shifted := d(len - 1, 0) << bLeadingZeros

  val rem_temp = ws + wc
  val rem_fixed = Mux(rem_temp(wLen - 1), rem_temp + d, rem_temp)
  val rem_abs = (ws << recoveryShift) (2 * len, len + 1)

  when(newReq) {
    ws := Cat(0.U(4.W), Mux(divZero, a, aVal))
    wc := 0.U
    d := Cat(0.U(4.W), bVal)
  }.elsewhen(state === s_normlize) {
    d := Cat(0.U(3.W), b_shifted, 0.U(1.W))
    ws := Mux(qBitsIsOdd, a_shifted, a_shifted << 1)
  }.elsewhen(state === s_recurrence) {
    ws := Mux(rec_enough, ws_next, ws_next << 2)
    wc := Mux(rec_enough, wc_next, wc_next << 2)
  }.elsewhen(state === s_recovery_1) {
    ws := rem_fixed
  }.elsewhen(state === s_recovery_2) {
    ws := rem_abs
  }

  cnt_next := Mux(state === s_normlize, (quotientBits + 3.U) >> 1, cnt - 1.U)

  /** Quotient selection
    *
    * the quotient selection table use truncated 7-bit remainder
    * and 3-bit divisor
    */
  val sel_0 :: sel_d :: sel_dx2 :: sel_neg_d :: sel_neg_dx2 :: Nil = Enum(5)
  val dx2, neg_d, neg_dx2 = Wire(UInt(wLen.W))
  dx2 := d << 1
  neg_d := (~d).asUInt() // add '1' in carry-save adder later
  neg_dx2 := neg_d << 1

  val q_sel = Wire(UInt(3.W))
  val wc_adj = MuxLookup(q_sel, 0.U(2.W), Seq(
    sel_d -> 1.U(2.W),
    sel_dx2 -> 2.U(2.W)
  ))

  val w_truncated = (ws(wLen - 1, wLen - 1 - 6) + wc(wLen - 1, wLen - 1 - 6)).asSInt()
  val d_truncated = d(len - 1, len - 3)

  val qSelTable = Array(
    Array(12, 4, -4, -13),
    Array(14, 4, -6, -15),
    Array(15, 4, -6, -16),
    Array(16, 4, -6, -18),
    Array(18, 6, -8, -20),
    Array(20, 6, -8, -20),
    Array(20, 8, -8, -22),
    Array(24, 8, -8, -24)
  )

  // ge(x): w_truncated >= x
  var ge = Map[Int, Bool]()
  for (row <- qSelTable) {
    for (k <- row) {
      if (!ge.contains(k)) ge = ge + (k -> (w_truncated >= k.S(7.W)))
    }
  }
  q_sel := MuxLookup(d_truncated, sel_0,
    qSelTable.map(x =>
      MuxCase(sel_neg_dx2, Seq(
        ge(x(0)) -> sel_dx2,
        ge(x(1)) -> sel_d,
        ge(x(2)) -> sel_0,
        ge(x(3)) -> sel_neg_d
      ))
    ).zipWithIndex.map({ case (v, i) => i.U -> v })
  )

  /** Calculate (ws[j+1],wc[j+1]) by a [3-2]carry-save adder
    *
    * (ws[j+1], wc[j+1]) = 4(ws[j],wc[j]) - q(j+1)*d
    */
  val csa = Module(new CSA3_2(wLen))
  csa.io.in(0) := ws
  csa.io.in(1) := Cat(wc(wLen - 1, 2), wc_adj)
  csa.io.in(2) := MuxLookup(q_sel, 0.U, Seq(
    sel_d -> neg_d,
    sel_dx2 -> neg_dx2,
    sel_neg_d -> d,
    sel_neg_dx2 -> dx2
  ))
  ws_next := csa.io.out(0)
  wc_next := csa.io.out(1) << 1

  // On the fly quotient conversion
  val q, qm = Reg(UInt(len.W))
  when(newReq) {
    q := 0.U
    qm := 0.U
  }.elsewhen(state === s_recurrence) {
    val qMap = Seq(
      sel_0 -> (q, 0),
      sel_d -> (q, 1),
      sel_dx2 -> (q, 2),
      sel_neg_d -> (qm, 3),
      sel_neg_dx2 -> (qm, 2)
    )
    q := MuxLookup(q_sel, 0.U,
      qMap.map(m => m._1 -> Cat(m._2._1(len - 3, 0), m._2._2.U(2.W)))
    )
    val qmMap = Seq(
      sel_0 -> (qm, 3),
      sel_d -> (q, 0),
      sel_dx2 -> (q, 1),
      sel_neg_d -> (qm, 2),
      sel_neg_dx2 -> (qm, 1)
    )
    qm := MuxLookup(q_sel, 0.U,
      qmMap.map(m => m._1 -> Cat(m._2._1(len - 3, 0), m._2._2.U(2.W)))
    )
  }.elsewhen(state === s_recovery_1) {
    q := Mux(rem_temp(wLen - 1), qm, q)
  }


  val remainder = Mux(aSignReg, -ws(len - 1, 0), ws(len - 1, 0))
  val quotient = Mux(qSignReg, -q, q)

  val res = Mux(isHi,
    Mux(divZeroReg, ws(len - 1, 0), remainder),
    Mux(divZeroReg, Fill(len, 1.U(1.W)), quotient)
  )
  io.out_data := Mux(isW,
    SignExt(res(31, 0), len),
    res
  )
  io.in_ready := state === s_idle
  io.out_valid := finished // state === s_finish
}

class SRT4Divider(len: Int) extends AbstractDivider(len) {

  val newReq = io.in.fire()

  val uop = io.in.bits.uop
  val uopReg = RegEnable(uop, newReq)
  val ctrlReg = RegEnable(ctrl, newReq)

  val divDataModule = Module(new SRT4DividerDataModule(len))

  val kill_w = uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  val kill_r = !divDataModule.io.in_ready && uopReg.roqIdx.needFlush(io.redirectIn, io.flushIn)

  divDataModule.io.src1 := io.in.bits.src(0)
  divDataModule.io.src2 := io.in.bits.src(1)
  divDataModule.io.valid := io.in.valid
  divDataModule.io.sign := sign
  divDataModule.io.kill_w := kill_w
  divDataModule.io.kill_r := kill_r
  divDataModule.io.isHi := ctrlReg.isHi
  divDataModule.io.isW := ctrlReg.isW
  divDataModule.io.out_ready := io.out.ready

  io.in.ready := divDataModule.io.in_ready
  io.out.valid := divDataModule.io.out_valid
  io.out.bits.data := divDataModule.io.out_data
  io.out.bits.uop := uopReg
}
