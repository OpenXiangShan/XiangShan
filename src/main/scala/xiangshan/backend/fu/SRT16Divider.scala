/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

// This file contains components originally written by Yifei He, see
// https://github.com/OpenXiangShan/XS-Verilog-Library/tree/main/int_div_radix_4_v1
// Email of original author: hyf_sysu@qq.com

package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.fu.util.CSA3_2

class SRT16DividerDataModule(len: Int) extends Module {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(len.W)))
    val valid, sign, kill_w, kill_r, isHi, isW = Input(Bool())
    val in_ready = Output(Bool())
    val out_valid = Output(Bool())
    val out_validNext = Output(Bool())
    val out_data = Output(UInt(len.W))
    val out_ready = Input(Bool())
  })

  // consts
  val lzc_width = log2Up(len)
  val itn_len = 1 + len + 2 + 1

  val (a, d, sign, valid, kill_w, kill_r, isHi, isW) =
    (io.src(0), io.src(1), io.sign, io.valid, io.kill_w, io.kill_r, io.isHi, io.isW)
  val in_fire = valid && io.in_ready
  val out_fire = io.out_ready && io.out_valid
  val newReq = in_fire
  val s_idle :: s_pre_0 :: s_pre_1 :: s_iter :: s_post_0 :: s_post_1 :: s_finish :: Nil = Enum(7)
  val quot_neg_2 :: quot_neg_1 :: quot_0 :: quot_pos_1 :: quot_pos_2 :: Nil = Enum(5)


  val state = RegInit((1 << s_idle.litValue.toInt).U(7.W))

  // reused wires
//  val aNormAbs = Wire(UInt((len + 1).W)) // Inputs of xNormAbs regs below
//  val dNormAbs = Wire(UInt((len + 1).W))
  val quotIter = Wire(UInt(len.W))
  val quotM1Iter = Wire(UInt(len.W))
  val aLZC = Wire(UInt((lzc_width + 1).W))
  val dLZC = Wire(UInt((lzc_width + 1).W))

  val rNext = Wire(UInt(itn_len.W))
  val rNextPd = Wire(UInt(itn_len.W))

  val aInverter = Wire(UInt(len.W)) // results of global inverter
  val dInverter = Wire(UInt(len.W))

  val finalIter = Wire(Bool())
  val special = Wire(Bool())

  // reused regs
//  val aNormAbsReg = RegEnable(aNormAbs, newReq | state(s_pre_0) | state(s_post_0)) // reg for normalized a & d and rem & rem+d
//  val dNormAbsReg = RegEnable(dNormAbs, newReq | state(s_pre_0) | state(s_post_0))
  val quotIterReg = RegEnable(quotIter, state(s_pre_1) | state(s_iter) | state(s_post_0))
  val quotM1IterReg = RegEnable(quotM1Iter, state(s_pre_1) | state(s_iter) | state(s_post_0))
  val specialReg = RegEnable(special, state(s_pre_1))
  val aReg = RegEnable(a, in_fire)

  when(kill_r) {
    state := UIntToOH(s_idle, 7)
  } .elsewhen(state(s_idle) && in_fire && !kill_w) {
    state := UIntToOH(s_pre_0, 7)
  } .elsewhen(state(s_pre_0)) { // leading zero detection
    state := UIntToOH(s_pre_1, 7)
  } .elsewhen(state(s_pre_1)) { // shift a/b
    state := Mux(special, UIntToOH(s_post_1, 7), UIntToOH(s_iter, 7))
  } .elsewhen(state(s_iter)) { // (ws[j+1], wc[j+1]) = 4(ws[j],wc[j]) - q(j+1)*d
    state := Mux(finalIter, UIntToOH(s_post_0, 7), UIntToOH(s_iter, 7))
  } .elsewhen(state(s_post_0)) { // if rem < 0, rem = rem + d
    state := UIntToOH(s_post_1, 7)
  } .elsewhen(state(s_post_1)) {
    state := UIntToOH(s_finish, 7)
  } .elsewhen(state(s_finish) && io.out_ready) {
    state := UIntToOH(s_idle, 7)
  } .otherwise {
    state := state
  }

  io.in_ready := state(s_idle)
  aInverter := -Mux(state(s_idle), a, quotIterReg) // 64, 0
  dInverter := -Mux(state(s_idle), d, quotM1IterReg) // 64, 0

  val aSign = io.sign && a(len - 1) // 1
  val dSign = io.sign && d(len - 1)
  val dSignReg = RegEnable(dSign, newReq)

  val aAbs = Mux(aSign, aInverter, a) // 64, 0
  val dAbs = Mux(dSign, dInverter, d)
  val aAbsReg = RegEnable(aAbs, newReq)
  val dAbsReg = RegEnable(dAbs, newReq)

  val aNorm = (aAbsReg(len - 1, 0) << aLZC(lzc_width - 1, 0))(len - 1, 0) // 64, 65
  val dNorm = (dAbsReg(len - 1, 0) << dLZC(lzc_width - 1, 0))(len - 1, 0)

  val aNormReg = RegEnable(aNorm, state(s_pre_0))
  val dNormReg = RegEnable(dNorm, state(s_pre_0))

//  aNormAbs := Mux1H(Seq(
//    state(s_idle) -> Cat(0.U(1.W), aAbs), // 65, 0
//    state(s_pre_0) -> Cat(0.U(1.W), aNorm), // 65, 0
//    state(s_post_0) -> rNext(len + 3, 3) // remainder 65, 64. highest is sign bit
//  ))
//  dNormAbs := Mux1H(Seq(
//    state(s_idle) -> Cat(0.U(1.W), dAbs),
//    state(s_pre_0) -> Cat(0.U(1.W), dNorm),
//    state(s_post_0) -> rNextPd(len + 3, 3)
//    ))

  // Second cycle, state is pre_0
  // calculate lzc and move div* and lzc diff check if no_iter_needed

  aLZC := PriorityEncoder(aAbsReg(len - 1, 0).asBools.reverse)
  dLZC := PriorityEncoder(dAbsReg(len - 1, 0).asBools.reverse)
  val aLZCReg = RegEnable(aLZC, state(s_pre_0)) // 7, 0
  val dLZCReg = RegEnable(dLZC, state(s_pre_0))

  val lzcWireDiff = Cat(0.U(1.W), dLZC(lzc_width - 1, 0)) - Cat(0.U(1.W), aLZC(lzc_width - 1, 0)) // 7, 0
  val lzcRegDiff = Cat(0.U(1.W), dLZCReg(lzc_width - 1, 0)) - Cat(0.U(1.W), aLZCReg(lzc_width - 1, 0))
//  val lzcDiff = Mux(state(s_pre_0), lzcWireDiff, lzcRegDiff)

  // special case:
  // divisor is 1 or -1; dividend has less bits than divisor; divisor is zero
  // s_pre_0:
  val dIsOne = dLZC(lzc_width - 1, 0).andR
  val dIsZero = ~dNormReg.orR
  val aIsZero = RegEnable(aLZC(lzc_width), state(s_pre_0))
  val aTooSmall = RegEnable(aLZC(lzc_width) | lzcWireDiff(lzc_width), state(s_pre_0))
  special := dIsOne | dIsZero | aTooSmall

  val quotSpecial = Mux(dIsZero, VecInit(Seq.fill(len)(true.B)).asUInt,
                            Mux(aTooSmall, 0.U,
                              Mux(dSignReg, -aReg, aReg) //  signed 2^(len-1)
                            ))
  val remSpecial = Mux(dIsZero || aTooSmall, aReg, 0.U)
  val quotSpecialReg = RegEnable(quotSpecial, state(s_pre_1))
  val remSpecialReg = RegEnable(remSpecial, state(s_pre_1))

  // s_pre_1
  val quotSign = Mux(state(s_idle), aSign ^ dSign, true.B) // if not s_idle then must be s_pre_1 & dIsZero, and that we have
  val rSign = aSign
  val quotSignReg = RegEnable(quotSign, in_fire | (state(s_pre_1) & dIsZero))
  val rSignReg = RegEnable(rSign, in_fire)

  val rShift = lzcRegDiff(0)
  val oddIter = lzcRegDiff(1) ^ lzcRegDiff(0)
  val iterNum = Wire(UInt((lzc_width - 2).W))
  val iterNumReg = RegEnable(iterNum, state(s_pre_1) | state(s_iter))
  iterNum := Mux(state(s_pre_1), (lzcRegDiff + 1.U) >> 2, iterNumReg -% 1.U)
  finalIter := iterNumReg === 0.U

  val rSumInit = Cat(0.U(3.W), Mux(rShift, Cat(0.U(1.W), aNormReg), Cat(aNormReg, 0.U(1.W)))) //(1, 67), 0.001xxx
  val rCarryInit = 0.U(itn_len.W)

  val rSumInitTrunc = Cat(0.U(1.W), rSumInit(itn_len - 4, itn_len - 4 - 4 + 1)) // 0.00___
  val mInitPos1 = MuxLookup(dNormReg(len-2, len-4), "b00100".U(5.W),
    Array(
      0.U -> "b00100".U(5.W),
      1.U -> "b00100".U(5.W),
      2.U -> "b00100".U(5.W),
      3.U -> "b00110".U(5.W),
      4.U -> "b00110".U(5.W),
      5.U -> "b00110".U(5.W),
      6.U -> "b00110".U(5.W),
      7.U -> "b01000".U(5.W),
    )
  )
  val mInitPos2 = MuxLookup(dNormReg(len-2, len-4), "b01100".U(5.W),
    Array(
      0.U -> "b01100".U(5.W),
      1.U -> "b01110".U(5.W),
      2.U -> "b01111".U(5.W),
      3.U -> "b10000".U(5.W),
      4.U -> "b10010".U(5.W),
      5.U -> "b10100".U(5.W),
      6.U -> "b10110".U(5.W),
      7.U -> "b10110".U(5.W),
    )
  )
  val initCmpPos1 = rSumInitTrunc >= mInitPos1
  val initCmpPos2 = rSumInitTrunc >= mInitPos2
  val qInit = Mux(initCmpPos2, UIntToOH(quot_pos_2, 5), Mux(initCmpPos1, UIntToOH(quot_pos_1, 5), UIntToOH(quot_0, 5)))

  // in pre_1 we also obtain m_i + 16u * d for all u
  // udNeg -> (rud, r2ud) -> (rudPmNeg, r2udPmNeg)
  val dPos = Cat(0.U(1.W), dNormReg)                          // +d, 0.1xxx, (1, 64)
  val dNeg = -Cat(0.U(1.W), dNormReg) // -d, 1.xxxx, (1, 64)
  // val m = Wire(Vec(4, UInt(7.W)))     // we have to sigext them to calculate rqd-m_k

  // index 0 is for q=-2 and 4 is for q=2!!!
  val mNeg = Wire(Vec(4, UInt(12.W))) // selected m, extended to (6, 6) bits
  val rudNeg = Wire(Vec(5, UInt(10.W))) // (4, 6)
  val r2udNeg = Wire(Vec(5, UInt(12.W))) // (6, 6)

  // Selection Block with improved timing
  val rudPmNeg = Wire(Vec(5, Vec(4, UInt(10.W)))) // -(r*u*d+m_k), (5, 5) bits
  val r2ws = Wire(UInt(10.W)) // r^2*ws (5, 5) bits
  val r2wc = Wire(UInt(10.W))
  // calculating exact values of w
  val udNeg = Wire(Vec(5, UInt(itn_len.W))) // (3, 65), 1 signExt'ed Bit
  // val r3udNeg = Wire(Vec(5, UInt(13.W)))

  // Speculative Block
  val r2udPmNeg = Wire(Vec(5, Vec(4, UInt(13.W)))) // -(r^2*d*d+m_k), (7, 6) bits. 1st index for q 2nd for m
  val r3ws = Wire(UInt(13.W)) // r^3*ws, (7, 6) bits
  val r3wc = Wire(UInt(13.W))
  val qSpec = Wire(Vec(5, UInt(5.W))) // 5 speculative results of qNext2
  // output wires
  val qNext = Wire(UInt(5.W))
  val qNext2 = Wire(UInt(5.W))
  val rCarryIter = Wire(UInt(itn_len.W)) // (1, 67)
  val rSumIter = Wire(UInt(itn_len.W))
  // val r3wsIter = Wire(UInt(13.W))
  // val r3wcIter = Wire(UInt(13.W))
  // Input Regs of whole Spec + Sel + sum adder block
  val qPrevReg = RegEnable(Mux(state(s_pre_1), qInit, qNext2), state(s_pre_1) | state(s_iter))
  val rSumReg = RegEnable(Mux(state(s_pre_1), rSumInit, rSumIter), state(s_pre_1) | state(s_iter)) // (1, 67)
  val rCarryReg = RegEnable(Mux(state(s_pre_1), rCarryInit, rCarryIter), state(s_pre_1) | state(s_iter))

  // Give values to the regs and wires above...
  val dForLookup = dPos(len-2, len-4)
  mNeg := VecInit(Cat(SignExt(MuxLookup(dNormReg(len-2, len-4), "b00000000".U(7.W), mLookUpTable2.minus_m(0)), 11), 0.U(1.W)), // (2, 5) -> (6, 6)
                  Cat(SignExt(MuxLookup(dNormReg(len-2, len-4), "b00000000".U(7.W), mLookUpTable2.minus_m(1)), 10) ,0.U(2.W)), // (3, 4) -> (6, 6)
                  Cat(SignExt(MuxLookup(dNormReg(len-2, len-4), "b00000000".U(7.W), mLookUpTable2.minus_m(2)), 10) ,0.U(2.W)),
                  Cat(SignExt(MuxLookup(dNormReg(len-2, len-4), "b00000000".U(7.W), mLookUpTable2.minus_m(3)), 11) ,0.U(1.W))
  )
  udNeg := VecInit( Cat(SignExt(dPos, 66), 0.U(2.W)),
                    Cat(SignExt(dPos, 67), 0.U(1.W)),
                    0.U,
                    Cat(SignExt(dNeg, 67), 0.U(1.W)),
                    Cat(SignExt(dNeg, 66), 0.U(2.W))
  )

  rudNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-11)})
  r2udNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-13)})
  // r3udNeg := VecInit(Seq.tabulate(5){i => udNeg(i)(itn_len-2, itn_len-13)})
  rudPmNeg := VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(rudNeg(i)(9, 1), 10) + mNeg(j)(10, 1)})})
  r2udPmNeg := VecInit(Seq.tabulate(5){i => VecInit(Seq.tabulate(4){ j => SignExt(r2udNeg(i), 13) + SignExt(mNeg(j), 13)})})
  r3ws := rSumReg(itn_len-1, itn_len-13)
  r3wc := rCarryReg(itn_len-1, itn_len-13)

  r2ws := rSumReg(itn_len-1, itn_len-10)
  r2wc := rCarryReg(itn_len-1, itn_len-10)

  val udNegReg = RegEnable(udNeg, state(s_pre_1))
//  val rudNegReg = RegEnable(rudNeg, state(s_pre_1))
  val rudPmNegReg = RegEnable(rudPmNeg, state(s_pre_1))
  val r2udPmNegReg = RegEnable(r2udPmNeg, state(s_pre_1))

  def DetectSign(signs: UInt, name: String): UInt = {
    val qVec = Wire(Vec(5, Bool())).suggestName(name)
    qVec(quot_neg_2) := signs(0) && signs(1) && signs(2)
    qVec(quot_neg_1) := ~signs(0) && signs(1) && signs(2)
    qVec(quot_0) := signs(2) && ~signs(1)
    qVec(quot_pos_1) := signs(3) && ~signs(2) && ~signs(1)
    qVec(quot_pos_2) := ~signs(3) && ~signs(2) && ~signs(1)
    qVec.asUInt
  }
  // Selection block
  val signs = VecInit(Seq.tabulate(4){ i => {
    val csa = Module(new CSA3_2(10)).suggestName(s"csa_sel_${i}")
    csa.io.in(0) := r2ws
    csa.io.in(1) := r2wc
    csa.io.in(2) := Mux1H(qPrevReg, rudPmNegReg.toSeq)(i) // rudPmNeg(OHToUInt(qPrevReg))(i)

      (csa.io.out(0) + (csa.io.out(1)(8, 0) << 1))(9)
    }})
  qNext := DetectSign(signs.asUInt, s"sel_q")
  val csaWide1 = Module(new CSA3_2(itn_len)).suggestName("csa_sel_wide_1")
  val csaWide2 = Module(new CSA3_2(itn_len)).suggestName("csa_sel_wide_2")
  csaWide1.io.in(0) := rSumReg << 2
  csaWide1.io.in(1) := rCarryReg << 2
  csaWide1.io.in(2) := Mux1H(qPrevReg, udNegReg.toSeq) << 2//udNeg(OHToUInt(qPrevReg)) << 2
  csaWide2.io.in(0) := csaWide1.io.out(0) << 2
  csaWide2.io.in(1) := (csaWide1.io.out(1) << 1)(itn_len-1, 0) << 2
  csaWide2.io.in(2) := Mux1H(qNext, udNegReg.toSeq) << 2 // udNeg(OHToUInt(qNext)) << 2
  rSumIter := Mux(~oddIter & finalIter, csaWide1.io.out(0), csaWide2.io.out(0))
  rCarryIter := Mux(~oddIter & finalIter, (csaWide1.io.out(1) << 1)(itn_len-1, 0), (csaWide2.io.out(1) << 1)(itn_len-1, 0))
  // r3wsIter := r3udNeg(OHToUInt(qNext))
  // r3wcIter := (csaWide1.io.out(0)(itn_len-3, itn_len-16) + (csaWide1.io.out(1) << 1)(itn_len-3, itn_len-16))(13,1)
  // Speculative block
  qSpec := VecInit(Seq.tabulate(5){ q_spec => {
      val csa1 = Module(new CSA3_2(13)).suggestName(s"csa_spec_${q_spec}")
      csa1.io.in(0) := r3ws
      csa1.io.in(1) := r3wc
      csa1.io.in(2) := SignExt(udNegReg(q_spec)(itn_len-2, itn_len-11), 13) // (4, 6) -> (7, 6)
      val signs2 = VecInit(Seq.tabulate(4){ i => {
        val csa2 = Module(new CSA3_2(13)).suggestName(s"csa_spec_${q_spec}_${i}")
        csa2.io.in(0) := csa1.io.out(0)
        csa2.io.in(1) := (csa1.io.out(1) << 1)(12, 0)
        csa2.io.in(2) := Mux1H(qPrevReg, r2udPmNegReg.toSeq)(i) // r2udPmNeg(OHToUInt(qPrevReg))(i)
        (csa2.io.out(0) + (csa2.io.out(1)(11, 0) << 1))(12)
      }})
      val qVec2 = DetectSign(signs2.asUInt, s"spec_q_${q_spec}")
      qVec2
  }})
  // qNext2 := qSpec(OHToUInt(qNext)) // TODO: Use Mux1H!!

  qNext2 := Mux1H(qNext, qSpec.toSeq)

  // on the fly quotient conversion
  val quotHalfIter = Wire(UInt(64.W))
  val quotM1HalfIter = Wire(UInt(64.W))
  val quotIterNext = Wire(UInt(64.W))
  val quotM1IterNext = Wire(UInt(64.W))
  def OTFC(q: UInt, quot: UInt, quotM1: UInt): (UInt, UInt) = {
    val quotNext = Mux1H(Seq(
    q(quot_pos_2) -> (quot << 2 | "b10".U),
    q(quot_pos_1) -> (quot << 2 | "b01".U),
    q(quot_0)     -> (quot << 2 | "b00".U),
    q(quot_neg_1) -> (quotM1 << 2 | "b11".U),
    q(quot_neg_2) -> (quotM1 << 2 | "b10".U)
    ))
    val quotM1Next = Mux1H(Seq(
    q(quot_pos_2) -> (quot << 2 | "b01".U),
    q(quot_pos_1) -> (quot << 2 | "b00".U),
    q(quot_0)     -> (quotM1 << 2 | "b11".U),
    q(quot_neg_1) -> (quotM1 << 2 | "b10".U),
    q(quot_neg_2) -> (quotM1 << 2 | "b01".U)
    ))
    (quotNext(len-1, 0), quotM1Next(len-1, 0))
  }
  quotHalfIter := OTFC(qPrevReg, quotIterReg, quotM1IterReg)._1
  quotM1HalfIter := OTFC(qPrevReg, quotIterReg, quotM1IterReg)._2
  quotIterNext := Mux(~oddIter && finalIter, quotHalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter)._1)
  quotM1IterNext := Mux(~oddIter && finalIter, quotM1HalfIter, OTFC(qNext, quotHalfIter, quotM1HalfIter)._2)
  // quotIter := Mux(state(s_pre_1),  0.U(len.W),
  //                     Mux(state(s_iter), quotIterNext,
  //                       Mux(quotSignReg, aInverter, quotIterReg)))
  // quotM1Iter := Mux(state(s_pre_1),
  //                       0.U(len.W), Mux(state(s_iter), quotM1IterNext,
  //                         Mux(quotSignReg, dInverter, quotM1IterReg)))

  quotIter := Mux(state(s_iter), quotIterNext,
                    Mux(state(s_pre_1), 0.U(len.W),
                      Mux(quotSignReg, aInverter, quotIterReg)))
  quotM1Iter := Mux(state(s_iter), quotM1IterNext,
                      Mux(state(s_pre_1), 0.U(len.W),
                        Mux(quotSignReg, dInverter, quotM1IterReg)))
  // finally, to the recovery stages!

  when(rSignReg) {
    rNext := ~rSumReg + ~rCarryReg + 2.U
    rNextPd := ~rSumReg + ~rCarryReg + ~Cat(0.U(1.W), dNormReg, 0.U(3.W)) + 3.U
  } .otherwise {
    rNext := rSumReg + rCarryReg
    rNextPd := rSumReg + rCarryReg + Cat(0.U(1.W), dNormReg, 0.U(3.W))
  }
  val rNextReg = RegEnable(rNext(len + 3, 3), state(s_post_0))
  val rNextPdReg = RegEnable(rNextPd(len + 3, 3), state(s_post_0))
  dontTouch(rNextReg)
  // post_1
  val r = rNextReg
  val rPd = rNextPdReg
  val rIsZero = ~(r.orR)
  val needCorr = Mux(rSignReg, ~r(len) & r.orR, r(len)) // when we get pos rem for a<0 or neg rem for a>0
  val rPreShifted = Mux(needCorr, rPd, r)
  val rightShifter = Module(new RightShifter(len, lzc_width))
  rightShifter.io.in := rPreShifted
  rightShifter.io.shiftNum := dLZCReg
  rightShifter.io.msb := Mux(~(rPreShifted.orR), 0.U, rSignReg)
  val rShifted = rightShifter.io.out
  val rFinal = RegEnable(Mux(specialReg, remSpecialReg, rShifted), state(s_post_1))// right shifted remainder. shift by the number of bits divisor is shifted
  val qFinal = RegEnable(Mux(specialReg, quotSpecialReg, Mux(needCorr, quotM1IterReg, quotIterReg)), state(s_post_1))
  val res = Mux(isHi, rFinal, qFinal)
  io.out_data := Mux(isW,
    SignExt(res(31, 0), len),
    res
  )
  io.in_ready := state(s_idle)
  io.out_valid := state(s_finish)
  io.out_validNext := state(s_post_1)
}

object mLookUpTable2 {
  // Usage :
  // result := decoder(QMCMinimizer, index, mLookupTable.xxx)
  val minus_m = Seq(
    Array( // -m[-1]
      0.U -> "b00_11010".U(7.W),
      1.U -> "b00_11110".U(7.W),
      2.U -> "b01_00000".U(7.W),
      3.U -> "b01_00100".U(7.W),
      4.U -> "b01_00110".U(7.W),
      5.U -> "b01_01010".U(7.W),
      6.U -> "b01_01100".U(7.W),
      7.U -> "b01_10000".U(7.W)
    ),
    Array( // -m[0]
      0.U -> "b000_0100".U(7.W),
      1.U -> "b000_0110".U(7.W),
      2.U -> "b000_0110".U(7.W),
      3.U -> "b000_0110".U(7.W),
      4.U -> "b000_1000".U(7.W),
      5.U -> "b000_1000".U(7.W),
      6.U -> "b000_1000".U(7.W),
      7.U -> "b000_1000".U(7.W)
    ),
    Array( //-m[1]
      0.U -> "b111_1101".U(7.W),
      1.U -> "b111_1100".U(7.W),
      2.U -> "b111_1100".U(7.W),
      3.U -> "b111_1100".U(7.W),
      4.U -> "b111_1011".U(7.W),
      5.U -> "b111_1010".U(7.W),
      6.U -> "b111_1010".U(7.W),
      7.U -> "b111_1010".U(7.W)
    ),
    Array( //-m[2]
      0.U -> "b11_01000".U(7.W),
      1.U -> "b11_00100".U(7.W),
      2.U -> "b11_00010".U(7.W),
      3.U -> "b10_11110".U(7.W),
      4.U -> "b10_11100".U(7.W),
      5.U -> "b10_11000".U(7.W),
      6.U -> "b10_10110".U(7.W),
      7.U -> "b10_10010".U(7.W)
    ))
}
