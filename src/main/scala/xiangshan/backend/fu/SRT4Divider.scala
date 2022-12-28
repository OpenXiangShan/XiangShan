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

// The "SRT4DividerDataModule" in this file is a scala rewrite of SRT4 divider by Yifei He, see
// https://github.com/OpenXiangShan/XS-Verilog-Library/tree/main/int_div_radix_4_v1
// Email of original author: hyf_sysu@qq.com

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.SignExt
import xiangshan.backend.fu.util.CSA3_2

/** A Radix-4 SRT Integer Divider
  *
  * 2 ~ (5 + (len+3)/2) cycles are needed for each division.
  */
class SRT4DividerDataModule(len: Int) extends Module {
  val io = IO(new Bundle() {
    val src = Vec(2, Input(UInt(len.W)))
    val valid, sign, kill_w, kill_r, isHi, isW = Input(Bool())
    val in_ready = Output(Bool())
    val out_valid = Output(Bool())
    val out_data = Output(UInt(len.W))
    val out_ready = Input(Bool())
  })

  // consts
  val lzc_width = log2Up(len)
  val itn_len = 1 + len + 2 + 1
  require(lzc_width == 6)

  val (a, d, sign, valid, kill_w, kill_r, isHi, isW) =
    (io.src(0), io.src(1), io.sign, io.valid, io.kill_w, io.kill_r, io.isHi, io.isW)
  val in_fire = valid && io.in_ready
  val out_fire = io.out_ready && io.out_valid
  val newReq = in_fire
  val startHandShake = io.in_ready && valid
  val s_idle :: s_pre_0 :: s_pre_1 :: s_iter :: s_post_0 :: s_post_1 :: s_finish :: Nil = Enum(7)

  val state = RegInit(UIntToOH(s_idle, 7))

  val quot_neg_2 :: quot_neg_1 :: quot_0 :: quot_pos_1 :: quot_pos_2 :: Nil = Enum(5)

  val finished = state(s_finish)

  // reused wire declarations
  val aIsZero = Wire(Bool())
  val dIsZero = Wire(Bool())
  val aTooSmall = Wire(Bool()) // this is output of reg!
  val noIter = Wire(Bool()) // this is output of reg!
  val finalIter = Wire(Bool())
  val aLZC = Wire(UInt((lzc_width + 1).W))
  val dLZC = Wire(UInt((lzc_width + 1).W))
  val aNormAbs = Wire(UInt((len + 1).W))
  val dNormAbs = Wire(UInt((len + 1).W))
  val aInverter = Wire(UInt(len.W)) // results of global inverter
  val dInverter = Wire(UInt(len.W))

  val rPreShifted = Wire(UInt((len + 1).W))

  val quotIter = Wire(UInt(len.W))
  val quotM1Iter = Wire(UInt(len.W))
  val qIterEnd = Wire(UInt(5.W))

  val rNext = Wire(UInt(itn_len.W))
  val rNextPd = Wire(UInt(itn_len.W)) // non-redundant remainder plus d, 68, 67
  //reused ctrl regs

  //reused other regs
  val aNormAbsReg = RegEnable(aNormAbs, startHandShake | state(s_pre_0) | state(s_post_0)) // reg for normalized a & d and rem & rem+d
  val dNormAbsReg = RegEnable(dNormAbs, startHandShake | state(s_pre_0) | state(s_post_0))
  val quotIterReg = RegEnable(quotIter, state(s_pre_1) | state(s_iter) | state(s_post_0))
  val quotM1IterReg = RegEnable(quotM1Iter, state(s_pre_1) | state(s_iter) | state(s_post_0))

  when(kill_r) {
    state := UIntToOH(s_idle, 7)
  } .elsewhen(state(s_idle) && in_fire && !kill_w) {
    state := UIntToOH(s_pre_0, 7)
  } .elsewhen(state(s_pre_0)) { // leading zero detection
    state := UIntToOH(s_pre_1, 7)
  } .elsewhen(state(s_pre_1)) { // shift a/b
    state := Mux(dIsZero | aTooSmall | noIter, UIntToOH(s_post_0, 7), UIntToOH(s_iter, 7))
  } .elsewhen(state(s_iter)) { // (ws[j+1], wc[j+1]) = 4(ws[j],wc[j]) - q(j+1)*d
    state := Mux(finalIter, UIntToOH(s_post_0, 7), UIntToOH(s_iter, 7))
  } .elsewhen(state(s_post_0)) { // if rem < 0, rem = rem + d
    state := UIntToOH(s_post_1, 7)
  } .elsewhen(state(s_post_1)) {
    state := UIntToOH(s_finish, 7)
  } .elsewhen(state(s_finish) && out_fire) {
    state := UIntToOH(s_idle, 7)
  } .otherwise {
    state := state
  }

  // First cycle:
  // State is idle, we gain absolute value of a and b, using global inverter

  io.in_ready := state(s_idle)

  aInverter := -Mux(state(s_idle), a, quotIterReg) // 64, 0
  dInverter := -Mux(state(s_idle), d, quotM1IterReg) // 64, 0

  val aSign = io.sign && a(len - 1) // 1
  val dSign = io.sign && d(len - 1)

  val aAbs = Mux(aSign, aInverter, a) // 64, 0
  val dAbs = Mux(dSign, dInverter, d)
  val aNorm = (aNormAbsReg(len - 1, 0) << aLZC(lzc_width - 1, 0))(len - 1, 0) // 64, 65
  val dNorm = (dNormAbsReg(len - 1, 0) << dLZC(lzc_width - 1, 0))(len - 1, 0)

  aNormAbs := Mux1H(Seq(
    state(s_idle) -> Cat(0.U(1.W), aAbs), // 65, 0
    state(s_pre_0) -> Cat(0.U(1.W), aNorm), // 65, 0
    state(s_post_0) -> rNext(len + 3, 3) // remainder 65, 64. highest is sign bit
  ))
  dNormAbs := Mux1H(Seq(
    state(s_idle) -> Cat(0.U(1.W), dAbs),
    state(s_pre_0) -> Cat(0.U(1.W), dNorm),
    state(s_post_0) -> rNextPd(len + 3, 3)
    ))

  // Second cycle, state is pre_0
  // calculate lzc and move div* and lzc diff check if no_iter_needed

  aLZC := PriorityEncoder(aNormAbsReg(len - 1, 0).asBools().reverse)
  dLZC := PriorityEncoder(dNormAbsReg(len - 1, 0).asBools().reverse)
  val aLZCReg = RegEnable(aLZC, state(s_pre_0)) // 7, 0
  val dLZCReg = RegEnable(dLZC, state(s_pre_0))



  val lzcWireDiff = Cat(0.U(1.W), dLZC(lzc_width - 1, 0)) - Cat(0.U(1.W), aLZC(lzc_width - 1, 0)) // 7, 0
  val lzcRegDiff = Cat(0.U(1.W), dLZCReg(lzc_width - 1, 0)) - Cat(0.U(1.W), aLZCReg(lzc_width - 1, 0))
  val lzcDiff = Mux(state(s_pre_0), lzcWireDiff, lzcRegDiff)
  aIsZero := aLZC(lzc_width) // this is state pre_0
  dIsZero := dLZCReg(lzc_width) // this is pre_1 and all stages after
  val dIsOne = dLZC(lzc_width - 1, 0).andR() // this is pre_0
  val noIterReg = RegEnable(dIsOne & aNormAbsReg(len - 1), state(s_pre_0)) // This means dividend has lzc 0 so iter is 17
  noIter := noIterReg
  val aTooSmallReg = RegEnable(aIsZero | lzcDiff(lzc_width), state(s_pre_0)) // a is zero or a smaller than d
  aTooSmall := aTooSmallReg

  val quotSign = Mux(state(s_idle), aSign ^ dSign, true.B) // if not s_idle then must be s_pre_1 & dIsZero, and that we have
  val rSign = aSign
  val quotSignReg = RegEnable(quotSign, startHandShake | (state(s_pre_1) & dIsZero))
  val rSignReg = RegEnable(rSign, startHandShake)

  val rShift = lzcDiff(0) // odd lzc diff, for SRT4
  val rightShifted = Wire(UInt(len.W))
  val rSumInit = Mux(aTooSmallReg | aIsZero, Cat(0.U(1.W), rightShifted, 0.U(3.W)), // right shift the dividend (which is already l-shifted)
                    Mux(noIterReg, 0.U(itn_len.W), //
                      Cat(0.U(3.W),
                          Mux(rShift, Cat(0.U(1.W), aNormAbsReg(len - 1, 0)), Cat(aNormAbsReg(len - 1, 0), 0.U(1.W)))
                        ) // Normal init value. 68, 67; For even lzcDiff, 0.001xxx0; for odd lzcDiff 0.0001xxx
                      )
                    ) // state is s_pre_1
  val rCarryInit = 0.U(itn_len.W)

  val rightShifter = Module(new RightShifter(len, lzc_width))
  rightShifter.io.in := Mux(state(s_pre_1), aNormAbsReg(len - 1, 0), rPreShifted(len - 1, 0))
  rightShifter.io.shiftNum := Mux(state(s_pre_1), aLZCReg,
                                  Mux(aTooSmallReg | dIsZero, 0.U(lzc_width.W), dLZCReg))
  rightShifter.io.msb := state(s_post_1) & rSignReg & rPreShifted(len)
  rightShifted := rightShifter.io.out

  // obtaining 1st quotient
  val rSumInitTrunc = Cat(0.U(1.W), rSumInit(itn_len - 4, itn_len - 4 - 4 + 1)) // 0.00___
  val mInitPos1 = MuxLookup(dNormAbsReg(len - 2, len - 2 - 3 + 1), "b00100".U(5.W),
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
  val mInitPos2 = MuxLookup(dNormAbsReg(len - 2, len - 2 - 3 + 1), "b01100".U(5.W),
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
  val qPrev = Mux(state(s_pre_1), qInit, qIterEnd)
  val qPrevReg = RegEnable(qPrev, state(s_pre_1) | state(s_iter))
  val specialDivisorReg = RegEnable(dNormAbsReg(len - 2, len - 2 - 3 + 1) === 0.U, state(s_pre_1)) // d=0.1000xxx
  // rCarry and rSum in Iteration
  val qXd = Mux1H(Seq(
    qPrevReg(quot_neg_2) -> Cat(dNormAbsReg(len - 1, 0), 0.U(4.W)), // 68, 67 1.xxxxx0000
    qPrevReg(quot_neg_1) -> Cat(0.U(1.W), dNormAbsReg(len - 1, 0), 0.U(3.W)), // 0.1xxxxx000
    qPrevReg(quot_0)     -> 0.U(itn_len.W),
    qPrevReg(quot_pos_1) -> ~Cat(0.U(1.W), dNormAbsReg(len - 1, 0), 0.U(3.W)), // don't forget to plus 1 later
    qPrevReg(quot_pos_2) -> ~Cat(dNormAbsReg(len - 1, 0), 0.U(4.W))  // don't forget to plus 1 later
  ))
  val csa = Module(new CSA3_2(itn_len))

  val rSumIter = csa.io.out(0)
  val rCarryIter = Cat(csa.io.out(1)(itn_len - 2, 0), qPrevReg(quot_pos_1) | qPrevReg(quot_pos_2))
  val rSumReg = RegEnable(Mux(state(s_pre_1), rSumInit, rSumIter), state(s_pre_1) | state(s_iter)) // 68, 67
  val rCarryReg = RegEnable(Mux(state(s_pre_1), rCarryInit, rCarryIter), state(s_pre_1) | state(s_iter))
  csa.io.in(0) := rSumReg << 2
  csa.io.in(1) := rCarryReg << 2
  csa.io.in(2) := qXd

  val qds = Module(new SRT4QDS(len, itn_len))
  qds.io.remSum := rSumReg
  qds.io.remCarry := rCarryReg
  qds.io.d := dNormAbsReg(len - 1, 0) // Maybe optimize here to lower power consumption?
  qds.io.specialDivisor := specialDivisorReg
  qds.io.qPrev := qPrevReg
  qIterEnd := qds.io.qIterEnd

  //on the fly conversion
  val quotIterNext = Wire(UInt(len.W))
  val quotIterM1Next = Wire(UInt(len.W))
  quotIterNext := Mux1H(Seq(
    qPrevReg(quot_pos_2) -> (quotIterReg << 2 | "b10".U),
    qPrevReg(quot_pos_1) -> (quotIterReg << 2 | "b01".U),
    qPrevReg(quot_0)     -> (quotIterReg << 2 | "b00".U),
    qPrevReg(quot_neg_1) -> (quotM1IterReg << 2 | "b11".U),
    qPrevReg(quot_neg_2) -> (quotM1IterReg << 2 | "b10".U)
  ))
  quotIterM1Next := Mux1H(Seq(
    qPrevReg(quot_pos_2) -> (quotIterReg << 2 | "b01".U),
    qPrevReg(quot_pos_1) -> (quotIterReg << 2 | "b00".U),
    qPrevReg(quot_0)     -> (quotM1IterReg << 2 | "b11".U),
    qPrevReg(quot_neg_1) -> (quotM1IterReg << 2 | "b10".U),
    qPrevReg(quot_neg_2) -> (quotM1IterReg << 2 | "b01".U)
  ))


  quotIter := Mux(state(s_pre_1),
                      Mux(dIsZero, VecInit(Seq.fill(len)(true.B)).asUInt,
                        Mux(noIterReg, aNormAbsReg(len - 1, 0), 0.U(len.W))),
                      Mux(state(s_iter), quotIterNext,
                        Mux(quotSignReg, aInverter, quotIterReg)))
  quotM1Iter := Mux(state(s_pre_1),
                        0.U(len.W), Mux(state(s_iter), quotIterM1Next,
                          Mux(quotSignReg, dInverter, quotM1IterReg)))


  // iter num
  val iterNum = Wire(UInt((lzc_width - 1).W))
  val iterNumReg = RegEnable(iterNum, state(s_pre_1) | state(s_iter))

  iterNum := Mux(state(s_pre_1), lzcDiff(lzc_width - 1, 1) +% lzcDiff(0), iterNumReg -% 1.U)
  finalIter := iterNumReg === 0.U

  // Post Process

  when(rSignReg) {
    rNext := ~rSumReg + ~rCarryReg + 2.U
    rNextPd := ~rSumReg + ~rCarryReg + ~Cat(0.U(1.W), dNormAbsReg(len - 1, 0), 0.U(3.W)) + 3.U
  } .otherwise {
    rNext := rSumReg + rCarryReg
    rNextPd := rSumReg + rCarryReg + Cat(0.U(1.W), dNormAbsReg(len - 1, 0), 0.U(3.W))
  }

  val r = aNormAbsReg
  val rPd = dNormAbsReg
  val rIsZero = ~(r.orR())
  val needCorr = (~dIsZero & ~noIterReg) & Mux(rSignReg, ~r(len) & ~rIsZero, r(len)) // when we get pos rem for d<0 or neg rem for d>0
  rPreShifted := Mux(needCorr, rPd, r)
  val rFinal = RegEnable(rightShifted, state(s_post_1))// right shifted remainder. shift by the number of bits divisor is shifted
  val qFinal = Mux(needCorr, quotM1IterReg, quotIterReg)
  val res = Mux(isHi, rFinal, qFinal)
  io.out_data := Mux(isW,
    SignExt(res(31, 0), len),
    res
  )
  io.in_ready := state(s_idle)
  io.out_valid := state(s_finish) // state === s_finish
}

class RightShifter(len: Int, lzc_width: Int) extends Module {
  val io = IO(new Bundle() {
    val shiftNum = Input(UInt(lzc_width.W))
    val in = Input(UInt(len.W))
    val msb = Input(Bool())
    val out = Output(UInt(len.W))
  })
  require(len == 64 || len == 32)
  val shift = io.shiftNum
  val msb = io.msb
  val s0 = Mux(shift(0), Cat(VecInit(Seq.fill(1)(msb)).asUInt, io.in(len - 1, 1)), io.in)
  val s1 = Mux(shift(1), Cat(VecInit(Seq.fill(2)(msb)).asUInt, s0(len - 1, 2)), s0)
  val s2 = Mux(shift(2), Cat(VecInit(Seq.fill(4)(msb)).asUInt, s1(len - 1, 4)), s1)
  val s3 = Mux(shift(3), Cat(VecInit(Seq.fill(8)(msb)).asUInt, s2(len - 1, 8)), s2)
  val s4 = Mux(shift(4), Cat(VecInit(Seq.fill(16)(msb)).asUInt, s3(len - 1, 16)), s3)
  val s5 = Wire(UInt(len.W))
  if (len == 64) {
    s5 := Mux(shift(5), Cat(VecInit(Seq.fill(32)(msb)).asUInt, s4(len - 1, 32)), s4)
  } else if (len == 32) {
    s5 := s4
  }
  io.out := s5
}

object mLookUpTable {
  // Usage :
  // result := decoder(QMCMinimizer, index, mLookupTable.xxx)
  val minus_m = Seq(
    Array( // -m[-1]
      0.U -> "b00_11010".U,
      1.U -> "b00_11110".U,
      2.U -> "b01_00000".U,
      3.U -> "b01_00100".U,
      4.U -> "b01_00110".U,
      5.U -> "b01_01010".U,
      6.U -> "b01_01100".U,
      7.U -> "b01_10000".U
    ),
    Array( // -m[0]
      0.U -> "b000_0101".U,
      1.U -> "b000_0110".U,
      2.U -> "b000_0110".U,
      3.U -> "b000_0110".U,
      4.U -> "b000_1001".U,
      5.U -> "b000_1000".U,
      6.U -> "b000_1000".U,
      7.U -> "b000_1000".U
    ),
    Array( //-m[1]
      0.U -> "b111_1101".U,
      1.U -> "b111_1100".U,
      2.U -> "b111_1100".U,
      3.U -> "b111_1100".U,
      4.U -> "b111_1011".U,
      5.U -> "b111_1010".U,
      6.U -> "b111_1010".U,
      7.U -> "b111_1010".U
    ),
    Array( //-m[2]
      0.U -> "b11_01000".U,
      1.U -> "b11_00100".U,
      2.U -> "b11_00010".U,
      3.U -> "b10_11110".U,
      4.U -> "b10_11100".U,
      5.U -> "b10_11000".U,
      6.U -> "b10_10110".U,
      7.U -> "b10_10010".U
    ))
}

class SRT4QDS(len: Int, itn_len: Int) extends Module {
  // srt4 quotientr digit selection
  val io = IO(new Bundle() {
    val remSum = Input(UInt(itn_len.W)) // 68, 67
    val remCarry = Input(UInt(itn_len.W))
    val d = Input(UInt(len.W)) // 64, 64
    val specialDivisor = Input(Bool())
    val qPrev = Input(UInt(5.W))
    val qIterEnd = Output(UInt(5.W))
  })
  val remSumX16 = io.remSum << 4 // 72, 67 Top 2 bits unused
  val remCarryX16 = io.remCarry << 4
  def trunc25(rem: UInt): UInt = {rem(itn_len, itn_len - 7 + 1)}
  def trunc34(rem: UInt): UInt = {rem(itn_len + 1, itn_len + 1 - 7 + 1)}

  val quot_neg_2 :: quot_neg_1 :: quot_0 :: quot_pos_1 :: quot_pos_2 :: Nil = Enum(5)

  val d = Cat(0.U(1.W), io.d, 0.U(3.W)) // 68, 67
  val (dX4, dX8, dXNeg4, dXNeg8) = (d << 2, d(itn_len - 2, 0) << 3, ~(d << 2), ~(d(itn_len - 2, 0) << 3)) // 70, 67
  val dForLookup = io.d(len - 2, len - 2 - 3 + 1)

  val dXq = Mux1H(Seq(
    io.qPrev(quot_neg_2) -> dX8,
    io.qPrev(quot_neg_1) -> dX4,
    io.qPrev(quot_0) -> 0.U((itn_len + 2).W),
    io.qPrev(quot_pos_1) -> dXNeg4,
    io.qPrev(quot_pos_2) -> dXNeg8
  ))
  val signs = VecInit(Seq.tabulate(4){ // -1 0 1 2
    i => {
      val csa1 = Module(new CSA3_2(7))
      val csa2 = Module(new CSA3_2(7))
      if (i == 1 || i == 2) {
        csa1.io.in(0) := trunc34(remSumX16)
        csa1.io.in(1) := trunc34(remCarryX16)
        csa2.io.in(2) := trunc34(dXq)
      } else {
        csa1.io.in(0) := trunc25(remSumX16)
        csa1.io.in(1) := trunc25(remCarryX16)
        csa2.io.in(2) := trunc25(dXq)
      }
      csa1.io.in(2) := MuxLookup(dForLookup, "b0000000".U, mLookUpTable.minus_m(i))
      csa2.io.in(0) := csa1.io.out(0)
      csa2.io.in(1) := csa1.io.out(1)(5, 0) << 1
      (csa2.io.out(0) + (csa2.io.out(1)(5, 0) << 1))(6)
    }
  })
  val qVec = Wire(Vec(5, Bool()))
  qVec(quot_neg_2) := signs(0) && signs(1) && signs(2)
  qVec(quot_neg_1) := ~signs(0) && signs(1) && signs(2)
  qVec(quot_0) := signs(2) && ~signs(1)
  qVec(quot_pos_1) := signs(3) && ~signs(2) && ~signs(1)
  qVec(quot_pos_2) := ~signs(3) && ~signs(2) && ~signs(1)
  io.qIterEnd := qVec.asUInt
  // assert(PopCount(qVec) === 1.U)
}


class SRT4Divider(len: Int)(implicit p: Parameters) extends AbstractDivider(len) {

  val newReq = io.in.fire()

  val uop = io.in.bits.uop
  val uopReg = RegEnable(uop, newReq)
  val ctrlReg = RegEnable(ctrl, newReq)

  val divDataModule = Module(new SRT4DividerDataModule(len))

  val kill_w = uop.robIdx.needFlush(io.redirectIn)
  val kill_r = !divDataModule.io.in_ready && uopReg.robIdx.needFlush(io.redirectIn)

  divDataModule.io.src(0) := io.in.bits.src(0)
  divDataModule.io.src(1) := io.in.bits.src(1)
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
