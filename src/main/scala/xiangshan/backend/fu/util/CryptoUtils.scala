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

package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._
import xiangshan._
import org.chipsalliance.cde.config.Parameters

// 32bits shift right
object SHR32 {
  def apply(bits: UInt, shamt: Int) = {
    require(shamt>0 && shamt<32)
    if(shamt == 31) Cat(0.U(63.W), bits(31))
    else            Cat(0.U((32+shamt).W), bits(31,shamt))
  }
}

// 64bits shift right
object SHR64 {
  def apply(bits: UInt, shamt: Int) = {
    require(shamt>0 && shamt<64)
    if(shamt == 63) Cat(bits(62,0), bits(63))
    else            Cat(0.U(shamt.W), bits(63,shamt))
  }
}

// 32bits Rotate shift
object ROR32 {
  def apply(bits: UInt, shamt: Int) = {
    require(shamt>0 && shamt<32)
    if(shamt == 1)       Cat(0.U(32.W), bits(0), bits(31,1))
    else if(shamt == 31) Cat(0.U(32.W), bits(30,0), bits(31))
    else                 Cat(0.U(32.W), bits(shamt-1,0), bits(31,shamt))
  }
}

// 64bits Rotate shift
object ROR64 {
  def apply(bits: UInt, shamt: Int) = {
    require(shamt>0 && shamt<64)
    if(shamt == 1)       Cat(bits(0), bits(63,1))
    else if(shamt == 63) Cat(bits(62,0), bits(63))
    else                 Cat(bits(shamt-1,0), bits(63,shamt))
  }
}

// AES forward shift rows
object ForwardShiftRows {
  def apply(src1: Seq[UInt], src2: Seq[UInt]): Seq[UInt] = {
    VecInit(Seq(src1(0), src1(5), src2(2), src2(7),
                src1(4), src2(1), src2(6), src1(3)))
  }
}

// AES inverse shift rows
object InverseShiftRows {
  def apply(src1: Seq[UInt], src2: Seq[UInt]): Seq[UInt] = {
    VecInit(Seq(src1(0), src2(5), src2(2), src1(7),
                src1(4), src1(1), src2(6), src2(3)))
  }
}

// AES encode sbox top
object SboxAesTop {
  def apply(i: UInt): Seq[Bool] = {
    val t = Wire(Vec(6, Bool()))
    val o = Wire(Vec(21, Bool()))
    t( 0) := i( 3) ^ i( 1)
    t( 1) := i( 6) ^ i( 5)
    t( 2) := i( 6) ^ i( 2)
    t( 3) := i( 5) ^ i( 2)
    t( 4) := i( 4) ^ i( 0)
    t( 5) := i( 1) ^ i( 0)
    o( 0) := i( 0)
    o( 1) := i( 7) ^ i( 4)
    o( 2) := i( 7) ^ i( 2)
    o( 3) := i( 7) ^ i( 1)
    o( 4) := i( 4) ^ i( 2)
    o( 5) := o( 1) ^ t( 0)
    o( 6) := i( 0) ^ o( 5)
    o( 7) := i( 0) ^ t( 1)
    o( 8) := o( 5) ^ t( 1)
    o( 9) := o( 3) ^ o( 4)
    o(10) := o( 5) ^ t( 2)
    o(11) := t( 0) ^ t( 2)
    o(12) := t( 0) ^ t( 3)
    o(13) := o( 7) ^ o(12)
    o(14) := t( 1) ^ t( 4)
    o(15) := o( 1) ^ o(14)
    o(16) := t( 1) ^ t( 5)
    o(17) := o( 2) ^ o(16)
    o(18) := o( 2) ^ o( 8)
    o(19) := o(15) ^ o(13)
    o(20) := o( 1) ^ t( 3)
    o
  }
}

// AES decode sbox top
object SboxIaesTop {
  def apply(i: UInt): Seq[Bool] = {
    val t = Wire(Vec(5, Bool()))
    val o = Wire(Vec(21, Bool()))
    t( 0) := i( 1) ^  i( 0)
    t( 1) := i( 6) ^  i( 1)
    t( 2) := i( 5) ^ ~i( 2)
    t( 3) := i( 2) ^ ~i( 1)
    t( 4) := i( 5) ^ ~i( 3)
    o( 0) := i( 7) ^  t( 2)
    o( 1) := i( 4) ^  i( 3)
    o( 2) := i( 7) ^ ~i( 6)
    o( 3) := o( 1) ^  t( 0)
    o( 4) := i( 3) ^  o( 6)
    o( 5) := o(16) ^  t( 2)
    o( 6) := i( 6) ^ ~o(17)
    o( 7) := i( 0) ^ ~o( 1)
    o( 8) := o( 2) ^  o(18)
    o( 9) := o( 2) ^  t( 0)
    o(10) := o( 8) ^  t( 3)
    o(11) := o( 8) ^  o(20)
    o(12) := t( 1) ^  t( 4)
    o(13) := i( 5) ^ ~o(14)
    o(14) := o(16) ^  t( 0)
    o(15) := o(18) ^  t( 1)
    o(16) := i( 6) ^ ~i( 4)
    o(17) := i( 7) ^  i( 4)
    o(18) := i( 3) ^ ~i( 0)
    o(19) := i( 5) ^ ~o( 1)
    o(20) := o( 1) ^  t( 3)
    o
  }
}

// SM4 encode/decode sbox top
object SboxSm4Top {
  def apply(i: UInt): Seq[Bool] = {
    val t = Wire(Vec(7, Bool()))
    val o = Wire(Vec(21, Bool()))
    t( 0) := i(3) ^  i( 4)
    t( 1) := i(2) ^  i( 7)
    t( 2) := i(7) ^  o(18)
    t( 3) := i(1) ^  t( 1)
    t( 4) := i(6) ^  i( 7)
    t( 5) := i(0) ^  o(18)
    t( 6) := i(3) ^  i( 6)
    o( 0) := i(5) ^ ~o(10)
    o( 1) := t(0) ^  t( 3)
    o( 2) := i(0) ^  t( 0)
    o( 3) := i(3) ^  o( 4)
    o( 4) := i(0) ^  t( 3)
    o( 5) := i(5) ^  t( 5)
    o( 6) := i(0) ^ ~i( 1)
    o( 7) := t(0) ^ ~o(10)
    o( 8) := t(0) ^  t( 5)
    o( 9) := i(3)
    o(10) := i(1) ^  o(18)
    o(11) := t(0) ^  t( 4)
    o(12) := i(5) ^  t( 4)
    o(13) := i(5) ^ ~o( 1)
    o(14) := i(4) ^ ~t( 2)
    o(15) := i(1) ^ ~t( 6)
    o(16) := i(0) ^ ~t( 2)
    o(17) := t(0) ^ ~t( 2)
    o(18) := i(2) ^  i( 6)
    o(19) := i(5) ^ ~o(14)
    o(20) := i(0) ^  t( 1)
    o
  }
}

// Sbox middle part for AES, AES^-1, SM4
object SboxInv {
  def apply(i: Seq[Bool]): Seq[Bool] = {
    val t = Wire(Vec(46, Bool()))
    val o = Wire(Vec(18, Bool()))
    t( 0) := i( 3) ^ i(12)
    t( 1) := i( 9) & i( 5)
    t( 2) := i(17) & i( 6)
    t( 3) := i(10) ^ t( 1)
    t( 4) := i(14) & i( 0)
    t( 5) := t( 4) ^ t( 1)
    t( 6) := i( 3) & i(12)
    t( 7) := i(16) & i( 7)
    t( 8) := t( 0) ^ t( 6)
    t( 9) := i(15) & i(13)
    t(10) := t( 9) ^ t( 6)
    t(11) := i( 1) & i(11)
    t(12) := i( 4) & i(20)
    t(13) := t(12) ^ t(11)
    t(14) := i( 2) & i( 8)
    t(15) := t(14) ^ t(11)
    t(16) := t( 3) ^ t( 2)
    t(17) := t( 5) ^ i(18)
    t(18) := t( 8) ^ t( 7)
    t(19) := t(10) ^ t(15)
    t(20) := t(16) ^ t(13)
    t(21) := t(17) ^ t(15)
    t(22) := t(18) ^ t(13)
    t(23) := t(19) ^ i(19)
    t(24) := t(22) ^ t(23)
    t(25) := t(22) & t(20)
    t(26) := t(21) ^ t(25)
    t(27) := t(20) ^ t(21)
    t(28) := t(23) ^ t(25)
    t(29) := t(28) & t(27)
    t(30) := t(26) & t(24)
    t(31) := t(20) & t(23)
    t(32) := t(27) & t(31)
    t(33) := t(27) ^ t(25)
    t(34) := t(21) & t(22)
    t(35) := t(24) & t(34)
    t(36) := t(24) ^ t(25)
    t(37) := t(21) ^ t(29)
    t(38) := t(32) ^ t(33)
    t(39) := t(23) ^ t(30)
    t(40) := t(35) ^ t(36)
    t(41) := t(38) ^ t(40)
    t(42) := t(37) ^ t(39)
    t(43) := t(37) ^ t(38)
    t(44) := t(39) ^ t(40)
    t(45) := t(42) ^ t(41)
    o( 0) := t(38) & i( 7)
    o( 1) := t(37) & i(13)
    o( 2) := t(42) & i(11)
    o( 3) := t(45) & i(20)
    o( 4) := t(41) & i( 8)
    o( 5) := t(44) & i( 9)
    o( 6) := t(40) & i(17)
    o( 7) := t(39) & i(14)
    o( 8) := t(43) & i( 3)
    o( 9) := t(38) & i(16)
    o(10) := t(37) & i(15)
    o(11) := t(42) & i( 1)
    o(12) := t(45) & i( 4)
    o(13) := t(41) & i( 2)
    o(14) := t(44) & i( 5)
    o(15) := t(40) & i( 6)
    o(16) := t(39) & i( 0)
    o(17) := t(43) & i(12)
    o
  }
}

// AES encode sbox out
object SboxAesOut {
  def apply(i: Seq[Bool]): UInt = {
    val t = Wire(Vec(30, Bool()))
    val o = Wire(Vec(8, Bool()))
    t( 0) := i(11) ^  i(12)
    t( 1) := i( 0) ^  i( 6)
    t( 2) := i(14) ^  i(16)
    t( 3) := i(15) ^  i( 5)
    t( 4) := i( 4) ^  i( 8)
    t( 5) := i(17) ^  i(11)
    t( 6) := i(12) ^  t( 5)
    t( 7) := i(14) ^  t( 3)
    t( 8) := i( 1) ^  i( 9)
    t( 9) := i( 2) ^  i( 3)
    t(10) := i( 3) ^  t( 4)
    t(11) := i(10) ^  t( 2)
    t(12) := i(16) ^  i( 1)
    t(13) := i( 0) ^  t( 0)
    t(14) := i( 2) ^  i(11)
    t(15) := i( 5) ^  t( 1)
    t(16) := i( 6) ^  t( 0)
    t(17) := i( 7) ^  t( 1)
    t(18) := i( 8) ^  t( 8)
    t(19) := i(13) ^  t( 4)
    t(20) := t( 0) ^  t( 1)
    t(21) := t( 1) ^  t( 7)
    t(22) := t( 3) ^  t(12)
    t(23) := t(18) ^  t( 2)
    t(24) := t(15) ^  t( 9)
    t(25) := t( 6) ^  t(10)
    t(26) := t( 7) ^  t( 9)
    t(27) := t( 8) ^  t(10)
    t(28) := t(11) ^  t(14)
    t(29) := t(11) ^  t(17)
    o( 0) := t( 6) ^ ~t(23)
    o( 1) := t(13) ^ ~t(27)
    o( 2) := t(25) ^  t(29)
    o( 3) := t(20) ^  t(22)
    o( 4) := t( 6) ^  t(21)
    o( 5) := t(19) ^ ~t(28)
    o( 6) := t(16) ^ ~t(26)
    o( 7) := t( 6) ^  t(24)
    o.asUInt
  }
}

// AES decode sbox out
object SboxIaesOut {
  def apply(i: Seq[Bool]): UInt = {
    val t = Wire(Vec(30, Bool()))
    val o = Wire(Vec(8, Bool()))
    t( 0) := i( 2) ^ i(11)
    t( 1) := i( 8) ^ i( 9)
    t( 2) := i( 4) ^ i(12)
    t( 3) := i(15) ^ i( 0)
    t( 4) := i(16) ^ i( 6)
    t( 5) := i(14) ^ i( 1)
    t( 6) := i(17) ^ i(10)
    t( 7) := t( 0) ^ t( 1)
    t( 8) := i( 0) ^ i( 3)
    t( 9) := i( 5) ^ i(13)
    t(10) := i( 7) ^ t( 4)
    t(11) := t( 0) ^ t( 3)
    t(12) := i(14) ^ i(16)
    t(13) := i(17) ^ i( 1)
    t(14) := i(17) ^ i(12)
    t(15) := i( 4) ^ i( 9)
    t(16) := i( 7) ^ i(11)
    t(17) := i( 8) ^ t( 2)
    t(18) := i(13) ^ t( 5)
    t(19) := t( 2) ^ t( 3)
    t(20) := t( 4) ^ t( 6)
    t(21) := 0.U
    t(22) := t( 2) ^ t( 7)
    t(23) := t( 7) ^ t( 8)
    t(24) := t( 5) ^ t( 7)
    t(25) := t( 6) ^ t(10)
    t(26) := t( 9) ^ t(11)
    t(27) := t(10) ^ t(18)
    t(28) := t(11) ^ t(25)
    t(29) := t(15) ^ t(20)
    o( 0) := t( 9) ^ t(16)
    o( 1) := t(14) ^ t(23)
    o( 2) := t(19) ^ t(24)
    o( 3) := t(23) ^ t(27)
    o( 4) := t(12) ^ t(22)
    o( 5) := t(17) ^ t(28)
    o( 6) := t(26) ^ t(29)
    o( 7) := t(13) ^ t(22)
    o.asUInt
  }
}

// SM4 encode/decode sbox out
object SboxSm4Out {
  def apply(i: Seq[Bool]): UInt = {
    val t = Wire(Vec(30, Bool()))
    val o = Wire(Vec(8, Bool()))
    t( 0) := i( 4) ^  i( 7)
    t( 1) := i(13) ^  i(15)
    t( 2) := i( 2) ^  i(16)
    t( 3) := i( 6) ^  t( 0)
    t( 4) := i(12) ^  t( 1)
    t( 5) := i( 9) ^  i(10)
    t( 6) := i(11) ^  t( 2)
    t( 7) := i( 1) ^  t( 4)
    t( 8) := i( 0) ^  i(17)
    t( 9) := i( 3) ^  i(17)
    t(10) := i( 8) ^  t( 3)
    t(11) := t( 2) ^  t( 5)
    t(12) := i(14) ^  t( 6)
    t(13) := t( 7) ^  t( 9)
    t(14) := i( 0) ^  i( 6)
    t(15) := i( 7) ^  i(16)
    t(16) := i( 5) ^  i(13)
    t(17) := i( 3) ^  i(15)
    t(18) := i(10) ^  i(12)
    t(19) := i( 9) ^  t( 1)
    t(20) := i( 4) ^  t( 4)
    t(21) := i(14) ^  t( 3)
    t(22) := i(16) ^  t( 5)
    t(23) := t( 7) ^  t(14)
    t(24) := t( 8) ^  t(11)
    t(25) := t( 0) ^  t(12)
    t(26) := t(17) ^  t( 3)
    t(27) := t(18) ^  t(10)
    t(28) := t(19) ^  t( 6)
    t(29) := t( 8) ^  t(10)
    o( 0) := t(11) ^ ~t(13)
    o( 1) := t(15) ^ ~t(23)
    o( 2) := t(20) ^  t(24)
    o( 3) := t(16) ^  t(25)
    o( 4) := t(26) ^ ~t(22)
    o( 5) := t(21) ^  t(13)
    o( 6) := t(27) ^ ~t(12)
    o( 7) := t(28) ^ ~t(29)
    o.asUInt
  }
}

object SboxAes {
  def apply(byte: UInt): UInt = {
    SboxAesOut(SboxInv(SboxAesTop(byte)))
  }
}

object SboxIaes {
  def apply(byte: UInt): UInt = {
    SboxIaesOut(SboxInv(SboxIaesTop(byte)))
  }
}

object SboxSm4 {
  def apply(byte: UInt): UInt = {
    SboxSm4Out(SboxInv(SboxSm4Top(byte)))
  }
}

// Mix Column
object XtN {
  def Xt2(byte: UInt): UInt = ((byte << 1) ^ Mux(byte(7), "h1b".U, 0.U))(7,0)

  def apply(byte: UInt, t: UInt): UInt = {
    val byte1 = Xt2(byte)
    val byte2 = Xt2(byte1)
    val byte3 = Xt2(byte2)
    val result = Mux(t(0), byte, 0.U) ^ Mux(t(1), byte1, 0.U) ^ Mux(t(2), byte2, 0.U) ^ Mux(t(3), byte3, 0.U)
    result(7,0)
  }
}

object ByteEnc {
  def apply(bytes: Seq[UInt]): UInt = {
    XtN(bytes(0), "h2".U) ^ XtN(bytes(1), "h3".U) ^ bytes(2) ^ bytes(3)
  }
}

object ByteDec {
  def apply(bytes: Seq[UInt]): UInt = {
    XtN(bytes(0), "he".U) ^ XtN(bytes(1), "hb".U) ^ XtN(bytes(2), "hd".U) ^ XtN(bytes(3), "h9".U)
  }
}

object MixFwd {
  def apply(bytes: Seq[UInt]): UInt = {
    Cat(ByteEnc(Seq(bytes(3), bytes(0), bytes(1), bytes(2))),
        ByteEnc(Seq(bytes(2), bytes(3), bytes(0), bytes(1))),
        ByteEnc(Seq(bytes(1), bytes(2), bytes(3), bytes(0))),
        ByteEnc(Seq(bytes(0), bytes(1), bytes(2), bytes(3))))
  }
}

object MixInv {
  def apply(bytes: Seq[UInt]): UInt = {
    Cat(ByteDec(Seq(bytes(3), bytes(0), bytes(1), bytes(2))),
        ByteDec(Seq(bytes(2), bytes(3), bytes(0), bytes(1))),
        ByteDec(Seq(bytes(1), bytes(2), bytes(3), bytes(0))),
        ByteDec(Seq(bytes(0), bytes(1), bytes(2), bytes(3))))
  }
}
