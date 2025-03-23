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

package xiangshan.backend.fu.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import utility._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.util._
import xiangshan.backend.fu.vector.utils._
import yunsuan.VcryppType
import yunsuan.util.{GatedValidRegNext, LookupTree}

class AES128EncryptDecrypt0LatIO() extends Bundle {
  val state     = Input(UInt(128.W))
  val rkey      = Input(UInt(128.W))
  val op        = Input(UInt(8.W)) // 00: dec middle, 01: dec final, 10: enc middle, 11: enc final
  val result    = Output(UInt(128.W))
}

/**
 * AES-128 excryption/decryption module for vector unit
 * @param p
 */
class AES128EncryptDecrypt0Lat(implicit p: Parameters) extends XSModule {
  val io = IO(new AES128EncryptDecrypt0LatIO)
  dontTouch(io.state)
  dontTouch(io.rkey)
  dontTouch(io.op)
  dontTouch(io.result)

  val state        = io.state
  val rkey         = io.rkey
  val isFinalRound = io.op(0)
  val isFwd        = io.op(1)
  val isZeroRound  = io.op(2)


  val decFinal  = Wire(UInt(128.W)) // 2nd cycle
  val decMiddle = Wire(UInt(128.W)) // 2nd cycle
  val encFinal  = Wire(UInt(128.W)) // 2nd cycle
  val encMiddle = Wire(UInt(128.W)) // 2nd cycle, NOTE: also used for vaesz.vs

  val srDec = Wire(UInt(128.W))
  val srEnc = Wire(UInt(128.W))
  val ark   = Wire(UInt(128.W)) // 2nd cycle

  // sub-bytes
  val sbox = Module(new AES128SubBytesBidirection0Lat)
  sbox.io.src         := Mux(isFwd, srEnc, srDec)
  sbox.io.isFwd       := isFwd
  val sb = sbox.io.result

  // decryption final round
  srDec := AES128ShiftRowsInv(state)
  ark := sb ^ rkey
  decFinal := ark

  // decryption middle round
  decMiddle := Cat((0 until 4).map(i =>
    MixInv((4 * i until 4 * (i + 1)).map(j =>
      ark(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  )

  // encryption final round
  srEnc := AES128ShiftRowsFwd(state) // since shift-rows and sub-bytes commute
  encFinal := ark

  // encryption middle round
  val mixEncMiddle = Cat((0 until 4).map(i =>
    MixFwd((4 * i until 4 * (i + 1)).map(j =>
      sb(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  ) // 2nd cycle
  encMiddle := Mux(isZeroRound, state, mixEncMiddle) ^ rkey

  io.result := Mux(
    isFinalRound,
    Mux(isFwd, encFinal, decFinal),
    Mux(isFwd, encMiddle, decMiddle)
  )
}

class AES128KeySchedule0LatIO extends Bundle {
  val round      = Input(UInt(4.W))
  val currRndKey = Input(UInt(128.W)) // current round key
  val prevRndKey = Input(UInt(128.W)) // previous round key
  val isKf2      = Input(Bool())
  val result     = Output(UInt(128.W))
}

class AES128KeySchedule0Lat(implicit p: Parameters) extends XSModule {
  val io = IO(new AES128KeySchedule0LatIO)
  dontTouch(io.round)
  dontTouch(io.currRndKey)
  dontTouch(io.prevRndKey)
  dontTouch(io.isKf2)
  dontTouch(io.result)

  private val round = io.round
  private val isKf2 = io.isKf2

  // vaeskf1.vi && vaeskf2.vi
  private val rndLegal1 = Wire(UInt(4.W))
  rndLegal1 := Cat(
    Mux((round > 10.U) || (round === 0.U), ~round(3), round(3)),
    round(2, 0)
  )
  private val rndLegal2 = Wire(UInt(4.W))
  rndLegal2 := Cat(
    0.U(1.W),
    Mux((round > 14.U) || (round < 2.U), ~round(3), round(3)),
    round(2, 1)
  )

  private val isKf2OddRnd = isKf2 && round(0) === 1.U

  private val currRndKeyBytes = io.currRndKey.asTypeOf(Vec(4, UInt(32.W)))
  private val prevRndKeyBytes = io.prevRndKey.asTypeOf(Vec(4, UInt(32.W)))
  private val resWords = Wire(Vec(4, UInt(32.W)))

  private val rndLegal = Mux(isKf2, rndLegal2, rndLegal1)
  private val xorRndKeyBytes = Mux(isKf2, prevRndKeyBytes, currRndKeyBytes)
  private val subBytesSrc = Mux(
    isKf2OddRnd,
    currRndKeyBytes(3),
    AESRotword(currRndKeyBytes(3))
  )
  private val decodeRcon = Wire(UInt(32.W))
  private val decodeRconTmp = Wire(UInt(32.W))
  decodeRcon := AESDecodeRcon(rndLegal)
  decodeRconTmp := Mux(isKf2OddRnd, 0.U(32.W), decodeRcon)
  dontTouch(decodeRcon)
  dontTouch(decodeRconTmp)

  resWords(0) := AES32SubBytesFwd0Lat(subBytesSrc) ^
                 decodeRconTmp ^
                 xorRndKeyBytes(0)
  resWords(1) := resWords(0) ^ xorRndKeyBytes(1)
  resWords(2) := resWords(1) ^ xorRndKeyBytes(2)
  resWords(3) := resWords(2) ^ xorRndKeyBytes(3)

  io.result := Cat(resWords.reverse)
}


class VecShangMi40LatIO extends Bundle {
  val roundGroup = Input(UInt(3.W)) // imm
  val roundKeys  = Input(UInt(128.W)) // vs2
  val state      = Input(UInt(128.W)) // vd
  val isSM4R     = Input(Bool())
  val result     = Output(UInt(128.W))
}

class VecShangMi40Lat(implicit p: Parameters) extends XSModule {
  val io = IO(new VecShangMi40LatIO)
  dontTouch(io.roundGroup)
  dontTouch(io.roundKeys)
  dontTouch(io.state)
  dontTouch(io.isSM4R)
  dontTouch(io.result)

  // SM4 Constant Key (CK)
  private val ckSeq: Seq[Long] = Seq(
    0x00070E15L, 0x1C232A31L, 0x383F464DL, 0x545B6269L,
    0x70777E85L, 0x8C939AA1L, 0xA8AFB6BDL, 0xC4CBD2D9L,
    0xE0E7EEF5L, 0xFC030A11L, 0x181F262DL, 0x343B4249L,
    0x50575E65L, 0x6C737A81L, 0x888F969DL, 0xA4ABB2B9L,
    0xC0C7CED5L, 0xDCE3EAF1L, 0xF8FF060DL, 0x141B2229L,
    0x30373E45L, 0x4C535A61L, 0x686F767DL, 0x848B9299L,
    0xA0A7AEB5L, 0xBCC3CAD1L, 0xD8DFE6EDL, 0xF4FB0209L,
    0x10171E25L, 0x2C333A41L, 0x484F565DL, 0x646B7279L
  )
  private val ck = VecInit(ckSeq.map(_.U(32.W)))

  val b           = Wire(Vec(4, UInt(32.W)))
  val s           = Wire(Vec(4, UInt(32.W)))
  val isSM4R_s    = Wire(Vec(5, Bool()))
  val rk_s        = Wire(Vec(5, Vec(8, UInt(32.W))))
  val x_s         = Wire(Vec(5, Vec(8, UInt(32.W))))
  val rnd_s       = Wire(Vec(4, UInt(3.W)))

  isSM4R_s(0) := io.isSM4R
  for (lat <- 0 until 4) {
    isSM4R_s(lat + 1) := isSM4R_s(lat)
    // connection for old values, NOTE: BY DEFAULT
    for (i <- 0 until 8) {
      rk_s(lat + 1)(i) := rk_s(lat)(i)
      x_s (lat + 1)(i) := x_s (lat)(i)
    }
  }
  rnd_s(0) := io.roundGroup(2, 0)
  for (lat <- 0 until 3) {
    rnd_s(lat + 1) := rnd_s(lat)
  }

  for (i <- 4 until 8) { // by default
    rk_s(0)(i) := 0.U
    x_s (0)(i) := 0.U
  }
  for (i <- 0 until 4) {
    rk_s(0)(i) := io.roundKeys(32 * (i + 1) - 1, 32 * i)
    x_s (0)(i) := io.state    (32 * (i + 1) - 1, 32 * i)
  }
  b(0) := Mux(
    isSM4R_s(0),
    x_s (0)(1) ^ x_s (0)(2) ^ x_s (0)(3) ^ rk_s(0)(0),
    rk_s(0)(1) ^ rk_s(0)(2) ^ rk_s(0)(3) ^ ck(Cat(rnd_s(0), 0.U(2.W)))
  )
  s(0) := SM4Subword0Lat(b(0))

  rk_s(1)(4) := SM4RoundKey(rk_s(1)(0), s(0))
  x_s (1)(4) := SM4Round   (x_s (1)(0), s(0))
  b(1) := Mux(
    isSM4R_s(1),
    x_s (1)(2) ^ x_s (1)(3) ^ x_s (1)(4) ^ rk_s(1)(1),
    rk_s(1)(2) ^ rk_s(1)(3) ^ rk_s(1)(4) ^ ck(Cat(rnd_s(1), 1.U(2.W)))
  )
  s(1) := SM4Subword0Lat(b(1))

  rk_s(2)(5) := SM4RoundKey(rk_s(2)(1), s(1))
  x_s (2)(5) := SM4Round   (x_s (2)(1), s(1))
  b(2) := Mux(
    isSM4R_s(2),
    x_s (2)(3) ^ x_s (2)(4) ^ x_s (2)(5) ^ rk_s(2)(2),
    rk_s(2)(3) ^ rk_s(2)(4) ^ rk_s(2)(5) ^ ck(Cat(rnd_s(2), 2.U(2.W)))
  )
  s(2) := SM4Subword0Lat(b(2))

  rk_s(3)(6) := SM4RoundKey(rk_s(3)(2), s(2))
  x_s (3)(6) := SM4Round   (x_s (3)(2), s(2))
  b(3) := Mux(
    isSM4R_s(3),
    x_s (3)(4) ^ x_s (3)(5) ^ x_s (3)(6) ^ rk_s(3)(3),
    rk_s(3)(4) ^ rk_s(3)(5) ^ rk_s(3)(6) ^ ck(Cat(rnd_s(3), 3.U(2.W)))
  )
  s(3) := SM4Subword0Lat(b(3))

  rk_s(4)(7) := SM4RoundKey(rk_s(4)(3), s(3))
  x_s (4)(7) := SM4Round   (x_s (4)(3), s(3))
  io.result := Mux(
    isSM4R_s(4),
    Cat(x_s (4)(7), x_s (4)(6), x_s (4)(5), x_s (4)(4)),
    Cat(rk_s(4)(7), rk_s(4)(6), rk_s(4)(5), rk_s(4)(4))
  )
}


class VecSHACompress0LatIO(implicit p: Parameters) extends XSBundle {
  val curr = Input(new VecSHAState)
  val messageSchedPlusC = Input(Vec(4, UInt(32.W)))
  val isVSHA2cl = Input(Bool())
  val next = Output(new VecSHAState)
}

class VecSHACompress0Lat(implicit p: Parameters) extends XSModule {
  private val SEW = 32 // TODO: if modification is needed, change arguments below

  val io = IO(new VecSHACompress0LatIO)
  dontTouch(io.curr)
  dontTouch(io.messageSchedPlusC)
  dontTouch(io.isVSHA2cl)
  dontTouch(io.next)

  val curr = io.curr
  val messageSchedPlusC = io.messageSchedPlusC
  val isVSHA2cl = io.isVSHA2cl

  private def ch(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ ((~x).asUInt & z)
  private def maj(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ (x & z) ^ (y & z)

  val words = Wire(Vec(2, UInt(32.W))) // l vs h difference is the words selected
  words := Mux(
    isVSHA2cl,
    VecInit(messageSchedPlusC(0), messageSchedPlusC(1)),
    VecInit(messageSchedPlusC(2), messageSchedPlusC(3))
  )

  private val tmp1_s1 = Wire(UInt(SEW.W))
  private val tmp2_s1 = Wire(UInt(SEW.W))
  private val words1_s1 = Wire(UInt(32.W))
  val curr_s1 = Wire(new VecSHAState)
  tmp1_s1 := curr.h + SHA2.sha256sum1(curr.e) + ch(curr.e, curr.f, curr.g) + words(0)
  tmp2_s1 := SHA2.sha256sum0(curr.a) + maj(curr.a, curr.b, curr.c)
  words1_s1 := words(1)
  curr_s1 := curr

  private val words1_s2 = Wire(UInt(32.W))
  private val mid_s2 = Wire(new VecSHAState)
  words1_s2 := words1_s1
  mid_s2 := (tmp1_s1 + tmp2_s1, curr_s1.a, curr_s1.b, curr_s1.c, curr_s1.d + tmp1_s1, curr_s1.e, curr_s1.f, curr_s1.g)

  private val tmp1_s3 = Wire(UInt(SEW.W))
  private val tmp2_s3 = Wire(UInt(SEW.W))
  private val mid_s3 = Wire(new VecSHAState)
  tmp1_s3 := mid_s2.h + SHA2.sha256sum1(mid_s2.e) + ch(mid_s2.e, mid_s2.f, mid_s2.g) + words1_s2
  tmp2_s3 := SHA2.sha256sum0(mid_s2.a) + maj(mid_s2.a, mid_s2.b, mid_s2.c)
  mid_s3 := mid_s2

  io.next := (tmp1_s3 + tmp2_s3, mid_s3.a, mid_s3.b, mid_s3.c, mid_s3.d + tmp1_s3, mid_s3.e, mid_s3.f, mid_s3.g)
}

class VecSHAMessageWords0LatIO(implicit p: Parameters) extends XSBundle {
  val wordsLow = Input(Vec(4, UInt(32.W)))
  val wordsMid = Input(Vec(4, UInt(32.W)))
  val wordsHigh = Input(Vec(4, UInt(32.W)))
  val out = Output(UInt(128.W))
}

class VecSHAMessageWords0Lat(implicit p: Parameters) extends XSModule {
  val io = IO(new VecSHAMessageWords0LatIO)
  dontTouch(io.wordsLow)
  dontTouch(io.wordsMid)
  dontTouch(io.wordsHigh)
  dontTouch(io.out)

  /** w[5,6,7,8] are not used, w[16,17,18,19] are generated later */
  private val w = Wire(Vec(16, UInt(32.W)))
  w.foreach(w => w := 0.U) // initialize

  /**
   * Assign values to this message words.
   * @param wordsLow {W[3] @ W[2] @ W[1] @ W[0]} from oldVd
   * @param wordsMid {W[11] @ W[10] @ W[9] @ W[4]} from vs2
   * @param wordsHigh {W[15] @ W[14] @ W[13] @ W[12]} from vs1
   */
  Seq( 0,  1,  2,  3).zipWithIndex.foreach(i => w(i._1) := io.wordsLow(i._2))
  Seq( 4,  9, 10, 11).zipWithIndex.foreach(i => w(i._1) := io.wordsMid(i._2))
  Seq(12, 13, 14, 15).zipWithIndex.foreach(i => w(i._1) := io.wordsHigh(i._2))

  private val w16_s1 = Wire(UInt(32.W))
  private val w17_s1 = Wire(UInt(32.W))
  private val w18Part_s1 = Wire(UInt(32.W))
  private val w19Part_s1 = Wire(UInt(32.W))

  w16_s1 := SHA2.sha256sig1(w(14)) + w( 9) + SHA2.sha256sig0(w(1)) + w(0)
  w17_s1 := SHA2.sha256sig1(w(15)) + w(10) + SHA2.sha256sig0(w(2)) + w(1)
  w18Part_s1 := w(11) + SHA2.sha256sig0(w(3)) + w(2)
  w19Part_s1 := w(12) + SHA2.sha256sig0(w(4)) + w(3)

  private val w16_s2 = Wire(UInt(32.W))
  private val w17_s2 = Wire(UInt(32.W))
  private val w18_s2 = Wire(UInt(32.W))
  private val w19_s2 = Wire(UInt(32.W))
  w16_s2 := w16_s1
  w17_s2 := w17_s1
  w18_s2 := SHA2.sha256sig1(w16_s1) + w18Part_s1
  w19_s2 := SHA2.sha256sig1(w17_s1) + w19Part_s1

  io.out := Cat(w19_s2, w18_s2, w17_s2, w16_s2)
}
