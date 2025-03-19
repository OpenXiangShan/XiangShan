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


/**
 * Pipelined vector crypto unit
 * @param cfg configuration
 */
class VcryptoPiped2(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VcryppType.dummy, "Vcrypp OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 128
  private val numVecModule = dataWidth / dataWidthOfDataModule
  require(numVecModule == 1, "AES128EncryptDecrypt module only supports 128-bit data width")

  // io alias
  // def vaesdf_vv = "b10000001".U(OpTypeWidth.W)
  // def vaesdf_vs = "b10001001".U(OpTypeWidth.W)
  // def vaesdm_vv = "b10000000".U(OpTypeWidth.W)
  // def vaesdm_vs = "b10001000".U(OpTypeWidth.W)
  // def vaesef_vv = "b10000011".U(OpTypeWidth.W)
  // def vaesef_vs = "b10001011".U(OpTypeWidth.W)
  // def vaesem_vv = "b10000010".U(OpTypeWidth.W)
  // def vaesem_vs = "b10001010".U(OpTypeWidth.W)
  // def vaeskf1   = "b10000100".U(OpTypeWidth.W)
  // def vaeskf2   = "b10000101".U(OpTypeWidth.W)
  // def vaesz_vs  = "b10001110".U(OpTypeWidth.W)
  private val isCipher = true.B
  private val isAes = true.B
  private val isAesEncDec = VecInit(
    VcryppType.vaesdf_vv, VcryppType.vaesdf_vs,
    VcryppType.vaesdm_vv, VcryppType.vaesdm_vs,
    VcryppType.vaesef_vv, VcryppType.vaesef_vs,
    VcryppType.vaesem_vv, VcryppType.vaesem_vs,
    VcryppType.vaesz_vs
  ).contains(fuOpType)
  private val isAesKeySchedule = VecInit(
    VcryppType.vaeskf1, VcryppType.vaeskf2
  ).contains(fuOpType)

  private val sew = 2.U(2.W) // SEW==32. Note it's used at 2nd cycle. TODO: modified later
  private val sewVal = 32
//  private val illegalSew = true.B // TODO: modify it to "sew is not 32.U"

  val fire = io.in.valid

  private val isAesEncDec_1s = RegEnable(isAesEncDec, fire)

  // modules
  private val vaesEncDec = Module(new AES128EncryptDecrypt)
  private val vaesKeySchedule = Module(new AES128KeySchedule)
  private val resultData = Wire(UInt(dataWidthOfDataModule.W))

  /**
   * [[vaesEncDec]]'s in connection
   */
  vaesEncDec.io.state := oldVd
  vaesEncDec.io.rkey := vs2
  vaesEncDec.io.op := fuOpType(7, 0)
  vaesEncDec.io.regEnable := fire

  /**
   * [[vaesKeySchedule]]'s in connection
   */
  vaesKeySchedule.io.round      := vs1(3, 0) // immediate at src0 TODO: confirm this source signal
  vaesKeySchedule.io.currRndKey := vs2
  vaesKeySchedule.io.prevRndKey := oldVd
  vaesKeySchedule.io.isKf2      := fuOpType(0)
  vaesKeySchedule.io.regEnable  := fire

  resultData := Mux(
    isAesEncDec_1s,
    vaesEncDec.io.result,
    vaesKeySchedule.io.result
  )

  /**
   * First, specify the output's tailing bits.
   * An operation is done with several parameters: [[sew]]==32
   * [[outVecCtrl.vta]] controls the output's tailing bits.
   *   If vta==0, the output's tailing bits must be the same as the input's.
   *   If vta==1, the output's tailing bits can be any value.
   * [[outVecCtrl.vuopIdx]] is the operation's index.
   *   Since these operatiosn has [[sew]]==32, so we need to specify the tail
   */
  private val numBytes = dataWidth / 8
  private val maskTailGen = Module(new ByteMaskTailGen(dataWidth))

  maskTailGen.io.in.begin := 0.U // consider vstart as 0
  maskTailGen.io.in.end := outVl // TODO: confirm this source signal
  maskTailGen.io.in.vma := false.B // no masks
  maskTailGen.io.in.vta := outVecCtrl.vta // TODO: confirm this source signal
  maskTailGen.io.in.vsew := sew
  maskTailGen.io.in.maskUsed := ~(0.U(numBytes.W)) // no masks
  maskTailGen.io.in.vdIdx := outVecCtrl.vuopIdx // TODO: confirm this source signal

  private val activeEn = maskTailGen.io.out.activeEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  private val byte1s: UInt = (~0.U(8.W)).asUInt
  private val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVecByte = resultData.asTypeOf(resVecByte)
  private val oldVdVecByte = outOldVd.asTypeOf(resVecByte)

  /**
   * result = if (active) vd elif (agnostic) 0xff else oldVd
   */
  for (i <- 0 until numBytes) {
    resVecByte(i) := MuxCase(oldVdVecByte(i), Seq(
      activeEn(i) -> vdVecByte(i),
      agnosticEn(i) -> byte1s,
    ))
  }

  /** Connect the output */
  io.out.bits.res.data := resVecByte.asUInt
}


class VcryptoPiped5(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VcryppType.dummy, "Vcrypp OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 128
  private val numVecModule = dataWidth / dataWidthOfDataModule
  require(numVecModule == 1, "SM4 module only supports 128-bit data width")

  // io alias
  // def vsm4k_vi  = "b00010000".U(OpTypeWidth.W) // Vector SM4 KeyExpansion
  // def vsm4r_vv  = "b00010010".U(OpTypeWidth.W) // Vector SM4 Rounds vector-vector
  // def vsm4r_vs  = "b00010011".U(OpTypeWidth.W) // Vector SM4 Rounds vector-scalar
  private val isCipher = true.B
  private val isAes = false.B
  private val isSM4R = VecInit(
    VcryppType.vsm4r_vv, VcryppType.vsm4r_vs
  ).contains(fuOpType)

  private val sew = 2.U(2.W) // SEW==32. Note it's used at 2nd cycle. TODO: modified later
  private val sewVal = 32
  //  private val illegalSew = true.B // TODO: modify it to "sew is not 32.U"

  val fire = io.in.valid

  // modules
  private val sm4 = Module(new ShangMi4)
  private val resultData = Wire(UInt(dataWidthOfDataModule.W))

  /**
   * [[sm4]]'s in connection
   */
  sm4.io.roundGroup   := vs1(2, 0)
  sm4.io.roundKeys    := vs2
  sm4.io.state        := oldVd
  sm4.io.isSM4R       := isSM4R
  sm4.io.regEnable    := fire

  resultData := sm4.io.result

  /**
   * First, specify the output's tailing bits.
   * An operation is done with several parameters: [[sew]]==32
   * [[outVecCtrl.vta]] controls the output's tailing bits.
   *   If vta==0, the output's tailing bits must be the same as the input's.
   *   If vta==1, the output's tailing bits can be any value.
   * [[outVecCtrl.vuopIdx]] is the operation's index.
   *   Since these operatiosn has [[sew]]==32, so we need to specify the tail
   */
  private val numBytes = dataWidth / 8
  private val maskTailGen = Module(new ByteMaskTailGen(dataWidth))

  maskTailGen.io.in.begin := 0.U // consider vstart as 0
  maskTailGen.io.in.end := outVl // TODO: confirm this source signal
  maskTailGen.io.in.vma := false.B // no masks
  maskTailGen.io.in.vta := outVecCtrl.vta // TODO: confirm this source signal
  maskTailGen.io.in.vsew := sew
  maskTailGen.io.in.maskUsed := ~(0.U(numBytes.W)) // no masks
  maskTailGen.io.in.vdIdx := outVecCtrl.vuopIdx // TODO: confirm this source signal

  private val activeEn = maskTailGen.io.out.activeEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  private val byte1s: UInt = (~0.U(8.W)).asUInt
  private val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  private val vdVecByte = resultData.asTypeOf(resVecByte)
  private val oldVdVecByte = outOldVd.asTypeOf(resVecByte)

  /**
   * result = if (active) vd elif (agnostic) 0xff else oldVd
   */
  for (i <- 0 until numBytes) {
    resVecByte(i) := MuxCase(oldVdVecByte(i), Seq(
      activeEn(i) -> vdVecByte(i),
      agnosticEn(i) -> byte1s,
    ))
  }

  /** Connect the output */
  io.out.bits.res.data := resVecByte.asUInt
}


class AES128EncryptDecryptIO() extends Bundle {
  val state     = Input(UInt(128.W))
  val rkey      = Input(UInt(128.W))
  val op        = Input(UInt(8.W)) // 00: dec middle, 01: dec final, 10: enc middle, 11: enc final
  val regEnable = Input(Bool())
  val result    = Output(UInt(128.W))
}


/**
 * AES-128 excryption/decryption module for vector unit
 * @param p
 */
class AES128EncryptDecrypt(implicit p: Parameters) extends XSModule {
  val io = IO(new AES128EncryptDecryptIO)

  val state        = io.state
  val rkey         = io.rkey
  val isFinalRound = io.op(0)
  val isFwd        = io.op(1)
  val isZeroRound  = io.op(2)
  val regEnable    = io.regEnable

  val decFinal  = Wire(UInt(128.W)) // 2nd cycle
  val decMiddle = Wire(UInt(128.W)) // 2nd cycle
  val encFinal  = Wire(UInt(128.W)) // 2nd cycle
  val encMiddle = Wire(UInt(128.W)) // 2nd cycle, NOTE: also used for vaesz.vs

  val srDec = Wire(UInt(128.W))
  val srEnc = Wire(UInt(128.W))
  val ark   = Wire(UInt(128.W)) // 2nd cycle

  val isFwd_s1         = RegEnable(isFwd, io.regEnable)
  val state_s1         = RegEnable(state, io.regEnable)
  val isZeroRound_s1   = RegEnable(isZeroRound, io.regEnable)
  val rkey_s1          = RegEnable(rkey, io.regEnable)
  val isFinalRound_s1  = RegEnable(isFinalRound, io.regEnable)

  // sub-bytes
  val sbox = Module(new AES128SubBytesBidirection)
  sbox.io.src         := Mux(isFwd, srEnc, srDec)
  sbox.io.isFwd       := isFwd
  sbox.io.isFwd_s1    := isFwd_s1
  sbox.io.regEnable   := regEnable
  val sb = sbox.io.result

  // decryption final round
  srDec := AES128ShiftRowsInv(state)
  ark := sb ^ rkey_s1
  decFinal := ark

  // decryption middle round
  decMiddle := Cat((0 until 4).map(i =>
    MixInv((4 * i until 4 * (i + 1)).map(j =>
      ark(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  )
  XLEN.W

  // encryption final round
  srEnc := AES128ShiftRowsFwd(state) // since shift-rows and sub-bytes commute
  encFinal := ark

  // encryption middle round
  val mixEncMiddle = Cat((0 until 4).map(i =>
    MixFwd((4 * i until 4 * (i + 1)).map(j =>
      sb(8 * (j + 1) - 1, 8 * j)
    ))).reverse
  ) // 2nd cycle
  encMiddle := Mux(isZeroRound_s1, state_s1, mixEncMiddle) ^ rkey_s1

  io.result := Mux(
    isFinalRound_s1,
    Mux(isFwd_s1, encFinal, decFinal),
    Mux(isFwd_s1, encMiddle, decMiddle)
  )
}


class AES128KeyScheduleIO extends Bundle {
  val round      = Input(UInt(4.W))
  val currRndKey = Input(UInt(128.W)) // current round key
  val prevRndKey = Input(UInt(128.W)) // previous round key
  val regEnable  = Input(Bool())
  val isKf2      = Input(Bool())
  val result     = Output(UInt(128.W))
}


class AES128KeySchedule(implicit p: Parameters) extends XSModule {
  val io = IO(new AES128KeyScheduleIO)
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

  private val rndLegal_s1 = RegEnable(Mux(isKf2, rndLegal2, rndLegal1), io.regEnable)
  private val xorRndKeyBytes_s1 = RegEnable(Mux(isKf2, prevRndKeyBytes, currRndKeyBytes), io.regEnable)
  private val isKf2OddRnd_s1 = RegEnable(isKf2OddRnd, io.regEnable)
  private val subBytesSrc = Mux(
    isKf2OddRnd,
    currRndKeyBytes(3),
    AESRotword(currRndKeyBytes(3))
  )
  private val decodeRcon = Wire(UInt(32.W))
  private val decodeRconTmp = Wire(UInt(32.W))
  decodeRcon := AESDecodeRcon(rndLegal_s1)
  decodeRconTmp := Mux(isKf2OddRnd_s1, 0.U(32.W), decodeRcon)
  dontTouch(decodeRcon)
  dontTouch(decodeRconTmp)

  resWords(0) := AES32SubBytesFwd(subBytesSrc, io.regEnable) ^
                 decodeRconTmp ^
                 xorRndKeyBytes_s1(0)
  resWords(1) := resWords(0) ^ xorRndKeyBytes_s1(1)
  resWords(2) := resWords(1) ^ xorRndKeyBytes_s1(2)
  resWords(3) := resWords(2) ^ xorRndKeyBytes_s1(3)

  io.result := Cat(resWords.reverse)
}


class GHashIO extends Bundle {
  val partialHash = Input(UInt(128.W)) // vd
  val CipherText  = Input(UInt(128.W)) // vs1
  val HashSubkey  = Input(UInt(128.W)) // vs2
  val regEnable   = Input(Bool())
  val isAddMul    = Input(Bool())
  val result      = Output(UInt(128.W))
}


class GHash(implicit p: Parameters) extends XSModule {
  val io = IO(new GHashIO)
  private val y = io.partialHash
  private val x = io.CipherText
  private val h = io.HashSubkey
  private val regEnable = io.regEnable
  private val isAddMul = io.isAddMul

  private val z = Wire(UInt(128.W))
  private val s = Cat(Brev8(y ^ x).reverse)
  for (bit <- 0 until 128) {
    when(s(bit)) {
      z := z ^ h
    }
    val reduce = h(127)
    h := h << 1
    when(reduce) {
      h := h ^ 0x87.U
    }
  }

  //   let Y = (get_velem(vd,EGW=128,i)); // current partial-hash
  //   let X = get_velem(vs1,EGW=128,i); // block cipher output
  //   let H = brev8(get_velem(vs2,EGW=128,i)); // Hash subkey
  //   let Z : bits(128) = 0;
  //   let S = brev8(Y ^ X);
  //   for (int bit = 0; bit < 128; bit++) {
  //     if bit_to_bool(S[bit])
  //     Z ^= H
  //     bool reduce = bit_to_bool(H[127]);
  //     H = H << 1; // left shift H by 1
  //     if (reduce)
  //     H ^= 0x87; // Reduce using x^7 + x^2 + x^1 + 1 polynomial
  //     }
  //   let result = brev8(Z); // bit reverse bytes to get back to GCM standard ordering
  //   set_velem(vd, EGW=128, i, result);


  io.result := 0.U
}


class ShangMi4IO extends Bundle {
  val roundGroup = Input(UInt(3.W)) // imm
  val roundKeys  = Input(UInt(128.W)) // vs2
  val state      = Input(UInt(128.W)) // vd
  val isSM4R     = Input(Bool())
  val regEnable  = Input(Bool())
  val result     = Output(UInt(128.W))
}


class ShangMi4(implicit p: Parameters) extends XSModule {
  val io = IO(new ShangMi4IO)


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
  val regEnable_s = Wire(Vec(4, Bool()))
  val isSM4R_s    = Wire(Vec(5, Bool()))
  val rk_s        = Wire(Vec(5, Vec(8, UInt(32.W))))
  val x_s         = Wire(Vec(5, Vec(8, UInt(32.W))))
  val rnd_s       = Wire(Vec(4, UInt(3.W)))

  regEnable_s(0) := io.regEnable
  for (lat <- 0 until 3) {
    regEnable_s(lat + 1) := GatedValidRegNext(regEnable_s(lat))
  }

  isSM4R_s(0) := io.isSM4R
  for (lat <- 0 until 4) {
    isSM4R_s(lat + 1) := RegEnable(isSM4R_s(lat), regEnable_s(lat))
    // connection for old values, NOTE: BY DEFAULT
    for (i <- 0 until 8) {
      rk_s(lat + 1)(i) := RegEnable(rk_s(lat)(i), regEnable_s(lat))
      x_s (lat + 1)(i) := RegEnable(x_s (lat)(i), regEnable_s(lat))
    }
  }
  rnd_s(0) := io.roundGroup(2, 0)
  for (lat <- 0 until 3) {
    rnd_s(lat + 1) := RegEnable(rnd_s(lat), regEnable_s(lat))
  }

  //  let B : bits(32) = 0;
  //  let S : bits(32) = 0;
  //  let rk4 : bits(32) = 0;
  //  let rk5 : bits(32) = 0;
  //  let rk6 : bits(32) = 0;
  //  let rk7 : bits(32) = 0;
  //  let rnd : bits(3) = uimm[2:0]; // Lower 3 bits

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
  s(0) := SM4Subword(b(0), regEnable_s(0))

  rk_s(1)(4) := SM4RoundKey(rk_s(1)(0), s(0))
  x_s (1)(4) := SM4Round   (x_s (1)(0), s(0))
  b(1) := Mux(
    isSM4R_s(1),
    x_s (1)(2) ^ x_s (1)(3) ^ x_s (1)(4) ^ rk_s(1)(1),
    rk_s(1)(2) ^ rk_s(1)(3) ^ rk_s(1)(4) ^ ck(Cat(rnd_s(1), 1.U(2.W)))
  )
  s(1) := SM4Subword(b(1), regEnable_s(1))

  rk_s(2)(5) := SM4RoundKey(rk_s(2)(1), s(1))
  x_s (2)(5) := SM4Round   (x_s (2)(1), s(1))
  b(2) := Mux(
    isSM4R_s(2),
    x_s (2)(3) ^ x_s (2)(4) ^ x_s (2)(5) ^ rk_s(2)(2),
    rk_s(2)(3) ^ rk_s(2)(4) ^ rk_s(2)(5) ^ ck(Cat(rnd_s(2), 2.U(2.W)))
  )
  s(2) := SM4Subword(b(2), regEnable_s(2))

  rk_s(3)(6) := SM4RoundKey(rk_s(3)(2), s(2))
  x_s (3)(6) := SM4Round   (x_s (3)(2), s(2))
  b(3) := Mux(
    isSM4R_s(3),
    x_s (3)(4) ^ x_s (3)(5) ^ x_s (3)(6) ^ rk_s(3)(3),
    rk_s(3)(4) ^ rk_s(3)(5) ^ rk_s(3)(6) ^ ck(Cat(rnd_s(3), 3.U(2.W)))
  )
  s(3) := SM4Subword(b(3), regEnable_s(3))

  rk_s(4)(7) := SM4RoundKey(rk_s(4)(3), s(3))
  x_s (4)(7) := SM4Round   (x_s (4)(3), s(3))
  io.result := Mux(
    isSM4R_s(4),
    Cat(x_s (4)(7), x_s (4)(6), x_s (4)(5), x_s (4)(4)),
    Cat(rk_s(4)(7), rk_s(4)(6), rk_s(4)(5), rk_s(4)(4))
  )
}
