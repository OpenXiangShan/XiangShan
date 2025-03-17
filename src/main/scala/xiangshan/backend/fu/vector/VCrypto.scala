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
import chisel3.util.experimental.decode.TruthTable
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import utility._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.util._
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import xiangshan.backend.fu.vector.utils._
import xiangshan.backend.fu.wrapper.VectorCvtTop
import yunsuan.VcryppType
import yunsuan.util.{GatedValidRegNext, LookupTree}
import yunsuan.vector.SewOH


/**
 * Pipelined vector crypto unit
 * @param cfg configuration
 */
class VcryptoPiped(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VcryppType.dummy, "Vfcvt OpType not supported")

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
  val fireReg = GatedValidRegNext(fire)

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


class SM4IO extends Bundle {
  val roundGroup    = Input(UInt(3.W)) // imm
  val curr4RoundKey = Input(UInt(128.W)) // vs2
  val regEnable     = Input(Bool())
  val op            = Input(UInt(8.W))
  val result        = Output(UInt(128.W))
}


class SM4(implicit p: Parameters) extends XSModule {
  val io = IO(new SM4IO)

  // SM4 Constant Key (CK)
  private val ck = Wire(Vec(32, UInt(32.W)))
  ck := VecInit(
    0x00070E15.U, 0x1C232A31.U, 0x383F464D.U, 0x545B6269.U,
    0x70777E85.U, 0x8C939AA1.U, 0xA8AFB6BD.U, 0xC4CBD2D9.U,
    0xE0E7EEF5.U, 0xFC030A11.U, 0x181F262D.U, 0x343B4249.U,
    0x50575E65.U, 0x6C737A81.U, 0x888F969D.U, 0xA4ABB2B9.U,
    0xC0C7CED5.U, 0xDCE3EAF1.U, 0xF8FF060D.U, 0x141B2229.U,
    0x30373E45.U, 0x4C535A61.U, 0x686F767D.U, 0x848B9299.U,
    0xA0A7AEB5.U, 0xBCC3CAD1.U, 0xD8DFE6ED.U, 0xF4FB0209.U,
    0x10171E25.U, 0x2C333A41.U, 0x484F565D.U, 0x646B7279.U
  )

  val b = Wire(Vec(4, UInt(32.W)))
  val s = Wire(Vec(4, UInt(32.W)))
  val rk = Wire(Vec(8, UInt(32.W)))
  val rnd = Wire(UInt(3.W))
  rnd := io.roundGroup(2, 0)
  //  let B : bits(32) = 0;
  //  let S : bits(32) = 0;
  //  let rk4 : bits(32) = 0;
  //  let rk5 : bits(32) = 0;
  //  let rk6 : bits(32) = 0;
  //  let rk7 : bits(32) = 0;
  //  let rnd : bits(3) = uimm[2:0]; // Lower 3 bits


  for (i <- 0 until 4) {
    rk(i) := io.curr4RoundKey(32 * (i + 1) - 1, 32 * i)
  }
  b(0) := rk(1) ^ rk(2) ^ rk(3) ^ ck(4 * rnd)
    s(0) := SM4Subword(b(0))


  //  foreach (i from eg_start to eg_len-1) {
//    let (rk3 @ rk2 @ rk1 @ rk0) : bits(128) = get_velem(vs2, 128, i);
//    B = rk1 ^ rk2 ^ rk3 ^ ck(4 * rnd);
//    S = sm4_subword(B);
//    rk4 = ROUND_KEY(rk0, S);
//    B = rk2 ^ rk3 ^ rk4 ^ ck(4 * rnd + 1);
//    S = sm4_subword(B);
//    rk5 = ROUND_KEY(rk1, S);
//    B = rk3 ^ rk4 ^ rk5 ^ ck(4 * rnd + 2);
//    S = sm4_subword(B);
//    rk6 = ROUND_KEY(rk2, S);
//    B = rk4 ^ rk5 ^ rk6 ^ ck(4 * rnd + 3);
//    S = sm4_subword(B);
//    rk7 = ROUND_KEY(rk3, S);
//    // Update the destination register.
//    set_velem(vd, EGW=128, i, (rk7 @ rk6 @ rk5 @ rk4));
//    }
//  RETIRE_SUCCESS
//  }
//}
//val round_key : bits(32) -> bits(32)
//function ROUND_KEY(X, S) = ((X) ^ ((S) ^ ROL32((S), 13) ^ ROL32((S), 23)))
//

  io.result := 0.U
}
