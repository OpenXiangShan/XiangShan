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
//  val test_28Enc = Module(new AES128EncryptDecrypt0Lat)
//  test_28Enc.io.state := 0.U
//  test_28Enc.io.rkey := 0.U
//  test_28Enc.io.op := 0.U
//  val sdffds = test_28Enc.io.result
//  dontTouch(sdffds)
//
//  val test_28Key = Module(new AES128KeySchedule0Lat)
//  test_28Key.io.round := 0.U
//  test_28Key.io.currRndKey := 0.U
//  test_28Key.io.prevRndKey := 0.U
//  test_28Key.io.isKf2 := false.B
//  val sdfuys = test_28Key.io.result
//  dontTouch(sdfuys)
//
//  val test_hangM = Module(new VecShangMi40Lat)
//  test_hangM.io.roundGroup := 0.U
//  test_hangM.io.roundKeys := 0.U
//  test_hangM.io.state := 0.U
//  test_hangM.io.isSM4R := false.B
//  val sdfds = test_hangM.io.result
//  dontTouch(sdfds)
//
//  val test_HACom = Module(new VecSHACompress0Lat)
//  test_HACom.io.curr := (0.U, 0.U, 0.U, 0.U, 0.U, 0.U, 0.U, 0.U)
//  test_HACom.io.messageSchedPlusC(0) := 0.U
//  test_HACom.io.messageSchedPlusC(1) := 0.U
//  test_HACom.io.messageSchedPlusC(2) := 0.U
//  test_HACom.io.messageSchedPlusC(3) := 0.U
//  test_HACom.io.isVSHA2cl := false.B
//  val sdfs = test_HACom.io.next
//  dontTouch(sdfs)
//
//  val test_HAMes = Module(new VecSHAMessageWords0Lat)
//  test_HAMes.io.wordsLow := (0.U(128.W)).asTypeOf(Vec(4, UInt(32.W)))
//  test_HAMes.io.wordsMid := (0.U(128.W)).asTypeOf(Vec(4, UInt(32.W)))
//  test_HAMes.io.wordsHigh := (0.U(128.W)).asTypeOf(Vec(4, UInt(32.W)))
//  val werwer = test_HAMes.io.out
//  dontTouch(werwer)

  // generate verilog of each module, and output to files


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

class VcryptoNonPiped(cfg: FuConfig)(implicit p: Parameters) extends VecNonPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VcryppType.dummy, "Vcrypp OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 128
  private val numVecModule = dataWidth / dataWidthOfDataModule
  require(numVecModule == 1, "SHA-256 module only supports 128-bit data width")

  // io alias
  // def vsha2ch_vv = "b00100000".U(OpTypeWidth.W) // Vector SM4 Rounds vector-vector
  // def vsha2cl_vv = "b00100001".U(OpTypeWidth.W) // Vector SM4 Rounds vector-scalar
  // def vsha2ms_vv = "b00100010".U(OpTypeWidth.W) // Vector SM4 KeyExpansion

  private val sew = 2.U(2.W) // SEW==32. Note it's used at 2nd cycle. TODO: modified later
  private val sewVal = 32
  //  private val illegalSew = true.B // TODO: modify it to "sew is not 32.U"

  val fire = io.in.valid

  // modules
  private val sha2 = Module(new VecSHA2)
  private val resultData = Wire(UInt(dataWidthOfDataModule.W))
  private val resultDataReg = Reg(UInt(dataWidthOfDataModule.W))
  private val outNotFired = RegInit(false.B)

  /**
   * [[sha2]]'s in connection
   */
  sha2.io.in.bits.vs1Val    := vs1
  sha2.io.in.bits.vs2Val    := vs2
  sha2.io.in.bits.oldVdVal  := oldVd
  sha2.io.in.bits.op        := fuOpType(1, 0)
  sha2.io.in.valid := io.in.valid
  io.in.ready := sha2.io.in.ready && !outNotFired

  when (io.out.fire) {
    outNotFired := false.B
  }.elsewhen (io.out.valid && !io.out.ready) {
    outNotFired := true.B
  }
  when (io.out.valid) {
    resultDataReg := resultData
  }

  resultData := Mux(outNotFired, resultDataReg, sha2.io.out.bits)
  io.out.valid := outNotFired || sha2.io.out.valid


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
  private val sm4 = Module(new VecShangMi4)
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
  val sbox = Module(new AES128SubBytesShare)
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


//class VecGHashIO extends Bundle {
//  val partialHash = Input(UInt(128.W)) // vd
//  val CipherText  = Input(UInt(128.W)) // vs1
//  val HashSubkey  = Input(UInt(128.W)) // vs2
//  val regEnable   = Input(Bool())
//  val isAddMul    = Input(Bool())
//  val result      = Output(UInt(128.W))
//}
//
//
//class VecGHash(implicit p: Parameters) extends XSModule {
//  val io = IO(new VecGHashIO)
//  private val y = io.partialHash
//  private val x = io.CipherText
//  private val h = io.HashSubkey
//  private val regEnable = io.regEnable
//  private val isAddMul = io.isAddMul
//
//  private val z = Wire(UInt(128.W))
//  private val s = Cat(Brev8(y ^ x).reverse)
//  for (bit <- 0 until 128) {
//    when(s(bit)) {
//      z := z ^ h
//    }
//    val reduce = h(127)
//    h := h << 1
//    when(reduce) {
//      h := h ^ 0x87.U
//    }
//  }
//
//  //   let Y = (get_velem(vd,EGW=128,i)); // current partial-hash
//  //   let X = get_velem(vs1,EGW=128,i); // block cipher output
//  //   let H = brev8(get_velem(vs2,EGW=128,i)); // Hash subkey
//  //   let Z : bits(128) = 0;
//  //   let S = brev8(Y ^ X);
//  //   for (int bit = 0; bit < 128; bit++) {
//  //     if bit_to_bool(S[bit])
//  //     Z ^= H
//  //     bool reduce = bit_to_bool(H[127]);
//  //     H = H << 1; // left shift H by 1
//  //     if (reduce)
//  //     H ^= 0x87; // Reduce using x^7 + x^2 + x^1 + 1 polynomial
//  //     }
//  //   let result = brev8(Z); // bit reverse bytes to get back to GCM standard ordering
//  //   set_velem(vd, EGW=128, i, result);
//
//
//  io.result := 0.U
//}


class VecShangMi4IO extends Bundle {
  val roundGroup = Input(UInt(3.W)) // imm
  val roundKeys  = Input(UInt(128.W)) // vs2
  val state      = Input(UInt(128.W)) // vd
  val isSM4R     = Input(Bool())
  val regEnable  = Input(Bool())
  val result     = Output(UInt(128.W))
}


class VecShangMi4(implicit p: Parameters) extends XSModule {
  val io = IO(new VecShangMi4IO)


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

class VecSHAState extends Bundle {
  val abef = Vec(4, UInt(32.W))
  val cdgh = Vec(4, UInt(32.W))

  def a: UInt = abef(3)
  def b: UInt = abef(2)
  def e: UInt = abef(1)
  def f: UInt = abef(0)

  def c: UInt = cdgh(3)
  def d: UInt = cdgh(2)
  def g: UInt = cdgh(1)
  def h: UInt = cdgh(0)

  def :=(that: VecSHAState): Unit = {
    this.abef := that.abef
    this.cdgh := that.cdgh
  }

  def :=(abef: Vec[UInt], cdgh: Vec[UInt]): Unit = {
    this.abef := abef
    this.cdgh := cdgh
  }

  def :=(a: UInt, b: UInt, c: UInt, d: UInt, e: UInt, f: UInt, g: UInt, h: UInt): Unit = {
    this.abef := VecInit(f, e, b, a)
    this.cdgh := VecInit(h, g, d, c)
  }
}

class VecSHAComp1StepIO(implicit p: Parameters) extends XSBundle {
  val oldStates = Input(new VecSHAState)
  val word = Input(UInt(32.W))

  val newStates = Output(new VecSHAState)
}

/**
 * 1 step of SHA-256 compression
 */
class VecSHAComp1Step(implicit  p: Parameters) extends XSModule {
  private def ch(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ ((~x).asUInt & z)
  private def maj(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ (x & z) ^ (y & z)

  val io = IO(new VecSHAComp1StepIO)
  val old = io.oldStates
  val word = io.word

  val tmp1 = Wire(UInt(32.W))
  val tmp2 = Wire(UInt(32.W))
  tmp1 := old.h + SHA2.sha256sum1(old.e) + ch(old.e, old.f, old.g) + word
  tmp2 := SHA2.sha256sum0(old.a) + maj(old.a, old.b, old.c)

  io.newStates := (tmp1 + tmp2, old.a, old.b, old.c, old.d + tmp1, old.e, old.f, old.g)
}

class VecSHACompressInput(implicit p: Parameters) extends XSBundle {
  val curr = new VecSHAState
  val messageSchedPlusC = Vec(4, UInt(32.W))
  val isVSHA2cl = Bool()
}

class VecSHACompressIO(implicit p: Parameters) extends XSBundle {
  val in = Flipped(DecoupledIO(new VecSHACompressInput))
  val out = ValidIO(new VecSHAState)
}

class VecSHACompress(implicit p: Parameters) extends XSModule {
  private val SEW = 32 // TODO: if modification is needed, change arguments below
  private val latency = 2
  val io = IO(new VecSHACompressIO)


  val regEnable_s = Wire(Vec(latency + 1, Bool()))
  val occupied = (1 until regEnable_s.length).map(regEnable_s(_)).reduce(_ || _)
  regEnable_s(0) := Mux(occupied, false.B, io.in.valid)
  for (i <- 0 until regEnable_s.length - 1) {
    regEnable_s(i + 1) := GatedValidRegNext(regEnable_s(i))
  }

  io.in.ready := !occupied
  io.out.valid := regEnable_s.last

  val comp1Step = Module(new VecSHAComp1Step)

  val oldStates = Wire(new VecSHAState)
  val word      = Wire(UInt(32.W))
  val newStates = Wire(new VecSHAState)

  val mid = Reg(new VecSHAState)
  val word0 = Wire(UInt(32.W))
  val word1 = Reg(UInt(32.W))

  val result = Reg(new VecSHAState)

  word0 := Mux(io.in.bits.isVSHA2cl, io.in.bits.messageSchedPlusC(0), io.in.bits.messageSchedPlusC(2))
  when (regEnable_s(0)) {
    mid := newStates
    word1 := Mux(io.in.bits.isVSHA2cl, io.in.bits.messageSchedPlusC(1), io.in.bits.messageSchedPlusC(3))
  }

  when (regEnable_s(1)) {
    result := newStates
  }

  oldStates := Mux(regEnable_s(0), io.in.bits.curr, mid)
  word := Mux(regEnable_s(0), word0, word1)

  comp1Step.io.oldStates := oldStates
  comp1Step.io.word      := word
  comp1Step.io.newStates <> newStates

  io.out.bits := result
}

class VecSHAMess1StepIO(implicit p: Parameters) extends XSBundle {
  val sig0AndNormalAdders = Input(Vec(3, UInt(32.W)))
  val normalAdders        = Input(Vec(2, UInt(32.W)))
  val sig1Adders          = Input(Vec(2, UInt(32.W)))

  val out = Output(Vec(2, UInt(32.W)))
}

class VecSHAMess1Step(implicit p: Parameters) extends XSModule {
  val io = IO(new VecSHAMess1StepIO)
  private val sig0s = io.sig0AndNormalAdders
  private val norms = io.normalAdders
  private val sig1s = io.sig1Adders

  io.out(0) := SHA2.sha256sig1(sig1s(0)) + norms(0) + SHA2.sha256sig0(sig0s(1)) + sig0s(0)
  io.out(1) := SHA2.sha256sig1(sig1s(1)) + norms(1) + SHA2.sha256sig0(sig0s(2)) + sig0s(1)
}

class VecSHAMessageWordsInput(implicit p: Parameters) extends XSBundle {
  val wordsLow = Vec(4, UInt(32.W))
  val wordsMid = Vec(4, UInt(32.W))
  val wordsHigh = Vec(4, UInt(32.W))
}

class VecSHAMessageWordsIO(implicit p: Parameters) extends XSBundle {
  val in  = Flipped(DecoupledIO(new VecSHAMessageWordsInput))
  val out = ValidIO(UInt(128.W))
}

class VecSHAMessageWords(implicit p: Parameters) extends XSModule {
  private val SEW = 32 // TODO: if modification is needed, change arguments below
  private val latency = 2
  val io = IO(new VecSHAMessageWordsIO)

  val wordsLow = io.in.bits.wordsLow
  val wordsMid = io.in.bits.wordsMid
  val wordsHigh = io.in.bits.wordsHigh

  /** w[5,6,7,8] are not used, w[16,17,18,19] are generated later */
  private val w = Wire(Vec(16, UInt(32.W)))
  w.foreach(w => w := 0.U) // initialize
  /**
   * Assign values to this message words.
   *   [[wordsLow]] {W[3] @ W[2] @ W[1] @ W[0]} from oldVd
   *   [[wordsMid]] {W[11] @ W[10] @ W[9] @ W[4]} from vs2
   *   [[wordsHigh]] {W[15] @ W[14] @ W[13] @ W[12]} from vs1
   */
  Seq( 0,  1,  2,  3).zipWithIndex.foreach(i => w(i._1) := wordsLow(i._2))
  Seq( 4,  9, 10, 11).zipWithIndex.foreach(i => w(i._1) := wordsMid(i._2))
  Seq(12, 13, 14, 15).zipWithIndex.foreach(i => w(i._1) := wordsHigh(i._2))


  val regEnable_s = Wire(Vec(latency + 1, Bool()))
  val occupied = (1 until regEnable_s.length).map(regEnable_s(_)).reduce(_ || _)
  regEnable_s(0) := Mux(occupied, false.B, io.in.valid)
  for (i <- 0 until regEnable_s.length - 1) {
    regEnable_s(i + 1) := GatedValidRegNext(regEnable_s(i))
  }

  io.in.ready := !occupied
  io.out.valid := regEnable_s.last
  val mess1Step = Module(new VecSHAMess1Step)

  // IO signals of mess1Step Module
  val sig0AndNormalAdders = Wire(Vec(3, UInt(32.W)))
  val sig1Adders          = Wire(Vec(2, UInt(32.W)))
  val normalAdders        = Wire(Vec(2, UInt(32.W)))
  val stepResult = Wire(Vec(2, UInt(32.W))) // output of each step

  // First step of calculation
  val sig0AndNormalAdders_0s = Wire(Vec(3, UInt(32.W)))
  val normalAdders_0s        = Wire(Vec(2, UInt(32.W)))
  val sig1Adders_0s          = Wire(Vec(2, UInt(32.W)))

  // Second step of calculation
  val sig0AndNormalAdders_1s = Reg(Vec(3, UInt(32.W)))
  val normalAdders_1s        = Reg(Vec(2, UInt(32.W)))
  val sig1Adders_1s          = Wire(Vec(2, UInt(32.W)))

  // First step calculation result after 1 cycle
  val stepResult_1s = Reg(Vec(2, UInt(32.W)))

  // Final result
  val finalResult = Reg(UInt(128.W))

  sig0AndNormalAdders_0s := VecInit(w(0), w(1), w(2))
  normalAdders_0s        := VecInit(w(9), w(10))
  sig1Adders_0s          := VecInit(w(14), w(15))
  when (regEnable_s(0)) {
    sig0AndNormalAdders_1s := VecInit(w(2), w(3), w(4))
    normalAdders_1s        := VecInit(w(11), w(12))
    stepResult_1s          := stepResult
  }
  sig1Adders_1s   := VecInit(stepResult_1s(0), stepResult_1s(1))

  when (regEnable_s(1)) {
    finalResult := Cat(stepResult(1), stepResult(0), stepResult_1s(1), stepResult_1s(0))
  }

  sig0AndNormalAdders := Mux(regEnable_s(0), sig0AndNormalAdders_0s, sig0AndNormalAdders_1s)
  sig1Adders          := Mux(regEnable_s(0), sig1Adders_0s         , sig1Adders_1s         )
  normalAdders        := Mux(regEnable_s(0), normalAdders_0s       , normalAdders_1s       )

  mess1Step.io.sig0AndNormalAdders := sig0AndNormalAdders
  mess1Step.io.normalAdders        := normalAdders
  mess1Step.io.sig1Adders          := sig1Adders
  mess1Step.io.out                 <> stepResult

  io.out.bits := finalResult
}

class VecSHA2Input(implicit p: Parameters) extends XSBundle {
  val vs1Val    = UInt(128.W) // vd
  val vs2Val    = UInt(128.W) // vs2
  val oldVdVal  = UInt(128.W) // oldVd
  val op        = UInt(2.W)   // fuOpType(1, 0)
}

class VecSHA2IO(implicit p: Parameters) extends XSBundle {
  val in  = Flipped(DecoupledIO(new VecSHA2Input))
  val out = ValidIO(UInt(128.W))
}

class VecSHA2(implicit p: Parameters) extends XSModule {
  // val latency = 2
  val io = IO(new VecSHA2IO)

  // def vsha2ch_vv: UInt = "b00100000".U(OpTypeWidth.W) // Vector SM4 Rounds vector-vector
  // def vsha2cl_vv: UInt = "b00100001".U(OpTypeWidth.W) // Vector SM4 Rounds vector-scalar
  // def vsha2ms_vv: UInt = "b00100010".U(OpTypeWidth.W) // Vector SM4 KeyExpansion

  val vs1AsWords = io.in.bits.vs1Val.asTypeOf(Vec(4, UInt(32.W)))
  val vs2AsWords = io.in.bits.vs2Val.asTypeOf(Vec(4, UInt(32.W)))
  val oldVdAsWords = io.in.bits.oldVdVal.asTypeOf(Vec(4, UInt(32.W)))

  val isVSHA2cl = io.in.bits.op(0)
  val isMessageSchedule = io.in.bits.op(1)

  // vsha2c[hl].vv: Vector SHA-2 two rounds of compression
  val vshaCompress = Module(new VecSHACompress)
  vshaCompress.io.in.bits.curr               := (vs2AsWords, oldVdAsWords)
  vshaCompress.io.in.bits.messageSchedPlusC  := vs1AsWords
  vshaCompress.io.in.bits.isVSHA2cl          := isVSHA2cl
  vshaCompress.io.in.valid                   := io.in.valid && !isMessageSchedule
  val vsha2cResult = Cat(vshaCompress.io.out.bits.abef.reverse)

  // vsha2ms.vv: Vector SHA-2 message schedule
  val vshaMessageWords = Module(new VecSHAMessageWords)
  vshaMessageWords.io.in.bits.wordsLow    := oldVdAsWords
  vshaMessageWords.io.in.bits.wordsMid    := vs2AsWords
  vshaMessageWords.io.in.bits.wordsHigh   := vs1AsWords
  vshaMessageWords.io.in.valid            := io.in.valid && isMessageSchedule
  val messageWordsHigh = vshaMessageWords.io.out.bits

  io.in.ready := vshaCompress.io.in.ready && vshaMessageWords.io.in.ready
  io.out.bits := Mux(vshaMessageWords.io.out.valid, messageWordsHigh, vsha2cResult)
  io.out.valid := vshaCompress.io.out.valid || vshaMessageWords.io.out.valid
}
