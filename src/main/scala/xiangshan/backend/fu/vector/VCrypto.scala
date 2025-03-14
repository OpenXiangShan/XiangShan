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

class AES128EncryptDecryptIO() extends Bundle {
  val state     = Input(UInt(128.W))
  val rkey      = Input(UInt(128.W))
  val op        = Input(UInt(2.W)) // 00: dec middle, 01: dec final, 10: enc middle, 11: enc final
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
  val isZeroRound  = io.op(2)
  val isFwd        = io.op(1)
  val isFinalRound = io.op(0)
  val regEnable    = io.regEnable

  val decFinal  = Wire(UInt(128.W)) // 2nd cycle
  val decMiddle = Wire(UInt(128.W)) // 2nd cycle
  val encFinal  = Wire(UInt(128.W)) // 2nd cycle
  val encMiddle = Wire(UInt(128.W)) // 2nd cycle

  val srDec = Wire(UInt(128.W))
  val srEnc = Wire(UInt(128.W))
  val ark   = Wire(UInt(128.W)) // 2nd cycle

  val isFwd_1s         = RegEnable(isFwd, io.regEnable)
  val rkey_1s          = RegEnable(rkey, io.regEnable)
  val isFinalRound_1s  = RegEnable(isFinalRound, io.regEnable)

  // sub-bytes
  val sbox = Module(new AES128SubBytesBidirection)
  sbox.io.src         := Mux(isFwd, srEnc, srDec)
  sbox.io.isFwd       := isFwd
  sbox.io.isZeroRound := isZeroRound
  sbox.io.isFwd_1s    := isFwd_1s
  sbox.io.regEnable   := regEnable
  val sb = sbox.io.result

  // decryption final round
  srDec := AES128ShiftRowsInv(state)
  ark := sb ^ rkey_1s
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
  encMiddle := mixEncMiddle ^ rkey_1s

  io.result := Mux(
    isFinalRound_1s,
    Mux(isFwd_1s, encFinal, decFinal),
    Mux(isFwd_1s, encMiddle, decMiddle)
  )
}


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
  // def vaeskf1   = "b00100000".U(OpTypeWidth.W)
  // def vaeskf2   = "b10100000".U(OpTypeWidth.W)
  // def vaesz_vs  = "b10001111".U(OpTypeWidth.W)
  private val isCipher = true.B
  private val isAes = true.B
  private val isAesEncDec = VecInit(
    VcryppType.vaesdf_vv, VcryppType.vaesdf_vs,
    VcryppType.vaesdm_vv, VcryppType.vaesdm_vs,
    VcryppType.vaesef_vv, VcryppType.vaesef_vs,
    VcryppType.vaesem_vv, VcryppType.vaesem_vs,
    VcryppType.vaesz_vs
  ).contains(fuOpType)

  private val sew = 2.U(2.W) // SEW==32. Note it's used at 2nd cycle. TODO: modified later
  private val sewVal = 32
//  private val illegalSew = true.B // TODO: modify it to "sew is not 32.U"

  val fire = io.in.valid
  val fireReg = GatedValidRegNext(fire)

  // modules
  private val vaes = Module(new AES128EncryptDecrypt)
  private val resultData = Wire(UInt(dataWidthOfDataModule.W))

  /**
   * [[vaes]]'s in connection
   */
//  private val isVs2Scalar = fuOpType(3)
//  private val scalarVs2 = Cat(Seq.fill(4)(vs2(sewVal - 1, 0)))(dataWidthOfDataModule - 1, 0)
  vaes.io.state := oldVd
//  vaes.io.rkey := Mux(isVs2Scalar, scalarVs2, vs2)
  vaes.io.rkey := vs2
  vaes.io.op := fuOpType(1, 0)
  vaes.io.regEnable := fire
  vaes.io.result <> resultData

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
