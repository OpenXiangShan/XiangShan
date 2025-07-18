// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.phr

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.BufferParams.pipe
import freechips.rocketchip.regmapper.RRTest0Map.re
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.XSModule
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.phr.PhrPtr
import xiangshan.frontend.ftq.FtqPtr

class Phr()(implicit p: Parameters) extends PhrModule with HasPhrParameters with Helpers {

  // PHR: Predicted History Register
  val io: PhrIO = IO(new PhrIO())

  private val phr = RegInit(0.U.asTypeOf(Vec(PhrBitsWidth, Bool())))
  // PHR train from redirct/s2_prediction/s3_prediction
  private val phrPtr = RegInit(0.U.asTypeOf(new PhrPtr))

  private val s0_stall = io.train.s0_stall
  private val s1_valid = io.train.s1_valid
  private val s0_fire  = io.train.stageCtrl.s0_fire
  private val s1_fire  = io.train.stageCtrl.s1_fire
  private val s2_fire  = io.train.stageCtrl.s2_fire
  private val s3_fire  = io.train.stageCtrl.s3_fire

  private val s0_phrPtr    = WireInit(0.U.asTypeOf(new PhrPtr))
  private val s0_phrPtrReg = RegEnable(s0_phrPtr, 0.U.asTypeOf(new PhrPtr), !s0_stall)
  private val s1_phrPtr    = RegEnable(s0_phrPtr, 0.U.asTypeOf(new PhrPtr), s0_fire)
  private val s2_phrPtr    = RegEnable(s1_phrPtr, 0.U.asTypeOf(new PhrPtr), s1_fire)
  private val s3_phrPtr    = RegEnable(s2_phrPtr, 0.U.asTypeOf(new PhrPtr), s2_fire)

  // phr folded history
  private val s0_foldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  private val s0_foldedPhrReg =
    RegEnable(s0_foldedPhr, 0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)), !s0_stall)
  private val s1_foldedPhrReg =
    RegEnable(s0_foldedPhr, 0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)), s0_fire)
  private val s2_foldedPhrReg =
    RegEnable(s1_foldedPhrReg, 0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)), s1_fire)
  private val s3_foldedPhrReg =
    RegEnable(s2_foldedPhrReg, 0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)), s2_fire)

  private val oldFh    = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))
  private val updateFh = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(TageFoldedGHistInfos)))

  private val redirectData    = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val s1_overrideData = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val s3_override     = WireInit(false.B)
  private val s3_overrideData = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val updateData      = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val updatePc        = WireInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val updateOverride  = WireInit(false.B)
  private val redirctPhr      = WireInit(0.U(PhrBitsWidth.W))

  redirectData.valid  := io.train.redirectValid
  redirectData.taken  := io.train.redirectTaken
  redirectData.pc     := io.train.redirectPc
  redirectData.phrPtr := io.train.redirectPhrPtr

  s3_override               := io.train.s3_override
  s3_overrideData.valid     := s3_override
  s3_overrideData.taken     := io.train.s3_taken
  s3_overrideData.pc        := io.train.s3_pc
  s3_overrideData.phrPtr    := s3_phrPtr
  s3_overrideData.foldedPhr := s3_foldedPhrReg

  s1_overrideData.valid     := s1_valid
  s1_overrideData.taken     := io.train.s1_taken
  s1_overrideData.pc        := io.train.s1_pc
  s1_overrideData.phrPtr    := s1_phrPtr
  s1_overrideData.foldedPhr := s1_foldedPhrReg

  updatePc       := updateData.pc
  updateOverride := redirectData.valid || s3_override

  updateData := MuxCase(
    0.U.asTypeOf(new PhrUpdateData),
    Seq(
      redirectData.valid -> redirectData,
      s3_override        -> s3_overrideData,
      s1_valid           -> s1_overrideData
    )
  )
  private val shiftBits =
    (((updatePc >> 1) ^ (updatePc >> 3)) ^ ((updatePc >> 5) ^ (updatePc >> 7)))(Shamt - 1, 0)

  private def getPhr(ptr: PhrPtr): UInt =
    (Cat(phr.asUInt, phr.asUInt) >> ptr.value + 1.U)(PhrBitsWidth - 1, 0)

  when(updateData.valid) {
    when(updateData.taken) {
      phr(updateData.phrPtr.value - 1.U) := shiftBits(1)
      phr(updateData.phrPtr.value - 2.U) := shiftBits(0)
      phrPtr                             := updateData.phrPtr - 2.U
    }.elsewhen(updateOverride) {
      phrPtr := updateData.phrPtr
    }
  }

  s0_phrPtr       := phrPtr
  io.phrPtr       := phrPtr
  io.phrs         := getPhr(phrPtr).asTypeOf(Vec(PhrBitsWidth, Bool()))
  io.s0_foldedPhr := s0_foldedPhr
  io.s1_foldedPhr := s1_foldedPhrReg
  io.s2_foldedPhr := s2_foldedPhrReg
  io.s3_foldedPhr := s3_foldedPhrReg

  TageFoldedGHistInfos.foreach { case (histLen, compLen) =>
    s0_foldedPhr.getHistWithInfo((histLen, compLen)).foldedHist := computeFoldedHist(phr.asUInt, compLen)(
      histLen
    )
  }

  when(redirectData.valid) {
    redirctPhr := getPhr(redirectData.phrPtr)
    TageFoldedGHistInfos.foreach { case (histLen, compLen) =>
      redirectData.foldedPhr.getHistWithInfo((histLen, compLen)).foldedHist := computeFoldedHist(redirctPhr, compLen)(
        histLen
      )
    }
    s0_foldedPhr := redirectData.foldedPhr
    when(redirectData.taken) {
      s0_foldedPhr := redirectData.foldedPhr.update(VecInit(redirctPhr.asBools), redirectData.phrPtr, Shamt, shiftBits)
    }
  }.elsewhen(s3_override) {
    s0_foldedPhr := s3_foldedPhrReg
    when(s3_overrideData.taken) {
      s0_foldedPhr := s3_foldedPhrReg.update(VecInit(getPhr(s3_phrPtr).asBools), s3_phrPtr, Shamt, shiftBits)
    }
  }.otherwise {
    s0_foldedPhr := s1_foldedPhrReg
  }

  s0_foldedPhr := Mux(
    updateData.valid,
    updateData.foldedPhr.update(VecInit(getPhr(updateData.phrPtr).asBools), updateData.phrPtr, Shamt, shiftBits),
    s0_foldedPhrReg
  )

  // TODO: remove dontTouch
  dontTouch(s0_foldedPhr)
  dontTouch(s1_foldedPhrReg)
  dontTouch(s2_foldedPhrReg)

}
