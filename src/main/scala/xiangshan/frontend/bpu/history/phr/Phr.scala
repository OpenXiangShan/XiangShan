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

package xiangshan.frontend.bpu.history.phr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.XSWarn
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BpuTrain

// PHR: Predicted History Register
class Phr(implicit p: Parameters) extends PhrModule with HasPhrParameters with Helpers {
  class PhrIO(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
    val s0_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s1_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s2_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s3_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val phrs:           Vec[Bool]             = Output(Vec(PhrHistoryLength, Bool()))
    val phrPtr:         PhrPtr                = Output(new PhrPtr)
    val train:          PhrUpdate             = Input(new PhrUpdate)       // redirect from backend
    val commit:         Valid[BpuTrain]       = Input(Valid(new BpuTrain)) // update from commit
    val trainFoldedPhr: PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
  }
  val io: PhrIO = IO(new PhrIO)

  private val phr    = RegInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
  private val phrPtr = RegInit(0.U.asTypeOf(new PhrPtr))

  private def getPhr(ptr: PhrPtr): UInt =
    (Cat(phr.asUInt, phr.asUInt) >> (ptr.value + 1.U))(PhrHistoryLength - 1, 0)

  /*
   * PHR train from redirct/s2_prediction/s3_prediction
   */

  private val s0_stall = io.train.s0_stall
  private val s1_valid = io.train.s1_valid
  private val s0_fire  = io.train.stageCtrl.s0_fire
  private val s1_fire  = io.train.stageCtrl.s1_fire
  private val s2_fire  = io.train.stageCtrl.s2_fire
  private val s3_fire  = io.train.stageCtrl.s3_fire

  private val histFoldedPhr = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo))) // for diff
  private val s0_foldedPhr  = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s0_foldedPhrReg =
    RegEnable(s0_foldedPhr, 0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)), !s0_stall)
  private val s1_foldedPhrReg =
    RegEnable(s0_foldedPhr, 0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)), s0_fire)
  private val s2_foldedPhrReg =
    RegEnable(s1_foldedPhrReg, 0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)), s1_fire)
  private val s3_foldedPhrReg =
    RegEnable(s2_foldedPhrReg, 0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)), s2_fire)

  private val s0_phrPtr    = WireInit(0.U.asTypeOf(new PhrPtr))
  private val s0_phrPtrReg = RegEnable(s0_phrPtr, 0.U.asTypeOf(new PhrPtr), !s0_stall)
  private val s1_phrPtr    = RegEnable(s0_phrPtr, 0.U.asTypeOf(new PhrPtr), s0_fire)
  private val s2_phrPtr    = RegEnable(s1_phrPtr, 0.U.asTypeOf(new PhrPtr), s1_fire)
  private val s3_phrPtr    = RegEnable(s2_phrPtr, 0.U.asTypeOf(new PhrPtr), s2_fire)

  private val s0_phrValue = getPhr(s0_phrPtr) // debug use it
  private val phrValue    = getPhr(phrPtr)

  private val redirectData    = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val s1_overrideData = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val s3_override     = WireInit(false.B)
  private val s3_overrideData = WireInit(0.U.asTypeOf(new PhrUpdateData))

  private val updateData  = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val updatePc    = WireInit(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  private val redirectPhr = WireInit(0.U(PhrHistoryLength.W))

  redirectData.valid  := io.train.redirect.valid
  redirectData.taken  := io.train.redirect.bits.taken
  redirectData.pc     := io.train.redirect.bits.startVAddr
  redirectData.phrPtr := io.train.redirect.bits.speculationMeta.phrHistPtr

  s3_override               := io.train.s3_override
  s3_overrideData.valid     := s3_override
  s3_overrideData.taken     := io.train.s3_prediction.taken
  s3_overrideData.pc        := getBranchAddr(io.train.s3_pc, io.train.s3_prediction.cfiPosition)
  s3_overrideData.phrPtr    := s3_phrPtr
  s3_overrideData.foldedPhr := s3_foldedPhrReg

  s1_overrideData.valid     := s1_valid
  s1_overrideData.taken     := io.train.s1_prediction.taken
  s1_overrideData.pc        := getBranchAddr(io.train.s1_pc, io.train.s1_prediction.cfiPosition)
  s1_overrideData.phrPtr    := s1_phrPtr
  s1_overrideData.foldedPhr := s1_foldedPhrReg

  updateData := MuxCase(
    0.U.asTypeOf(new PhrUpdateData),
    Seq(
      redirectData.valid -> redirectData,
      s3_override        -> s3_overrideData,
      s1_valid           -> s1_overrideData
    )
  )

  updatePc := updateData.pc

  private val shiftBits =
    (((updatePc >> 1) ^ (updatePc >> 3)) ^ ((updatePc >> 5) ^ (updatePc >> 7)))(Shamt - 1, 0)

  when(updateData.valid) {
    phrPtr    := updateData.phrPtr
    s0_phrPtr := updateData.phrPtr
    when(updateData.taken) {
      phr(updateData.phrPtr.value)         := shiftBits(1)
      phr((updateData.phrPtr - 1.U).value) := shiftBits(0)
      phrPtr                               := updateData.phrPtr - 2.U
      s0_phrPtr                            := updateData.phrPtr - 2.U
    }
  }.otherwise {
    s0_phrPtr := phrPtr
  }

  /*
   * PHR folded history compute & maintenance
   */
  AllFoldedHistoryInfo.foreach { info =>
    s0_foldedPhr.getHistWithInfo(info).foldedHist :=
      computeFoldedHist(phrValue, info.FoldedLength)(info.HistoryLength)
  }

  when(redirectData.valid) {
    redirectPhr := getPhr(redirectData.phrPtr)
    AllFoldedHistoryInfo.foreach { info =>
      redirectData.foldedPhr.getHistWithInfo(info).foldedHist :=
        computeFoldedHist(redirectPhr, info.FoldedLength)(info.HistoryLength)
    }
    s0_foldedPhr := redirectData.foldedPhr
    when(redirectData.taken) {
      s0_foldedPhr := redirectData.foldedPhr.update(VecInit(redirectPhr.asBools), redirectData.phrPtr, Shamt, shiftBits)
    }
  }.elsewhen(s3_override) {
    s0_foldedPhr := s3_foldedPhrReg
    when(s3_overrideData.taken) {
      s0_foldedPhr := s3_foldedPhrReg.update(VecInit(getPhr(s3_phrPtr).asBools), s3_phrPtr, Shamt, shiftBits)
    }
  }.elsewhen(s1_valid) {
    s0_foldedPhr := s1_foldedPhrReg
    when(s1_overrideData.taken) {
      s0_foldedPhr := s1_foldedPhrReg.update(VecInit(getPhr(s1_phrPtr).asBools), s1_phrPtr, Shamt, shiftBits)
    }
  }.otherwise {
    s0_foldedPhr := s0_foldedPhrReg
  }

  AllFoldedHistoryInfo.foreach { info =>
    histFoldedPhr.getHistWithInfo(info).foldedHist :=
      computeFoldedHist(phrValue, info.FoldedLength)(info.HistoryLength)
  }

  /*
   * bpu training folded phr compute
   */
  private val commitValid   = RegNext(io.commit.valid)
  private val commit        = RegEnable(io.commit.bits, io.commit.valid)
  private val predictHist   = getPhr(commit.meta.phr)
  private val metaPhrFolded = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  AllFoldedHistoryInfo.foreach { info =>
    metaPhrFolded.getHistWithInfo(info).foldedHist :=
      computeFoldedHist(predictHist, info.FoldedLength)(info.HistoryLength)
  }

  io.phrPtr         := s1_phrPtr
  io.phrs           := phr
  io.s0_foldedPhr   := s0_foldedPhr
  io.s1_foldedPhr   := s1_foldedPhrReg
  io.s2_foldedPhr   := s2_foldedPhrReg
  io.s3_foldedPhr   := s3_foldedPhrReg
  io.trainFoldedPhr := metaPhrFolded

  // commit time phr checker
  if (EnableCommitGHistDiff) {
    val commitHist    = RegInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
    val commitHistPtr = RegInit(0.U.asTypeOf(new PhrPtr))

    def getCommitHist(ptr: PhrPtr): UInt =
      (Cat(commitHist.asUInt, commitHist.asUInt) >> (ptr.value + 1.U))(PhrHistoryLength - 1, 0)

    def shiftCommitBits(pc: PrunedAddr): UInt =
      (((pc >> 1) ^ (pc >> 3)) ^ ((pc >> 5) ^ (pc >> 7)))(Shamt - 1, 0)

    def getMaxHistLens: Int = bpuParameters.tageParameters.TableInfos.map(_.HistoryLength).max

    val commitTaken = commit.branches(0).bits.taken
    val commitTakenPc = Mux(
      commitValid && commit.branches(0).bits.mispredict.asBools.reduce(_ || _),
      commit.startVAddr,
      getBranchAddr(commit.startVAddr, commit.branches(0).bits.cfiPosition)
    )
    val commitShiftBits = shiftCommitBits(commitTakenPc)

    when(commitValid && commitTaken) {
      commitHist(commitHistPtr.value)         := commitShiftBits(1)
      commitHist((commitHistPtr - 1.U).value) := commitShiftBits(0)
      commitHistPtr                           := commitHistPtr - 2.U
    }

    val commitHistValue        = commitHist.asUInt
    val commitTrueHist         = getCommitHist(commitHistPtr)
    val commitFDiffPredictFVec = WireInit(0.U.asTypeOf(Vec(AllFoldedHistoryInfo.size, Bool())))
    AllFoldedHistoryInfo.zipWithIndex foreach { case (info, i) =>
      val commitTrueFHist = computeFoldedHist(commitTrueHist, info.FoldedLength)(info.HistoryLength)
      val predictFHist    = computeFoldedHist(predictHist, info.FoldedLength)(info.HistoryLength)
      commitFDiffPredictFVec(i) := commitTrueFHist =/= predictFHist
      XSWarn(
        commitValid && commitFDiffPredictFVec(i),
        p"predict time ghist: ${predictFHist} is different from commit time: ${commitTrueFHist}\n"
      )
    }
    val predictFHist_diff_commitTrueFHist = commitValid && commitFDiffPredictFVec.reduce(_ || _)
    val predictHist_diff_commitHist =
      commitValid && predictHist(getMaxHistLens - 1, 0) =/= commitTrueHist(getMaxHistLens - 1, 0)
    XSPerfAccumulate(f"predictFHist_diff_commitTrueFHist", predictFHist_diff_commitTrueFHist)
    XSPerfAccumulate(f"predictHist_diff_commitHist", predictHist_diff_commitHist)
    dontTouch(commitHistValue)
    dontTouch(commitTrueHist)
    dontTouch(commitShiftBits)
    dontTouch(predictHist)
    dontTouch(commitHistPtr)
    dontTouch(predictFHist_diff_commitTrueFHist)
    dontTouch(predictHist_diff_commitHist)
    dontTouch(commitFDiffPredictFVec.asUInt)
    dontTouch(commitTakenPc)
  }

  private val diffFoldedPhr = histFoldedPhr.asUInt =/= s0_foldedPhrReg.asUInt
  XSPerfAccumulate("histFoldedPhr_diff_s0_foldedPhrReg", diffFoldedPhr)
  // TODO: remove dontTouch
  dontTouch(s0_foldedPhr)
  dontTouch(s1_foldedPhrReg)
  dontTouch(s2_foldedPhrReg)
  dontTouch(s0_phrValue)
  dontTouch(phrValue)
  dontTouch(histFoldedPhr)
  dontTouch(redirectPhr)
  dontTouch(diffFoldedPhr)
  dontTouch(s0_phrPtr)
  dontTouch(s1_phrPtr)
}
