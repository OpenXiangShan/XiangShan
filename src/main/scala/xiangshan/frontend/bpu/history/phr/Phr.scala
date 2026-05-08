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
import xiangshan.frontend.bpu.Train

// PHR: Predicted History Register
class Phr(implicit p: Parameters) extends PhrModule with HasPhrParameters with Helpers {
  class PhrIO(implicit p: Parameters) extends PhrBundle with HasPhrParameters {
    val s0_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s1_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s2_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s3_foldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val phr:            Vec[Bool]             = Output(Vec(PhrHistoryLength, Bool()))
    val phrMeta:        PhrMeta               = Output(new PhrMeta)
    val train:          PhrUpdate             = Input(new PhrUpdate)    // redirect from backend
    val s1Train:        S1Train               = Input(new S1Train)
    val commit:         Valid[Train]          = Input(Valid(new Train)) // trian bp data from reslove
    val oldFoldedPhr:   PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val trainFoldedPhr: PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
  }
  val io: PhrIO = IO(new PhrIO)

  private val phr    = RegInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
  private val phrPtr = RegInit(0.U.asTypeOf(new PhrPtr))

  private def getPhr(ptr: PhrPtr): UInt =
    (Cat(phr.asUInt, phr.asUInt) >> (ptr.value + 1.U))(PhrHistoryLength - 1, 0)

  private def getRedirectPhr(phrMeta: PhrMeta): UInt = {
    val redirectErrorPhr = getPhr(phrMeta.phrPtr)
    Cat(redirectErrorPhr(PhrHistoryLength - 1, PathHashHighWidth), phrMeta.phrLowBits)
  }

  /*
   * PHR train from redirect/s2_prediction/s3_prediction
   */

  private val s0_stall = io.train.s0_stall
  private val s1_valid = io.s1Train.valid
  private val s0_fire  = io.train.stageCtrl.s0_fire
  private val s1_fire  = io.train.stageCtrl.s1_fire
  private val s2_fire  = io.train.stageCtrl.s2_fire

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

  private val s0_phrPtr = WireInit(0.U.asTypeOf(new PhrPtr))
  private val s1_phrPtr = RegEnable(s0_phrPtr, 0.U.asTypeOf(new PhrPtr), s0_fire)

  private val s1_phrValue = getPhr(s1_phrPtr)
  private val phrValue    = getPhr(phrPtr)

  private val s1AbtbUpdateData = VecInit.fill(NumAheadBtbPredictionEntries)(0.U.asTypeOf(new PhrUpdateData))
  private val s1UbtbUpdateData = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val redirectData     = WireInit(0.U.asTypeOf(new PhrUpdateData))
  private val s3_override      = WireInit(false.B)
  private val s3_overrideData  = WireInit(0.U.asTypeOf(new PhrUpdateData))

  private val redirectS0PhrPtr     = WireInit(0.U.asTypeOf(new PhrPtr))
  private val redirectS0PhrLowBits = WireInit(0.U(PathHashHighWidth.W))
  private val redirectS0FoldedPhr  = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val redirectUpdate       = WireInit(0.U.asTypeOf(new PhrUpdateResult))
  private val s3S0PhrPtr           = WireInit(0.U.asTypeOf(new PhrPtr))
  private val s3S0PhrLowBits       = WireInit(0.U(PathHashHighWidth.W))
  private val s3S0FoldedPhr        = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s3Update             = WireInit(0.U.asTypeOf(new PhrUpdateResult))
  private val s1S0PhrPtr           = WireInit(0.U.asTypeOf(new PhrPtr))
  private val s1S0PhrLowBits       = WireInit(0.U(PathHashHighWidth.W))
  private val s1S0FoldedPhr        = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  private val s1Update             = WireInit(0.U.asTypeOf(new PhrUpdateResult))
  private val updatePhrLowBits     = WireInit(0.U(PathHashHighWidth.W))
  private val redirectPhr          = WireInit(0.U(PhrHistoryLength.W))

  // Organize the input data into the structure required for PHR updates

  redirectData.valid   := io.train.redirect.valid
  redirectData.taken   := io.train.redirect.bits.taken
  redirectData.cfiPc   := io.train.redirect.bits.cfiPc
  redirectData.target  := io.train.redirect.bits.target
  redirectData.phrMeta := io.train.redirect.bits.meta.phr

  s3_override             := io.train.s3_override
  s3_overrideData.valid   := s3_override
  s3_overrideData.taken   := io.train.s3_prediction.taken
  s3_overrideData.cfiPc   := getCfiPcFromPosition(io.train.s3_startPc, io.train.s3_prediction.cfiPosition)
  s3_overrideData.target  := io.train.s3_prediction.target
  s3_overrideData.phrMeta := io.train.s3_phrMeta

  s1AbtbUpdateData.zip(io.s1Train.abtbPrediction).foreach { case (data, pred) =>
    data.valid := s1_valid && pred.valid
    // Since S1 does not require recovery, and because the "taken" signal provided by abtbPrediction may not reflect the actual branch outcome,
    // we always assume "taken" when computing s1NextPhr and s1NextFoldedPhr.
    // If the branch is actually not taken, we simply leave Phr unchanged
    data.taken              := s1_valid && pred.valid
    data.cfiPc              := getCfiPcFromPosition(io.s1Train.startPc, pred.bits.cfiPosition)
    data.target             := pred.bits.target
    data.phrMeta.phrPtr     := s1_phrPtr
    data.phrMeta.phrLowBits := s1_phrValue(PathHashHighWidth - 1, 0)
  }

  s1UbtbUpdateData.valid  := s1_valid
  s1UbtbUpdateData.taken  := io.s1Train.ubtbPrediction.bits.taken
  s1UbtbUpdateData.cfiPc  := getCfiPcFromPosition(io.s1Train.startPc, io.s1Train.ubtbPrediction.bits.cfiPosition)
  s1UbtbUpdateData.target := io.s1Train.ubtbPrediction.bits.target
  s1UbtbUpdateData.phrMeta.phrPtr     := s1_phrPtr
  s1UbtbUpdateData.phrMeta.phrLowBits := s1_phrValue(PathHashHighWidth - 1, 0)

  // Compute all ShiftBits values and the high bits of the hash
  private val redirectHashComponents = getPathHashComponents(redirectData.cfiPc, redirectData.target)
  private val s3HashComponents       = getPathHashComponents(s3_overrideData.cfiPc, s3_overrideData.target)
  private val s1UbtbHashComponents   = getPathHashComponents(s1UbtbUpdateData.cfiPc, s1UbtbUpdateData.target)
  private val s1AbtbHashComponents   = s1AbtbUpdateData.map(data => getPathHashComponents(data.cfiPc, data.target))

  private val redirectShiftBits = redirectHashComponents._1
  private val redirectHashHigh  = redirectHashComponents._2
  private val s3ShiftBits       = s3HashComponents._1
  private val s3HashHigh        = s3HashComponents._2

  // Compute all phrPtr and phrLowBits for updates
  redirectUpdate := getUpdatePtrs(redirectData, redirectHashHigh)
  s3Update       := getUpdatePtrs(s3_overrideData, s3HashHigh)
  private val s1UbtbUpdate = getUpdatePtrs(s1UbtbUpdateData, s1UbtbHashComponents._2)
  private val s1AbtbUpdateCandidates = s1AbtbUpdateData.zip(s1AbtbHashComponents).map { case (data, hash) =>
    getUpdatePtrs(data, hash._2)
  }
  private val s1AbtbUpdateOH = Mux1H(io.s1Train.abtbFirstTakenBrOH, s1AbtbUpdateCandidates)

  s1Update := Mux(io.s1Train.abtbValid, s1AbtbUpdateOH, s1UbtbUpdate)
  private val s1AbtbShiftBits = Mux1H(io.s1Train.abtbFirstTakenBrOH, s1AbtbHashComponents.map(_._1))
  private val s1ShiftBits     = Mux(io.s1Train.abtbValid, s1AbtbShiftBits, s1UbtbHashComponents._1)

  redirectS0PhrPtr     := redirectUpdate.phrPtr
  redirectS0PhrLowBits := redirectUpdate.phrLowBits
  s3S0PhrPtr           := s3Update.phrPtr
  s3S0PhrLowBits       := s3Update.phrLowBits
  s1S0PhrPtr           := Mux(io.s1Train.taken, s1Update.phrPtr, s1_phrPtr)
  s1S0PhrLowBits       := Mux(io.s1Train.taken, s1Update.phrLowBits, s1_phrValue(PathHashHighWidth - 1, 0))

  private val shiftBits = MuxCase(
    0.U(Shamt.W),
    Seq(
      redirectData.valid -> redirectShiftBits,
      s3_override        -> s3ShiftBits,
      s1_valid           -> s1ShiftBits
    )
  )

  phrPtr := MuxCase(
    phrPtr,
    Seq(
      redirectData.valid -> redirectS0PhrPtr,
      s3_override        -> s3S0PhrPtr,
      s1_valid           -> s1S0PhrPtr
    )
  )
  s0_phrPtr := MuxCase(
    phrPtr,
    Seq(
      redirectData.valid -> redirectS0PhrPtr,
      s3_override        -> s3S0PhrPtr,
      s1_valid           -> s1S0PhrPtr
    )
  )
  updatePhrLowBits := MuxCase(
    0.U(PathHashHighWidth.W),
    Seq(
      redirectData.valid -> redirectS0PhrLowBits,
      s3_override        -> s3S0PhrLowBits,
      s1_valid           -> s1S0PhrLowBits
    )
  )

  redirectPhr := getRedirectPhr(redirectData.phrMeta)
  redirectS0FoldedPhr := getNextFoldedPhr(
    redirectData,
    computeAllFoldedPhr(redirectPhr),
    redirectPhr,
    redirectHashHigh,
    redirectShiftBits
  )
  s3S0FoldedPhr := getNextFoldedPhr(
    s3_overrideData,
    s3_foldedPhrReg,
    getRedirectPhr(s3_overrideData.phrMeta),
    s3HashHigh,
    s3ShiftBits
  )
  private val s1UbtbS0FoldedPhr = getNextFoldedPhr(
    s1UbtbUpdateData,
    s1_foldedPhrReg,
    getRedirectPhr(s1UbtbUpdateData.phrMeta),
    s1UbtbHashComponents._2,
    s1UbtbHashComponents._1
  )
  private val s1AbtbS0FoldedPhrCandidates = s1AbtbUpdateData.zip(s1AbtbHashComponents).map { case (data, hash) =>
    getNextFoldedPhr(
      data,
      s1_foldedPhrReg,
      getRedirectPhr(data.phrMeta),
      hash._2,
      hash._1
    )
  }
  private val s1AbtbS0FoldedPhr = Mux1H(io.s1Train.abtbFirstTakenBrOH, s1AbtbS0FoldedPhrCandidates)
  s1S0FoldedPhr := Mux(io.s1Train.abtbValid, s1AbtbS0FoldedPhr, s1UbtbS0FoldedPhr)

  private val updateValid = redirectData.valid || s3_overrideData.valid || io.s1Train.valid
  private val updateTaken = MuxCase(
    false.B,
    Seq(
      redirectData.valid -> redirectData.taken,
      s3_override        -> s3_overrideData.taken,
      s1_valid           -> io.s1Train.taken
    )
  )
  // If this update includes a taken branch, updatePtr should be s0_phrPtr + Shamt.U; otherwise, it should be s0_phrPtr.
  private val updatePtr = Mux(updateTaken, s0_phrPtr + Shamt.U, s0_phrPtr)
  when(updateValid) {
    for (i <- 1 to PathHashHighWidth) {
      phr((updatePtr + i.U).value) := updatePhrLowBits(i - 1)
    }
    when(updateTaken) {
      for (i <- 0 until Shamt) {
        phr((updatePtr - i.U).value) := shiftBits(Shamt - 1 - i)
      }
    }
  }

  /*
   * PHR folded history select
   */
  s0_foldedPhr := MuxCase(
    s0_foldedPhrReg,
    Seq(
      redirectData.valid             -> redirectS0FoldedPhr,
      s3_override                    -> s3S0FoldedPhr,
      (s1_valid && io.s1Train.taken) -> s1S0FoldedPhr
    )
  )

  AllFoldedHistoryInfo.foreach { info =>
    histFoldedPhr.getHistWithInfo(info).foldedHist :=
      computeFoldedHist(phrValue, info.FoldedLength)(info.HistoryLength)
  }

  /*
   * bpu training folded phr compute
   */
  private val bpTrainValid  = io.commit.valid
  private val bpTrain       = io.commit.bits
  private val predictHist   = getRedirectPhr(bpTrain.meta.phr)
  private val metaPhrFolded = WireInit(0.U.asTypeOf(new PhrAllFoldedHistories(AllFoldedHistoryInfo)))
  AllFoldedHistoryInfo.foreach { info =>
    metaPhrFolded.getHistWithInfo(info).foldedHist :=
      computeFoldedHist(predictHist, info.FoldedLength)(info.HistoryLength)
  }
  private val oldFoldedPhr = MuxCase(
    s1_foldedPhrReg,
    Seq(
      redirectData.valid -> computeAllFoldedPhr(redirectPhr),
      s3_override        -> s3_foldedPhrReg,
      s1_valid           -> s1_foldedPhrReg
    )
  )

  io.phrMeta.phrPtr     := s1_phrPtr
  io.phrMeta.phrLowBits := s1_phrValue(PathHashHighWidth - 1, 0)
  io.phrMeta.predFoldedHist.foreach(_ := s1_foldedPhrReg)
  io.phr            := phr
  io.s0_foldedPhr   := s0_foldedPhr
  io.s1_foldedPhr   := s1_foldedPhrReg
  io.s2_foldedPhr   := s2_foldedPhrReg
  io.s3_foldedPhr   := s3_foldedPhrReg
  io.trainFoldedPhr := metaPhrFolded
  io.oldFoldedPhr   := oldFoldedPhr

  // TODO: Currently unavailable，waiting for ftq commit info
  // commit time phr checker
  if (EnableCommitGHistDiff) {
    val commitValid   = RegNext(io.commit.valid)
    val commit        = RegEnable(io.commit.bits, io.commit.valid)
    val commitHist    = RegInit(0.U.asTypeOf(Vec(PhrHistoryLength, Bool())))
    val commitHistPtr = RegInit(0.U.asTypeOf(new PhrPtr))

    // FIXME: getPhr logic has changed
    def getCommitHist(ptr: PhrPtr): UInt =
      (Cat(commitHist.asUInt, commitHist.asUInt) >> (ptr.value + 1.U))(PhrHistoryLength - 1, 0)

    def shiftCommitBits(pc: PrunedAddr): UInt =
      (((pc >> 1) ^ (pc >> 3)) ^ ((pc >> 5) ^ (pc >> 7)))(Shamt - 1, 0)

    val commitTaken = commit.branches(0).bits.taken
    val commitTakenPc = Mux(
      commitValid && commit.branches(0).bits.mispredict.asBools.reduce(_ || _),
      commit.startPc,
      getCfiPcFromPosition(commit.startPc, commit.branches(0).bits.cfiPosition)
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
      commitValid && predictHist(MaxHistLens - 1, 0) =/= commitTrueHist(MaxHistLens - 1, 0)
    val histFolded_diff_s0Folded = histFoldedPhr.asUInt =/= s0_foldedPhrReg.asUInt
    when(s0_fire) {
      assert(
        !histFolded_diff_s0Folded,
        f"The history of on-site folding is inconsistent with the updated results of folding history"
      )
    }

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
    dontTouch(histFolded_diff_s0Folded)
  }

  if (io.commit.bits.meta.phr.predFoldedHist.isDefined) {
    val debug_predFoldedHist = io.commit.bits.meta.phr.predFoldedHist.get
    require(
      debug_predFoldedHist.hist.length == metaPhrFolded.hist.length,
      "pred folded hist length mismatch"
    )
    val predictFHist_diff_trainFHist = io.commit.valid && debug_predFoldedHist.asUInt =/= metaPhrFolded.asUInt
    XSPerfAccumulate(f"predictFHist_diff_trainFHist", predictFHist_diff_trainFHist)
  }

  // TODO: remove dontTouch
  dontTouch(s0_foldedPhr)
  dontTouch(s1_foldedPhrReg)
  dontTouch(s2_foldedPhrReg)
  dontTouch(phrValue)
  dontTouch(histFoldedPhr)
  dontTouch(redirectPhr)
  dontTouch(s0_phrPtr)
  dontTouch(s1_phrPtr)
}
