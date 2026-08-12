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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.ChiselDB
import utility.ParallelSingedExpandingAdd
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.ScTableInfo
import xiangshan.frontend.bpu.history.commonhr.CommonHREntry
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.tage.{TakenCounter => TageTakenCounter}

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult: Vec[Valid[Prediction]] = Input(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val providerTakenCtrs: Vec[Valid[SaturateCounter]] =
      Input(Vec(NumBtbResultEntries, Valid(TageTakenCounter()))) // s2 stage tage info
    val foldedPathHist:      PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val imli:                UInt                  = Input(UInt(ImliHistoryLength.W))
    val commonHR:            CommonHREntry         = Input(new CommonHREntry())
    val trainFoldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val scTakenMask:         Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val scUsed:              Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                ScMeta                = Output(new ScMeta())
  }
  val io: ScIO = IO(new ScIO)

  /*
   * stage control signals
   */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable
  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire && io.enable

  /*
   *  instantiate tables
   */
  private val pathTable = PathTableInfos.zipWithIndex.map { case (info, i) =>
    Module(new ScTable(info.NumSets, NumWays, "pathTable", i) with PathTableHelper {
      override protected def TableInfo: ScTableInfo = info
    })
  }

  private val globalTable = GlobalTableInfos.zipWithIndex.map { case (info, i) =>
    Module(new ScTable(info.NumSets, NumWays, "globalTable", i) with CommonTableHelper {
      override protected def TableInfo: ScTableInfo = info
    })
  }

  private val bwTable = BackwardTableInfos.zipWithIndex.map { case (info, i) =>
    Module(new ScTable(info.NumSets, NumWays, "bwTable", i) with CommonTableHelper {
      override protected def TableInfo: ScTableInfo = info
    })
  }
  private val imliTable = Module(new ScTable(ImliTableInfo.NumSets, NumWays, "imliTable", 0) with CommonTableHelper {
    override protected def TableInfo: ScTableInfo = ImliTableInfo
  })

  private val biasTable =
    Module(new ScTable(BiasTableInfo.NumSets, BiasTableNumWays, "biasTable", 0) with BiasTableHelper {
      override protected def TableInfo: ScTableInfo = BiasTableInfo
    })

  private val scThreshold = RegInit(VecInit.tabulate(NumWays)(_ => ThresholdCounter.Init))

  io.sramResetDone := (
    pathTable.map(_.io.sramResetDone) ++
      globalTable.map(_.io.sramResetDone) ++
      bwTable.map(_.io.sramResetDone) :+
      imliTable.io.sramResetDone :+
      biasTable.io.sramResetDone
  ).reduce(_ && _)

  /*
   * ghr stage ctrl signals
   */
  private val s0_commonHR      = io.commonHR
  private val s1_commonHRValid = RegEnable(s0_commonHR.valid, s0_fire)

  /*
   *  predict pipeline stage 0
   */
  private val s0_startPc  = io.startPc
  private val s0_bankMask = getBankMask(s0_startPc)
  private val s0_pathIdx = PathTableInfos.zip(pathTable).map { case (info, table) =>
    table.getPathTableIdx(
      s0_startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.NumSets))),
      io.foldedPathHist
    )
  }

  private val s1_pathIdx = s0_pathIdx.map(RegEnable(_, s0_fire)) // for debug
  private val s2_pathIdx = s1_pathIdx.map(RegEnable(_, s1_fire)) // for debug

  private val s0_globalIdx = GlobalTableInfos.zip(globalTable).map { case (info, table) =>
    table.getTableIdx(s0_startPc, s0_commonHR.ghr(info.HistoryLength - 1, 0))
  }

  private val s1_globalIdx = s0_globalIdx.map(RegEnable(_, s0_fire)) // for debug
  private val s2_globalIdx = s1_globalIdx.map(RegEnable(_, s1_fire)) // for debug

  private val s0_imliIdx = imliTable.getTableIdx(s0_startPc, io.imli)
  private val s1_imliIdx = RegEnable(s0_imliIdx, s0_fire) // for debug
  private val s2_imliIdx = RegEnable(s1_imliIdx, s1_fire) // for debug

  private val s0_biasIdx = biasTable.getBiasTableIdx(s0_startPc)
  private val s1_biasIdx = RegEnable(s0_biasIdx, s0_fire) // for debug
  private val s2_biasIdx = RegEnable(s1_biasIdx, s1_fire) // for debug

  private val s0_bwIdx = BackwardTableInfos.zip(bwTable).map { case (info, table) =>
    table.getTableIdx(s0_startPc, s0_commonHR.bw(info.HistoryLength - 1, 0))
  }
  private val s1_bwIdx = s0_bwIdx.map(RegEnable(_, s0_fire)) // for debug
  private val s2_bwIdx = s1_bwIdx.map(RegEnable(_, s1_fire)) // for debug

  pathTable.zip(s0_pathIdx).foreach { case (table, idx) =>
    table.io.predictReadReq.valid         := s0_fire && PathEnable.B
    table.io.predictReadReq.bits.setIdx   := idx
    table.io.predictReadReq.bits.bankMask := s0_bankMask
  }

  globalTable.zip(s0_globalIdx).foreach { case (table, idx) =>
    table.io.predictReadReq.valid := s0_fire && s0_commonHR.valid && GlobalEnable.B // if ghr invalid not request global table
    table.io.predictReadReq.bits.setIdx   := idx
    table.io.predictReadReq.bits.bankMask := s0_bankMask
  }

  bwTable.zip(s0_bwIdx).foreach { case (table, idx) =>
    table.io.predictReadReq.valid         := s0_fire && s0_commonHR.valid && BWEnable.B
    table.io.predictReadReq.bits.setIdx   := idx
    table.io.predictReadReq.bits.bankMask := s0_bankMask
  }

  imliTable.io.predictReadReq.valid         := s0_fire && ImliEnable.B
  imliTable.io.predictReadReq.bits.setIdx   := s0_imliIdx
  imliTable.io.predictReadReq.bits.bankMask := s0_bankMask

  biasTable.io.predictReadReq.valid         := s0_fire && BiasEnable.B
  biasTable.io.predictReadReq.bits.setIdx   := s0_biasIdx
  biasTable.io.predictReadReq.bits.bankMask := s0_bankMask

  /*
   *  predict pipeline stage 1
   *  calculate each ctr's percsum
   */
  private val s1_startPc = RegEnable(io.startPc, s0_fire)
  private val s1_pathResp = Mux(
    PathEnable.B,
    VecInit(pathTable.map(_.io.predictReadResp)),
    VecInit.fill(NumPathTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  // if s0_commonHR invalid, global table resp is also invalid
  private val s1_globalResp = Mux(
    s1_commonHRValid && GlobalEnable.B,
    VecInit(globalTable.map(_.io.predictReadResp)),
    VecInit.fill(NumGlobalTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )

  private val s1_bwResp = Mux(
    s1_commonHRValid && BWEnable.B,
    VecInit(bwTable.map(_.io.predictReadResp)),
    VecInit.fill(NumBWTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )

  private val s1_imliResp = Mux(
    ImliEnable.B,
    imliTable.io.predictReadResp,
    VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry()))
  )

  private val s1_biasResp = Mux(
    BiasEnable.B,
    biasTable.io.predictReadResp,
    VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry()))
  )

  private val s1_mergeResp = VecInit(s1_pathResp ++ s1_globalResp ++ s1_bwResp ++ Seq(s1_imliResp))

  private val s1_pathPercsum =
    VecInit.tabulate(NumWays)(w => s1_pathResp.map(entry => getPercsum(entry(w).ctr.value)).reduce(_ +& _))

  private val s1_globalPercsum =
    VecInit.tabulate(NumWays)(w => s1_globalResp.map(entry => getPercsum(entry(w).ctr.value)).reduce(_ +& _))

  private val s1_bwPercsum =
    VecInit.tabulate(NumWays)(w => s1_bwResp.map(entry => getPercsum(entry(w).ctr.value)).reduce(_ +& _))

  private val s1_imliPercsum = VecInit(s1_imliResp.map(entry => getPercsum(entry.ctr.value)))
  private val s1_biasPercsum = VecInit(s1_biasResp.map(entry => getPercsum(entry.ctr.value)))

  /*
   *  predict pipeline stage 2
   *  match entries and calculate final percSum
   */
  private val s2_startPc       = RegEnable(s1_startPc, s1_fire)
  private val s2_commonHRValid = RegEnable(s1_commonHRValid, s1_fire)

  private val s2_biasPercsum = VecInit(s1_biasPercsum.map(RegEnable(_, s1_fire)))

  private val s2_bwPercsum     = VecInit(s1_bwPercsum.map(RegEnable(_, s1_fire)))
  private val s2_imliPercsum   = VecInit(s1_imliPercsum.map(RegEnable(_, s1_fire)))
  private val s2_pathPercsum   = VecInit(s1_pathPercsum.map(RegEnable(_, s1_fire)))
  private val s2_globalPercsum = VecInit(s1_globalPercsum.map(RegEnable(_, s1_fire)))

  private val s2_mergePercsum = Seq(s2_pathPercsum, s2_globalPercsum, s2_bwPercsum, s2_imliPercsum)
  private val s2_sumPercsum   = VecInit.tabulate(NumWays)(j => ParallelSingedExpandingAdd(s2_mergePercsum.map(_(j))))

  private val s2_mbtbResult        = io.mbtbResult
  private val s2_providerTakenMask = VecInit(io.providerTakenCtrs.map(_.bits.isPositive))
  private val s2_providerValid     = VecInit(io.providerTakenCtrs.map(_.valid))
  private val s2_providerCtr       = VecInit(io.providerTakenCtrs.map(_.bits))

  private val s2_hitMask = VecInit(s2_mbtbResult.map { mbtbResult =>
    mbtbResult.valid && mbtbResult.bits.attribute.isConditional
  })

  private val s2_wayIdx = s2_mbtbResult.map(mbtbResult => getWayIdx(mbtbResult.bits.cfiPosition))
  private val s2_biasIdxLowBits = VecInit(s2_providerTakenMask.zip(s2_providerValid).zip(s2_providerCtr).map {
    case ((taken, valid), ctr) => Cat(valid && ctr.isWeak, valid && taken)
  })
  private val s2_biasWayIdx = s2_wayIdx.zipWithIndex.map {
    case (wayIdx, i) =>
      val biasIdx = Cat(wayIdx, s2_biasIdxLowBits(i))
      biasIdx
  }

  private val s2_pathPred   = s2_wayIdx.map(wayIdx => s2_pathPercsum(wayIdx) >= 0.S)       // for performance counter
  private val s2_globalPred = s2_wayIdx.map(wayIdx => s2_globalPercsum(wayIdx) >= 0.S)     // for performance counter
  private val s2_bwPred     = s2_wayIdx.map(wayIdx => s2_bwPercsum(wayIdx) >= 0.S)         // for performance counter
  private val s2_imliPred   = s2_wayIdx.map(wayIdx => s2_imliPercsum(wayIdx) >= 0.S)       // for performance counter
  private val s2_biasPred   = s2_biasWayIdx.map(biasIdx => s2_biasPercsum(biasIdx) >= 0.S) // for performance counter

  private val s2_totalPercsumAll = VecInit(s2_biasPercsum.zipWithIndex.map {
    case (biasPercsum, wayIdx) =>
      val idx = wayIdx >> BiasUseTageBitWidth
      biasPercsum +& s2_sumPercsum(idx)
  }.grouped(BiasTableNumWays / NumWays).toSeq.map(group => VecInit(group)))

  private val Seq(s2_sumAboveThresholdShift1All, s2_sumAboveThresholdShift2All, s2_sumAboveThresholdShift3All) =
    Seq(4, 5, 6).map(shiftRight =>
      VecInit(s2_totalPercsumAll.zipWithIndex.map { case (vec, idx) =>
        VecInit(vec.map(percsum =>
          aboveThreshold(percsum, scThreshold(idx).value >> shiftRight)
        ))
      })
    )

  private val s2_totalPercsum = VecInit(s2_wayIdx.zip(s2_biasIdxLowBits).map { case (wayIdx, lowBits) =>
    s2_totalPercsumAll(wayIdx)(lowBits)
  })

  private val s2_sumAboveThresholdShift1 = VecInit(s2_wayIdx.zip(s2_biasIdxLowBits).map { case (wayIdx, lowBits) =>
    s2_sumAboveThresholdShift1All(wayIdx)(lowBits)
  })
  private val s2_sumAboveThresholdShift2 = VecInit(s2_wayIdx.zip(s2_biasIdxLowBits).map { case (wayIdx, lowBits) =>
    s2_sumAboveThresholdShift2All(wayIdx)(lowBits)
  })
  private val s2_sumAboveThresholdShift3 = VecInit(s2_wayIdx.zip(s2_biasIdxLowBits).map { case (wayIdx, lowBits) =>
    s2_sumAboveThresholdShift3All(wayIdx)(lowBits)
  })

  require(NumWays == s2_mbtbResult.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbResult.length}")

  private val s2_scPred        = VecInit(s2_totalPercsum.map(_ >= 0.S))
  private val s2_thresholds    = VecInit(scThreshold.map(_.value >> 3))
  private val s2_useScPred     = WireInit(VecInit.fill(NumWays)(false.B))
  private val s2_sumAboveThres = WireInit(VecInit.fill(NumWays)(false.B))

  for (i <- 0 until NumWays) {
    val predValid    = s2_hitMask(i) && s2_providerValid(i)
    val sum          = s2_totalPercsum(i)
    val thres        = s2_thresholds(s2_wayIdx(i))
    val tageConfHigh = s2_providerCtr(i).isSaturatePositive || s2_providerCtr(i).isSaturateNegative
    val tageConfMid  = s2_providerCtr(i).isMid
    val tageConfLow  = s2_providerCtr(i).isWeak

    val conf = MuxCase(
      false.B,
      Seq(
        (predValid && tageConfHigh) -> s2_sumAboveThresholdShift1(i),
        (predValid && tageConfMid)  -> s2_sumAboveThresholdShift2(i),
        (predValid && tageConfLow)  -> s2_sumAboveThresholdShift3(i)
      )
    )
    s2_useScPred(i)     := conf
    s2_sumAboveThres(i) := Mux(predValid, conf, true.B)
    dontTouch(tageConfHigh)
    dontTouch(tageConfMid)
    dontTouch(tageConfLow)
    dontTouch(conf)
  }

  io.scTakenMask := s2_scPred
  io.scUsed      := s2_useScPred

  s2_useScPred.zip(s2_providerValid).foreach { case (use, valid) =>
    XSError(s2_fire && use && !valid, "SC useScPred is true but tage provider is invalid!\n")
  }

  io.meta.scBiasLowerBits := RegEnable(s2_biasIdxLowBits, s2_fire)

  io.meta.scPred        := RegEnable(s2_scPred, s2_fire)
  io.meta.tagePred      := RegEnable(s2_providerTakenMask, s2_fire)
  io.meta.tageCtr       := RegEnable(VecInit(s2_providerCtr.map(_.value)), s2_fire)
  io.meta.tagePredValid := RegEnable(s2_providerValid, s2_fire)
  io.meta.useScPred     := RegEnable(s2_useScPred, s2_fire)
  io.meta.sumAboveThres := RegEnable(s2_sumAboveThres, s2_fire)

  if (EnableScDebug) {
    io.meta.debug_scPathTakenVec.get   := RegEnable(VecInit(s2_pathPred), s2_fire)
    io.meta.debug_scGlobalTakenVec.get := RegEnable(VecInit(s2_globalPred), s2_fire)
    io.meta.debug_scBWTakenVec.get     := RegEnable(VecInit(s2_bwPred), s2_fire)
    io.meta.debug_scImliTakenVec.get   := RegEnable(VecInit(s2_imliPred), s2_fire)
    io.meta.debug_scBiasTakenVec.get   := RegEnable(VecInit(s2_biasPred), s2_fire)

    io.meta.debug_predPathIdx.get   := RegEnable(MixedVecInit(s2_pathIdx), s2_fire)
    io.meta.debug_predGlobalIdx.get := RegEnable(MixedVecInit(s2_globalIdx), s2_fire)
    io.meta.debug_predBWIdx.get     := RegEnable(MixedVecInit(s2_bwIdx), s2_fire)
    io.meta.debug_predImliIdx.get   := RegEnable(s2_imliIdx, s2_fire)
    io.meta.debug_predBiasIdx.get   := RegEnable(s2_biasIdx, s2_fire)
    io.meta.debug_totalPercsum.get := RegEnable(
      VecInit(s2_totalPercsum.map(_.pad(ScSumWidth).asUInt)),
      s2_fire
    )
    io.meta.debug_threshold.get := RegEnable(
      VecInit(s2_wayIdx.map(wayIdx => s2_thresholds(wayIdx))),
      s2_fire
    )
    io.meta.debug_commonHRValid.get := RegEnable(s2_commonHRValid, s2_fire)
  }

  /*
   *  train pipeline stage 0
   */
  private val t0_fire     = io.stageCtrl.t0_fire && io.enable
  private val t0_meta     = io.train.meta.sc
  private val t0_commonHR = io.train.meta.commonHR
  private val t0_bankMask = getBankMask(io.train.startPc)
  private val t0_pathIdx = PathTableInfos.zip(pathTable).map { case (info, table) =>
    table.getPathTableIdx(
      io.train.startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.NumSets))),
      io.trainFoldedPathHist
    )
  }
  private val t0_globalIdx = GlobalTableInfos.zip(globalTable).map { case (info, table) =>
    table.getTableIdx(io.train.startPc, t0_commonHR.ghr(info.HistoryLength - 1, 0))
  }
  private val t0_bwIdx = BackwardTableInfos.zip(bwTable).map { case (info, table) =>
    table.getTableIdx(io.train.startPc, t0_commonHR.bw(info.HistoryLength - 1, 0))
  }
  private val t0_imliIdx     = imliTable.getTableIdx(io.train.startPc, t0_commonHR.imli)
  private val t0_biasIdx     = biasTable.getBiasTableIdx(io.train.startPc)
  private val t0_branches    = io.train.branches
  private val t0_mbtbEntries = io.train.meta.mbtb.entries.flatten
  // if the branch cfi not in mbtbResult, do not train
  // During training, find the predicted scPred and lowBits values in the order of the predicted mbtbResult
  // MBTB may invalidate entry with larger idx during multihit, and the order needs to be reversed
  private val t0_branchesScIdxHitVec = WireInit(VecInit.fill(ResolveEntryBranchNumber)(false.B))
  private val t0_branchesScIdxVec    = WireInit(VecInit.fill(ResolveEntryBranchNumber)(0.U(log2Ceil(NumWays).W)))
  t0_branches.zipWithIndex.foreach { case (branch, branchIdx) =>
    for (i <- (0 until NumWays).reverse) {
      when(branch.valid && t0_mbtbEntries(i).hit(branch.bits)) {
        t0_branchesScIdxHitVec(branchIdx) := true.B
        t0_branchesScIdxVec(branchIdx)    := i.U
      }
    }
  }
  private val t0_writeTakenVec =
    VecInit(t0_branches.map(b => b.valid && b.bits.taken && b.bits.attribute.isConditional))
  private val t0_writeValidVec =
    VecInit(t0_branches.zip(t0_branchesScIdxHitVec).zip(t0_branchesScIdxVec).zip(t0_writeTakenVec).map {
      case (((b, hit), predIdx), taken) =>
        b.valid && b.bits.attribute.isConditional && hit && t0_meta.tagePredValid(predIdx) &&
        (!(t0_meta.useScPred(predIdx) && t0_meta.scPred(predIdx) === taken) || !(t0_meta.useScPred(predIdx) &&
          t0_meta.tagePredValid(predIdx) && t0_meta.scPred(predIdx) === t0_meta.tagePred(predIdx)))
    })
  private val t0_writeValidVecRef =
    VecInit(t0_branches.zip(t0_branchesScIdxHitVec).zip(t0_branchesScIdxVec).zip(t0_writeTakenVec).map {
      case (((b, hit), predIdx), taken) =>
        b.valid && b.bits.attribute.isConditional && hit && t0_meta.tagePredValid(predIdx) &&
        (t0_meta.scPred(predIdx) =/= taken || !t0_meta.sumAboveThres(predIdx))
    })
  private val t0_writeValidBothVec =
    VecInit(t0_writeValidVec.zip(t0_writeValidVecRef).map { case (impl, ref) => impl && ref })
  private val t0_writeValidImplOnlyVec =
    VecInit(t0_writeValidVec.zip(t0_writeValidVecRef).map { case (impl, ref) => impl && !ref })
  private val t0_writeValidRefOnlyVec =
    VecInit(t0_writeValidVec.zip(t0_writeValidVecRef).map { case (impl, ref) => !impl && ref })
  private val t0_writeValidImplOnlyScCorrectTageWrongVec = VecInit(
    t0_writeValidImplOnlyVec.zip(t0_branchesScIdxVec).zip(t0_writeTakenVec).map {
      case ((implOnly, predIdx), taken) =>
        implOnly && t0_meta.scPred(predIdx) === taken && t0_meta.scPred(predIdx) =/= t0_meta.tagePred(predIdx)
    }
  )
  private val t0_useScPredSumAboveThresMismatchVec = VecInit(
    t0_branches.zip(t0_branchesScIdxHitVec).zip(t0_branchesScIdxVec).map {
      case ((branch, hit), predIdx) =>
        branch.valid && branch.bits.attribute.isConditional && hit && t0_meta.tagePredValid(predIdx) &&
        t0_meta.useScPred(predIdx) =/= t0_meta.sumAboveThres(predIdx)
    }
  )

  private val t0_needWrite    = t0_writeValidVec.reduce(_ || _)
  private val t0_needWriteRef = t0_writeValidVecRef.reduce(_ || _)
  private val t0_bankConflict = t0_needWrite && s0_fire && t0_bankMask === s0_bankMask
  io.trainReady := !t0_bankConflict

  XSPerfAccumulate("t0_writeValid_impl_branches", Mux(t0_fire, PopCount(t0_writeValidVec), 0.U))
  XSPerfAccumulate("t0_writeValid_ref_branches", Mux(t0_fire, PopCount(t0_writeValidVecRef), 0.U))
  XSPerfAccumulate("t0_writeValid_both_branches", Mux(t0_fire, PopCount(t0_writeValidBothVec), 0.U))
  XSPerfAccumulate("t0_writeValid_impl_only_branches", Mux(t0_fire, PopCount(t0_writeValidImplOnlyVec), 0.U))
  XSPerfAccumulate("t0_writeValid_ref_only_branches", Mux(t0_fire, PopCount(t0_writeValidRefOnlyVec), 0.U))
  XSPerfAccumulate(
    "t0_writeValid_impl_only_sc_correct_tage_wrong_branches",
    Mux(t0_fire, PopCount(t0_writeValidImplOnlyScCorrectTageWrongVec), 0.U)
  )
  XSPerfAccumulate(
    "t0_useScPred_sumAboveThres_mismatch_branches",
    Mux(t0_fire, PopCount(t0_useScPredSumAboveThresMismatchVec), 0.U)
  )
  XSPerfAccumulate("t0_needWrite_impl", t0_fire && t0_needWrite)
  XSPerfAccumulate("t0_needWrite_ref", t0_fire && t0_needWriteRef)
  XSPerfAccumulate("t0_needWrite_impl_only", t0_fire && t0_needWrite && !t0_needWriteRef)
  XSPerfAccumulate("t0_needWrite_ref_only", t0_fire && !t0_needWrite && t0_needWriteRef)

  pathTable.zip(t0_pathIdx).foreach { case (table, idx) =>
    table.io.trainReadReq.valid         := t0_fire && t0_needWrite && PathEnable.B
    table.io.trainReadReq.bits.setIdx   := idx
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }
  globalTable.zip(t0_globalIdx).foreach { case (table, idx) =>
    table.io.trainReadReq.valid         := t0_fire && t0_needWrite && t0_commonHR.valid && GlobalEnable.B
    table.io.trainReadReq.bits.setIdx   := idx
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }
  bwTable.zip(t0_bwIdx).foreach { case (table, idx) =>
    table.io.trainReadReq.valid         := t0_fire && t0_needWrite && t0_commonHR.valid && BWEnable.B
    table.io.trainReadReq.bits.setIdx   := idx
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }
  imliTable.io.trainReadReq.valid         := t0_fire && t0_needWrite && ImliEnable.B
  imliTable.io.trainReadReq.bits.setIdx   := t0_imliIdx
  imliTable.io.trainReadReq.bits.bankMask := t0_bankMask

  biasTable.io.trainReadReq.valid         := t0_fire && t0_needWrite && BiasEnable.B
  biasTable.io.trainReadReq.bits.setIdx   := t0_biasIdx
  biasTable.io.trainReadReq.bits.bankMask := t0_bankMask

  dontTouch(t0_bankConflict)
  XSPerfAccumulate("t0_writeConflict", t0_bankConflict && t0_fire)

  /*
   *  train pipeline stage 1
   */
  private val t1_fire     = RegNext(t0_fire, false.B)
  private val t1_branches = RegEnable(io.train.branches, t0_fire)
  private val t1_meta     = RegEnable(t0_meta, 0.U.asTypeOf(t0_meta), t0_fire)
  private val t1_commonHR = RegEnable(t0_commonHR, t0_fire)
  private val t1_startPc  = RegEnable(io.train.startPc, t0_fire)

  private val t1_bankMask     = RegEnable(t0_bankMask, t0_fire)
  private val t1_pathSetIdx   = RegEnable(VecInit(t0_pathIdx), t0_fire)
  private val t1_globalSetIdx = RegEnable(VecInit(t0_globalIdx), t0_fire)

  private val t1_bwSetIdx   = RegEnable(VecInit(t0_bwIdx), t0_fire)
  private val t1_imliSetIdx = RegEnable(t0_imliIdx, t0_fire)
  private val t1_biasSetIdx = RegEnable(t0_biasIdx, t0_fire)

  private val t1_oldPathEntries = Mux(
    PathEnable.B,
    VecInit(pathTable.map(_.io.trainReadResp)),
    VecInit.fill(NumPathTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  private val t1_oldGlobalEntries = Mux(
    t1_commonHR.valid && GlobalEnable.B,
    VecInit(globalTable.map(_.io.trainReadResp)),
    VecInit.fill(NumGlobalTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  private val t1_oldBWEntries = Mux(
    t1_commonHR.valid && BWEnable.B,
    VecInit(bwTable.map(_.io.trainReadResp)),
    VecInit.fill(NumBWTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  private val t1_oldImliEntries = Mux(
    ImliEnable.B,
    imliTable.io.trainReadResp,
    VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry()))
  )
  private val t1_oldBiasEntries = Mux(
    BiasEnable.B,
    biasTable.io.trainReadResp,
    VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry()))
  )
  private val t1_oldBiasLowBits = RegEnable(t0_meta.scBiasLowerBits, t0_fire)

  private val t1_branchesWayIdxVec   = VecInit(t1_branches.map(b => getWayIdx(b.bits.cfiPosition)))
  private val t1_branchesScIdxHitVec = RegEnable(t0_branchesScIdxHitVec, t0_fire)
  private val t1_branchesScIdxVec    = RegEnable(t0_branchesScIdxVec, t0_fire)

  private val t1_writeTakenVec    = RegEnable(t0_writeTakenVec, t0_fire)
  private val t1_writeValidVecReg = RegEnable(t0_writeValidVec, t0_fire)
  private val t1_writeValidVec    = VecInit(t1_writeValidVecReg.map(_ && t1_fire))
  private val t1_writeValid       = t1_writeValidVec.reduce(_ || _)

  require(
    t1_branchesWayIdxVec(0).getWidth == log2Ceil(NumWays),
    s"t1_branchesWayIdxVec entry width: ${t1_branchesWayIdxVec(0).getWidth} " +
      s"should be the same as log2Ceil(NumWays): ${log2Ceil(NumWays)}"
  )

  /************ get new threshold************/
  private val t1_thresholdOverflowVec  = WireInit(VecInit.fill(NumWays)(false.B))
  private val t1_thresholdUnderflowVec = WireInit(VecInit.fill(NumWays)(false.B))
  private val t1_writeThresVec         = VecInit.tabulate(NumWays)(_ => ThresholdCounter.Init)

  // For each reslove branch, record its update direction, whether it has been updated, and which way it has been updated to
  private val thresholdWayMask =
    VecInit(Seq.fill(ResolveEntryBranchNumber)(VecInit(Seq.fill(NumWays)(false.B))))
  private val thresholdDirMask =
    VecInit(Seq.fill(ResolveEntryBranchNumber)(VecInit(Seq.fill(NumWays)(false.B))))
  t1_writeValidVec.zip(t1_writeTakenVec).zip(t1_branchesWayIdxVec).zip(t1_branchesScIdxVec).zipWithIndex.foreach {
    case ((((valid, taken), writeIdx), oldIdx), i) =>
      val scWrong = taken =/= t1_meta.scPred(oldIdx)
      val needUpdate = valid && t1_meta.tagePredValid(oldIdx) &&
        (scWrong || !t1_meta.sumAboveThres(oldIdx))
      thresholdWayMask(i)(writeIdx) := needUpdate
      thresholdDirMask(i)(writeIdx) := scWrong
  }
  scThreshold.zip(t1_writeThresVec).zipWithIndex.foreach { case ((oldEntry, newEntry), i) =>
    val writeHit = thresholdWayMask.map(_(i))
    val writeDir = thresholdDirMask.map(_(i))
    val inc      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && dir })
    val dec      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && !dir })
    val updated  = Mux(inc >= dec, oldEntry.getIncrease(inc - dec), oldEntry.getDecrease(dec - inc))
    t1_thresholdOverflowVec(i)  := updated.value > MaxThreshold.U
    t1_thresholdUnderflowVec(i) := updated.value < MinThreshold.U
    newEntry := Mux(
      updated.value >= MinThreshold.U && updated.value <= MaxThreshold.U,
      updated,
      scThreshold(i)
    )
  }
  dontTouch(t1_writeThresVec)

  // calculate new path table entries
  private val t1_writePathEntryVec = WireInit(
    VecInit.fill(NumPathTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldPathEntries.zip(t1_writePathEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(
        oldEntries,
        t1_writeValidVec,
        t1_writeTakenVec,
        t1_branchesWayIdxVec,
        t1_branchesScIdxVec,
        t1_meta
      )
  }
  dontTouch(t1_writePathEntryVec)

  // calculate new global table entries
  private val t1_writeGlobalEntryVec = WireInit(
    VecInit.fill(NumGlobalTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldGlobalEntries.zip(t1_writeGlobalEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(
        oldEntries,
        t1_writeValidVec,
        t1_writeTakenVec,
        t1_branchesWayIdxVec,
        t1_branchesScIdxVec,
        t1_meta
      )
  }

  private val t1_writeBWEntryVec = WireInit(
    VecInit.fill(NumBWTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldBWEntries.zip(t1_writeBWEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(
        oldEntries,
        t1_writeValidVec,
        t1_writeTakenVec,
        t1_branchesWayIdxVec,
        t1_branchesScIdxVec,
        t1_meta
      )
  }

  private val t1_writeImliEntryVec = updateEntry(
    t1_oldImliEntries,
    t1_writeValidVec,
    t1_writeTakenVec,
    t1_branchesWayIdxVec,
    t1_branchesScIdxVec,
    t1_meta
  )

  // calculate bias table new entries and wayMask
  private val t1_writeBiasEntryVec = WireInit(VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry())))

  // For each reslove branch, record its update direction, whether it has been updated, and which way it has been updated to
  private val writeBiasWayMask =
    VecInit(Seq.fill(t1_writeValidVec.length)(VecInit(Seq.fill(t1_oldBiasEntries.length)(false.B))))
  private val writeBiasDirMask =
    VecInit(Seq.fill(t1_writeValidVec.length)(VecInit(Seq.fill(t1_oldBiasEntries.length)(false.B))))
  t1_writeValidVec.zip(t1_writeTakenVec).zip(t1_branchesWayIdxVec).zip(t1_branchesScIdxVec).zipWithIndex.foreach {
    case ((((valid, taken), writeIdx), oldIdx), i) =>
      val biasWayIdx = Cat(writeIdx, t1_oldBiasLowBits(oldIdx))
      val needUpdate = valid && t1_meta.tagePredValid(oldIdx) &&
        (t1_meta.scPred(oldIdx) =/= taken || !t1_meta.sumAboveThres(oldIdx))
      writeBiasWayMask(i)(biasWayIdx) := needUpdate
      writeBiasDirMask(i)(biasWayIdx) := taken
  }
  t1_oldBiasEntries.zip(t1_writeBiasEntryVec).zipWithIndex.foreach { case ((oldEntry, newEntry), i) =>
    val writeHit = writeBiasWayMask.map(_(i))
    val writeDir = writeBiasDirMask.map(_(i))
    val inc      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && dir })
    val dec      = PopCount(writeHit.zip(writeDir).map { case (hit, dir) => hit && !dir })

    newEntry.ctr := Mux(inc >= dec, oldEntry.ctr.getIncrease(inc - dec), oldEntry.ctr.getDecrease(dec - inc))
  }
  dontTouch(t1_startPc)
  dontTouch(t1_branchesScIdxHitVec)
  dontTouch(writeBiasWayMask)
  dontTouch(writeBiasDirMask)
  dontTouch(t1_writeBiasEntryVec)

  when(t1_writeValid) {
    scThreshold := t1_writeThresVec
  }

  /*
   *  train pipeline stage 2
   */
  private val t2_writeValid          = RegNext(t1_writeValid, false.B)
  private val t2_bankMask            = RegEnable(t1_bankMask, t1_fire)
  private val t2_pathSetIdx          = RegEnable(t1_pathSetIdx, t1_fire)
  private val t2_globalSetIdx        = RegEnable(t1_globalSetIdx, t1_fire)
  private val t2_bwSetIdx            = RegEnable(t1_bwSetIdx, t1_fire)
  private val t2_imliSetIdx          = RegEnable(t1_imliSetIdx, t1_fire)
  private val t2_biasSetIdx          = RegEnable(t1_biasSetIdx, t1_fire)
  private val t2_commonHR            = RegEnable(t1_commonHR, t1_fire)
  private val t2_oldPathEntries      = RegEnable(t1_oldPathEntries, t1_fire)
  private val t2_oldGlobalEntries    = RegEnable(t1_oldGlobalEntries, t1_fire)
  private val t2_oldBWEntries        = RegEnable(t1_oldBWEntries, t1_fire)
  private val t2_oldImliEntries      = RegEnable(t1_oldImliEntries, t1_fire)
  private val t2_oldBiasEntries      = RegEnable(t1_oldBiasEntries, t1_fire)
  private val t2_writePathEntryVec   = RegEnable(t1_writePathEntryVec, t1_fire)
  private val t2_writeGlobalEntryVec = RegEnable(t1_writeGlobalEntryVec, t1_fire)
  private val t2_writeBWEntryVec     = RegEnable(t1_writeBWEntryVec, t1_fire)
  private val t2_writeBiasEntryVec   = RegEnable(t1_writeBiasEntryVec, t1_fire)
  private val t2_writeImliEntryVec   = RegEnable(t1_writeImliEntryVec, t1_fire)
  private val t2_writePathWayMaskVec =
    t2_oldPathEntries.zip(t2_writePathEntryVec).map { case (oldEntries, newEntries) =>
      updateWayMask(oldEntries, newEntries)
    }
  private val t2_writeGlobalEntryWayMaskVec =
    t2_oldGlobalEntries.zip(t2_writeGlobalEntryVec).map { case (oldEntries, newEntries) =>
      updateWayMask(oldEntries, newEntries)
    }
  private val t2_writeBWEntryWayMaskVec =
    t2_oldBWEntries.zip(t2_writeBWEntryVec).map { case (oldEntries, newEntries) =>
      updateWayMask(oldEntries, newEntries)
    }
  private val t2_writeBiasWayMask = WireInit(VecInit.fill(BiasTableNumWays)(false.B))
  t2_oldBiasEntries.zip(t2_writeBiasEntryVec).zip(t2_writeBiasWayMask).foreach {
    case ((oldEntry, newEntry), wayMask) =>
      when(oldEntry.ctr =/= newEntry.ctr) {
        wayMask := true.B
      }
  }
  private val t2_writeImliWayMask = updateWayMask(t2_oldImliEntries, t2_writeImliEntryVec)

  // new entries write back to tables
  pathTable.zip(t2_pathSetIdx).zip(t2_writePathEntryVec).zip(t2_writePathWayMaskVec).foreach {
    case (((table, idx), writeEntries), writeWayMask) =>
      table.io.update.valid    := t2_writeValid && PathEnable.B
      table.io.update.setIdx   := idx
      table.io.update.bankMask := t2_bankMask
      table.io.update.wayMask  := writeWayMask
      table.io.update.entryVec := writeEntries
  }

  globalTable.zip(t2_globalSetIdx).zip(t2_writeGlobalEntryVec).zip(t2_writeGlobalEntryWayMaskVec).foreach {
    case (((table, idx), writeEntries), writeWayMask) =>
      table.io.update.valid    := t2_writeValid && t2_commonHR.valid && GlobalEnable.B
      table.io.update.setIdx   := idx
      table.io.update.bankMask := t2_bankMask
      table.io.update.wayMask  := writeWayMask
      table.io.update.entryVec := writeEntries
  }

  bwTable.zip(t2_bwSetIdx).zip(t2_writeBWEntryVec).zip(t2_writeBWEntryWayMaskVec).foreach {
    case (((table, idx), writeEntries), writeWayMask) =>
      table.io.update.valid    := t2_writeValid && t2_commonHR.valid && BWEnable.B
      table.io.update.setIdx   := idx
      table.io.update.bankMask := t2_bankMask
      table.io.update.wayMask  := writeWayMask
      table.io.update.entryVec := writeEntries
  }

  imliTable.io.update.valid    := t2_writeValid && ImliEnable.B
  imliTable.io.update.setIdx   := t2_imliSetIdx
  imliTable.io.update.bankMask := t2_bankMask
  imliTable.io.update.wayMask  := t2_writeImliWayMask
  imliTable.io.update.entryVec := t2_writeImliEntryVec

  biasTable.io.update.valid    := t2_writeValid && BiasEnable.B
  biasTable.io.update.setIdx   := t2_biasSetIdx
  biasTable.io.update.bankMask := t2_bankMask
  biasTable.io.update.wayMask  := t2_writeBiasWayMask
  biasTable.io.update.entryVec := t2_writeBiasEntryVec

  private val t1_resolvedScValidVec = VecInit.tabulate(ResolveEntryBranchNumber) { i =>
    val predSlotIdx = t1_branchesScIdxVec(i)
    t1_fire && t1_branches(i).valid && t1_branches(i).bits.attribute.isConditional &&
    t1_branchesScIdxHitVec(i) && t1_meta.tagePredValid(predSlotIdx)
  }

  if (EnableScDebug) {
    val branchIndices   = 0 until ResolveEntryBranchNumber
    val actualTaken     = branchIndices.map(i => t1_branches(i).bits.taken)
    val predSlotIdx     = branchIndices.map(i => t1_branchesScIdxVec(i))
    val useScEvent      = branchIndices.map(i => t1_resolvedScValidVec(i) && t1_meta.useScPred(predSlotIdx(i)))
    val notUseScEvent   = branchIndices.map(i => t1_resolvedScValidVec(i) && !t1_meta.useScPred(predSlotIdx(i)))
    val trainUseScEvent = branchIndices.map(i => t1_writeValidVec(i) && t1_meta.useScPred(predSlotIdx(i)))

    val scCorrectEvent = branchIndices.map(i => useScEvent(i) && actualTaken(i) === t1_meta.scPred(predSlotIdx(i)))
    val scWrongEvent   = branchIndices.map(i => useScEvent(i) && actualTaken(i) =/= t1_meta.scPred(predSlotIdx(i)))
    val tageCorrectEvent = branchIndices.map { i =>
      useScEvent(i) && actualTaken(i) === t1_meta.tagePred(predSlotIdx(i))
    }
    val tageWrongEvent = branchIndices.map(i => useScEvent(i) && actualTaken(i) =/= t1_meta.tagePred(predSlotIdx(i)))

    def componentCorrect(pred: Vec[Bool], enabled: Bool = true.B): Seq[Bool] = branchIndices.map { i =>
      useScEvent(i) && enabled && actualTaken(i) === pred(predSlotIdx(i))
    }
    def componentWrong(pred: Vec[Bool], enabled: Bool = true.B): Seq[Bool] = branchIndices.map { i =>
      useScEvent(i) && enabled && actualTaken(i) =/= pred(predSlotIdx(i))
    }
    def countAtPredSlot(events: Seq[Bool], slot: Int): UInt =
      PopCount(events.zip(predSlotIdx).map { case (event, idx) => event && idx === slot.U })

    val pathCorrectEvent   = componentCorrect(t1_meta.debug_scPathTakenVec.get, PathEnable.B)
    val pathWrongEvent     = componentWrong(t1_meta.debug_scPathTakenVec.get, PathEnable.B)
    val globalValid        = t1_meta.debug_commonHRValid.get && GlobalEnable.B
    val globalCorrectEvent = componentCorrect(t1_meta.debug_scGlobalTakenVec.get, globalValid)
    val globalWrongEvent   = componentWrong(t1_meta.debug_scGlobalTakenVec.get, globalValid)
    val bwValid            = t1_meta.debug_commonHRValid.get && BWEnable.B
    val bwCorrectEvent     = componentCorrect(t1_meta.debug_scBWTakenVec.get, bwValid)
    val bwWrongEvent       = componentWrong(t1_meta.debug_scBWTakenVec.get, bwValid)
    val imliCorrectEvent   = componentCorrect(t1_meta.debug_scImliTakenVec.get, ImliEnable.B)
    val imliWrongEvent     = componentWrong(t1_meta.debug_scImliTakenVec.get, ImliEnable.B)
    val biasCorrectEvent   = componentCorrect(t1_meta.debug_scBiasTakenVec.get, BiasEnable.B)
    val biasWrongEvent     = componentWrong(t1_meta.debug_scBiasTakenVec.get, BiasEnable.B)

    val pathChangeVec = VecInit.tabulate(NumWays) { i =>
      PathEnable.B && t1_oldPathEntries.zip(t1_writePathEntryVec).map {
        case (oldEntries, writeEntries) => oldEntries(i).ctr =/= writeEntries(i).ctr
      }.reduce(_ || _)
    }
    val globalChangeVec = VecInit.tabulate(NumWays) { i =>
      GlobalEnable.B && t1_commonHR.valid && t1_oldGlobalEntries.zip(t1_writeGlobalEntryVec).map {
        case (oldEntries, writeEntries) => oldEntries(i).ctr =/= writeEntries(i).ctr
      }.reduce(_ || _)
    }
    val bwChangeVec = VecInit.tabulate(NumWays) { i =>
      BWEnable.B && t1_commonHR.valid && t1_oldBWEntries.zip(t1_writeBWEntryVec).map {
        case (oldEntries, writeEntries) => oldEntries(i).ctr =/= writeEntries(i).ctr
      }.reduce(_ || _)
    }
    val imliChangeVec = VecInit.tabulate(NumWays) { i =>
      ImliEnable.B && t1_oldImliEntries(i).ctr =/= t1_writeImliEntryVec(i).ctr
    }
    val biasWaysPerScWay = 1 << BiasUseTageBitWidth
    val biasChangeVec = VecInit.tabulate(NumWays) { i =>
      BiasEnable.B && (0 until biasWaysPerScWay).map { lowBits =>
        val biasWay = i * biasWaysPerScWay + lowBits
        t1_oldBiasEntries(biasWay).ctr =/= t1_writeBiasEntryVec(biasWay).ctr
      }.reduce(_ || _)
    }
    val changeVec = VecInit.tabulate(NumWays) { i =>
      pathChangeVec(i) || globalChangeVec(i) || bwChangeVec(i) || imliChangeVec(i) || biasChangeVec(i)
    }

    for (i <- 0 until NumWays) {
      XSPerfAccumulate(
        s"sc_correct_tage_wrong${i}",
        countAtPredSlot(
          scCorrectEvent.zip(tageWrongEvent).map {
            case (scCorrect, tageWrong) => scCorrect && tageWrong
          },
          i
        )
      )
      XSPerfAccumulate(
        s"sc_wrong_tage_correct${i}",
        countAtPredSlot(
          scWrongEvent.zip(tageCorrectEvent).map {
            case (scWrong, tageCorrect) => scWrong && tageCorrect
          },
          i
        )
      )
      XSPerfAccumulate(
        s"sc_correct_tage_correct${i}",
        countAtPredSlot(
          scCorrectEvent.zip(tageCorrectEvent).map {
            case (scCorrect, tageCorrect) => scCorrect && tageCorrect
          },
          i
        )
      )
      XSPerfAccumulate(
        s"sc_wrong_tage_wrong${i}",
        countAtPredSlot(
          scWrongEvent.zip(tageWrongEvent).map {
            case (scWrong, tageWrong) => scWrong && tageWrong
          },
          i
        )
      )

      XSPerfAccumulate(s"t1_use_sc${i}", countAtPredSlot(useScEvent, i))
      XSPerfAccumulate(s"t1_not_use_sc${i}", countAtPredSlot(notUseScEvent, i))

      XSPerfAccumulate(s"sc_path_correct${i}", countAtPredSlot(pathCorrectEvent, i))
      XSPerfAccumulate(s"sc_path_wrong${i}", countAtPredSlot(pathWrongEvent, i))
      XSPerfAccumulate(s"sc_global_correct${i}", countAtPredSlot(globalCorrectEvent, i))
      XSPerfAccumulate(s"sc_global_wrong${i}", countAtPredSlot(globalWrongEvent, i))
      XSPerfAccumulate(s"sc_bw_correct${i}", countAtPredSlot(bwCorrectEvent, i))
      XSPerfAccumulate(s"sc_bw_wrong${i}", countAtPredSlot(bwWrongEvent, i))
      XSPerfAccumulate(s"sc_imli_correct${i}", countAtPredSlot(imliCorrectEvent, i))
      XSPerfAccumulate(s"sc_imli_wrong${i}", countAtPredSlot(imliWrongEvent, i))
      XSPerfAccumulate(s"sc_bias_correct${i}", countAtPredSlot(biasCorrectEvent, i))
      XSPerfAccumulate(s"sc_bias_wrong${i}", countAtPredSlot(biasWrongEvent, i))

      XSPerfAccumulate(s"path_table_change${i}", pathChangeVec(i))
      XSPerfAccumulate(s"global_table_change${i}", globalChangeVec(i))
      XSPerfAccumulate(s"bw_table_change${i}", bwChangeVec(i))
      XSPerfAccumulate(s"imli_table_change${i}", imliChangeVec(i))
      XSPerfAccumulate(s"bias_table_change${i}", biasChangeVec(i))
      XSPerfAccumulate(s"sc_train${i}", changeVec(i))
    }

    val scCorrectTageWrongEvent = scCorrectEvent.zip(tageWrongEvent).map {
      case (scCorrect, tageWrong) => scCorrect && tageWrong
    }
    val scWrongTageCorrectEvent = scWrongEvent.zip(tageCorrectEvent).map {
      case (scWrong, tageCorrect) => scWrong && tageCorrect
    }
    val scCorrectTageCorrectEvent = scCorrectEvent.zip(tageCorrectEvent).map {
      case (scCorrect, tageCorrect) => scCorrect && tageCorrect
    }
    val scWrongTageWrongEvent = scWrongEvent.zip(tageWrongEvent).map {
      case (scWrong, tageWrong) => scWrong && tageWrong
    }
    val notUsedScCorrectTageWrongEvent = branchIndices.map { i =>
      notUseScEvent(i) && actualTaken(i) === t1_meta.scPred(predSlotIdx(i)) &&
      actualTaken(i) =/= t1_meta.tagePred(predSlotIdx(i))
    }
    val notUsedScWrongTageCorrectEvent = branchIndices.map { i =>
      notUseScEvent(i) && actualTaken(i) =/= t1_meta.scPred(predSlotIdx(i)) &&
      actualTaken(i) === t1_meta.tagePred(predSlotIdx(i))
    }

    XSPerfAccumulate("total_sc_train", PopCount(changeVec))
    XSPerfAccumulate("total_train_use_sc", PopCount(trainUseScEvent))
    XSPerfAccumulate("total_pred_use_sc", PopCount(useScEvent))
    XSPerfAccumulate("total_sc_correct_tage_wrong", PopCount(scCorrectTageWrongEvent))
    XSPerfAccumulate("total_sc_wrong_tage_correct", PopCount(scWrongTageCorrectEvent))
    XSPerfAccumulate("total_sc_correct_tage_correct", PopCount(scCorrectTageCorrectEvent))
    XSPerfAccumulate("total_sc_wrong_tage_wrong", PopCount(scWrongTageWrongEvent))
    XSPerfAccumulate("total_sc_correct", PopCount(scCorrectEvent))
    XSPerfAccumulate("total_sc_wrong", PopCount(scWrongEvent))
    XSPerfAccumulate("total_tage_correct", PopCount(tageCorrectEvent))
    XSPerfAccumulate("total_tage_wrong", PopCount(tageWrongEvent))
    XSPerfAccumulate("total_not_used_sc_correct_tage_wrong", PopCount(notUsedScCorrectTageWrongEvent))
    XSPerfAccumulate("total_not_used_sc_wrong_tage_correct", PopCount(notUsedScWrongTageCorrectEvent))

    XSPerfAccumulate("total_sc_path_correct", PopCount(pathCorrectEvent))
    XSPerfAccumulate("total_sc_path_wrong", PopCount(pathWrongEvent))
    XSPerfAccumulate("total_sc_global_correct", PopCount(globalCorrectEvent))
    XSPerfAccumulate("total_sc_global_wrong", PopCount(globalWrongEvent))
    XSPerfAccumulate("total_sc_bw_correct", PopCount(bwCorrectEvent))
    XSPerfAccumulate("total_sc_bw_wrong", PopCount(bwWrongEvent))
    XSPerfAccumulate("total_sc_imli_correct", PopCount(imliCorrectEvent))
    XSPerfAccumulate("total_sc_imli_wrong", PopCount(imliWrongEvent))
    XSPerfAccumulate("total_sc_bias_correct", PopCount(biasCorrectEvent))
    XSPerfAccumulate("total_sc_bias_wrong", PopCount(biasWrongEvent))

    XSPerfAccumulate("threshold_try_overflow", PopCount(t1_thresholdOverflowVec))
    XSPerfAccumulate("threshold_try_underflow", PopCount(t1_thresholdUnderflowVec))

    val pathPredIdxDiff = t1_writeValid && t1_meta.debug_predPathIdx.get.zip(t1_pathSetIdx).map {
      case (predIdx, trainIdx) => predIdx =/= trainIdx
    }.reduce(_ || _)
    val globalPredIdxDiff = t1_writeValid && t1_meta.debug_predGlobalIdx.get.zip(t1_globalSetIdx).map {
      case (predIdx, trainIdx) => predIdx =/= trainIdx
    }.reduce(_ || _)
    val biasPredIdxDiff = t1_writeValid && t1_meta.debug_predBiasIdx.get =/= t1_biasSetIdx

    XSPerfAccumulate("sc_path_predIdx_diff_trainIdx", pathPredIdxDiff)
    XSPerfAccumulate("sc_global_predIdx_diff_trainIdx", globalPredIdxDiff)
    XSPerfAccumulate("sc_bias_predIdx_diff_trainIdx", biasPredIdxDiff)
  }

  XSPerfAccumulate("sc_global_table_invalid", s0_fire && !s0_commonHR.valid)
  XSPerfAccumulate("sc_global_table_valid", s0_fire && s0_commonHR.valid)

  if (EnableScTrace) {
    val scTraceVec = Wire(Vec(ResolveEntryBranchNumber, Valid(new ScConditionalBranchTrace)))
    scTraceVec.zipWithIndex.foreach { case (trace, i) =>
      val predSlotIdx = t1_branchesScIdxVec(i)
      val tableWayIdx = t1_branchesWayIdxVec(i)
      val actualTaken = t1_branches(i).bits.taken
      val scCorrect   = actualTaken === t1_meta.scPred(predSlotIdx)
      val tageCorrect = actualTaken === t1_meta.tagePred(predSlotIdx)
      val finalPred   = Mux(t1_meta.useScPred(predSlotIdx), t1_meta.scPred(predSlotIdx), t1_meta.tagePred(predSlotIdx))

      trace.valid               := t1_resolvedScValidVec(i)
      trace.bits.startPc        := t1_startPc
      trace.bits.cfiPc          := t1_branches(i).bits.debug_realCfiPc.getOrElse(0.U(VAddrBits.W))
      trace.bits.predSlotIdx    := predSlotIdx
      trace.bits.tableWayIdx    := tableWayIdx
      trace.bits.updateValid    := t1_writeValidVec(i)
      trace.bits.trainDataValid := t1_writeValid

      trace.bits.providerValid := t1_meta.tagePredValid(predSlotIdx)
      trace.bits.providerTaken := t1_meta.tagePred(predSlotIdx)
      trace.bits.providerCtr   := t1_meta.tageCtr(predSlotIdx)

      trace.bits.predCommonHRValid := t1_meta.debug_commonHRValid.get
      trace.bits.predPathSetIdx    := VecInit(t1_meta.debug_predPathIdx.get.map(_.pad(ScSetIdxWidth)))
      trace.bits.predGlobalSetIdx  := VecInit(t1_meta.debug_predGlobalIdx.get.map(_.pad(ScSetIdxWidth)))
      trace.bits.predBWSetIdx      := VecInit(t1_meta.debug_predBWIdx.get.map(_.pad(ScSetIdxWidth)))
      trace.bits.predImliSetIdx    := t1_meta.debug_predImliIdx.get
      trace.bits.predBiasSetIdx    := t1_meta.debug_predBiasIdx.get
      trace.bits.pathPred          := t1_meta.debug_scPathTakenVec.get(predSlotIdx)
      trace.bits.globalPred        := t1_meta.debug_scGlobalTakenVec.get(predSlotIdx)
      trace.bits.bwPred            := t1_meta.debug_scBWTakenVec.get(predSlotIdx)
      trace.bits.imliPred          := t1_meta.debug_scImliTakenVec.get(predSlotIdx)
      trace.bits.biasPred          := t1_meta.debug_scBiasTakenVec.get(predSlotIdx)
      trace.bits.totalPercsum      := t1_meta.debug_totalPercsum.get(predSlotIdx)
      trace.bits.threshold         := t1_meta.debug_threshold.get(predSlotIdx)
      trace.bits.sumAboveThres     := t1_meta.sumAboveThres(predSlotIdx)
      trace.bits.scPred            := t1_meta.scPred(predSlotIdx)
      trace.bits.useSc             := t1_meta.useScPred(predSlotIdx)

      trace.bits.trainCommonHRValid := t1_commonHR.valid
      trace.bits.trainPathSetIdx    := VecInit(t1_pathSetIdx.map(_.pad(ScSetIdxWidth)))
      trace.bits.trainGlobalSetIdx  := VecInit(t1_globalSetIdx.map(_.pad(ScSetIdxWidth)))
      trace.bits.trainBWSetIdx      := VecInit(t1_bwSetIdx.map(_.pad(ScSetIdxWidth)))
      trace.bits.trainImliSetIdx    := t1_imliSetIdx
      trace.bits.trainBiasSetIdx    := t1_biasSetIdx
      trace.bits.trainPathResp      := VecInit(t1_oldPathEntries.map(entries => entries(tableWayIdx).asUInt))
      trace.bits.trainGlobalResp    := VecInit(t1_oldGlobalEntries.map(entries => entries(tableWayIdx).asUInt))
      trace.bits.trainBWResp        := VecInit(t1_oldBWEntries.map(entries => entries(tableWayIdx).asUInt))
      trace.bits.trainImliResp      := t1_oldImliEntries(tableWayIdx).asUInt
      val biasWayIdx = Cat(tableWayIdx, t1_oldBiasLowBits(predSlotIdx))
      trace.bits.trainBiasResp := t1_oldBiasEntries(biasWayIdx).asUInt

      trace.bits.actualTaken  := actualTaken
      trace.bits.mispredict   := t1_branches(i).bits.mispredict
      trace.bits.finalPred    := finalPred
      trace.bits.finalCorrect := finalPred === actualTaken

      trace.bits.scCorrectTageWrong   := scCorrect && !tageCorrect
      trace.bits.scWrongTageCorrect   := !scCorrect && tageCorrect
      trace.bits.scCorrectTageCorrect := scCorrect && tageCorrect
      trace.bits.scWrongTageWrong     := !scCorrect && !tageCorrect
      trace.bits.scWrong              := !scCorrect
      trace.bits.scCorrect            := scCorrect
    }

    val scTraceDBTables = (0 until ResolveEntryBranchNumber).map { i =>
      ChiselDB.createTable(s"scCondTrace_${i}", new ScConditionalBranchTrace, EnableScTrace)
    }
    scTraceDBTables.zip(scTraceVec).foreach { case (dbTable, condTrace) =>
      dbTable.log(
        data = condTrace.bits,
        en = condTrace.valid,
        clock = clock,
        reset = reset
      )
    }
  }
}
