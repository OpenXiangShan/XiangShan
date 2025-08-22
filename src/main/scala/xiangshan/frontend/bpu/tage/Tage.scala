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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BtbHelper
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.mbtb.MainBtbResult

class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with Helpers with BtbHelper {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction bundles
    val mbtbResult: MainBtbResult = Input(new MainBtbResult)
    val prediction: Prediction    = Output(new Prediction)
    val meta:       TageMeta      = Output(new TageMeta)
  }

  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val baseTableSramBanks =
    Seq.tabulate(BaseTableNumAlignBanks, BaseTableInternalBanks) { (alignIdx, bankIdx) =>
      Module(
        new SRAMTemplate(
          new SaturateCounter(BaseTableCtrWidth),
          set = BaseTableSramSets,
          way = FetchBlockAlignInstNum,
          singlePort = true,
          shouldReset = true,
          withClockGate = true,
          hasMbist = hasMbist,
          hasSramCtl = hasSramCtl
        )
      ).suggestName(s"tage_base_table_sram_align${alignIdx}_bank${bankIdx}")
    }

  private val resetDone = RegInit(false.B)
  when(baseTableSramBanks.flatMap(_.map(_.io.r.req.ready)).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  private val baseTableWriteBuffers =
    Seq.tabulate(BaseTableNumAlignBanks, BaseTableInternalBanks) { (_, _) =>
      Module(new WriteBuffer(new BaseTableSramWriteReq, WriteBufferSize, numPorts = 1, pipe = true, hasTag = false))
    }

  // Connect write buffers to SRAMs
  baseTableSramBanks.flatten.zip(baseTableWriteBuffers.flatten).foreach {
    case (bank, buf) => {
      bank.io.w.req.valid := buf.io.read(0).valid && !bank.io.r.req.valid
      bank.io.w.req.bits.waymask.foreach(_ := buf.io.read(0).bits.waymasks)
      bank.io.w.req.bits.data   := buf.io.read(0).bits.ctrs
      bank.io.w.req.bits.setIdx := buf.io.read(0).bits.setIdx
      buf.io.read(0).ready      := bank.io.w.req.ready && !bank.io.r.req.valid
    }
  }

  /* predict stage 0
   * setup SRAM
   */
  private val s0_fire           = io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr     = io.startVAddr
  private val s0_baseSetIdx     = getBaseSetIndex(s0_startVAddr)
  private val s0_nextBaseSetIdx = s0_baseSetIdx + 1.U
  // FIXME: halfAlign
  private val s0_baseTableInternalBankIdx  = getBaseTableInternalBankIndex(s0_startVAddr)
  private val s0_baseTableInternalBankMask = UIntToOH(s0_baseTableInternalBankIdx, BaseTableInternalBanks)

  baseTableSramBanks foreach { alignmentBank =>
    alignmentBank zip s0_baseTableInternalBankMask.asBools foreach {
      case (bank, bankEnable) => {
        bank.io.r.req.valid       := s0_fire && bankEnable
        bank.io.r.req.bits.setIdx := s0_baseSetIdx
      }
    }
  }

  /* predict stage 1
   * get result from SRAM
   */
  private val s1_fire                      = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr                = RegEnable(s0_startVAddr, s0_fire)
  private val s1_baseTableInternalBankMask = RegEnable(s0_baseTableInternalBankMask, s0_fire)
  private val s1_rawBaseTableCtrs: Vec[SaturateCounter] =
    VecInit(baseTableSramBanks.flatMap((alignmentBank: Seq[SRAMTemplate[SaturateCounter]]) =>
      Mux1H(s1_baseTableInternalBankMask, alignmentBank.map(_.io.r.resp.data))
    ))
  require(s1_rawBaseTableCtrs.length == FetchBlockInstNum)

  /* predict stage 2
   * calculate prediction
   */
  private val s2_fire             = io.stageCtrl.s2_fire && io.enable
  private val s2_rawBaseTableCtrs = RegEnable(s1_rawBaseTableCtrs, s1_fire)
  // positions should be directly from SRAM, without tag comparison
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbHitMask    = io.mbtbResult.hitMask
  private val s2_mbtbTargets    = io.mbtbResult.targets
  private val s2_mbtbAttributes = io.mbtbResult.attributes
  private val s2_takens = s2_mbtbPositions.zip(s2_mbtbHitMask).map { case (pos, hit) =>
    hit && s2_rawBaseTableCtrs(pos).isPositive
  }
  private val s2_firstTakenOH   = getFirstTakenEntryWayIdxOH(s2_mbtbPositions, s2_takens)
  private val s2_target         = Mux1H(s2_firstTakenOH, s2_mbtbTargets)
  private val s2_taken          = s2_takens.reduce(_ || _)
  private val s2_takenPosition  = Mux1H(s2_firstTakenOH, s2_mbtbPositions)
  private val s2_takenAttribute = Mux1H(s2_firstTakenOH, s2_mbtbAttributes)

  /* predict stage 3
   * do basically nothing, clean reg out
   */
  private val s3_fire             = io.stageCtrl.s3_fire && io.enable
  private val s3_target           = RegEnable(s2_target, s2_fire)
  private val s3_taken            = RegEnable(s2_taken, s2_fire)
  private val s3_takenPosition    = RegEnable(s2_takenPosition, s2_fire)
  private val s3_takenAttribute   = RegEnable(s2_takenAttribute, s2_fire)
  private val s3_rawBaseTableCtrs = RegEnable(s2_rawBaseTableCtrs, s2_fire)

  io.prediction.target      := s3_target
  io.prediction.taken       := s3_taken
  io.prediction.cfiPosition := s3_takenPosition
  io.prediction.attribute   := s3_takenAttribute

  io.meta.valid               := s3_fire
  io.meta.baseTableCtrs       := s3_rawBaseTableCtrs
  io.meta.debug_taken         := s3_taken
  io.meta.debug_takenPosition := s3_takenPosition

  /* training */
  private val t1_trainValid                = RegNext(io.train.valid && io.enable)
  private val t1_train                     = RegEnable(io.train.bits, io.train.valid && io.enable)
  private val t1_startVAddr                = t1_train.startVAddr
  private val t1_alignBankIdx              = getAlignBankIndex(t1_startVAddr)
  private val t1_baseTableInternalBankIdx  = getBaseTableInternalBankIndex(t1_startVAddr)
  private val t1_baseTableInternalBankMask = UIntToOH(t1_baseTableInternalBankIdx, BaseTableInternalBanks).asBools
  private val t1_baseTableSetIdx           = getBaseSetIndex(t1_startVAddr)
  private val t1_oldBaseTableCtrs          = t1_train.meta.tage.baseTableCtrs
  private val t1_mbtbPositions             = t1_train.meta.mbtb.positions
  private val t1_mbtbHitMasks              = t1_train.meta.mbtb.hitMask
  // FIXME: train only on conditional
  private val t1_cfiPosition = t1_train.cfiPosition
  private val t1_taken       = t1_train.taken
  // FIXME: halfAlign
  private val t1_baseTableWriteCtrs: Vec[Vec[SaturateCounter]] =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableCtrWidth))))
  private val t1_baseTableWriteWaymask: Vec[Vec[Bool]] =
    Wire(Vec(BaseTableNumAlignBanks, Vec(FetchBlockAlignInstNum, Bool())))
  private val t1_debug_taken         = t1_train.meta.tage.debug_taken
  private val t1_debug_takenPosition = t1_train.meta.tage.debug_takenPosition
  private val t1_debug_mispredict    = t1_debug_takenPosition =/= t1_cfiPosition || t1_debug_taken =/= t1_taken

  t1_baseTableWriteCtrs := DontCare
  t1_baseTableWriteWaymask.foreach(_.foreach(_ := false.B))
  for (pos <- 0 until PredictWidth) {
    // FIXME: won't work when alignment bank is not 2
    when(pos.U < t1_cfiPosition) {
      t1_baseTableWriteCtrs(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ).value := t1_oldBaseTableCtrs(pos).getDecrease
      t1_baseTableWriteWaymask(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ) := true.B
    }.elsewhen(pos.U === t1_cfiPosition) {
      t1_baseTableWriteCtrs(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ).value := t1_oldBaseTableCtrs(pos).getUpdate(t1_taken)
      t1_baseTableWriteWaymask(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ) := true.B
    }.otherwise {
      t1_baseTableWriteCtrs(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ).value := DontCare
      t1_baseTableWriteWaymask(pos.U.asBools.last + t1_alignBankIdx)(
        pos.U.take(log2Ceil(FetchBlockAlignInstNum))
      ) := false.B
    }
  }

  // Write to writebuffer
  baseTableWriteBuffers zip t1_baseTableWriteCtrs zip t1_baseTableWriteWaymask foreach {
    case ((alignmentBank, ctrs), waymask) =>
      alignmentBank zip t1_baseTableInternalBankMask foreach {
        case (buf, bankEnable) => {
          buf.io.write(0).valid         := t1_trainValid && bankEnable
          buf.io.write(0).bits.setIdx   := t1_baseTableSetIdx
          buf.io.write(0).bits.ctrs     := ctrs
          buf.io.write(0).bits.waymasks := waymask.asUInt
        }
      }
  }

  /* *** perf counters */
  XSPerfAccumulate("tage_mispredict", t1_debug_mispredict && t1_trainValid)

}
