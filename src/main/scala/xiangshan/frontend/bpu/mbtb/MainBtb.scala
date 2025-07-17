// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchPrediction
import xiangshan.frontend.bpu.WriteBuffer

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // training specific bundle
    val train:      Valid[MainBtbTrain] = Flipped(Valid(new MainBtbTrain))
    val prediction: BranchPrediction    = Output(new BranchPrediction)
    val meta:       MainBtbMeta         = Output(new MainBtbMeta)
  }

  val io: MainBtbIO = IO(new MainBtbIO)

  /* *** internal parameters *** */
  private val Alignment = FetchBlockSize / NumAlignBanks

  /* *** submodules *** */
  private val sramBanks =
    Seq.tabulate(NumAlignBanks, NumWay, NumInternalBanks) { (alignIdx, wayIdx, bankIdx) =>
      Module(
        new SRAMTemplate(
          new MainBtbEntry,
          set = NumSets,
          way = 1, // Not using way in the template, preparing for future skewed assoc
          singlePort = true,
          shouldReset = true,
          withClockGate = true,
          hasMbist = hasMbist,
          hasSramCtl = hasSramCtl
        )
      ).suggestName(s"mbtb_sram_bank_align${alignIdx}_way${wayIdx}_bank${bankIdx}")
    }
  private val writeBuffers = Seq.tabulate(NumAlignBanks, NumWay, NumInternalBanks) { (_, _, _) =>
    Module(new WriteBuffer(new MainBtbSramWriteReq, WriteBufferSize, pipe = true))
  }

  sramBanks.map(_.map(_.map { m =>
    m.io.r.req.valid       := false.B
    m.io.r.req.bits.setIdx := 0.U
    m.io.w.req.valid       := false.B
    m.io.w.req.bits.setIdx := 0.U
    m.io.w.req.bits.data   := DontCare
  })) // Default closed, addrs are pulled to 0 to reduce power.

  sramBanks.flatten.flatten.zip(writeBuffers.flatten.flatten).foreach {
    case (bank: SRAMTemplate[MainBtbEntry], buf: WriteBuffer[MainBtbSramWriteReq]) =>
      bank.io.w.req.valid        := buf.io.read.valid && !bank.io.r.req.valid
      bank.io.w.req.bits.data(0) := buf.io.read.bits.entry
      bank.io.w.req.bits.setIdx  := buf.io.read.bits.setIdx
      buf.io.read.ready          := bank.io.w.req.ready && !bank.io.r.req.valid
  }

  /* predict stage 0
   * setup SRAM
   */
  private val s0_fire             = io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr       = io.startVAddr
  private val s0_thisSetIdx       = getSetIndex(s0_startVAddr)
  private val s0_nextSetIdx       = s0_thisSetIdx + 1.U
  private val s0_internalBankIdx  = getInternalBankIndex(s0_startVAddr)
  private val s0_internalBankMask = UIntToOH(s0_internalBankIdx) & Fill(NumInternalBanks, s0_fire)
  private val s0_alignBankIdx     = getAlignBankIndex(s0_startVAddr)
  private val s0_setIdxVec: Vec[UInt] =
    VecInit.tabulate(NumAlignBanks)(bankIdx => Mux(bankIdx.U < s0_alignBankIdx, s0_nextSetIdx, s0_thisSetIdx))
  require(s0_thisSetIdx.getWidth == SetIdxLen, s"Set index width mismatch: ${s0_thisSetIdx.getWidth} != $SetIdxLen")
  XSError(
    s0_internalBankIdx >= NumInternalBanks.U,
    s"Invalid internal bank index: $s0_internalBankIdx, max: ${NumInternalBanks - 1}"
  )
  sramBanks zip s0_setIdxVec foreach { case (alignmentBank, setIdx) =>
    alignmentBank.foreach { way =>
      way zip s0_internalBankMask.asBools foreach { case (internalBank, bankEnable) =>
        internalBank.io.r.req.valid := bankEnable
        internalBank.io.r.req.bits.setIdx := Mux(
          bankEnable,
          setIdx,
          0.U
        ) // pull to 0 when not firing to reduce power.
      }
    }
  }

  /* predict stage 1
   *
   * get result from SRAM
   * rotate SRAM result
   */
  private val s1_fire             = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr       = RegEnable(s0_startVAddr, s0_fire)
  private val s1_internalBankIdx  = RegEnable(s0_internalBankIdx, s0_fire)
  private val s1_internalBankMask = RegEnable(s0_internalBankMask, s0_fire)
  private val s1_tag              = getTag(s1_startVAddr)
  private val s1_alignBankIdx     = getAlignBankIndex(s1_startVAddr)
  private val s1_posHighestBits: Vec[UInt] =
    VecInit(for {
      bankIdx <- 0 until NumAlignBanks
      _       <- 0 until NumWay
    } yield bankIdx.U + s1_alignBankIdx) // FIXME: not working for NumAlignBanks > 2
  private val s1_rawBtbEntries: Vec[MainBtbEntry] =
    VecInit(sramBanks.flatMap(a =>
      VecInit(a.map(w => Mux1H(s1_internalBankMask, VecInit(w.map(_.io.r.resp.data(0))))))
    ))

  require(s1_alignBankIdx.getWidth == log2Ceil(NumAlignBanks))

  /* predict stage 2
   *
   * do tag compare and postion compare
   * calculate target
   * map results into a per-slot vec
   * resolve multi-hit
   */
  private val s2_fire          = io.stageCtrl.s2_fire && io.enable
  private val s2_startVAddr    = RegEnable(s1_startVAddr, s1_fire)
  private val s2_rawBtbEntries = RegEnable(s1_rawBtbEntries, s1_fire)
  private val s2_tag           = RegEnable(s1_tag, s1_fire)
  private val s2_posHighesBits = RegEnable(s1_posHighestBits, s1_fire)
  private val s2_positions = s2_rawBtbEntries zip s2_posHighesBits map { case (entry, h) =>
    Cat(h, entry.position) // Add higher bits before using
  }
  private val s2_hitMask: Vec[Bool] =
    VecInit(s2_rawBtbEntries.map(entry => entry.valid && entry.tag === s2_tag))
  private val s2_perBankSignals = Seq.tabulate(PredictWidth) { pos =>
    val posHitMask = s2_hitMask zip s2_positions map { case (hit, p) =>
      hit && p === pos.U
    }
    val hit      = posHitMask.reduce(_ || _)
    val entry    = PriorityMux(posHitMask, s2_rawBtbEntries) // Multi-hit is resolved here by priority mux
    val target   = 0.U                                       // FIXME: calculate target address
    val multihit = PopCount(posHitMask) > 1.U
    (hit, entry, target, multihit)
  }
  private val (s2_brValids, s2_btbEntries, s2_targets, s2_multihits) =
    (s2_perBankSignals.map(_._1), s2_perBankSignals.map(_._2), s2_perBankSignals.map(_._3), s2_perBankSignals.map(_._4))

  io.prediction              := DontCare // FIXME: temp
  io.meta.valid              := s2_fire
  io.meta.hitMask            := s2_hitMask
  io.meta.positions          := s2_positions
  io.meta.stronglyBiasedMask := DontCare // FIXME: add bias logic

  /* training stage 1 */
  private val t1_train_valid      = RegEnable(io.train.valid, io.enable)
  private val t1_train            = RegEnable(io.train.bits, io.train.valid)
  private val t1_taken            = t1_train.taken
  private val t1_internalBankIdx  = getInternalBankIndex(t1_train.startVAddr)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx)
  private val t1_thisSetIdx       = getSetIndex(t1_train.startVAddr)
  private val t1_nextSetIdx       = t1_thisSetIdx + 1.U
  private val t1_alignBankIdx     = getAlignBankIndex(t1_train.startVAddr)
  private val t1_meta             = t1_train.meta
  private val t1_LFSR             = random.LFSR(16, true.B)
  private val t1_setIdxVec: Vec[UInt] =
    VecInit.tabulate(NumAlignBanks)(bankIdx => Mux(bankIdx.U < t1_alignBankIdx, t1_nextSetIdx, t1_thisSetIdx))

  // Only write into sram when branch is not already in BTB
  // FIXME: take branch attribute and target into account
  private val t1_updateHit = t1_train_valid &&
    (t1_meta.hitMask zip t1_meta.positions map {
      case (hit, pos) => hit && pos === t1_train.cfiPosition
    }).reduce(_ || _)
  private val t1_writeValid = t1_train_valid && !t1_updateHit && t1_taken

  private val t1_writeEntry = Wire(new MainBtbEntry)
  t1_writeEntry.valid          := true.B                 // FIXME: invalidate
  t1_writeEntry.tag            := getTag(t1_train.startVAddr)
  t1_writeEntry.position       := t1_train.cfiPosition
  t1_writeEntry.target         := t1_train.target.asUInt // FIXME: calculate target address
  t1_writeEntry.attribute      := t1_train.attribute
  t1_writeEntry.stronglyBiased := false.B                // FIXME
  t1_writeEntry.replaceCnt     := DontCare               // FIXME:
  private val t1_writeWayMask = UIntToOH(t1_LFSR(log2Ceil(NumWay) - 1, 0))
  private val t1_writeAlignBankMask = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    bankIdx.U === (t1_train.cfiPosition.asBools.last + t1_alignBankIdx) // FIXME: not working for NumAlignBanks > 2
  )
  dontTouch(t1_writeAlignBankMask)

  // Write to SRAM
  writeBuffers zip t1_setIdxVec zip t1_writeAlignBankMask foreach { case ((alignmentBank, setIdx), alignBankEnable) =>
    alignmentBank zip t1_writeWayMask.asBools foreach { case (way, wayEnable) =>
      way zip t1_internalBankMask.asBools foreach { case (internalBank, bankEnable) =>
        val writeEnable = t1_writeValid && wayEnable && alignBankEnable && bankEnable
        internalBank.io.write.valid := writeEnable
        internalBank.io.write.bits.setIdx := Mux(
          writeEnable,
          setIdx,
          0.U
        ) // pull to 0 when not firing to reduce power.
        internalBank.io.write.bits.entry := t1_writeEntry
      }
    }
  }

  /* ** statistics ** */

  XSPerfAccumulate("mbtb_pred_has_hit", s2_fire && s2_hitMask.reduce(_ || _))
  XSPerfHistogram("mbtb_pred_hit_count", PopCount(s2_hitMask), s2_fire, 0, NumWay * NumAlignBanks)
  XSPerfAccumulate("mbtb_update_new_entry", t1_writeValid)
  XSPerfAccumulate("mbtb_update_hit", t1_updateHit)
  XSPerfHistogram("mbtb_multihit_count", PopCount(s2_multihits), s2_fire, 0, NumWay * NumAlignBanks)

}
