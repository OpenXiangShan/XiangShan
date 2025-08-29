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
import xiangshan.frontend.bpu.WriteBuffer

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction specific bundle
    val result: MainBtbResult = Output(new MainBtbResult)
    val meta:   MainBtbMeta   = Output(new MainBtbMeta)
  }

  val io: MainBtbIO = IO(new MainBtbIO)

  /* *** internal parameters *** */
  private val Alignment = FetchBlockSize / NumAlignBanks

  /* *** submodules *** */
  private val sramBanks =
    Seq.tabulate(NumAlignBanks, NumInternalBanks, NumWay) { (alignIdx, bankIdx, wayIdx) =>
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
      ).suggestName(s"mbtb_sram_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
    }
  private val writeBuffers = Seq.tabulate(NumAlignBanks, NumInternalBanks) { (_, _) =>
    Module(new WriteBuffer(new MainBtbSramWriteReq, WriteBufferSize, NumWay, pipe = true))
  }

  private val resetDone = RegInit(false.B)
  when(sramBanks.flatMap(_.flatMap(_.map(_.io.r.req.ready))).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  private val replacer = Module(new MainBtbReplacer)

  sramBanks.map(_.map(_.map { m =>
    m.io.r.req.valid       := false.B
    m.io.r.req.bits.setIdx := 0.U
    m.io.w.req.valid       := false.B
    m.io.w.req.bits.setIdx := 0.U
    m.io.w.req.bits.data   := DontCare
  })) // Default closed, addrs are pulled to 0 to reduce power.

  sramBanks.flatten.zip(writeBuffers.flatten).foreach {
    case (ways, buffer) =>
      ways zip buffer.io.read foreach {
        case (way: SRAMTemplate[MainBtbEntry], buf: DecoupledIO[MainBtbSramWriteReq]) =>
          way.io.w.req.valid        := buf.valid && !way.io.r.req.valid
          way.io.w.req.bits.data(0) := buf.bits.entry
          way.io.w.req.bits.setIdx  := buf.bits.setIdx
          buf.ready                 := way.io.w.req.ready && !way.io.r.req.valid
      }
  }

  /* predict stage 0
   * setup SRAM
   */
  private val s0_fire             = io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr       = io.startVAddr
  private val s0_thisSetIdx       = getSetIndex(s0_startVAddr)
  private val s0_nextSetIdx       = s0_thisSetIdx + 2.U
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
    alignmentBank zip s0_internalBankMask.asBools foreach { case (internalBank, bankEnable) =>
      when(bankEnable) {
        internalBank.foreach { way =>
          way.io.r.req.valid       := true.B
          way.io.r.req.bits.setIdx := setIdx
        }
      }.otherwise {
        // pull to 0 when not firing to reduce power.
        internalBank.foreach { way =>
          way.io.r.req.valid       := false.B
          way.io.r.req.bits.setIdx := 0.U
        }
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

  private val s1_rawBtbEntries: Vec[MainBtbEntry] = VecInit(sramBanks.map(a =>
    VecInit(for {
      wayIdx <- 0 until NumWay
    } yield {
      val way = VecInit(a.map(_(wayIdx).io.r.resp.data(0)))
      Mux1H(s1_internalBankMask, way)
    })
  ).flatten)

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
  private val s2_targets =
    s2_rawBtbEntries.map(e =>
      getFullTarget(s2_startVAddr, e.targetLowerBits, Some(e.targetCarry))
    ) // FIXME: parameterize target carry

  private val s2_alignBankIdx       = getAlignBankIndex(s2_startVAddr)
  private val s2_thisReplacerSetIdx = getReplacerSetIndex(s2_startVAddr)
  private val s2_nextReplacerSetIdx = s2_thisReplacerSetIdx + 1.U
  private val s2_replacerSetIdxVec: Vec[UInt] = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    Mux(bankIdx.U < s2_alignBankIdx, s2_nextReplacerSetIdx, s2_thisReplacerSetIdx)
  )

  private val s2_stateTouchs: Vec[Vec[Valid[UInt]]] =
    Wire(Vec(NumAlignBanks, Vec(NumWay, Valid(UInt(log2Up(NumWay).W)))))
  // FIXME: this is not a good way to do this, but it works for now
  for (alignIdx <- 0 until NumAlignBanks; wayIdx <- 0 until NumWay) {
    s2_stateTouchs(alignIdx)(wayIdx).valid := s2_fire && s2_hitMask(alignIdx * NumWay + wayIdx)
    s2_stateTouchs(alignIdx)(wayIdx).bits  := wayIdx.U
  }
  replacer.io.predictionSetIndxVec := s2_replacerSetIdxVec
  replacer.io.predictionTouchWays  := s2_stateTouchs
  replacer.io.predictionHitMask    := VecInit(s2_stateTouchs.map(_.map(_.valid).reduce(_ || _) && s2_fire))

  dontTouch(s2_replacerSetIdxVec)
  dontTouch(s2_stateTouchs)
  // dontTouch(s2_nextState)

  private val debug_s2_perBankSignals = Seq.tabulate(PredictWidth) { pos =>
    val posHitMask = s2_hitMask zip s2_positions map { case (hit, p) =>
      hit && p === pos.U
    }
    val hit      = posHitMask.reduce(_ || _)
    val entry    = PriorityMux(posHitMask, s2_rawBtbEntries) // Multi-hit is resolved here by priority mux
    val target   = 0.U                                       // FIXME: calculate target address
    val multihit = PopCount(posHitMask) > 1.U
    (hit, entry, target, multihit)
  }
  private val (debug_s2_brValids, debug_s2_btbEntries, debug_s2_targets, debug_s2_multihits) =
    (
      debug_s2_perBankSignals.map(_._1),
      debug_s2_perBankSignals.map(_._2),
      debug_s2_perBankSignals.map(_._3),
      debug_s2_perBankSignals.map(_._4)
    )

  io.result.hitMask    := s2_hitMask
  io.result.positions  := s2_rawBtbEntries.map(_.position)
  io.result.targets    := s2_targets
  io.result.attributes := s2_rawBtbEntries.map(_.attribute)

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
  private val t1_meta             = t1_train.meta.mbtb
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
  t1_writeEntry.valid           := true.B   // FIXME: invalidate
  t1_writeEntry.tag             := getTag(t1_train.startVAddr)
  t1_writeEntry.position        := t1_train.cfiPosition
  t1_writeEntry.targetLowerBits := getTargetLowerBits(t1_train.target)
  t1_writeEntry.targetCarry     := getTargetCarry(t1_train.startVAddr, t1_train.target)
  t1_writeEntry.attribute       := t1_train.attribute
  t1_writeEntry.stronglyBiased  := false.B  // FIXME
  t1_writeEntry.replaceCnt      := DontCare // FIXME:
  private val t1_writeAlignBankMask = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    bankIdx.U === (t1_train.cfiPosition.asBools.last + t1_alignBankIdx) // FIXME: not working for NumAlignBanks > 2
  )
  private val t1_thisReplacerSetIdx = getReplacerSetIndex(t1_train.startVAddr)
  private val t1_nextReplacerSetIdx = t1_thisReplacerSetIdx + 2.U
  private val t1_replacerSetIdxVec: Vec[UInt] = VecInit.tabulate(NumAlignBanks)(bankIdx =>
    Mux(bankIdx.U < t1_alignBankIdx, t1_nextReplacerSetIdx, t1_thisReplacerSetIdx)
  )
  private val t1_replacerSetIdx = Mux1H(t1_writeAlignBankMask, t1_replacerSetIdxVec)
  private val t1_replacerBankMask: Vec[Bool] = t1_writeAlignBankMask
  replacer.io.trainWriteValid    := t1_writeValid
  replacer.io.trainSetIndx       := t1_replacerSetIdx
  replacer.io.trainAlignBankMask := t1_writeAlignBankMask

  dontTouch(t1_replacerSetIdxVec)
  dontTouch(t1_replacerSetIdx)
  dontTouch(t1_writeAlignBankMask)
  private val t1_writeWayMask = UIntToOH(replacer.io.victimWayIdx)
  require(t1_writeWayMask.getWidth == NumWay, s"Write way mask width mismatch: ${t1_writeWayMask.getWidth} != $NumWay")

  // Write to SRAM
  writeBuffers zip t1_setIdxVec zip t1_writeAlignBankMask foreach {
    case ((alignmentBank, setIdx), alignBankEnable) =>
      alignmentBank zip t1_internalBankMask.asBools foreach { case (buffer, bankEnable) =>
        buffer.io.write zip t1_writeWayMask.asBools foreach { case (port, wayEnable) =>
          val writeEnable = t1_writeValid && wayEnable && alignBankEnable && bankEnable
          port.valid := writeEnable
          port.bits.setIdx := Mux(
            writeEnable,
            setIdx,
            0.U
          ) // pull to 0 when not firing to reduce power.
          port.bits.entry := t1_writeEntry
        }
      }
  }

  dontTouch(t1_writeValid)
  dontTouch(t1_writeAlignBankMask)
  dontTouch(t1_internalBankMask)
  dontTouch(t1_writeWayMask)

  /* ** statistics ** */

  XSPerfAccumulate("mbtb_pred_has_hit", s2_fire && s2_hitMask.reduce(_ || _))
  XSPerfHistogram("mbtb_pred_hit_count", PopCount(s2_hitMask), s2_fire, 0, NumWay * NumAlignBanks)
  XSPerfAccumulate("mbtb_update_new_entry", t1_writeValid)
  XSPerfAccumulate("mbtb_update_hit", t1_updateHit)
  XSPerfHistogram("mbtb_multihit_count", PopCount(debug_s2_multihits), s2_fire, 0, NumWay * NumAlignBanks)

}
