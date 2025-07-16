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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BranchPrediction
import xiangshan.frontend.bpu.BtbHelper
import xiangshan.frontend.bpu.SaturateCounter

/**
 * This module is the implementation of the ahead BTB (Branch Target Buffer).
 */
class AheadBtb(implicit p: Parameters) extends BasePredictor with HasAheadBtbParameters with Helpers with BtbHelper {
  val io: AheadBtbIO = IO(new AheadBtbIO)

  private val banks     = Seq.fill(NumBanks)(Module(new Bank))
  private val replacers = Seq.fill(NumBanks)(Module(new Replacer))

  private val takenCounter = Reg(Vec(NumBanks, Vec(NumSets, Vec(NumWays, new SaturateCounter(TakenCounterWidth)))))
  // TODO: do we need useful counter?
//  private val usefulCounter = Reg(Vec(NumBanks, Vec(NumSets, Vec(NumWays, new SaturateCounter(UsefulCounterWidth)))))

  // TODO: write ctr bypass to read
  // TODO: invliadate multi-hit entry
  // TODO: train after execution

  private val s0_fire = Wire(Bool())
  private val s1_fire = Wire(Bool())
  private val s2_fire = Wire(Bool())

  private val s1_ready = Wire(Bool())
  private val s2_ready = Wire(Bool())

  private val s1_valid = RegInit(false.B)
  private val s2_valid = RegInit(false.B)

  private val predictReqValid = io.enable && io.stageCtrl.s0_fire
  private val redirectValid   = io.redirectValid
  private val overrideValid   = io.overrideValid

  s0_fire := predictReqValid
  s1_fire := s1_valid && s2_ready && predictReqValid
  s2_fire := s2_valid

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid

  when(s0_fire)(s1_valid := true.B)
    .elsewhen(redirectValid)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(redirectValid)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !redirectValid)
    .elsewhen(s2_fire)(s2_valid := false.B)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - get set index and bank index
     - send read request to selected bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_previousPc = io.startVAddr

  private val s0_setIdx   = getSetIndex(s0_previousPc)
  private val s0_bankIdx  = getBankIndex(s0_previousPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx)

  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.readReq.valid       := predictReqValid && s0_bankMask(i)
    b.io.readReq.bits.setIdx := s0_setIdx
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get the latest start pc for compare tag in s2
     - get entries from bank
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_startPc = io.startVAddr

  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)
  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)

  private val s1_entries = Mux1H(s1_bankMask, banks.map(_.io.readResp.entries))

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get taken counter result
     - compare tag and get taken mask
     - compare positions and find first taken entry
     - get target from found entry
     - output prediction
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_setIdx   = RegEnable(s1_setIdx, s1_fire)
  private val s2_bankIdx  = RegEnable(s1_bankIdx, s1_fire)
  private val s2_bankMask = RegEnable(s1_bankMask, s1_fire)
  private val s2_entries  = RegEnable(s1_entries, s1_fire)
  private val s2_startPc  = RegEnable(s1_startPc, s1_fire)

  //  private val s2_entriesDelay1 = RegNext(s2_entries)

  private val s2_ctrResult = takenCounter(s2_bankIdx)(s2_setIdx).map(_.isPositive)

  private val s2_tag = getTag(s2_startPc)
//  private val s2_realEntries = Mux(RegNext(io.overrideValid), s2_entriesDelay1, s2_entries)
  private val s2_realEntries = s2_entries // TODO
  private val s2_hitMask     = s2_entries.map(entry => entry.valid && entry.tag === s2_tag)
  private val s2_hit         = s2_hitMask.reduce(_ || _)
  private val s2_takenMask   = s2_hitMask.zip(s2_ctrResult).map { case (hit, taken) => hit && taken }
  private val s2_taken       = s2_takenMask.reduce(_ || _)

  private val perf_s2_multiHit = PopCount(s2_hitMask) > 1.U

  private val s2_positions               = s2_realEntries.map(_.position)
  private val s2_firstTakenEntryWayIdxOH = getFirstTakenEntryWayIdxOH(s2_positions, s2_takenMask)
  private val s2_firstTakenEntry         = Mux1H(s2_firstTakenEntryWayIdxOH, s2_realEntries)

  private val s2_takenPosition = s2_firstTakenEntry.position
  private val s2_target        = getTarget(s2_firstTakenEntry, s2_startPc)

  private val s2_prediction = Wire(new BranchPrediction)
  s2_prediction.taken       := s2_valid && s2_taken
  s2_prediction.cfiPosition := s2_takenPosition
  s2_prediction.target      := s2_target
  s2_prediction.attribute   := s2_firstTakenEntry.attribute

  io.prediction := s2_prediction
  io.hit        := s2_valid && s2_hit

  // used for check abtb output
  io.debug_startVaddr := s2_startPc

  private val meta = Wire(new AheadBtbMeta)
  meta.valid       := s2_valid
  meta.hitMask     := s2_hitMask
  meta.taken       := s2_taken
  meta.takenWayIdx := OHToUInt(s2_firstTakenEntryWayIdxOH)
  meta.attributes  := s2_realEntries.map(_.attribute)
  meta.positions   := s2_positions
  meta.target      := s2_target // TODO: remove it

  io.meta := meta

  replacers.zipWithIndex.foreach { case (r, i) =>
    r.io.readValid   := s2_hit && s2_bankMask(i)
    r.io.readSetIdx  := s2_setIdx
    r.io.readWayMask := s2_hitMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_train = io.train

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - update taken counter
     - write new entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  // delay one cycle for better timing
  private val t1_trainValid = RegNext(t0_train.valid)
  private val t1_train      = RegEnable(t0_train.bits, t0_train.valid)

  private val t1_previousPcValid = RegNext(t1_trainValid, init = false.B)
  private val t1_previousPc      = RegEnable(t1_train.startPc, t1_trainValid)

  private val t1_bankIdx  = getBankIndex(t1_previousPc)
  private val t1_bankMask = UIntToOH(t1_bankIdx)

  private val t1_setIdx  = getSetIndex(t1_previousPc)
  private val t1_setMask = UIntToOH(t1_setIdx)

  private val t1_meta = t1_train.abtbMeta

  private val t1_valid = t1_previousPcValid && t1_trainValid && t1_meta.valid

  // TODO: if we want update after execution, we need FTQ send a previousPc with one update request
  //  if we want update after commit, we just ues previous update request to get bankIdx and setIdx
  //  because the order of prediction and commit is the same

  /* --------------------------------------------------------------------------------------------------------------
     update taken counter
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_positionBeforeMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) => hit && pos < t1_train.position && attr.isConditional
  }
  private val t1_positionEqualMask = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) => hit && pos === t1_train.position && attr.isConditional
  }

  private val t1_needResetCtr = takenCounter.zip(banks).map { case (bankCtrs, bank) =>
    val needReset = bank.io.writeResp.valid && bank.io.writeResp.bits.needResetCtr
    val setMask   = UIntToOH(bank.io.writeResp.bits.setIdx)
    val wayMask   = bank.io.writeResp.bits.wayMask
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zipWithIndex.map { case (_, wayIdx) =>
        needReset && setMask(setIdx) && wayMask(wayIdx)
      }
    }
  }

  private val t1_needIncreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionEqualMask).map { case (_, equal) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) && equal && t1_train.taken
      }
    }
  }

  private val t1_needDecreaseCtr = takenCounter.zipWithIndex.map { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.map { case (setCtrs, setIdx) =>
      setCtrs.zip(t1_positionBeforeMask).zip(t1_positionEqualMask).map { case ((_, before), equal) =>
        t1_valid && t1_bankMask(bankIdx) && t1_setMask(setIdx) && (before || (equal && !t1_train.taken))
      }
    }
  }

  takenCounter.zipWithIndex.foreach { case (bankCtrs, bankIdx) =>
    bankCtrs.zipWithIndex.foreach { case (setCtrs, setIdx) =>
      setCtrs.zipWithIndex.foreach { case (ctr, wayIdx) =>
        val needReset    = t1_needResetCtr(bankIdx)(setIdx)(wayIdx)
        val needDecrease = t1_needDecreaseCtr(bankIdx)(setIdx)(wayIdx)
        val needIncrease = t1_needIncreaseCtr(bankIdx)(setIdx)(wayIdx)
        when(needReset)(ctr.resetNeutral())
          .elsewhen(needDecrease)(ctr.decrease())
          .elsewhen(needIncrease)(ctr.increase())
      }
    }
  }

  /* --------------------------------------------------------------------------------------------------------------
     generate a new entry or modify an existing entry if needed
     -------------------------------------------------------------------------------------------------------------- */

  // if the taken branch is not hit, we need write a new entry
  private val t1_hitTakenBranch = t1_meta.hitMask.zip(t1_meta.positions).zip(t1_meta.attributes).map {
    case ((hit, pos), attr) =>
      hit && t1_train.taken && pos === t1_train.position && attr === t1_train.attribute
  }.reduce(_ || _)
  private val t1_needWriteNewEntry = !t1_hitTakenBranch

  // if the target of indirect branch is wrong, we need correct it
  private val t1_needCorrectTarget = t1_meta.taken && t1_train.taken &&
    t1_meta.attributes(t1_meta.takenWayIdx).isIndirect && t1_train.attribute.isIndirect &&
    t1_meta.positions(t1_meta.takenWayIdx) === t1_train.position &&
    t1_meta.target =/= t1_train.target

  // TODO: if the attribute of the taken branch is wrong, we need replace it or invalidate it

  private val t1_writeBankValid = t1_valid && (t1_needWriteNewEntry || t1_needCorrectTarget)

  private val t1_writeEntry = Wire(new AheadBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tag             := getTag(t1_train.startPc)
  t1_writeEntry.position        := t1_train.position
  t1_writeEntry.attribute       := t1_train.attribute
  t1_writeEntry.targetState     := getTargetState(t1_train.startPc, t1_train.target)
  t1_writeEntry.targetLowerBits := getTargetLowerBits(t1_train.target)

  banks.zip(replacers).zipWithIndex.foreach { case ((b, r), i) =>
    b.io.writeReq.valid             := t1_writeBankValid && t1_bankMask(i)
    b.io.writeReq.bits.needResetCtr := t1_needWriteNewEntry
    b.io.writeReq.bits.setIdx       := t1_setIdx
    b.io.writeReq.bits.wayIdx       := Mux(t1_needWriteNewEntry, r.io.victimWayIdx, t1_meta.takenWayIdx)
    b.io.writeReq.bits.entry        := t1_writeEntry

    r.io.needReplaceSetIdx := t1_setIdx
  }

  replacers.zip(banks).foreach { case (r, b) =>
    r.io.writeValid   := b.io.writeResp.valid
    r.io.writeSetIdx  := b.io.writeResp.bits.setIdx
    r.io.writeWayMask := b.io.writeResp.bits.wayMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("total_predict", s2_valid)
  XSPerfAccumulate("hit", s2_valid && s2_hit)
  XSPerfAccumulate("miss", s2_valid && !s2_hit)
  XSPerfAccumulate("hit_entry_num", Mux(s2_valid, PopCount(s2_hitMask), 0.U))
  XSPerfAccumulate("taken", s2_valid && s2_taken)
  XSPerfAccumulate("notTaken", s2_valid && s2_hit && !s2_taken)
  XSPerfAccumulate("multi_hit", s2_valid && perf_s2_multiHit)

  XSPerfAccumulate("total_update_from_ftq", io.train.valid)
  XSPerfAccumulate("total_update_in_abtb", t1_valid)
  XSPerfAccumulate("update_hit", t1_valid && t1_hitTakenBranch)
  XSPerfAccumulate("update_predictTaken", t1_valid && t1_meta.taken)
  XSPerfAccumulate("update_predictNotTaken", t1_valid && t1_meta.hitMask.reduce(_ || _) && !t1_meta.taken)
  XSPerfAccumulate("update_actualTaken", t1_valid && t1_train.taken)
  XSPerfAccumulate("update_actualNotTaken", t1_valid && !t1_train.taken)
  XSPerfAccumulate("update_total_write", t1_writeBankValid)
  XSPerfAccumulate("update_writeNewEntry", t1_valid && t1_needWriteNewEntry)
  XSPerfAccumulate("update_correctTarget", t1_valid && t1_needCorrectTarget)
//  XSPerfAccumulate("update_needReplaceEntry", needReplaceEntry)

  XSPerfAccumulate("update_reset_ctr", VecInit(t1_needResetCtr.flatten.flatten).reduce(_ || _))
  XSPerfAccumulate("update_decrease_ctr", VecInit(t1_needDecreaseCtr.flatten.flatten).reduce(_ || _))
  XSPerfAccumulate("update_increase_ctr", VecInit(t1_needIncreaseCtr.flatten.flatten).reduce(_ || _))
}
