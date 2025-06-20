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

package xiangshan.frontend.bpu.ubtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchPrediction

// TODO: 2-taken
class Ubtb(implicit p: Parameters) extends BasePredictor with HasUbtbParameters {
  class UbtbIO(implicit p: Parameters) extends BasePredictorIO {
    // predict request
    // ... all inherited from BasePredictorIO
    // train request
    val train: Valid[UbtbTrain] = Flipped(Valid(new UbtbTrain))
  }

  val io: UbtbIO = IO(new UbtbIO)

  /* *** submodules *** */
  private val entries = RegInit(VecInit(Seq.fill(nEntries)(0.U.asTypeOf(new UbtbEntry))))

  private val replacer = Module(new UbtbReplacer)
  replacer.io.usefulCnt := VecInit(entries.map(_.usefulCnt))

  /* *** predict stage 0 *** */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable

  private val s0_startVAddr = io.startVAddr

  /* *** predict stage 1 *** */
  private val s1_fire = io.stageCtrl.s1_fire && io.enable

  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)
  private val s1_tag        = getTag(s1_startVAddr)

  private val s1_hitOH = VecInit(entries.map(e => e.valid && e.tag === s1_tag)).asUInt
  assert(PopCount(s1_hitOH) <= 1.U, "Ubtb s1_hitOH should be one-hot")
  private val s1_hit    = s1_hitOH.orR
  private val s1_hitIdx = OHToUInt(s1_hitOH)

  io.prediction.valid := s1_hit
  io.prediction.bits := Mux1H(
    s1_hitOH,
    entries.map { e =>
      val info = Wire(new BranchPrediction)
      info.cfiPosition.valid := e.slot1.takenCnt.isPositive
      info.cfiPosition.bits  := e.slot1.position
      info.target            := getFullTarget(s1_startVAddr, e.slot1.target)
      info.attribute         := e.slot1.attribute
      info
    }
  )

  // update replacer
  replacer.io.predTouch.valid := s1_hit && s1_fire
  replacer.io.predTouch.bits  := s1_hitIdx

  /* *** train stage 0 *** */
  private val t0_valid = io.train.valid

  private val t0_startVAddr  = io.train.bits.startVAddr
  private val t0_tag         = getTag(t0_startVAddr)
  private val t0_actualTaken = io.train.bits.cfiPosition.valid
  private val t0_position    = io.train.bits.cfiPosition.bits
  private val t0_target      = getEntryTarget(io.train.bits.target)
  private val t0_attribute   = io.train.bits.attribute

  private val t0_hitOH  = VecInit(entries.map(e => e.valid && e.tag === t0_tag)).asUInt
  private val t0_hit    = t0_hitOH.orR
  private val t0_hitIdx = OHToUInt(t0_hitOH)

  // calculate hit states (flags), valid only when t0_hit
  private val t0_hitEntry         = entries(t0_hitIdx)
  private val t0_hitNotUseful     = t0_hitEntry.usefulCnt.isSaturateNegative
  private val t0_hitPositionLow   = t0_hitEntry.slot1.position > t0_position
  private val t0_hitPositionHigh  = t0_hitEntry.slot1.position < t0_position
  private val t0_hitPositionSame  = t0_hitEntry.slot1.position === t0_position
  private val t0_hitAttributeSame = t0_hitEntry.slot1.attribute === t0_attribute
  private val t0_hitTargetSame    = t0_hitEntry.slot1.target === t0_target
  private val t0_hitTaken         = t0_hitEntry.slot1.takenCnt.isPositive

  /* *** train stage 1 *** */
  private val t1_valid       = RegNext(t0_valid, false.B)
  private val t1_tag         = RegEnable(t0_tag, t0_valid)
  private val t1_actualTaken = RegEnable(t0_actualTaken, t0_valid)
  private val t1_position    = RegEnable(t0_position, t0_valid)
  private val t1_target      = RegEnable(t0_target, t0_valid)
  private val t1_attribute   = RegEnable(t0_attribute, t0_valid)

  private val t1_hit    = RegEnable(t0_hit, t0_valid)
  private val t1_hitIdx = RegEnable(t0_hitIdx, t0_valid)

  // hit states (flags), valid only when t1_hit
  private val t1_hitNotUseful     = RegEnable(t0_hitNotUseful, t0_valid)
  private val t1_hitPositionLow   = RegEnable(t0_hitPositionLow, t0_valid)
  private val t1_hitPositionHigh  = RegEnable(t0_hitPositionHigh, t0_valid)
  private val t1_hitPositionSame  = RegEnable(t0_hitPositionSame, t0_valid)
  private val t1_hitAttributeSame = RegEnable(t0_hitAttributeSame, t0_valid)
  private val t1_hitTargetSame    = RegEnable(t0_hitTargetSame, t0_valid)
  private val t1_hitTaken         = RegEnable(t0_hitTaken, t0_valid)

  // select the entry: if hit, use the hit entry, otherwise use the victim from replacer (first not useful, or PLRU)
  private val t1_updateIdx   = Mux(t1_hit, t1_hitIdx, replacer.io.victim)
  private val t1_updateEntry = entries(t1_updateIdx)

  // init a new entry
  private def updateEntryInit(): Unit = {
    t1_updateEntry.valid := true.B
    t1_updateEntry.tag   := t1_tag
    t1_updateEntry.usefulCnt.resetPositive() // usefulCnt inits at strong positive, in/decrease by policy
    // slot1
    t1_updateEntry.slot1.position  := t1_position
    t1_updateEntry.slot1.attribute := t1_attribute
    t1_updateEntry.slot1.target    := t1_target
    t1_updateEntry.slot1.takenCnt.resetNeutral() // takenCnt inits at neutral (weak taken), in/decrease by policy
    t1_updateEntry.slot1.isStaticTarget := true.B // inits at true, set to false when we see a different target
    // TODO: 2-taken train
    t1_updateEntry.slot2.valid := false.B
  }

  when(t1_valid) {
    when(!t1_hit) {
      // not hit
      // simply init a new entry
      updateEntryInit()
    }.elsewhen(!t1_hitAttributeSame) {
      // hit, but attribute mismatch
      // if already not useful, init a new entry, otherwise decrease usefulCnt
      when(t1_hitNotUseful) {
        updateEntryInit()
      }.otherwise {
        t1_updateEntry.usefulCnt.decrease()
      }
    }.elsewhen(t1_attribute.isConditional) {
      // attribute match, and is conditional (branch)
      when(
        // branch position match, and actual taken
        t1_hitPositionSame && t1_actualTaken
      ) {
        // increase takenCnt
        t1_updateEntry.slot1.takenCnt.increase()
      }.elsewhen(
        // branch position match, and actual not taken
        t1_hitPositionSame && !t1_actualTaken ||
          // an actual taken branch is at higher address -> the predicted position is actual not taken
          t1_hitPositionHigh && t1_actualTaken ||
          // actual not taken, but predicted taken -> no matter position, the predicted position is actual not taken
          !t1_actualTaken && t1_hitTaken
      ) {
        // decrease takenCnt
        t1_updateEntry.slot1.takenCnt.decrease()
      }
      when(
        // branch position match, and both actual & predicted taken -> prediction correct, useful
        t1_hitPositionSame && t1_actualTaken && t1_hitTaken
      ) {
        // increase usefulCnt
        t1_updateEntry.usefulCnt.increase()
      }.elsewhen(
        // position mismatch -> not useful
        !t1_hitPositionSame ||
          // actual not taken -> fall through, not useful
          !t1_actualTaken
      ) {
        // decrease usefulCnt
        t1_updateEntry.usefulCnt.decrease()
      }
    }.otherwise {
      // attribute match, and is direct / indirect jump
      when(!t1_hitPositionSame) {
        // position mismatch
        // if already not useful, init a new entry, otherwise decrease usefulCnt
        when(t1_hitNotUseful) {
          updateEntryInit()
        }.otherwise {
          t1_updateEntry.usefulCnt.decrease()
        }
      }.elsewhen(t1_hitTargetSame) {
        // position match, and target match
        // increase usefulCnt
        t1_updateEntry.usefulCnt.increase()
      }.otherwise {
        // position match, but target mismatch (should not happen for direct jumps)
        // decrease usefulCnt and mark as not static target
        t1_updateEntry.usefulCnt.decrease()
        t1_updateEntry.slot1.isStaticTarget := false.B // target is not static anymore
      }
    }
  }

  // update replacer
  replacer.io.trainTouch.valid := t1_valid
  replacer.io.trainTouch.bits  := t1_updateIdx
}
