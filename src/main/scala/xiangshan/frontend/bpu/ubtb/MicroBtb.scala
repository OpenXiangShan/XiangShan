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
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.Prediction

// TODO: 2-taken
class MicroBtb(implicit p: Parameters) extends BasePredictor with HasMicroBtbParameters with Helpers {
  class MicroBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // predict
    val prediction: Prediction = Output(new Prediction)
  }

  val io: MicroBtbIO = IO(new MicroBtbIO)

  io.resetDone := true.B

  /* *** submodules *** */
  private val entries = RegInit(VecInit(Seq.fill(NumEntries)(0.U.asTypeOf(new MicroBtbEntry))))

  private val replacer = Module(new MicroBtbReplacer)
  replacer.io.usefulCnt := VecInit(entries.map(_.usefulCnt))

  /* *** predict stage 0 ***
   * - io.startVAddr timing might be bad, simply cache it
   */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable

  private val s0_startVAddr = io.startVAddr

  /* *** predict stage 1 ***
   * - read entries
   * - check if it's hit
   * - generate prediction
   * - update replacer
   */
  private val s1_fire = io.stageCtrl.s1_fire && io.enable

  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)
  private val s1_tag        = getTag(s1_startVAddr)

  private val s1_hitOH = VecInit(entries.map(e => e.valid && e.tag === s1_tag)).asUInt
  assert(PopCount(s1_hitOH) <= 1.U, "MicroBtb s1_hitOH should be one-hot")
  private val s1_hit      = s1_hitOH.orR
  private val s1_hitIdx   = OHToUInt(s1_hitOH)
  private val s1_hitEntry = entries(s1_hitIdx)

  // we do not need to check attribute.isDirect/Indirect here, as entry.slot1.takenCnt is initialized to weak taken
  // and for those jumps, takenCnt will remain unchanged during training,
  // so e.slot1.takenCnt.isPositive is always true if e.slot1.attribute.isDirect/Indirect
  io.prediction.taken       := s1_hit && s1_hitEntry.slot1.takenCnt.isPositive
  io.prediction.cfiPosition := s1_hitEntry.slot1.position
  io.prediction.target      := getFullTarget(s1_startVAddr, s1_hitEntry.slot1.target, s1_hitEntry.slot1.targetCarry)
  io.prediction.attribute   := s1_hitEntry.slot1.attribute

  // update replacer
  replacer.io.predTouch.valid := s1_hit && s1_fire
  replacer.io.predTouch.bits  := s1_hitIdx

  /* *** train stage 0 ***
   * - read entries
   * - check if hits entries
   * - check if hits t1 stage
   * - calculate hit flags
   */
  private val t0_branchInfo = io.train.bits.firstMispredict

  private val t0_valid = io.train.valid && t0_branchInfo.valid && io.enable

  private val t0_startVAddr  = io.train.bits.startVAddr
  private val t0_tag         = getTag(t0_startVAddr)
  private val t0_actualTaken = t0_branchInfo.bits.taken
  private val t0_position    = t0_branchInfo.bits.cfiPosition
  private val t0_target      = getEntryTarget(t0_branchInfo.bits.target)
  private val t0_attribute   = t0_branchInfo.bits.attribute
  private val t0_targetCarry =
    if (EnableTargetFix) Option(getTargetCarry(t0_startVAddr, t0_branchInfo.bits.target)) else None

  private val t0_hitOH = VecInit(entries.map(e => e.valid && e.tag === t0_tag)).asUInt
  // t0 may hit t1, so we add a "real" prefix for entries hit
  private val t0_realHit    = t0_hitOH.orR
  private val t0_realHitIdx = OHToUInt(t0_hitOH)

  // If there are two contiguous trains, the first one is too late to be written to the entries,
  // the second train might be a false "not hit" and allocate a new entry, causing a multi-hit;
  // or, the first may replace the entry, causing a false "hit" in the second train, causing wrong update.
  // So, we define some of the t1 signals in advance, and use them to check if the contiguous trains are hit.
  private val t1_valid        = Wire(Bool())
  private val t1_tag          = Wire(UInt(TagWidth.W))
  private val t1_updateIdx    = Wire(UInt(log2Up(NumEntries).W))
  private val t1_hitEntry     = Wire(new MicroBtbEntry)
  private val t1_updatedEntry = WireDefault(t1_hitEntry) // will be updated in t1, then write back to entries
  private val t1_allocate     = Wire(Bool())

  // if t0_tag === t1_tag, t1 must be updating the entry, so we can see it as a hit, and use t1_updateIdx as hitIdx
  private val t0_hitT1Update = t1_valid && t0_tag === t1_tag
  // if t0 hits but t1 is replacing it, we should see it as not hit
  private val t0_hitT1Victim = t1_valid && t0_realHitIdx === replacer.io.victim && t1_allocate

  // fix final hit
  private val t0_hit = t0_realHit && !t0_hitT1Victim || t0_hitT1Update
  // select hit entry: use t1_updatedEntry if t0_hitT1Update, otherwise use real hit entry
  private val t0_hitIdx   = Mux(t0_hitT1Update, t1_updateIdx, t0_realHitIdx)
  private val t0_hitEntry = Mux(t0_hitT1Update, t1_updatedEntry, entries(t0_realHitIdx))

  // calculate hit flags, valid only when t0_hit
  private val t0_hitNotUseful     = t0_hitEntry.usefulCnt.isSaturateNegative
  private val t0_hitPositionLow   = t0_hitEntry.slot1.position > t0_position
  private val t0_hitPositionHigh  = t0_hitEntry.slot1.position < t0_position
  private val t0_hitPositionSame  = t0_hitEntry.slot1.position === t0_position
  private val t0_hitAttributeSame = t0_hitEntry.slot1.attribute === t0_attribute
  private val t0_hitTargetSame    = t0_hitEntry.slot1.target === t0_target
  private val t0_hitTaken         = t0_hitEntry.slot1.takenCnt.isPositive

  /* *** train stage 1 ***
   * - select victim
   * - generate updated entry
   * - update entries
   * - update replacer
   */
  t1_valid := RegNext(t0_valid, false.B)
  t1_tag   := RegEnable(t0_tag, t0_valid)
  private val t1_actualTaken = RegEnable(t0_actualTaken, t0_valid)
  private val t1_position    = RegEnable(t0_position, t0_valid)
  private val t1_target      = RegEnable(t0_target, t0_valid)
  private val t1_attribute   = RegEnable(t0_attribute, t0_valid)
  private val t1_targetCarry = t0_targetCarry.map(w => RegEnable(w, t0_valid)) // if (EnableTargetFix)

  private val t1_hit    = RegEnable(t0_hit, t0_valid)
  private val t1_hitIdx = RegEnable(t0_hitIdx, t0_valid)
  t1_hitEntry := RegEnable(t0_hitEntry, t0_valid)

  // hit states (flags), valid only when t1_hit
  private val t1_hitNotUseful     = RegEnable(t0_hitNotUseful, t0_valid)
  private val t1_hitPositionLow   = RegEnable(t0_hitPositionLow, t0_valid)
  private val t1_hitPositionHigh  = RegEnable(t0_hitPositionHigh, t0_valid)
  private val t1_hitPositionSame  = RegEnable(t0_hitPositionSame, t0_valid)
  private val t1_hitAttributeSame = RegEnable(t0_hitAttributeSame, t0_valid)
  private val t1_hitTargetSame    = RegEnable(t0_hitTargetSame, t0_valid)
  private val t1_hitTaken         = RegEnable(t0_hitTaken, t0_valid)

  // init a new entry
  private def initEntryIfNotUseful(notUseful: Bool): Unit =
    when(notUseful) {
      t1_updatedEntry.valid := true.B
      t1_updatedEntry.tag   := t1_tag
      t1_updatedEntry.usefulCnt.resetPositive() // usefulCnt inits at strong positive, in/decrease by policy
      // slot1
      t1_updatedEntry.slot1.position  := t1_position
      t1_updatedEntry.slot1.attribute := t1_attribute
      t1_updatedEntry.slot1.target    := t1_target
      t1_updatedEntry.slot1.takenCnt.resetNeutral() // takenCnt inits at neutral (weak taken), in/decrease by policy
      t1_updatedEntry.slot1.isStaticTarget := true.B // inits at true, set to false when we see a different target
      t1_updatedEntry.slot1.targetCarry.foreach(_ := t1_targetCarry.get) // if (EnableTargetFix)
      // TODO: 2-taken train
      t1_updatedEntry.slot2.valid := false.B
    }.otherwise {
      t1_updatedEntry.usefulCnt.value := t1_hitEntry.usefulCnt.getDecrease
    }

  when(t1_valid) {
    when(!t1_hit) {
      // not hit
      // simply init a new entry
      initEntryIfNotUseful(true.B)
    }.elsewhen(!t1_hitAttributeSame) {
      // hit, but attribute mismatch
      // if already not useful, init a new entry, otherwise decrease usefulCnt
      initEntryIfNotUseful(t1_hitNotUseful)
    }.elsewhen(t1_attribute.isConditional) {
      // attribute match, and is conditional (branch)
      when(
        // branch position match, and actual taken
        t1_hitPositionSame && t1_actualTaken
      ) {
        // increase takenCnt
        t1_updatedEntry.slot1.takenCnt.value := t1_hitEntry.slot1.takenCnt.getIncrease
      }.elsewhen(
        // branch position match, and actual not taken
        t1_hitPositionSame && !t1_actualTaken ||
          // an actual taken branch is at higher address -> the predicted position is actual not taken
          t1_hitPositionHigh && t1_actualTaken ||
          // actual not taken, but predicted taken -> no matter position, the predicted position is actual not taken
          !t1_actualTaken && t1_hitTaken
      ) {
        // decrease takenCnt
        t1_updatedEntry.slot1.takenCnt.value := t1_hitEntry.slot1.takenCnt.getDecrease
      }
      when(
        // branch position match, and both actual & predicted taken -> prediction correct, useful
        t1_hitPositionSame && t1_actualTaken && t1_hitTaken
      ) {
        // increase usefulCnt
        t1_updatedEntry.usefulCnt.value := t1_hitEntry.usefulCnt.getIncrease
      }.elsewhen(
        // position mismatch -> not useful
        !t1_hitPositionSame ||
          // actual not taken -> fall through, not useful
          !t1_actualTaken
      ) {
        // decrease usefulCnt
        t1_updatedEntry.usefulCnt.value := t1_hitEntry.usefulCnt.getDecrease
      }
    }.otherwise {
      // attribute match, and is direct / indirect jump
      when(!t1_hitPositionSame) {
        // position mismatch
        // if already not useful, init a new entry, otherwise decrease usefulCnt
        initEntryIfNotUseful(t1_hitNotUseful)
      }.elsewhen(t1_hitTargetSame) {
        // position match, and target match
        // increase usefulCnt
        t1_updatedEntry.usefulCnt.value := t1_hitEntry.usefulCnt.getIncrease
      }.otherwise {
        // position match, but target mismatch (should not happen for direct jumps unless self-modifies)
        // decrease usefulCnt
        t1_updatedEntry.usefulCnt.value := t1_hitEntry.usefulCnt.getDecrease
        // and, since we've seen a different target, target is not static anymore
        t1_updatedEntry.slot1.isStaticTarget := false.B
      }
    }
  }

  // select the entry: if hit, use the hit entry, otherwise use the victim from replacer (first not useful, or Plru)
  t1_allocate  := !t1_hit && t1_actualTaken
  t1_updateIdx := Mux(t1_hit, t1_hitIdx, replacer.io.victim)
  // and write back the updated entry
  when(t1_valid && (t1_hit || t1_allocate)) { // update entry if hit, or alloc entry only for taken branches
    entries(t1_updateIdx) := t1_updatedEntry
  }

  // update replacer
  replacer.io.trainTouch.valid := t1_valid
  replacer.io.trainTouch.bits  := t1_updateIdx

  /* *** perf *** */
  XSPerfAccumulate("trainHitEntries", t0_valid && t0_realHit)
  XSPerfAccumulate("trainHitT1Update", t0_valid && t0_hitT1Update)
  XSPerfAccumulate("trainHitT1Victim", t0_valid && t0_hitT1Victim)

  XSPerfAccumulate("allocateNotUseful", t1_valid && t1_allocate && replacer.io.perf.replaceNotUseful)
  XSPerfAccumulate("allocatePlru", t1_valid && t1_allocate && !replacer.io.perf.replaceNotUseful)
  XSPerfAccumulate(
    "replace",
    t1_valid && t1_hit && (
      !t1_hitAttributeSame && t1_hitNotUseful ||
        t1_hitAttributeSame && !t1_hitPositionSame && t1_hitNotUseful
    )
  )

  XSPerfAccumulate("mispredictAttribute", t1_valid && t1_hit && !t1_hitAttributeSame)

  XSPerfAccumulate(
    "mispredictConditional",
    t1_valid && t1_hit && t1_hitAttributeSame && t1_attribute.isConditional && (
      t1_hitPositionSame && t1_actualTaken && !t1_hitTaken ||
        t1_hitPositionHigh && t1_actualTaken ||
        !t1_actualTaken && t1_hitTaken
    )
  )

  XSPerfAccumulate(
    "mispredictDirect",
    t1_valid && t1_hit && t1_hitAttributeSame && t1_attribute.isDirect && (
      !t1_hitPositionSame ||
        !t1_hitTargetSame // should not happen unless self-modifies
    )
  )

  XSPerfAccumulate(
    "mispredictIndirect",
    t1_valid && t1_hit && t1_hitAttributeSame && t1_attribute.isIndirect && (
      !t1_hitPositionSame ||
        !t1_hitTargetSame
    )
  )
}
