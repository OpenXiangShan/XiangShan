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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.Prediction

// TODO: 2-taken
class MicroBtb(implicit p: Parameters) extends BasePredictor with HasMicroBtbParameters with Helpers {
  class MicroBtbIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    // predict
    val prediction: Prediction = Output(new Prediction)
  }

  val io: MicroBtbIO = IO(new MicroBtbIO)

  println(f"MicroBtb:")
  println(f"  Size(full-assoc): $NumEntries")
  println(f"  Use fast-train: $UseFastTrain")
  println(f"  Replacer: $Replacer")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  io.resetDone  := true.B
  io.trainReady := true.B

  /* *** submodules *** */
  private val entries = RegInit(VecInit(Seq.fill(NumEntries)(0.U.asTypeOf(new MicroBtbEntry))))

  private val replacer = Module(new MicroBtbReplacer)
  replacer.io.usefulCnt := VecInit(entries.map(_.usefulCnt))

  /* *** predict stage 0 ***
   * - io.startPc timing might be bad, simply cache it
   */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable

  private val s0_startPc = io.startPc

  /* *** predict stage 1 ***
   * - read entries
   * - check if it's hit
   * - generate prediction
   * - update replacer
   */
  private val s1_fire = io.stageCtrl.s1_fire && io.enable

  private val s1_startPc = RegEnable(s0_startPc, s0_fire)
  private val s1_tag     = getTag(s1_startPc)

  private val s1_hitOH = VecInit(entries.map(e => e.valid && e.tag === s1_tag)).asUInt
  assert(PopCount(s1_hitOH) <= 1.U, "MicroBtb s1_hitOH should be one-hot")
  private val s1_hit      = s1_hitOH.orR
  private val s1_hitIdx   = OHToUInt(s1_hitOH)
  private val s1_hitEntry = entries(s1_hitIdx)

  // we do always-taken prediction in ubtb
  io.prediction.taken       := s1_hit
  io.prediction.cfiPosition := s1_hitEntry.slot1.position
  io.prediction.target      := getFullTarget(s1_startPc, s1_hitEntry.slot1.target, s1_hitEntry.slot1.targetCarry)
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
  private val t0_fire        = Wire(Bool())
  private val t0_startPc     = Wire(PrunedAddr(VAddrBits))
  private val t0_actualTaken = Wire(Bool())
  private val t0_position    = Wire(UInt(CfiPositionWidth.W))
  private val t0_fullTarget  = Wire(PrunedAddr(VAddrBits))
  private val t0_attribute   = Wire(new BranchAttribute)

  if (UseFastTrain) {
    t0_fire        := io.fastTrain.get.valid && io.enable
    t0_startPc     := io.fastTrain.get.bits.startPc
    t0_actualTaken := io.fastTrain.get.bits.finalPrediction.taken
    t0_position    := io.fastTrain.get.bits.finalPrediction.cfiPosition
    t0_fullTarget  := io.fastTrain.get.bits.finalPrediction.target
    t0_attribute   := io.fastTrain.get.bits.finalPrediction.attribute
  } else {
    // FIXME: not sure if first mispredict is the best, maybe first taken?
    t0_fire        := io.stageCtrl.t0_fire && io.train.mispredictBranch.valid && io.enable
    t0_startPc     := io.train.startPc
    t0_actualTaken := io.train.mispredictBranch.bits.taken
    t0_position    := io.train.mispredictBranch.bits.cfiPosition
    t0_fullTarget  := io.train.mispredictBranch.bits.target
    t0_attribute   := io.train.mispredictBranch.bits.attribute
  }

  private val t0_tag         = getTag(t0_startPc)
  private val t0_target      = getEntryTarget(t0_fullTarget)
  private val t0_targetCarry = if (EnableTargetFix) Option(getTargetCarry(t0_startPc, t0_fullTarget)) else None

  private val t0_hitOH = VecInit(entries.map(e => e.valid && e.tag === t0_tag)).asUInt
  // t0 may hit t1, so we add a "real" prefix for entries hit
  private val t0_realHit    = t0_hitOH.orR
  private val t0_realHitIdx = OHToUInt(t0_hitOH)

  // If there are two contiguous trains, the first one is too late to be written to the entries,
  // the second train might be a false "not hit" and allocate a new entry, causing a multi-hit;
  // or, the first may replace the entry, causing a false "hit" in the second train, causing wrong update.
  // So, we define some of the t1 signals in advance, and use them to check if the contiguous trains are hit.
  private val t1_fire         = Wire(Bool())
  private val t1_tag          = Wire(UInt(TagWidth.W))
  private val t1_updateIdx    = Wire(UInt(log2Up(NumEntries).W))
  private val t1_hitEntry     = Wire(new MicroBtbEntry)
  private val t1_updatedEntry = WireDefault(t1_hitEntry) // will be updated in t1, then write back to entries
  private val t1_allocate     = Wire(Bool())

  // if t0_tag === t1_tag, t1 must be updating the entry, so we can see it as a hit, and use t1_updateIdx as hitIdx
  private val t0_hitT1Update = Wire(Bool())
  // if t0 hits but t1 is replacing it, we should see it as not hit
  private val t0_hitT1Victim = t1_fire && t0_realHitIdx === replacer.io.victim && t1_allocate

  // fix final hit
  private val t0_hit = t0_realHit && !t0_hitT1Victim || t0_hitT1Update
  // select hit entry: use t1_updatedEntry if t0_hitT1Update, otherwise use real hit entry
  private val t0_hitIdx   = Mux(t0_hitT1Update, t1_updateIdx, t0_realHitIdx)
  private val t0_hitEntry = Mux(t0_hitT1Update, t1_updatedEntry, entries(t0_realHitIdx))

  // calculate hit flags, valid only when t0_hit
  private val t0_hitNotUseful     = t0_hitEntry.usefulCnt.isSaturateNegative
  private val t0_hitPositionSame  = t0_hitEntry.slot1.position === t0_position
  private val t0_hitAttributeSame = t0_hitEntry.slot1.attribute === t0_attribute
  private val t0_hitTargetSame    = t0_hitEntry.slot1.target === t0_target

  /* *** train stage 1 ***
   * - select victim
   * - generate updated entry
   * - update entries
   * - update replacer
   */
  t1_fire := RegNext(t0_fire, false.B)
  t1_tag  := RegEnable(t0_tag, t0_fire)
  private val t1_actualTaken = RegEnable(t0_actualTaken, t0_fire)
  private val t1_position    = RegEnable(t0_position, t0_fire)
  private val t1_target      = RegEnable(t0_target, t0_fire)
  private val t1_attribute   = RegEnable(t0_attribute, t0_fire)
  private val t1_targetCarry = t0_targetCarry.map(w => RegEnable(w, t0_fire)) // if (EnableTargetFix)

  private val t1_hit    = RegEnable(t0_hit, t0_fire)
  private val t1_hitIdx = RegEnable(t0_hitIdx, t0_fire)
  t1_hitEntry := RegEnable(t0_hitEntry, t0_fire)

  // hit states (flags), valid only when t1_hit
  private val t1_hitNotUseful     = RegEnable(t0_hitNotUseful, t0_fire)
  private val t1_hitPositionSame  = RegEnable(t0_hitPositionSame, t0_fire)
  private val t1_hitAttributeSame = RegEnable(t0_hitAttributeSame, t0_fire)
  private val t1_hitTargetSame    = RegEnable(t0_hitTargetSame, t0_fire)
  // only when t1 is updating/allocating can t0 hit it
  t0_hitT1Update := t1_fire && t0_tag === t1_tag && (t1_hit || t1_allocate)
  // init a new entry
  private def initEntryIfNotUseful(notUseful: Bool): Unit =
    when(notUseful) {
      t1_updatedEntry.tag := t1_tag
      t1_updatedEntry.usefulCnt.resetSaturatePositive() // usefulCnt inits at strong positive, in/decrease by policy
      // slot1
      t1_updatedEntry.slot1.position       := t1_position
      t1_updatedEntry.slot1.attribute      := t1_attribute
      t1_updatedEntry.slot1.target         := t1_target
      t1_updatedEntry.slot1.isStaticTarget := true.B // inits at true, set to false when we see a different target
      t1_updatedEntry.slot1.targetCarry.foreach(_ := t1_targetCarry.get) // if (EnableTargetFix)
      // TODO: 2-taken train
      t1_updatedEntry.slot2.valid := false.B
    }.otherwise {
      t1_updatedEntry.usefulCnt := t1_hitEntry.usefulCnt.getDecrease()
    }

  when(t1_fire) {
    when(!t1_hit) {
      // not hit
      // init a new entry if actually taken
      initEntryIfNotUseful(true.B)
    }.elsewhen(!t1_hitAttributeSame || !t1_hitPositionSame || !t1_hitTargetSame || !t1_actualTaken) {
      // hit, but attribute/position mismatch, or actually not taken
      // if already not useful and actually taken, init a new entry, otherwise decrease usefulCnt
      initEntryIfNotUseful(t1_hitNotUseful)
      // and, if we've seen a different target, mark target as not static
      when(!t1_hitTargetSame) {
        t1_updatedEntry.slot1.isStaticTarget := false.B
      }
    }.otherwise {
      // everything matches, and actually taken
      // increase usefulCnt
      t1_updatedEntry.usefulCnt := t1_hitEntry.usefulCnt.getIncrease()
    }
  }

  // select the entry: if hit, use the hit entry, otherwise use the victim from replacer (first not useful, or Plru)
  t1_allocate  := !t1_hit && t1_actualTaken
  t1_updateIdx := Mux(t1_hit, t1_hitIdx, replacer.io.victim)
  // and write back the updated entry
  when(t1_fire && (t1_hit || t1_allocate)) { // update entry if hit, or alloc entry only for taken branches
    entries(t1_updateIdx) := t1_updatedEntry
  }

  // update replacer
  replacer.io.trainTouch.valid := t1_fire
  replacer.io.trainTouch.bits  := t1_updateIdx

  /* *** perf *** */
  XSPerfAccumulate("predHit", s1_hit && s1_fire)
  XSPerfAccumulate("predMiss", !s1_hit && s1_fire)

  XSPerfAccumulate("s1Hits3FallThrough", t1_fire && t1_hit && !t1_actualTaken)
  XSPerfAccumulate("s1Misses3Taken", t1_fire && !t1_hit && t1_actualTaken)
  XSPerfAccumulate("s1Hits3Taken", t1_fire && t1_hit && t1_actualTaken)
  XSPerfAccumulate("s1Misses3FallThrough", t1_fire && !t1_hit && !t1_actualTaken)

  XSPerfAccumulate("s1InvalidatedEntries", t1_fire && t1_hit && !t1_actualTaken && t1_hitNotUseful)

  XSPerfAccumulate("trainHitEntries", t0_fire && t0_realHit)
  XSPerfAccumulate("trainHitT1Update", t0_fire && t0_hitT1Update)
  XSPerfAccumulate("trainHitT1Victim", t0_fire && t0_hitT1Victim)

  XSPerfAccumulate("allocateNotUseful", t1_fire && t1_allocate && replacer.io.perf.replaceNotUseful)
  XSPerfAccumulate("allocatePlru", t1_fire && t1_allocate && !replacer.io.perf.replaceNotUseful)
  XSPerfAccumulate(
    "replace",
    t1_fire && t1_hit && (
      !t1_hitAttributeSame && t1_hitNotUseful ||
        t1_hitAttributeSame && !t1_hitPositionSame && t1_hitNotUseful
    )
  )
}
