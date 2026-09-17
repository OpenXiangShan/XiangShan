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

package xiangshan.frontend.bpu.ptage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.HasAheadPredictorIO
import xiangshan.frontend.bpu.HasFastTrainIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/** pTAGE: the 2-taken fast predictor.
  *
  * An entry is keyed by the *previous* group's start pc and the folded path history, and answers "what follows that",
  * so a single read yields a whole prediction group. That is what buys the second block: predicting two blocks from
  * the current pc would need two layout lookups, which one cycle of single-ported sram cannot serve.
  *
  * Indexing a group ahead is also what shapes the pipeline. a0 issues the read one cycle before the rest of the
  * predictor starts on that group, so s0 and s1 land level with the other s1-path predictors and the group leaves at
  * the same time theirs does. The cost is that a0's address arrives late, on the loop back from s1's own next pc, so
  * a0 does one XOR and nothing more: the tag compare is pushed down to s0, and the entry stores next pc bits that are
  * used as an index directly, with no adder in the way.
  */
class Ptage(implicit p: Parameters) extends BasePredictor with HasPtageParameters with Helpers {
  class PtageIO(implicit p: Parameters) extends BasePredictorIO with HasAheadPredictorIO with HasFastTrainIO {
    // resident folded histories, one set of folds per table span
    val foldedHist: PhrAllFoldedHistories =
      Input(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum))

    val prediction: PtagePrediction = Output(new PtagePrediction)
    val meta:       PtageMeta       = Output(new PtageMeta)
  }
  val io: PtageIO = IO(new PtageIO)

  println(f"Ptage:")
  println(f"  Size(table, bank, set): $NumTables * $NumBanks * $NumSets = $NumEntries")
  println(f"  Entry width: ${(new PtageEntry).getWidth} bits")
  println(f"  History spans: ${fastPhrParameters.Spans.mkString(", ")}")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  private val banks = Seq.tabulate(NumTables, NumBanks)((t, b) => Module(new PtageBank(t, b)))

  io.sramResetDone := banks.flatten.map(_.io.sramResetDone).reduce(_ && _)
  // training only ever writes, and a write buffer absorbs whatever the sram cannot take, so it never back-pressures
  io.trainReady := true.B

  private val s0_fire = io.stageCtrl.s0_fire && io.enable

  /* *** a0: issue the ahead read ***
   * The pc arriving now starts the group before the one this read answers for, which is exactly the key of the entry
   * that will be needed two cycles from here. Everything on this path is one XOR deep, because the pc itself is the
   * late signal, coming from the previous cycle's own prediction.
   */
  // On a correction cycle the pc at s0 belongs to the path just abandoned, so indexing from it returns an entry for
  // a group that is no longer going to be fetched. The corrected group is what everything after it now follows, and
  // the ahead history handed out this cycle already has that group folded in, so index from the corrected group and
  // carry on. A redirect is still sat out: its meta names the control-flow instruction rather than the block it sits
  // in, and an index built from the wrong pc is worse than none.
  private val a0_override = io.bpuS2Override || io.bpuS3Override
  private val a0_startPc  = Mux(a0_override, io.overrideOwnStartPc, io.startPc)
  private val a0_bankIdx  = getBankIndex(a0_startPc)

  private val a0_anchored = !io.redirect

  private def foldedFor(tableIdx: Int, width: Int): UInt = {
    val span = fastPhrParameters.Spans(tableIdx)
    io.foldedHist.getHistWithInfo(new xiangshan.frontend.bpu.FoldedHistoryInfo(
      span,
      scala.math.min(span, width)
    )).foldedHist
  }

  private val a0_setIdx = VecInit(Seq.tabulate(NumTables) { t =>
    foldedFor(t, fastPhrParameters.IdxWidth) ^ getSetIndexPc(a0_startPc)
  })

  banks.zipWithIndex.foreach { case (tableBanks, t) =>
    tableBanks.zipWithIndex.foreach { case (bank, b) =>
      bank.io.readReq.valid       := s0_fire && a0_bankIdx === b.U
      bank.io.readReq.bits.setIdx := a0_setIdx(t)
    }
  }

  /* *** s0: sram data returns, match tags ***
   * The tag compare sits here rather than in s1 so that s1 is left with only a priority select and a decode.
   *
   * The index had to be keyed on the previous group's pc, since it is needed a cycle before this one exists. The tag
   * does not: by now the predicted group's own start pc has arrived, so the entry can be tagged with where the group
   * actually starts rather than with where its predecessor did. An entry whose predecessor has since learned to jump
   * elsewhere then misses, instead of hitting and rebuilding its targets from the wrong base.
   */
  private val s0_startPc  = io.startPc
  private val s0_anchored = RegEnable(a0_anchored, false.B, s0_fire)
  private val s0_bankIdx  = RegEnable(a0_bankIdx, s0_fire)
  private val s0_setIdx   = RegEnable(a0_setIdx, s0_fire)
  private val s0_tagFold = RegEnable(
    VecInit(Seq.tabulate(NumTables) { t =>
      // two folds of different widths, so that a tag does not simply repeat what the set index already says
      foldedFor(t, fastPhrParameters.TagWidth) ^ (foldedFor(t, fastPhrParameters.TagWidth - 1) << 1).asUInt
    }),
    s0_fire
  )

  private val s0_entry = VecInit(Seq.tabulate(NumTables) { t =>
    Mux1H(UIntToOH(s0_bankIdx, NumBanks), banks(t).map(_.io.readResp.entry))
  })
  private val s0_tag    = VecInit(Seq.tabulate(NumTables)(t => (s0_tagFold(t) ^ getTagPc(s0_startPc))(TagWidth - 1, 0)))
  private val s0_hitVec = VecInit(Seq.tabulate(NumTables)(t => s0_entry(t).valid && s0_entry(t).tag === s0_tag(t)))

  /* *** useful marks ***
   * A mark says an entry earned its place, and allocation passes marked entries over. They live here rather than in
   * the entry itself for one reason: the only way to stop the tables filling with permanently-protected entries is to
   * let every mark go at once, and a write port that spends its cycles on training cannot sweep an sram to do it.
   * In flops the whole array clears in a cycle, and the entry loses a bit in the bargain.
   */
  private val usefulMarks = RegInit(VecInit(Seq.fill(NumTables)(
    VecInit(Seq.fill(NumBanks * NumSets)(false.B))
  )))
  private def usefulAddr(bank: UInt, set: UInt): UInt = Cat(bank, set)

  private val s0_useful =
    VecInit(Seq.tabulate(NumTables)(t => usefulMarks(t)(usefulAddr(s0_bankIdx, s0_setIdx(t)))))
  private val s1_useful = RegEnable(s0_useful, s0_fire)

  /* *** s1: select a provider and decode the group *** */
  private val s1_anchored = RegEnable(s0_anchored, false.B, s0_fire)
  private val s1_startPc  = RegEnable(s0_startPc, s0_fire)
  private val s1_entry    = RegEnable(s0_entry, s0_fire)
  private val s1_hitVec   = RegEnable(s0_hitVec, s0_fire)
  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_tag      = RegEnable(s0_tag, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)

  // The longest history that hit provides the prediction, as in any TAGE: a longer history is a more specific
  // context, so where one matches it is the better answer.
  private def selectLongest(mask: Seq[Bool]): Valid[UInt] = {
    val sel = Wire(Valid(UInt(log2Ceil(NumTables).W)))
    sel.valid := mask.reduce(_ || _)
    sel.bits  := (NumTables - 1).U - PriorityEncoder(mask.reverse)
    sel
  }

  private val s1_provider = selectLongest(s1_hitVec)

  private val s1_providerEntry = Mux1H(UIntToOH(s1_provider.bits, NumTables), s1_entry)

  // The alternate answer: the longest history that hit below the provider. A mark is only worth giving where the
  // provider said something the alternate did not -- an entry that merely agrees with a shorter history is not
  // holding any context of its own, and protecting it costs a place that a new group could have used.
  private val s1_altHitVec = VecInit(s1_hitVec.zipWithIndex.map { case (hit, t) =>
    hit && !(s1_provider.valid && s1_provider.bits === t.U)
  })
  private val s1_alt      = selectLongest(s1_altHitVec)
  private val s1_altEntry = Mux1H(UIntToOH(s1_alt.bits, NumTables), s1_entry)
  private val s1_providerDiffersFromAlt = s1_alt.valid && (
    s1_providerEntry.p1.taken =/= s1_altEntry.p1.taken ||
      s1_providerEntry.p1.cfiPosition =/= s1_altEntry.p1.cfiPosition
  )

  // The first block starts where this group starts; the second starts at the first's next pc, which is why that field
  // is not stored twice.
  private val s1_p1Target = getFullTarget(s1_startPc, s1_providerEntry.p1.nextPcLow, None)
  private val s1_p2Target = getFullTarget(s1_p1Target, s1_providerEntry.p2.nextPcLow, None)

  // A block can only be followed by another if its own target is the one the entry stored: a deferred exit takes its
  // target from elsewhere, so the second block's start would not be where the entry assumed.
  private val s1_p1Usable = s1_anchored && s1_provider.valid
  private val s1_p2Usable =
    s1_p1Usable && s1_providerEntry.p2Valid &&
      PtageBlock.hasStaticTarget(s1_providerEntry.p1.taken, s1_providerEntry.p1.attribute) &&
      // Only a conditional exit is ever learned as a second block, since anything else either takes its target from
      // elsewhere or moves the return stack. Checking it where the entry is used keeps a stale or aliased entry from
      // presenting one of those as a second block.
      s1_providerEntry.p2.attribute.isConditional &&
      // Both of the filters that used to sit here -- a saturated counter, and never pairing from the shortest table
      // -- were added when this predictor was finding the wrong entry half the time, and what they were really doing
      // was hiding that. They are left out now so the coverage the fixes recovered can reach the group, and the
      // duplicated lookup at s3 is what says whether a pair was worth putting out.
      true.B

  private def decode(block: PtageBlock, target: PrunedAddr): Prediction = {
    val prediction = Wire(new Prediction)
    prediction.taken       := block.taken
    prediction.cfiPosition := block.cfiPosition
    prediction.attribute   := block.attribute
    prediction.target      := target
    prediction
  }

  io.prediction.blocks(0).valid := s1_p1Usable
  io.prediction.blocks(0).bits  := decode(s1_providerEntry.p1, s1_p1Target)
  io.prediction.blocks(1).valid := s1_p2Usable
  io.prediction.blocks(1).bits  := decode(s1_providerEntry.p2, s1_p2Target)

  io.meta.setIdx                 := s1_setIdx
  io.meta.tag                    := s1_tag
  io.meta.bankIdx                := s1_bankIdx
  io.meta.usefulVec              := s1_useful
  io.meta.validVec               := VecInit(s1_entry.map(_.valid))
  io.meta.providerDiffersFromAlt := s1_providerDiffersFromAlt
  io.meta.provider               := s1_provider
  io.meta.p1Counter              := s1_providerEntry.p1.counter
  io.meta.p2Counter              := s1_providerEntry.p2.counter
  io.meta.p2Valid                := s1_providerEntry.p2Valid
  io.meta.p1CfiPosition          := s1_providerEntry.p1.cfiPosition
  io.meta.p1NextPcLow            := s1_providerEntry.p1.nextPcLow
  io.meta.p1Attribute            := s1_providerEntry.p1.attribute
  io.meta.p2CfiPosition          := s1_providerEntry.p2.cfiPosition
  io.meta.p2Attribute            := s1_providerEntry.p2.attribute
  io.meta.p2NextPcLow            := s1_providerEntry.p2.nextPcLow
  io.meta.noAnchor               := !s1_anchored

  /* *** training ***
   * Driven by s3's verified result, so pTAGE learns what the high-level predictor concluded rather than waiting for
   * the backend. The point is to track s3 closely and stop overriding it; final accuracy is s3's job, and the odd
   * group that s3 itself got wrong is corrected by a later training event.
   *
   * Nothing is read back here. Every index, tag and counter the update needs travelled down the pipeline with the
   * group, which is what keeps training from having to reconstruct a context that has since moved on, and leaves the
   * tables with a write port that only ever writes.
   */
  private val t0_valid   = io.fastTrain.get.valid && io.enable
  private val t0_train   = io.fastTrain.get.bits
  private val t0_meta    = t0_train.ptageMeta
  private val t0_branch  = t0_train.branch
  private val t0_startPc = t0_train.startPc
  private val t0_nextPc  = t0_branch.target

  // An entry can only describe a group whose next pc it is able to rebuild. A taken exit that jumps beyond the stored
  // low bits is not representable, so learning it would install a confidently wrong target; leaving it as a miss lets
  // the fallback answer instead. A return is the exception: its target comes from the return stack, not from here.
  private val t0_representable =
    !t0_branch.taken || getTargetCarry(t0_startPc, t0_nextPc).isFit || t0_branch.attribute.isReturn

  // Hold each verified group back by one training event. When the next one arrives we know whether it continues the
  // held group, and can therefore write a pair rather than a lone block.
  private val pending = RegInit(0.U.asTypeOf(Valid(new PtagePendingGroup)))

  private val t0_continuesPending =
    pending.valid && t0_valid &&
      PtageBlock.hasStaticTarget(pending.bits.taken, pending.bits.attribute) &&
      t0_startPc === pending.bits.nextPc &&
      // only a conditional exit may end a group's second block: anything else has no target the entry can rebuild
      t0_branch.attribute.isConditional && t0_representable

  // The first group after a correction belongs to no entry, so it is neither written nor used as a second block.
  private val t0_anchored = t0_valid && !t0_meta.noAnchor && t0_representable

  when(io.redirect) {
    pending.valid := false.B
  }.elsewhen(t0_valid) {
    pending.valid            := t0_anchored
    pending.bits.meta        := t0_meta
    pending.bits.cfiPosition := t0_branch.cfiPosition
    pending.bits.attribute   := t0_branch.attribute
    pending.bits.nextPcLow   := getEntryNextPc(t0_nextPc)
    pending.bits.nextPc      := t0_nextPc
    pending.bits.taken       := t0_branch.taken

    pending.bits.hasSecondBlock := t0_train.hasSecondBlock
  }

  /* *** t1: decide what to write, and build it *** */
  private val t1_write = RegInit(0.U.asTypeOf(Valid(new PtageTrainWrite)))

  private val held     = pending.bits
  private val heldMeta = held.meta
  private val heldHit  = heldMeta.provider.valid
  // The entry described this group correctly: the same exit, in the same place, going to the same address. Leaving
  // the target out of this counts an entry that rebuilds the wrong address as right, so it is strengthened rather
  // than corrected, and once strengthened it can earn a mark and hold its place -- a confidently wrong entry that
  // goes on being found and goes on being refused by the predictors that check it. Only the direction is left out,
  // because that is what the counter beside the entry is for.
  private val heldCorrect = heldHit &&
    heldMeta.p1CfiPosition === held.cfiPosition &&
    heldMeta.p1Attribute.asUInt === held.attribute.asUInt &&
    heldMeta.p1NextPcLow === held.nextPcLow

  // A wrong entry is handed to a longer history to tell the two contexts apart, so allocation looks above the
  // provider; a miss may go anywhere. A marked entry is passed over, and when every candidate is marked the
  // allocation simply fails -- the marks are what the reset below exists to take away.
  private val allocMask = VecInit(Seq.tabulate(NumTables) { t =>
    val longerThanProvider = if (t == 0) !heldHit else !heldHit || heldMeta.provider.bits < t.U
    longerThanProvider && !heldMeta.usefulVec(t)
  })
  // an allocation looks for the shortest history that is free, so a new context is learned as cheaply as possible
  private val allocSel = {
    val sel = Wire(Valid(UInt(log2Ceil(NumTables).W)))
    sel.valid := allocMask.reduce(_ || _)
    sel.bits  := PriorityEncoder(allocMask)
    sel
  }

  // There is one write to spend per event, so the outcomes are exclusive. An entry that was right is strengthened. A
  // wrong one is normally given to a longer history, keeping both contexts represented, but an entry that was never
  // confident, or one no table will take over from, is simply corrected where it stands.
  private val correctInPlace = heldHit && (heldMeta.p1Counter.isWeak || !allocSel.valid)
  private val doStrengthen   = pending.valid && heldCorrect
  private val doCorrect      = pending.valid && !heldCorrect && correctInPlace
  private val doAllocate     = pending.valid && !heldCorrect && !correctInPlace && allocSel.valid

  // Count refusals until one eviction is allowed, then start counting again. Clearing the count on an allocation that
  // succeeded normally would be wrong: successes and refusals interleave, so the count would never reach the limit
  // and marked entries would never be reclaimed at all.
  private val allocRefused = pending.valid && !heldCorrect && !allocSel.valid

  /* *** letting the marks go ***
   * Count sustained allocation pressure rather than a run of bad luck: up when an allocation found nowhere to go,
   * down when one succeeded. When the count says the tables have been full of marked entries for long enough, drop
   * every mark at once and let the entries that still deserve one earn it again.
   */
  private val usefulResetCnt = RegInit(0.U(log2Ceil(UsefulResetThreshold + 1).W))
  private val doUsefulReset  = t0_valid && allocRefused && usefulResetCnt === (UsefulResetThreshold - 1).U

  when(t0_valid) {
    when(allocRefused) {
      when(doUsefulReset) {
        usefulResetCnt := 0.U
        usefulMarks.foreach(_.foreach(_ := false.B))
      }.otherwise {
        usefulResetCnt := usefulResetCnt + 1.U
      }
    }.elsewhen(doAllocate && usefulResetCnt =/= 0.U) {
      usefulResetCnt := usefulResetCnt - 1.U
    }
  }

  // A mark is earned where an entry was right about a group an alternate would have called differently. It is also
  // lost: an allocation puts a different group in that place, and the mark belongs to the place rather than to the
  // entry now, so leaving it would hand the newcomer a protection it never earned.
  private val markEarned = doStrengthen && heldMeta.providerDiffersFromAlt
  when(t1_write.valid) {
    val markAddr = usefulAddr(t1_write.bits.bank, t1_write.bits.setIdx)
    when(RegNext(doAllocate, init = false.B)) {
      usefulMarks(t1_write.bits.table)(markAddr) := false.B
    }.elsewhen(RegNext(markEarned, init = false.B)) {
      usefulMarks(t1_write.bits.table)(markAddr) := true.B
    }
  }

  private val writeTable = Mux(doAllocate, allocSel.bits, heldMeta.provider.bits)
  // only a strengthened entry keeps its counter and its standing; the other two install the group afresh
  private val writeFresh = !doStrengthen

  private val entry = Wire(new PtageEntry)
  entry.valid := true.B
  entry.tag   := heldMeta.tag(writeTable)

  entry.p1.cfiPosition := held.cfiPosition
  entry.p1.attribute   := held.attribute
  entry.p1.nextPcLow   := held.nextPcLow
  entry.p1.counter := Mux(
    writeFresh,
    Mux(held.taken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
    heldMeta.p1Counter.getUpdate(held.taken)
  )

  // A group that kept a second block consumed its own successor, so the next training event is the group after it and
  // no continuation can form. The entry's stored second block is what produced that block and s3 accepted it, so it is
  // put back and reinforced instead of dropped. Clearing it here would erase a pair at the very moment it proved
  // correct, leaving the entry to learn the same pair over and over and never hold one long enough to use it twice.
  private val heldPairConfirmed = doStrengthen && held.hasSecondBlock && heldMeta.p2Valid

  entry.p2Valid        := t0_continuesPending || heldPairConfirmed
  entry.p2.cfiPosition := Mux(heldPairConfirmed, heldMeta.p2CfiPosition, t0_branch.cfiPosition)
  entry.p2.attribute   := Mux(heldPairConfirmed, heldMeta.p2Attribute, t0_branch.attribute)
  entry.p2.nextPcLow   := Mux(heldPairConfirmed, heldMeta.p2NextPcLow, getEntryNextPc(t0_nextPc))
  entry.p2.counter := Mux(
    heldPairConfirmed,
    heldMeta.p2Counter.getUpdate(true.B),
    Mux(
      writeFresh || !heldMeta.p2Valid,
      Mux(t0_branch.taken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
      heldMeta.p2Counter.getUpdate(t0_branch.taken)
    )
  )

  private val writeHappens = t0_valid && (doStrengthen || doCorrect || doAllocate)

  t1_write.valid := writeHappens
  when(t0_valid && pending.valid) {
    t1_write.bits.table  := writeTable
    t1_write.bits.bank   := heldMeta.bankIdx
    t1_write.bits.setIdx := heldMeta.setIdx(writeTable)
    t1_write.bits.entry  := entry
  }

  /* *** t2: hand the write to the banks ***
   * The write buffer inside a bank is the drain stage: it takes the request now and lands it on a cycle whose bank is
   * not busy serving a prediction.
   */
  banks.zipWithIndex.foreach { case (tableBanks, t) =>
    tableBanks.zipWithIndex.foreach { case (bank, b) =>
      bank.io.writeReq.valid       := t1_write.valid && t1_write.bits.table === t.U && t1_write.bits.bank === b.U
      bank.io.writeReq.bits.setIdx := t1_write.bits.setIdx
      bank.io.writeReq.bits.entry  := t1_write.bits.entry
    }
  }

  XSPerfAccumulate("trainEvent", t0_valid)
  XSPerfAccumulate("trainNoAnchor", t0_valid && t0_meta.noAnchor)
  XSPerfAccumulate("trainPaired", t0_valid && t0_continuesPending)
  XSPerfAccumulate("trainP2Kept", writeHappens && heldPairConfirmed)
  XSPerfAccumulate("trainP2Dropped", writeHappens && heldMeta.p2Valid && !entry.p2Valid)
  XSPerfAccumulate("trainStrengthen", t0_valid && doStrengthen)
  XSPerfAccumulate("trainCorrect", t0_valid && doCorrect)
  XSPerfAccumulate("trainAllocate", t0_valid && doAllocate)
  XSPerfAccumulate("trainNoTableFree", t0_valid && allocRefused)
  /* *** allocation and the marks, counted the way the model counts them ***
   * An allocation that lands on a live entry is an eviction; one that finds a table with nothing in it is free. The
   * refusal count and the resets say whether the tables are able to take new groups at all, which is the thing the
   * marks were quietly preventing.
   */
  XSPerfAccumulate("trainAllocateEvicted", t0_valid && doAllocate && heldMeta.validVec(allocSel.bits))
  XSPerfAccumulate("trainAllocateFree", t0_valid && doAllocate && !heldMeta.validVec(allocSel.bits))
  XSPerfAccumulate("usefulReset", doUsefulReset)

  /* *** what a training event actually manages to do ***
   * One write is spent per event, so an event that wants to correct a wrong provider and to hand the context to a
   * longer table can only do one of them. Count how often both are wanted: allocation always goes to a table above
   * the provider, so the two writes would never be to the same bank, and the restriction is this module's own.
   */
  private val wantsProviderUpdate = pending.valid && heldHit && !heldCorrect
  private val wantsAllocation     = pending.valid && !heldCorrect && allocSel.valid
  XSPerfAccumulate("trainWantsBoth", t0_valid && wantsProviderUpdate && wantsAllocation)
  XSPerfAccumulate(
    "trainRightExitWrongTarget",
    t0_valid && pending.valid && heldHit &&
      heldMeta.p1CfiPosition === held.cfiPosition &&
      heldMeta.p1Attribute.asUInt === held.attribute.asUInt &&
      heldMeta.p1NextPcLow =/= held.nextPcLow
  )
  XSPerfAccumulate("trainHitButWrong", t0_valid && pending.valid && heldHit && !heldCorrect)
  XSPerfAccumulate("trainMissAllocated", t0_valid && pending.valid && !heldHit && doAllocate)
  XSPerfAccumulate("trainMissRefused", t0_valid && pending.valid && !heldHit && !allocSel.valid)
  XSPerfAccumulate("trainWroteNothing", t0_valid && pending.valid && !writeHappens)
  // an entry counted correct may still have rebuilt the wrong target, since only where a block ends is compared
  XSPerfAccumulate(
    "trainCorrectButTargetWrong",
    t0_valid && heldCorrect && heldMeta.p2NextPcLow =/= getEntryNextPc(t0_nextPc) && held.hasSecondBlock
  )
  (0 until NumTables).foreach { t =>
    XSPerfAccumulate(s"trainAllocateTable$t", t0_valid && doAllocate && allocSel.bits === t.U)
    XSPerfAccumulate(s"trainAllocCandidateTable$t", t0_valid && allocMask(t))
  }
  XSPerfAccumulate("usefulMarkEarned", t1_write.valid && RegNext(markEarned, init = false.B))
  XSPerfAccumulate("usefulMarkWouldHaveBeenOld", t0_valid && doStrengthen)
  XSPerfAccumulate(
    "trainAllCandidatesMarked",
    t0_valid && allocRefused && VecInit(Seq.tabulate(NumTables) { t =>
      val longerThanProvider = if (t == 0) !heldHit else !heldHit || heldMeta.provider.bits < t.U
      longerThanProvider && heldMeta.usefulVec(t)
    }).reduce(_ || _)
  )
  XSPerfAccumulate("trainAllocPressureHigh", t0_valid && usefulResetCnt > (UsefulResetThreshold / 2).U)

  private val s1_fire = io.stageCtrl.s1_fire && io.enable

  XSPerfAccumulate("lookupHasAlt", s1_fire && s1_provider.valid && s1_alt.valid)
  XSPerfAccumulate("lookupProviderMarked", s1_fire && s1_provider.valid && s1_useful(s1_provider.bits))
  XSPerfAccumulate("lookupUnanchored", s1_fire && !s1_anchored)
  XSPerfAccumulate("lookupReplayed", s1_fire && RegEnable(RegEnable(a0_override, s0_fire), s0_fire))
  XSPerfAccumulate("predHit", s1_fire && s1_p1Usable)
  XSPerfAccumulate("predMiss", s1_fire && !s1_p1Usable)
  XSPerfAccumulate("predTwoBlocks", s1_fire && s1_p2Usable)
  XSPerfAccumulate("predP2SuppressedByAttribute", s1_fire && s1_p1Usable && s1_providerEntry.p2Valid && !s1_p2Usable)
  XSPerfAccumulate(
    "predP2WouldSuppressByCounter",
    s1_fire && s1_p2Usable && !s1_providerEntry.p2.counter.isSaturatePositive
  )
  XSPerfAccumulate("predP2WouldSuppressByTable0", s1_fire && s1_p2Usable && s1_provider.bits === 0.U)
  XSPerfAccumulate(
    "predP2SuppressedByCounter",
    s1_fire && s1_p1Usable && s1_providerEntry.p2Valid &&
      s1_providerEntry.p2.attribute.isConditional && !s1_providerEntry.p2.counter.isSaturatePositive
  )
  Seq.tabulate(NumTables)(t =>
    XSPerfAccumulate(s"providerTable$t", s1_fire && s1_provider.valid && s1_provider.bits === t.U)
  )
}
