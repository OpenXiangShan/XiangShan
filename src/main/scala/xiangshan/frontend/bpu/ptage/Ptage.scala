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
import xiangshan.frontend.bpu.HasFastTrainIO
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
  class PtageIO(implicit p: Parameters) extends BasePredictorIO with HasFastTrainIO {
    // resident folded histories, one set of folds per table span
    val foldedHist: PhrAllFoldedHistories =
      Input(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum))

    // a correction lands on the history a cycle after it is announced, so pTAGE has to know when one happened
    val redirectValid: Bool = Input(Bool())
    val overrideValid: Bool = Input(Bool())

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
   * The pc arriving now starts the group after the one entering the pipeline, which is exactly the key of the entry
   * that will be needed two cycles from here. Everything on this path is one XOR deep, because the pc itself is the
   * late signal, coming from the previous cycle's own prediction.
   */
  private val a0_startPc = io.startPc
  private val a0_bankIdx = getBankIndex(a0_startPc)

  // A read issued in a correction cycle indexes with the folded history as it stood before that correction reached
  // FastPhr, so whatever it returns belongs to the path just abandoned. pTAGE sits that one group out.
  private val a0_anchored = !io.redirectValid && !io.overrideValid

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
   * The tag compare sits here rather than in s1 so that s1 is left with only a priority select and a decode. Both
   * folded tag histories were captured with the address, so this is an xor of already-registered values.
   */
  private val s0_anchored = RegEnable(a0_anchored, false.B, s0_fire)
  private val s0_startPc  = RegEnable(a0_startPc, s0_fire)
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

  /* *** s1: select a provider and decode the group *** */
  private val s1_anchored = RegEnable(s0_anchored, false.B, s0_fire)
  private val s1_startPc  = RegEnable(io.startPc, s0_fire)
  private val s1_entry    = RegEnable(s0_entry, s0_fire)
  private val s1_hitVec   = RegEnable(s0_hitVec, s0_fire)
  private val s1_setIdx   = RegEnable(s0_setIdx, s0_fire)
  private val s1_tag      = RegEnable(s0_tag, s0_fire)
  private val s1_bankIdx  = RegEnable(s0_bankIdx, s0_fire)

  // The longest history that hit provides the prediction and the next longest is its alternative, as in any TAGE:
  // a longer history is a more specific context, so where one matches it is the better answer.
  private def selectLongest(mask: Seq[Bool]): Valid[UInt] = {
    val sel = Wire(Valid(UInt(log2Ceil(NumTables).W)))
    sel.valid := mask.reduce(_ || _)
    sel.bits  := (NumTables - 1).U - PriorityEncoder(mask.reverse)
    sel
  }

  // an allocation looks for the shortest history that is free, so a new context is learned as cheaply as possible
  private def selectShortest(mask: Seq[Bool]): Valid[UInt] = {
    val sel = Wire(Valid(UInt(log2Ceil(NumTables).W)))
    sel.valid := mask.reduce(_ || _)
    sel.bits  := PriorityEncoder(mask)
    sel
  }

  private val s1_provider = selectLongest(s1_hitVec)

  private val s1_providerEntry = Mux1H(UIntToOH(s1_provider.bits, NumTables), s1_entry)

  // The first block starts where this group starts; the second starts at the first's next pc, which is why that field
  // is not stored twice.
  private val s1_p1Target = getFullTarget(s1_startPc, s1_providerEntry.p1.nextPcLow, None)
  private val s1_p2Target = getFullTarget(s1_p1Target, s1_providerEntry.p2.nextPcLow, None)

  // A block can only be followed by another if its own target is the one the entry stored: a deferred exit takes its
  // target from elsewhere, so the second block's start would not be where the entry assumed.
  private val s1_p1Usable = s1_anchored && s1_provider.valid
  private val s1_p2Usable =
    s1_p1Usable && s1_providerEntry.p2Valid &&
      PtageBlock.hasStaticTarget(s1_providerEntry.p1.taken, s1_providerEntry.p1.attribute)

  private def decode(block: PtageBlock, target: PrunedAddr): PtageBlockPrediction = {
    val prediction = Wire(new PtageBlockPrediction)
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

  io.meta.setIdx        := s1_setIdx
  io.meta.tag           := s1_tag
  io.meta.bankIdx       := s1_bankIdx
  io.meta.usefulVec     := VecInit(s1_entry.map(_.useful))
  io.meta.provider      := s1_provider
  io.meta.p1Counter     := s1_providerEntry.p1.counter
  io.meta.p2Counter     := s1_providerEntry.p2.counter
  io.meta.p2Valid       := s1_providerEntry.p2Valid
  io.meta.p1CfiPosition := s1_providerEntry.p1.cfiPosition
  io.meta.p1Attribute   := s1_providerEntry.p1.attribute
  io.meta.p2CfiPosition := s1_providerEntry.p2.cfiPosition
  io.meta.p2Attribute   := s1_providerEntry.p2.attribute
  io.meta.p2NextPcLow   := s1_providerEntry.p2.nextPcLow
  io.meta.noAnchor      := !s1_anchored

  /* *** training ***
   * Driven by s3's verified result, so pTAGE learns what the high-level predictor concluded rather than waiting for
   * the backend. The point is to track s3 closely and stop overriding it; final accuracy is s3's job, and the odd
   * group that s3 itself got wrong is corrected by a later training event.
   *
   * Nothing is read back here. Every index, tag and counter the update needs travelled down the pipeline with the
   * group, which is what keeps training from having to reconstruct a context that has since moved on, and leaves the
   * tables with a write port that only ever writes.
   */
  private val t0_valid      = io.fastTrain.get.valid && io.enable
  private val t0_train      = io.fastTrain.get.bits
  private val t0_meta       = t0_train.ptageMeta
  private val t0_prediction = t0_train.finalPrediction
  private val t0_startPc    = t0_train.startPc
  private val t0_nextPc     = t0_prediction.target.unGuard

  // An entry rebuilds a target from the pc it was reached with, so a target out of that reach cannot be stored.
  private def targetInReach(startPc: PrunedAddr, target: PrunedAddr): Bool =
    getTargetUpper(startPc) === getTargetUpper(target)

  // An entry can only describe a group whose next pc it is able to rebuild. A taken exit that jumps beyond the stored
  // low bits is not representable, so learning it would install a confidently wrong target; leaving it as a miss lets
  // the fallback answer instead. A return is the exception: its target comes from the return stack, not from here.
  private val t0_representable =
    !t0_prediction.taken || targetInReach(t0_startPc, t0_nextPc) || t0_prediction.attribute.isReturn

  private val t0_cfiPc = getCfiPcFromPosition(t0_startPc, t0_prediction.cfiPosition)
  // the same hash Phr shifts into the path history for this block, so the entry can later supply it ready-made
  private val t0_pathHash = Mux(t0_prediction.taken, pathHash(t0_cfiPc, t0_nextPc), 0.U)

  // Hold each verified group back by one training event. When the next one arrives we know whether it continues the
  // held group, and can therefore write a pair rather than a lone block.
  private val pending = RegInit(0.U.asTypeOf(Valid(new PtagePendingGroup)))

  private val t0_continuesPending =
    pending.valid && t0_valid &&
      PtageBlock.hasStaticTarget(pending.bits.taken, pending.bits.attribute) &&
      t0_startPc === pending.bits.nextPc &&
      // only a conditional exit may end a group's second block: anything else has no target the entry can rebuild
      t0_prediction.attribute.isConditional && t0_representable

  // The first group after a correction belongs to no entry, so it is neither written nor used as a second block.
  private val t0_anchored = t0_valid && !t0_meta.noAnchor && t0_representable

  when(io.redirectValid) {
    pending.valid := false.B
  }.elsewhen(t0_valid) {
    pending.valid            := t0_anchored
    pending.bits.meta        := t0_meta
    pending.bits.cfiPosition := t0_prediction.cfiPosition
    pending.bits.attribute   := t0_prediction.attribute
    pending.bits.nextPcLow   := getEntryNextPc(t0_nextPc)
    pending.bits.nextPc      := t0_nextPc
    pending.bits.taken       := t0_prediction.taken
    pending.bits.pathHash    := t0_pathHash

    pending.bits.hasSecondBlock := t0_train.hasSecondBlock
  }

  /* *** t1: decide what to write, and build it *** */
  private val t1_write = RegInit(0.U.asTypeOf(Valid(new PtageTrainWrite)))

  private val held     = pending.bits
  private val heldMeta = held.meta
  private val heldHit  = heldMeta.provider.valid
  // the entry named this group's exit correctly, so only its direction was ever in question
  private val heldCorrect = heldHit &&
    heldMeta.p1CfiPosition === held.cfiPosition &&
    heldMeta.p1Attribute.asUInt === held.attribute.asUInt

  // Useful marks an entry that earned its place, and allocation passes those over. Nothing ever clears the mark, so
  // an entry that stopped being consulted would hold its table indefinitely and allocation would find nowhere left to
  // go. Sweeping the tables to clear marks is not open to a training path that only writes, so instead count how
  // often allocation is turned away and, once it has been often enough, let the next one take a marked entry.
  private val allocRefusals = RegInit(0.U(AllocRefusalLimitWidth.W))
  private val allocMayEvict = allocRefusals.andR

  // A wrong entry is handed to a longer history to tell the two contexts apart, so allocation looks above the
  // provider; a miss may go anywhere.
  private val allocMask = VecInit(Seq.tabulate(NumTables) { t =>
    val longerThanProvider = if (t == 0) !heldHit else !heldHit || heldMeta.provider.bits < t.U
    longerThanProvider && (!heldMeta.usefulVec(t) || allocMayEvict)
  })
  private val allocSel = selectShortest(allocMask)

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
  when(t0_valid) {
    when(doAllocate && allocMayEvict)(allocRefusals := 0.U)
      .elsewhen(pending.valid && !heldCorrect && !allocSel.valid)(allocRefusals := allocRefusals + 1.U)
  }

  private val writeTable = Mux(doAllocate, allocSel.bits, heldMeta.provider.bits)
  // only a strengthened entry keeps its counter and its standing; the other two install the group afresh
  private val writeFresh = !doStrengthen

  private val entry = Wire(new PtageEntry)
  entry.valid  := true.B
  entry.tag    := heldMeta.tag(writeTable)
  entry.useful := doStrengthen

  entry.p1.cfiPosition := held.cfiPosition
  entry.p1.attribute   := held.attribute
  entry.p1.nextPcLow   := held.nextPcLow
  entry.p1.counter := Mux(
    writeFresh,
    Mux(held.taken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
    Mux(held.taken, heldMeta.p1Counter.getIncrease(), heldMeta.p1Counter.getDecrease())
  )

  // A group that kept a second block consumed its own successor, so the next training event is the group after it and
  // no continuation can form. The entry's stored second block is what produced that block and s3 verified it, so it is
  // put back and reinforced instead of dropped. Clearing it here would erase a pair at the very moment it proved
  // correct, leaving the entry to learn the same pair over and over and never hold one long enough to use it twice.
  private val heldPairConfirmed = doStrengthen && held.hasSecondBlock && heldMeta.p2Valid

  entry.p2Valid        := t0_continuesPending || heldPairConfirmed
  entry.p2.cfiPosition := Mux(heldPairConfirmed, heldMeta.p2CfiPosition, t0_prediction.cfiPosition)
  entry.p2.attribute   := Mux(heldPairConfirmed, heldMeta.p2Attribute, t0_prediction.attribute)
  entry.p2.nextPcLow   := Mux(heldPairConfirmed, heldMeta.p2NextPcLow, getEntryNextPc(t0_nextPc))
  entry.p2.counter := Mux(
    heldPairConfirmed,
    heldMeta.p2Counter.getIncrease(),
    Mux(
      writeFresh || !heldMeta.p2Valid,
      Mux(t0_prediction.taken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
      Mux(t0_prediction.taken, heldMeta.p2Counter.getIncrease(), heldMeta.p2Counter.getDecrease())
    )
  )

  // Each block contributes a whole path hash, the second sitting Shamt above the first, exactly as Phr would have
  // applied them one after the other.
  entry.phrToken := Mux(
    t0_continuesPending,
    (held.pathHash << Shamt).asUInt ^ t0_pathHash,
    held.pathHash
  )
  entry.ghrShamt := held.taken.asUInt +& (t0_continuesPending && t0_prediction.taken).asUInt

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
  XSPerfAccumulate("trainNoTableFree", t0_valid && pending.valid && !heldCorrect && !allocSel.valid)
  XSPerfAccumulate("trainAllocateEvicted", t0_valid && doAllocate && allocMayEvict)

  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  XSPerfAccumulate("predHit", s1_fire && s1_p1Usable)
  XSPerfAccumulate("predMiss", s1_fire && !s1_p1Usable)
  XSPerfAccumulate("predTwoBlocks", s1_fire && s1_p2Usable)
  XSPerfAccumulate("predP2SuppressedByAttribute", s1_fire && s1_p1Usable && s1_providerEntry.p2Valid && !s1_p2Usable)
  Seq.tabulate(NumTables)(t =>
    XSPerfAccumulate(s"providerTable$t", s1_fire && s1_provider.valid && s1_provider.bits === t.U)
  )
}
