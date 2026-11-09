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
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfSeqAccumulate
import xiangshan.frontend.GuardedPc
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
      Input(new PhrAllFoldedHistories(FastFoldedHistoryInfo))

    // one block, two on a 2-taken hit, or none at all, each decoded into the shape every predictor hands the top level
    val prediction: Vec[Valid[Prediction]] = Output(Vec(MaxPredictionNum, Valid(new Prediction)))
    val meta:       PtageMeta              = Output(new PtageMeta)

    // how a second block this predictor put out resolved: the entry behind it, and whether the block was right
    val pairOutcome:        Valid[PtagePairTag] = Input(Valid(new PtagePairTag))
    val pairOutcomeCorrect: Bool                = Input(Bool())
    // where the groups an override corrects start, to check that a replayed read was keyed on them
    val debug_bpuS2StartPc: Option[GuardedPc] = Option.when(!env.FPGAPlatform)(Input(GuardedPc()))
    val debug_bpuS3StartPc: Option[GuardedPc] = Option.when(!env.FPGAPlatform)(Input(GuardedPc()))
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
  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire && io.enable

  // A correction puts a new group in s0 in the cycle it happens. A redirect wins over an override, as it does for the
  // pc itself.
  private val s0_override = !io.redirect && (io.bpuS2Override || io.bpuS3Override)

  /* *** a0: issue the ahead read ***
   * The pc arriving now starts the group in s0, which is the group the next one follows, so it is exactly the key of
   * the entry the next group will need. That holds on a correction cycle too: the group in s0 is then the corrected
   * path's, and the history handed out with it already covers everything before it. Everything on this path is one
   * XOR deep, because the pc itself is the late signal, coming from the previous cycle's own prediction.
   */
  private val a0_startPc = io.startPc
  private val a0_bankIdx = getBankIndex(a0_startPc)

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

  // two folds of different widths, so that a tag does not simply repeat what the set index already says
  private val a0_tagFold = VecInit(Seq.tabulate(NumTables) { t =>
    foldedFor(t, fastPhrParameters.TagWidth) ^ (foldedFor(t, fastPhrParameters.TagWidth - 1) << 1).asUInt
  })

  /* *** s0: sram data returns, match tags ***
   * The tag compare sits here rather than in s1 so that s1 is left with only a priority select and a decode.
   *
   * The index had to be keyed on the previous group's pc, since it is needed a cycle before this one exists. The tag
   * does not: by now the predicted group's own start pc has arrived, so the entry can be tagged with where the group
   * actually starts rather than with where its predecessor did. An entry whose predecessor has since learned to jump
   * elsewhere then misses, instead of hitting and rebuilding its targets from the wrong base.
   */
  private val s0_startPc = io.startPc
  // the group that was in s0 at the last fire, which is the one in s1 now
  private val s1_startPc = RegEnable(s0_startPc, s0_fire)

  // The read issued at a0, keyed on the group that was in s0 then. That group is this one's predecessor unless a
  // redirect has just put a different group in s0; on an override this read is not used.
  private val s0_sramRead = Wire(new PipelineState)
  s0_sramRead.bankIdx := RegEnable(a0_bankIdx, s0_fire)
  s0_sramRead.setIdx  := RegEnable(a0_setIdx, s0_fire)
  s0_sramRead.tagFold := RegEnable(a0_tagFold, s0_fire)
  s0_sramRead.debug_keyPc.foreach(_ := s1_startPc)
  s0_sramRead.entry := VecInit(Seq.tabulate(NumTables) { t =>
    Mux1H(UIntToOH(s0_sramRead.bankIdx, NumBanks), banks(t).map(_.io.readResp.entry))
  })
  s0_sramRead.valid := !io.redirect

  /* After an override, the group in s0 follows the group the override corrected. The read keyed on that group was
   * made while it sat in s0, and it is still held, as the lookup of the successor the override has just discarded: in
   * s1 for a group corrected at s2. A group corrected at s3 takes a copy of that read with it when it leaves s2, the
   * moment its successor's lookup is still in s1. Matching the read again against the new group's pc is the lookup a
   * read issued now would give a cycle too late. A redirect follows the mispredicted block, whose read is long gone,
   * so its group sits the lookup out.
   */
  private val s1_read       = RegInit(0.U.asTypeOf(new PipelineState))
  private val s3_replayRead = RegEnable(s1_read, 0.U.asTypeOf(new PipelineState), s2_fire)
  private val s0_read       = Mux(s0_override, Mux(io.bpuS3Override, s3_replayRead, s1_read), s0_sramRead)
  when(s0_fire)(s1_read := s0_read)

  // the corrected group's own start pc, which every replayed read must have been keyed on
  private val debug_overrideOwnStartPc = io.debug_bpuS2StartPc.zip(io.debug_bpuS3StartPc).map { case (s2Pc, s3Pc) =>
    Mux(io.bpuS3Override, s3Pc, s2Pc)
  }
  s0_read.debug_keyPc.zip(debug_overrideOwnStartPc).foreach { case (keyPc, ownStartPc) =>
    XSError(
      s0_fire && s0_override && s0_read.valid && keyPc.asUInt =/= ownStartPc.asUInt,
      "pTAGE replayed a read that was not keyed on the group the override corrected\n"
    )
  }

  private val s0_bankIdx = s0_read.bankIdx
  private val s0_setIdx  = s0_read.setIdx
  private val s0_entry   = s0_read.entry
  private val s0_tag =
    VecInit(Seq.tabulate(NumTables)(t => (s0_read.tagFold(t) ^ getTagPc(s0_startPc))(TagWidth - 1, 0)))
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

  /* *** pair confidence ***
   * How many times in a row an entry's second block has turned out right. Without a second lookup at s3 nothing on
   * the prediction side can tell a right second block from a wrong one, so for a block that was put out the count is
   * set from where the block resolves. For a pair held back, s3's prediction of the next group says whether the pair
   * would have been right. Kept beside the useful marks for the same reason they are: it is written from somewhere the
   * entry's own write port does not reach.
   */
  private val pairConf = RegInit(VecInit(Seq.fill(NumTables)(
    VecInit(Seq.fill(NumBanks * NumSets)(0.U(PairConfWidth.W)))
  )))
  private def confInc(c: UInt): UInt = Mux(c === PairConfMax.U, c, c + 1.U)

  private val s0_pairConf =
    VecInit(Seq.tabulate(NumTables)(t => pairConf(t)(usefulAddr(s0_bankIdx, s0_setIdx(t)))))
  private val s1_pairConf = RegEnable(s0_pairConf, s0_fire)

  /* *** s1: select a provider and decode the group *** */
  private val s1_valid  = s1_read.valid
  private val s1_entry  = s1_read.entry
  private val s1_hitVec = RegEnable(s0_hitVec, s0_fire)

  /* *** perf: where a lookup sits relative to a correction ***
   * Judged in the cycle the group leaves s0, when the correction that brought it there is still visible.
   */
  private val perf_kindNow = MuxCase(
    PtageLookupClass.Steady.U,
    Seq(
      io.redirect      -> PtageLookupClass.AtRedirect.U,
      io.bpuS3Override -> PtageLookupClass.AtS3Override.U,
      io.bpuS2Override -> PtageLookupClass.AtS2Override.U
    )
  )
  private val perf_lastKind = RegEnable(perf_kindNow, PtageLookupClass.Steady.U, s0_fire)
  private val perf_s0Class = Mux(
    perf_kindNow =/= PtageLookupClass.Steady.U,
    perf_kindNow,
    Mux(
      perf_lastKind =/= PtageLookupClass.Steady.U,
      // widening: an "after" class lies past the range of the "at" class it comes from
      perf_lastKind +& (PtageLookupClass.AfterRedirect - PtageLookupClass.AtRedirect).U,
      PtageLookupClass.Steady.U
    )
  )
  private val s1_perfClass = RegEnable(perf_s0Class, PtageLookupClass.Steady.U, s0_fire)

  // The longest history that hit provides the prediction, as in any TAGE: a longer history is a more specific
  // context, so where one matches it is the better answer. The select stays one-hot, since everything on the
  // prediction path picks with it and only the meta needs the table's index.
  private def selectLongestOH(mask: Seq[Bool]): Vec[Bool] = VecInit(PriorityEncoderOH(mask.reverse).reverse)

  private val s1_providerOH = selectLongestOH(s1_hitVec)
  private val s1_provider   = Wire(Valid(UInt(log2Ceil(NumTables).W)))
  s1_provider.valid := s1_hitVec.reduce(_ || _)
  s1_provider.bits  := OHToUInt(s1_providerOH)

  private val s1_providerEntry = Mux1H(s1_providerOH, s1_entry)

  // The alternate answer: the longest history that hit below the provider. A mark is only worth giving where the
  // provider said something the alternate did not -- an entry that merely agrees with a shorter history is not
  // holding any context of its own, and protecting it costs a place that a new group could have used.
  private val s1_altHitVec = VecInit((s1_hitVec zip s1_providerOH).map { case (hit, isProvider) => hit && !isProvider })
  private val s1_altEntry  = Mux1H(selectLongestOH(s1_altHitVec), s1_entry)
  private val s1_providerDiffersFromAlt = s1_altHitVec.reduce(_ || _) && (
    s1_providerEntry.p1.taken =/= s1_altEntry.p1.taken ||
      s1_providerEntry.p1.cfiPosition =/= s1_altEntry.p1.cfiPosition
  )

  // The first block starts where this group starts; the second starts at the first's next pc, which is why that field
  // is not stored twice.
  private val s1_p1Target = getFullTarget(s1_startPc, s1_providerEntry.p1.nextPcLow, None)
  private val s1_p2Target = getFullTarget(s1_p1Target, s1_providerEntry.p2.nextPcLow, None)

  // A block can only be followed by another if its own target is the one the entry stored: a deferred exit takes its
  // target from elsewhere, so the second block's start would not be where the entry assumed.
  private val s1_p1Usable = s1_valid && s1_provider.valid
  // An indirect exit, return included, takes its target from somewhere else, so the next pc the entry keeps is not
  // where control actually goes and cannot be where a second block starts.
  private val s1_p2Offered =
    s1_p1Usable && s1_providerEntry.p2Valid &&
      s1_providerEntry.p1.taken && !s1_providerEntry.p1.attribute.isIndirect &&
      // Only a conditional exit is ever learned as a second block, since anything else either takes its target from
      // elsewhere or moves the return stack. Checking it where the entry is used keeps a stale or aliased entry from
      // presenting one of those as a second block.
      s1_providerEntry.p2.attribute.isConditional

  // Nothing checks a second block before fetch, so it has to be right on its own: a wrong one costs a backend
  // redirect, while a right one saves at most a prediction cycle. Only a pair whose second block has turned out right
  // the last PairConfThreshold times it was due is put out.
  private val s1_providerConf = Mux1H(s1_providerOH, s1_pairConf)
  private val s1_confident    = s1_providerConf >= PairConfThreshold.U
  private val s1_p2Usable     = s1_p2Offered && s1_confident

  private def decode(block: PtageBlock, target: PrunedAddr): Prediction = {
    val prediction = Wire(new Prediction)
    prediction.taken       := block.taken
    prediction.cfiPosition := block.cfiPosition
    prediction.attribute   := block.attribute
    prediction.target      := target
    prediction
  }

  io.prediction(0).valid := s1_p1Usable
  io.prediction(0).bits  := decode(s1_providerEntry.p1, s1_p1Target)
  io.prediction(1).valid := s1_p2Usable
  io.prediction(1).bits  := decode(s1_providerEntry.p2, s1_p2Target)

  io.meta.valid  := s1_valid
  io.meta.setIdx := s1_read.setIdx
  io.meta.tag :=
    VecInit(Seq.tabulate(NumTables)(t => (s1_read.tagFold(t) ^ getTagPc(s1_startPc))(TagWidth - 1, 0)))
  io.meta.bankIdx                := s1_read.bankIdx
  io.meta.usefulVec              := s1_useful
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
  io.meta.p2Conf                 := s1_providerConf
  io.meta.perf_lookupClass       := s1_perfClass

  /* *** t0: receive a verified group ***
   * Driven by s3's verified result, so pTAGE learns what the high-level predictor concluded rather than waiting for
   * the backend. The point is to track s3 closely and stop overriding it; final accuracy is s3's job, and the odd
   * group that s3 itself got wrong is corrected by a later training event.
   *
   * Nothing is read back here. Every index, tag and counter the update needs travelled down the pipeline with the
   * group, which is what keeps training from having to reconstruct a context that has since moved on, and leaves the
   * tables with a write port that only ever writes.
   */
  private val t0_train   = io.fastTrain.get.bits
  private val t0_meta    = t0_train.ptageMeta
  private val t0_fire    = io.fastTrain.get.valid && t0_meta.valid && io.enable
  private val t0_branch  = t0_train.branch
  private val t0_startPc = t0_train.startPc
  private val t0_nextPc  = t0_branch.target

  // An entry can only describe a group whose next pc it is able to rebuild. A taken exit that jumps beyond the stored
  // low bits is not representable, so learning it would install a confidently wrong target; leaving it as a miss lets
  // the fallback answer instead. A return is the exception: its target comes from the return stack, not from here.
  private val t0_representable =
    !t0_branch.taken || getTargetCarry(t0_startPc, t0_nextPc).isFit || t0_branch.attribute.isReturn

  /* *** the held group ***
   * Each verified group is held back by one training event. When the next one arrives we know whether it continues
   * the held group, and can therefore write a pair rather than a lone block. This is what lets a pair be learned
   * without ever having been predicted as one; otherwise second blocks could only be learned where they existed.
   */
  private val heldValid       = RegInit(false.B)
  private val heldMeta        = RegEnable(t0_meta, t0_fire)
  private val heldCfiPosition = RegEnable(t0_branch.cfiPosition, t0_fire)
  private val heldAttribute   = RegEnable(t0_branch.attribute, t0_fire)
  private val heldNextPc      = RegEnable(t0_nextPc, t0_fire)
  private val heldTaken       = RegEnable(t0_branch.taken, t0_fire)
  // whether the held group already carried its own second block, which decides if a successor can still be expected
  private val heldHasSecondBlock = RegEnable(t0_train.hasSecondBlock, t0_fire)
  when(io.redirect) {
    heldValid := false.B
  }.elsewhen(t0_fire) {
    heldValid := t0_representable
  }

  // the arriving group continues the held one, so the two can be written as a pair
  private val t0_continuesHeld =
    heldValid && t0_fire && heldTaken && !heldAttribute.isIndirect && t0_startPc === heldNextPc &&
      // only a conditional exit may end a group's second block: anything else has no target the entry can rebuild
      t0_branch.attribute.isConditional && t0_representable

  /* *** t0: decide what to write for the held group, and build it *** */
  private val t0_heldHit       = heldMeta.provider.valid
  private val t0_heldNextPcLow = getEntryNextPc(heldNextPc)
  // The entry described the held group correctly: the same exit, in the same place, going to the same address.
  // Leaving the target out of this counts an entry that rebuilds the wrong address as right, so it is strengthened
  // rather than corrected, and once strengthened it can earn a mark and hold its place -- a confidently wrong entry
  // that goes on being found and goes on being refused by the predictors that check it. Only the direction is left
  // out, because that is what the counter beside the entry is for.
  private val t0_heldCorrect = t0_heldHit &&
    heldMeta.p1CfiPosition === heldCfiPosition &&
    heldMeta.p1Attribute.asUInt === heldAttribute.asUInt &&
    heldMeta.p1NextPcLow === t0_heldNextPcLow

  // A wrong entry is handed to a longer history to tell the two contexts apart, so allocation looks above the
  // provider; a miss may go anywhere. A marked entry is passed over, and when every candidate is marked the
  // allocation simply fails -- the marks are what the reset below exists to take away.
  private val t0_allocMask = VecInit(Seq.tabulate(NumTables) { t =>
    val longerThanProvider = if (t == 0) !t0_heldHit else !t0_heldHit || heldMeta.provider.bits < t.U
    longerThanProvider && !heldMeta.usefulVec(t)
  })
  // an allocation looks for the shortest history that is free, so a new context is learned as cheaply as possible
  private val t0_allocValid = t0_allocMask.reduce(_ || _)
  private val t0_allocTable = PriorityEncoder(t0_allocMask)

  // There is one write to spend per event, so the outcomes are exclusive. An entry that was right is strengthened. A
  // wrong one is normally given to a longer history, keeping both contexts represented, but an entry that was never
  // confident, or one no table will take over from, is simply corrected where it stands.
  private val t0_correctInPlace = t0_heldHit && (heldMeta.p1Counter.isWeak || !t0_allocValid)
  private val t0_needStrengthen = heldValid && t0_heldCorrect
  private val t0_needCorrect    = heldValid && !t0_heldCorrect && t0_correctInPlace
  private val t0_needAllocate   = heldValid && !t0_heldCorrect && !t0_correctInPlace && t0_allocValid

  // Count refusals until one eviction is allowed, then start counting again. Clearing the count on an allocation that
  // succeeded normally would be wrong: successes and refusals interleave, so the count would never reach the limit
  // and marked entries would never be reclaimed at all.
  private val t0_allocRefused = heldValid && !t0_heldCorrect && !t0_allocValid

  /* *** letting the marks go ***
   * Count sustained allocation pressure rather than a run of bad luck: up when an allocation found nowhere to go,
   * down when one succeeded. When the count says the tables have been full of marked entries for long enough, drop
   * every mark at once and let the entries that still deserve one earn it again.
   */
  private val usefulResetCnt     = RegInit(0.U(log2Ceil(UsefulResetThreshold + 1).W))
  private val t0_needUsefulReset = t0_fire && t0_allocRefused && usefulResetCnt === (UsefulResetThreshold - 1).U

  when(t0_fire) {
    when(t0_allocRefused) {
      when(t0_needUsefulReset) {
        usefulResetCnt := 0.U
        usefulMarks.foreach(_.foreach(_ := false.B))
      }.otherwise {
        usefulResetCnt := usefulResetCnt + 1.U
      }
    }.elsewhen(t0_needAllocate && usefulResetCnt =/= 0.U) {
      usefulResetCnt := usefulResetCnt - 1.U
    }
  }

  // A mark is earned where an entry was right about a group an alternate would have called differently. It is also
  // lost: an allocation puts a different group in that place, and the mark belongs to the place rather than to the
  // entry now, so leaving it would hand the newcomer a protection it never earned.
  private val t0_markEarned = t0_needStrengthen && heldMeta.providerDiffersFromAlt

  private val t0_writeTable = Mux(t0_needAllocate, t0_allocTable, heldMeta.provider.bits)
  // only a strengthened entry keeps its counter and its standing; the other two install the group afresh
  private val t0_writeFresh = !t0_needStrengthen

  private val t0_writeEntry = Wire(new PtageEntry)
  t0_writeEntry.valid := true.B
  t0_writeEntry.tag   := heldMeta.tag(t0_writeTable)

  t0_writeEntry.p1.cfiPosition := heldCfiPosition
  t0_writeEntry.p1.attribute   := heldAttribute
  t0_writeEntry.p1.nextPcLow   := t0_heldNextPcLow
  t0_writeEntry.p1.counter := Mux(
    t0_writeFresh,
    Mux(heldTaken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
    heldMeta.p1Counter.getUpdate(heldTaken)
  )

  // A group that kept a second block consumed its own successor, so the next training event is the group after it and
  // no continuation can form. The entry's stored second block is what produced that block, so it is put back as it
  // was: clearing it would erase a pair while it is in use. Whether it was right is the backend's to say, through
  // the pair's confidence, so nothing here reinforces it.
  private val t0_heldPairConfirmed = t0_needStrengthen && heldHasSecondBlock && heldMeta.p2Valid

  t0_writeEntry.p2Valid        := t0_continuesHeld || t0_heldPairConfirmed
  t0_writeEntry.p2.cfiPosition := Mux(t0_heldPairConfirmed, heldMeta.p2CfiPosition, t0_branch.cfiPosition)
  t0_writeEntry.p2.attribute   := Mux(t0_heldPairConfirmed, heldMeta.p2Attribute, t0_branch.attribute)
  t0_writeEntry.p2.nextPcLow   := Mux(t0_heldPairConfirmed, heldMeta.p2NextPcLow, getEntryNextPc(t0_nextPc))
  t0_writeEntry.p2.counter := Mux(
    t0_heldPairConfirmed,
    heldMeta.p2Counter,
    Mux(
      t0_writeFresh || !heldMeta.p2Valid,
      Mux(t0_branch.taken, PtageCounter.WeakPositive, PtageCounter.WeakNegative),
      heldMeta.p2Counter.getUpdate(t0_branch.taken)
    )
  )

  private val t0_needWrite = t0_fire && (t0_needStrengthen || t0_needCorrect || t0_needAllocate)
  // the held group was decided on this event, so what t1 writes is captured now
  private val t0_heldFire = t0_fire && heldValid

  // A pair that was held back is judged by what s3 predicted next: the entry stays right about its first block and
  // the next group is exactly the second block it stores. Anything else, a new first block or a different successor,
  // starts the count again. A pair that was put out keeps whatever the backend has made of it.
  private val t0_successorMatches = t0_heldHit && heldMeta.p2Valid &&
    heldMeta.p2CfiPosition === t0_branch.cfiPosition &&
    heldMeta.p2Attribute.asUInt === t0_branch.attribute.asUInt &&
    heldMeta.p2NextPcLow === getEntryNextPc(t0_nextPc)
  private val t0_pairConf =
    Mux(t0_needStrengthen && t0_continuesHeld && t0_successorMatches, confInc(heldMeta.p2Conf), 0.U)

  /* *** t1: write the entry, its mark and its pair confidence ***
   * The write buffer inside a bank is the drain stage: it takes the request now and lands it on a cycle whose bank is
   * not busy serving a prediction.
   */
  private val t1_needWrite      = RegNext(t0_needWrite, false.B)
  private val t1_needAllocate   = RegNext(t0_needAllocate, false.B)
  private val t1_markEarned     = RegNext(t0_markEarned, false.B)
  private val t1_needPairConf   = RegNext(t0_needWrite && !t0_heldPairConfirmed, false.B)
  private val t1_pairConf       = RegEnable(t0_pairConf, t0_heldFire)
  private val t1_writeTable     = RegEnable(t0_writeTable, t0_heldFire)
  private val t1_writeBank      = RegEnable(heldMeta.bankIdx, t0_heldFire)
  private val t1_writeSetIdx    = RegEnable(heldMeta.setIdx(t0_writeTable), t0_heldFire)
  private val t1_writeEntry     = RegEnable(t0_writeEntry, t0_heldFire)
  private val t1_writeEntryAddr = usefulAddr(t1_writeBank, t1_writeSetIdx)

  banks.zipWithIndex.foreach { case (tableBanks, t) =>
    tableBanks.zipWithIndex.foreach { case (bank, b) =>
      bank.io.writeReq.valid       := t1_needWrite && t1_writeTable === t.U && t1_writeBank === b.U
      bank.io.writeReq.bits.setIdx := t1_writeSetIdx
      bank.io.writeReq.bits.entry  := t1_writeEntry
    }
  }

  when(t1_needWrite) {
    when(t1_needAllocate) {
      usefulMarks(t1_writeTable)(t1_writeEntryAddr) := false.B
    }.elsewhen(t1_markEarned) {
      usefulMarks(t1_writeTable)(t1_writeEntryAddr) := true.B
    }
  }

  when(t1_needWrite && t1_needPairConf) {
    pairConf(t1_writeTable)(t1_writeEntryAddr) := t1_pairConf
  }
  // the backend's verdict on a second block that was put out wins over a pairing update to the same entry
  private val pairOutcomeAddr = usefulAddr(io.pairOutcome.bits.bankIdx, io.pairOutcome.bits.setIdx)
  when(io.pairOutcome.valid) {
    pairConf(io.pairOutcome.bits.table)(pairOutcomeAddr) :=
      Mux(io.pairOutcomeCorrect, confInc(pairConf(io.pairOutcome.bits.table)(pairOutcomeAddr)), 0.U)
  }

  XSPerfAccumulate("predHit", s1_fire && s1_p1Usable)
  XSPerfAccumulate("predMiss", s1_fire && !s1_p1Usable)
  XSPerfAccumulate("lookupInvalid", s1_fire && !s1_valid)
  Seq.tabulate(NumTables)(t =>
    XSPerfAccumulate(s"providerTable$t", s1_fire && s1_providerOH(t))
  )

  // how many pairs were on offer, how many were sure enough to be put out, and how sure they were
  private val perf_offered = s1_fire && s1_p2Offered
  XSPerfAccumulate("pairOffered", perf_offered)
  XSPerfAccumulate("pairEmitted", s1_fire && s1_p2Usable)
  XSPerfHistogram("pairConfAtOffer", s1_providerConf, perf_offered, 0, PairConfMax + 1)
  XSPerfAccumulate("pairConfRaisedByPairing", t1_needWrite && t1_needPairConf && t1_pairConf =/= 0.U)
  XSPerfAccumulate("pairConfClearedByPairing", t1_needWrite && t1_needPairConf && t1_pairConf === 0.U)
  XSPerfAccumulate("pairConfRaisedByBackend", io.pairOutcome.valid && io.pairOutcomeCorrect)
  XSPerfAccumulate("pairConfClearedByBackend", io.pairOutcome.valid && !io.pairOutcomeCorrect)

  XSPerfAccumulate("trainEvent", t0_fire)
  XSPerfAccumulate("trainInvalid", io.fastTrain.get.valid && !t0_meta.valid && io.enable)
  XSPerfAccumulate("trainPaired", t0_fire && t0_continuesHeld)
  XSPerfAccumulate("trainStrengthen", t0_fire && t0_needStrengthen)
  XSPerfAccumulate("trainCorrect", t0_fire && t0_needCorrect)
  XSPerfAccumulate("trainAllocate", t0_fire && t0_needAllocate)
  XSPerfAccumulate("trainNoTableFree", t0_fire && t0_allocRefused)
  XSPerfAccumulate("usefulReset", t0_needUsefulReset)

  // Per lookup class: how often a lookup hits, then what training did with it. A class whose lookups go wrong but
  // still train shows up as allocation churn here.
  private def perf_byClass(classOf: UInt): Seq[(String, Bool)] =
    PtageLookupClass.Names.zipWithIndex.map { case (name, c) => (name, classOf === c.U) }
  private val perf_s1Classes = perf_byClass(s1_perfClass)
  XSPerfSeqAccumulate("lookup", s1_fire, perf_s1Classes)
  XSPerfSeqAccumulate("lookupHit", s1_fire && s1_p1Usable, perf_s1Classes)
  // training writes the held group, so it is that group's lookup the counts belong to
  private val perf_t0Classes = perf_byClass(heldMeta.perf_lookupClass)
  XSPerfSeqAccumulate("train", t0_heldFire, perf_t0Classes)
  XSPerfSeqAccumulate("trainStrengthen", t0_heldFire && t0_needStrengthen, perf_t0Classes)
  XSPerfSeqAccumulate("trainCorrect", t0_heldFire && t0_needCorrect, perf_t0Classes)
  XSPerfSeqAccumulate("trainAllocate", t0_heldFire && t0_needAllocate, perf_t0Classes)
  XSPerfSeqAccumulate("trainNoTableFree", t0_heldFire && t0_allocRefused, perf_t0Classes)
}
