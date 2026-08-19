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

  // Training is not built yet, so nothing writes the tables and they stay as the sram reset left them: every lookup
  // misses, and pTAGE reports that honestly through its counters rather than predicting anything.
  banks.flatten.foreach { bank =>
    bank.io.writeReq.valid := false.B
    bank.io.writeReq.bits  := 0.U.asTypeOf(new BankWriteReq)
  }

  /* *** s0: sram data returns, match tags ***
   * The tag compare sits here rather than in s1 so that s1 is left with only a priority select and a decode. Both
   * folded tag histories were captured with the address, so this is an xor of already-registered values.
   */
  private val s0_startPc = RegEnable(a0_startPc, s0_fire)
  private val s0_bankIdx = RegEnable(a0_bankIdx, s0_fire)
  private val s0_setIdx  = RegEnable(a0_setIdx, s0_fire)
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
  private val s1_startPc = RegEnable(io.startPc, s0_fire)
  private val s1_entry   = RegEnable(s0_entry, s0_fire)
  private val s1_hitVec  = RegEnable(s0_hitVec, s0_fire)
  private val s1_setIdx  = RegEnable(s0_setIdx, s0_fire)
  private val s1_tag     = RegEnable(s0_tag, s0_fire)
  private val s1_bankIdx = RegEnable(s0_bankIdx, s0_fire)

  // The longest history that hit provides the prediction and the next longest is its alternative, as in any TAGE:
  // a longer history is a more specific context, so where one matches it is the better answer.
  private def selectLongest(mask: Seq[Bool]): Valid[UInt] = {
    val sel = Wire(Valid(UInt(log2Ceil(NumTables).W)))
    sel.valid := mask.reduce(_ || _)
    sel.bits  := (NumTables - 1).U - PriorityEncoder(mask.reverse)
    sel
  }

  private val s1_provider = selectLongest(s1_hitVec)
  private val s1_alt = selectLongest(s1_hitVec.zipWithIndex.map { case (h, t) => h && !(s1_provider.bits === t.U) })

  private val s1_providerEntry = Mux1H(UIntToOH(s1_provider.bits, NumTables), s1_entry)

  // The first block starts where this group starts; the second starts at the first's next pc, which is why that field
  // is not stored twice.
  private val s1_p1Target = getFullTarget(s1_startPc, s1_providerEntry.p1.nextPcLow, None)
  private val s1_p2Target = getFullTarget(s1_p1Target, s1_providerEntry.p2.nextPcLow, None)

  // A block can only be followed by another if its own target is the one the entry stored: a deferred exit takes its
  // target from elsewhere, so the second block's start would not be where the entry assumed.
  private val s1_p1Usable = s1_provider.valid
  private val s1_p2Usable =
    s1_p1Usable && s1_providerEntry.p2Valid && PtageAttribute.hasStaticTarget(s1_providerEntry.p1.attribute)

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

  io.meta.setIdx    := s1_setIdx
  io.meta.tag       := s1_tag
  io.meta.bankIdx   := s1_bankIdx
  io.meta.hitVec    := s1_hitVec
  io.meta.usefulVec := VecInit(s1_entry.map(_.useful))
  io.meta.provider  := s1_provider
  io.meta.alt       := s1_alt
  io.meta.p1Counter := s1_providerEntry.p1.counter
  io.meta.p2Counter := s1_providerEntry.p2.counter
  io.meta.p2Valid   := s1_providerEntry.p2Valid

  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  XSPerfAccumulate("predHit", s1_fire && s1_p1Usable)
  XSPerfAccumulate("predMiss", s1_fire && !s1_p1Usable)
  XSPerfAccumulate("predTwoBlocks", s1_fire && s1_p2Usable)
  XSPerfAccumulate("predP2SuppressedByAttribute", s1_fire && s1_p1Usable && s1_providerEntry.p2Valid && !s1_p2Usable)
  Seq.tabulate(NumTables)(t =>
    XSPerfAccumulate(s"providerTable$t", s1_fire && s1_provider.valid && s1_provider.bits === t.U)
  )
}
