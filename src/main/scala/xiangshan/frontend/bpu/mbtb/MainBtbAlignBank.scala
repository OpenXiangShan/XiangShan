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
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfSeqAccumulate
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.Pc
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.StageCtrl

class MainBtbAlignBank(
    alignIdx: Int
)(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbAlignBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        // NOTE: this startPc is not from Bpu top, it's calculated in MainBtb top
        // i.e. (VecInit.tabulate(NumAlignBanks)(startPc + _ * alignSize))(alignIdx) rotated right by startAlignIdx
        val startPc:       GuardedPc = new GuardedPc
        val posHigherBits: UInt      = UInt(AlignBankIdxLen.W)
        val crossPage:     Bool      = Bool()
      }

      class Resp extends Bundle {
        val predictions: Vec[Valid[Prediction]] = Vec(NumWay, Valid(new Prediction))
        val metas:       Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
      }
      // don't need Valid or Decoupled here, AlignBank's pipeline is coupled with top, so we use stageCtrl to control
      val req: Req = Input(new Req)

      val resp: Resp = Output(new Resp)

      val s1_positions: Vec[UInt] = Output(Vec(NumWay, UInt(CfiPositionWidth.W)))
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val needWrite: Bool = Bool()
        // similar to Read.Req.startPc, calculated in MainBtb top
        val startPc:  Pc                     = new Pc
        val branches: Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
        val meta:     Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
        // mispredictBranch is actually Mux1H(branches.map(b => b.valid && b.mispredict), b.bits),
        // but we still pass it through a port anyway,
        // perhaps in the future we can move this Mux1H to prior stages for better timing.
        val mispredictInfo: Valid[BranchInfo] = Valid(new BranchInfo)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val sramResetDone: Bool      = Output(Bool())
    val stageCtrl:     StageCtrl = Input(new StageCtrl)

    val read:  Read                  = new Read
    val write: Write                 = new Write
    val trace: MainBtbAlignBankTrace = Output(new MainBtbAlignBankTrace)

    // final s3_takenMask (mbtb + tage + sc), used to touch replacer accurately
    val s3_takenMask: Vec[Bool] = Input(Vec(NumWay, Bool()))

    // fast path of train pc, used to read replacer in advance for better timing
    val t0_startPc: Pc = Input(new Pc)
  }

  val io: MainBtbAlignBankIO = IO(new MainBtbAlignBankIO)

  // alias
  private val r = io.read
  private val w = io.write

  private val internalBanks = Seq.tabulate(NumInternalBanks) { bankIdx =>
    Module(new MainBtbInternalBank(alignIdx, bankIdx))
  }

  private val replacer = Module(new MainBtbReplacer)

  io.sramResetDone := internalBanks.map(_.io.sramResetDone).reduce(_ && _)

  /* *** s0 ***
   * send read req to internal banks (srams)
   */
  private val s0_fire             = io.stageCtrl.s0_fire
  private val s0_startPc          = r.req.startPc
  private val s0_posHigherBits    = r.req.posHigherBits
  private val s0_crossPage        = r.req.crossPage
  private val s0_setIdx           = getSetIndex(s0_startPc)
  private val s0_internalBankIdx  = getInternalBankIndex(s0_startPc)
  private val s0_internalBankMask = UIntToOH(s0_internalBankIdx, NumInternalBanks)
  private val s0_alignBankIdx     = getAlignBankIndex(s0_startPc)

  // mainBtb top is responsible for sending the correct startPc to alignBanks,
  // so here we should always see getAlignBankIndex(s0_startPc) == physical alignIdx.
  assert(!s0_fire || s0_alignBankIdx === alignIdx.U, "MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid       := s0_fire && s0_internalBankMask(i)
    b.io.read.req.bits.setIdx := s0_setIdx
  }

  /* *** s1 ***
   * receive read resp from internal banks
   * select 1 internal bank's resp
   * check entries hit
   * filter-out unneeded entries
   */
  private val s1_fire             = io.stageCtrl.s1_fire
  private val s1_startPc          = RegEnable(s0_startPc, s0_fire)
  private val s1_posHigherBits    = RegEnable(s0_posHigherBits, s0_fire)
  private val s1_crossPage        = RegEnable(s0_crossPage, s0_fire)
  private val s1_internalBankMask = RegEnable(s0_internalBankMask, s0_fire)

  private val s1_rawEntries = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.entries)
  )
  private val s1_rawCounters = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.counters)
  )

  private val s1_tag = getTag(s1_startPc)

  // send rawHit for training
  private val s1_rawHitMask = VecInit(s1_rawEntries.map(e => e.valid && e.tag === s1_tag))

  // NOTE: when we calculate startPc in MainBtb top, we have selected whether lower bits should be masked
  //       (see s0_startPcVec)
  //       so here, if this alignBank is not the first alignBank of the fetch block, we'll get s1_alignedInstOffset = 0
  //       and, we'll do a (e.position >= 0) check later, which is always true
  private val s1_alignedInstOffset = getAlignedInstOffset(s1_startPc)

  private val s1_predictions = VecInit((s1_rawEntries zip s1_rawCounters zip s1_rawHitMask).map {
    case ((e, c), rawHit) =>
      val pred = Wire(Valid(new Prediction))
      // filter out branches before alignedInstOffset
      // also filter out all entries if crossPage to satisfy Ifu/ICache's requirement
      val hit = rawHit && e.position >= s1_alignedInstOffset && !s1_crossPage
      pred.valid            := hit
      pred.bits.cfiPosition := Cat(s1_posHigherBits, e.position)
      pred.bits.target      := getFullTarget(s1_startPc, e.targetLowerBits, e.targetCarry)
      pred.bits.attribute   := e.attribute
      pred.bits.taken       := c.isPositive
      pred
  })

  io.read.s1_positions := s1_predictions.map(_.bits.cfiPosition)

  /* *** s2 ***
   * send resp to top
   * generate metadata for training
   */
  private val s2_fire             = io.stageCtrl.s2_fire
  private val s2_startPc          = RegEnable(s1_startPc, s1_fire)
  private val s2_internalBankMask = RegEnable(s1_internalBankMask, s1_fire)
  private val s2_rawCounters      = RegEnable(s1_rawCounters, s1_fire)
  private val s2_rawHitMask       = RegEnable(s1_rawHitMask, s1_fire)
  private val s2_predictions      = RegEnable(s1_predictions, s1_fire)

  private val s2_setIdx = getSetIndex(s2_startPc)

  // send resp
  r.resp.predictions := s2_predictions

  r.resp.metas.zipWithIndex.foreach { case (meta, i) =>
    meta.rawHit    := s2_rawHitMask(i)
    meta.attribute := s2_predictions(i).bits.attribute
    meta.position  := s2_predictions(i).bits.cfiPosition
    meta.counter   := s2_rawCounters(i)
  }

  // add an alias for hitMask for later use & debug purpose
  private val s2_hitMask = VecInit(r.resp.predictions.map(_.valid))
  dontTouch(s2_hitMask)

  /* *** s3 ***
   * touch replacer using final takenMask (mbtb + tage + sc)
   */
  private val s3_fire           = io.stageCtrl.s3_fire
  private val s3_replacerSetIdx = RegEnable(getReplacerSetIndex(s2_startPc), s2_fire)
  private val s3_takenMask      = io.s3_takenMask

  /* *** t0 ***
   * read replacer in advance for better timing
   */
  private val t0_fire    = io.stageCtrl.t0_fire
  private val t0_startPc = io.t0_startPc

  replacer.io.train.t0_setIdx := getReplacerSetIndex(t0_startPc)
  replacer.io.train.t0_fire   := t0_fire

  private val t0_victimMask = replacer.io.train.t0_victim

  /* *** t1 ***
   * send write req to internal banks (srams)
   */
  private val t1_fire             = w.req.valid
  private val t1_needWrite        = w.req.bits.needWrite
  private val t1_startPc          = w.req.bits.startPc
  private val t1_branches         = w.req.bits.branches
  private val t1_meta             = w.req.bits.meta
  private val t1_mispredictInfo   = w.req.bits.mispredictInfo
  private val t1_setIdx           = getSetIndex(t1_startPc)
  private val t1_internalBankIdx  = getInternalBankIndex(t1_startPc)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx, NumInternalBanks)
  private val t1_alignBankIdx     = getAlignBankIndex(t1_startPc)
  private val t1_victimMask       = RegEnable(t0_victimMask, t0_fire)

  /* *** update entry *** */
  // NOTE: the original rawHit result can be multi-hit (i.e. multiple rawHit && position match), so PriorityEncoderOH
  private val t1_hitMask = PriorityEncoderOH(VecInit(t1_meta.map(_.hit(t1_mispredictInfo.bits))).asUInt)
  private val t1_hit     = t1_hitMask.orR

  // Write entry only when there's a mispredict, and if:
  private val t1_entryNeedWrite = t1_needWrite && t1_mispredictInfo.valid && (
    // 1. not hit, always write a new entry, use mbtb replacer's victim way.
    !t1_hit ||
      // 2. hit, do write only if:
      //   a. it's an OtherIndirect-type branch (to update target and play the role of Ittage's base table).
      t1_mispredictInfo.bits.attribute.needIttage ||
      //   b. attribute changed, probably indicating a software self-modification.
      t1_mispredictInfo.bits.attribute =/= Mux1H(t1_hitMask, t1_meta.map(_.attribute))
  )
  // Use hit wayMask if hit, else use replacer's victim way
  private val t1_entryWayMask = Mux(t1_hit, t1_hitMask, t1_victimMask)

  private val t1_entry = Wire(new MainBtbEntry)
  t1_entry.tag             := getTag(t1_startPc)
  t1_entry.attribute       := t1_mispredictInfo.bits.attribute
  t1_entry.position        := t1_mispredictInfo.bits.cfiPosition
  t1_entry.targetLowerBits := getTargetLowerBits(t1_mispredictInfo.bits.target)
  t1_entry.targetCarry.foreach(_ := getTargetCarry(t1_startPc, t1_mispredictInfo.bits.target))

  // similar to s0 case
  assert(!t1_fire || t1_alignBankIdx === alignIdx.U, "MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeEntry.req.valid        := t1_fire && t1_entryNeedWrite && t1_internalBankMask(i)
    b.io.writeEntry.req.bits.setIdx  := t1_setIdx
    b.io.writeEntry.req.bits.wayMask := t1_entryWayMask
    b.io.writeEntry.req.bits.entry   := t1_entry
  }

  /* *** update counter *** */
  private val t1_newCounters    = Wire(Vec(NumWay, TakenCounter()))
  private val t1_counterWayMask = Wire(Vec(NumWay, Bool()))

  t1_meta.zipWithIndex.foreach { case (meta, i) =>
    val hitMask = t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && meta.position === branch.bits.cfiPosition
    }
    val actualTaken = Mux1H(hitMask, t1_branches.map(_.bits.taken))

    val entryOverridden = t1_entryNeedWrite && t1_entryWayMask(i)

    t1_counterWayMask(i) := entryOverridden || hitMask.reduce(_ || _)
    t1_newCounters(i)    := Mux(entryOverridden, TakenCounter.WeakPositive, meta.counter.getUpdate(actualTaken))
  }
  private val t1_actualTakenMask = VecInit(t1_meta.zipWithIndex.map { case (meta, i) =>
    val hitMask = t1_branches.map(branch =>
      branch.valid && meta.position === branch.bits.cfiPosition && meta.rawHit && branch.bits.taken
    )
    hitMask.reduce(_ || _)
  })
  private val t1_actualTakenOH = PriorityEncoderOH(t1_actualTakenMask.asUInt)

  private val t1_replacerSetIdx = getReplacerSetIndex(t1_startPc)
  // update replacer -- Allocation touch and training touch are co-timed and share the same replacer interface.
  replacer.io.train.t1_touch.valid        := t1_fire && (t1_entryNeedWrite || t1_actualTakenMask.reduce(_ || _))
  replacer.io.train.t1_touch.bits.setIdx  := t1_replacerSetIdx
  replacer.io.train.t1_touch.bits.wayMask := Mux(t1_entryNeedWrite, t1_entryWayMask, t1_actualTakenOH)

  // write counter anytime when needed
  private val t1_counterNeedWrite = t1_counterWayMask.reduce(_ || _)

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeCounter.req.valid         := t1_fire && t1_counterNeedWrite && t1_internalBankMask(i)
    b.io.writeCounter.req.bits.setIdx   := t1_setIdx
    b.io.writeCounter.req.bits.wayMask  := t1_counterWayMask.asUInt
    b.io.writeCounter.req.bits.counters := t1_newCounters
  }

  /* *** multi-hit detection & flush *** */
  private val s2_multiHitMask = detectMultiHit(s2_hitMask, s2_predictions.map(_.bits.cfiPosition))

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := s2_fire && s2_multiHitMask.orR && s2_internalBankMask(i)
    b.io.flush.req.bits.setIdx  := s2_setIdx
    b.io.flush.req.bits.wayMask := s2_multiHitMask
  }

  // mainBTB trace bundle
  io.trace.needWrite := t1_fire && t1_entryNeedWrite
  io.trace.setIdx    := t1_setIdx
  io.trace.bankIdx   := t1_internalBankIdx
  io.trace.wayIdx    := PriorityEncoder(t1_entryWayMask.asUInt)
  io.trace.entry     := t1_entry
  XSPerfHistogram("multihit_count", PopCount(s2_multiHitMask), s2_fire, 0, NumWay)

  XSPerfSeqAccumulate(
    "", // no common prefix is needed
    t1_fire && t1_mispredictInfo.valid,
    Seq(
      ("allocate", t1_entryNeedWrite),
      ("fixTarget", t1_hit && t1_mispredictInfo.bits.attribute.needIttage),
      ("fixAttribute", t1_hit && !(t1_mispredictInfo.bits.attribute === Mux1H(t1_hitMask, t1_meta.map(_.attribute))))
    )
  )

  XSPerfAccumulate("updateCounter", Mux(t1_fire, PopCount(t1_counterWayMask), 0.U))
}
