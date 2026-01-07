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
import xiangshan.frontend.PrunedAddr
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
        val startPc:       PrunedAddr = new PrunedAddr(VAddrBits)
        val posHigherBits: UInt       = UInt(AlignBankIdxLen.W)
        val crossPage:     Bool       = Bool()
      }

      class Resp extends Bundle {
        val predictions: Vec[Valid[Prediction]] = Vec(NumWay, Valid(new Prediction))
        val metas:       Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
      }

      // don't need Valid or Decoupled here, AlignBank's pipeline is coupled with top, so we use stageCtrl to control
      val req: Req = Input(new Req)

      val resp: Resp = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        // similar to Read.Req.startPc, calculated in MainBtb top
        val startPc:  PrunedAddr             = new PrunedAddr(VAddrBits)
        val branches: Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
        val meta:     Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
        // mispredictBranch is actually Mux1H(branches.map(b => b.valid && b.mispredict), b.bits),
        // but we still pass it through a port anyway,
        // perhaps in the future we can move this Mux1H to prior stages for better timing.
        val mispredictInfo: Valid[BranchInfo] = Valid(new BranchInfo)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val resetDone: Bool      = Output(Bool())
    val stageCtrl: StageCtrl = Input(new StageCtrl)

    val read:  Read  = new Read
    val write: Write = new Write
  }

  val io: MainBtbAlignBankIO = IO(new MainBtbAlignBankIO)

  // alias
  private val r = io.read
  private val w = io.write

  private val internalBanks = Seq.tabulate(NumInternalBanks) { bankIdx =>
    Module(new MainBtbInternalBank(alignIdx, bankIdx))
  }

  private val replacer = Module(new MainBtbReplacer)

  io.resetDone := internalBanks.map(_.io.resetDone).reduce(_ && _)

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
    // NOTE: if crossPage, we need to drop the entries to satisfy Ifu/ICache's requirement,
    //       so we also can drop read req to save power.
    // FIXME: but this might be timing critical, need to be verified.
    b.io.read.req.valid       := s0_fire && s0_internalBankMask(i) && !s0_crossPage
    b.io.read.req.bits.setIdx := s0_setIdx
  }

  /* *** s1 ***
   * receive read resp from internal banks
   * select 1 internal bank's resp
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

  /* *** s2 ***
   * check entries hit
   * filter-out unneeded entries
   * send resp to top
   */
  private val s2_fire             = io.stageCtrl.s2_fire
  private val s2_startPc          = RegEnable(s1_startPc, s1_fire)
  private val s2_posHigherBits    = RegEnable(s1_posHigherBits, s1_fire)
  private val s2_crossPage        = RegEnable(s1_crossPage, s1_fire)
  private val s2_internalBankMask = RegEnable(s1_internalBankMask, s1_fire)
  private val s2_rawEntries       = RegEnable(s1_rawEntries, s1_fire)
  private val s2_rawCounters      = RegEnable(s1_rawCounters, s1_fire)

  private val s2_setIdx = getSetIndex(s2_startPc)
  private val s2_tag    = getTag(s2_startPc)

  // NOTE: when we calculate startPc in MainBtb top, we have selected whether lower bits should be masked
  //       (see s0_startPcVec)
  //       so here, if this alignBank is not the first alignBank of the fetch block, we'll get s2_alignedInstOffset = 0
  //       and, we'll do a (e.position >= 0) check later, which is always true
  private val s2_alignedInstOffset = getAlignedInstOffset(s2_startPc)

  // send resp
  (r.resp.predictions zip r.resp.metas zip s2_rawEntries zip s2_rawCounters).foreach { case (((pred, meta), e), c) =>
    // send rawHit for training
    val rawHit = e.valid && e.tag === s2_tag
    // filter out branches before alignedInstOffset
    // also filter out all entries if crossPage to satisfy Ifu/ICache's requirement
    val hit = rawHit && e.position >= s2_alignedInstOffset && !s2_crossPage
    pred.valid            := hit
    pred.bits.cfiPosition := Cat(s2_posHigherBits, e.position)
    pred.bits.target      := getFullTarget(s2_startPc, e.targetLowerBits, Some(e.targetCarry))
    pred.bits.attribute   := e.attribute
    pred.bits.taken       := c.isPositive

    meta.rawHit    := rawHit
    meta.attribute := e.attribute
    meta.position  := Cat(s2_posHigherBits, e.position)
    meta.counter   := c
  }

  // add an alias for hitMask for later use & debug purpose
  private val s2_hitMask = VecInit(r.resp.predictions.map(_.valid))
  dontTouch(s2_hitMask)

  // update replacer
  /* touch taken entries only: not-taken conditional entries are considered not very useful and should be killed first
   * TODO: As tage/sc results have worse timing and more complexity, here we use baseTable (in mbtb) only,
   *       hopefully this is enough for replacer updates.
   */
  private val s2_takenMask = VecInit(r.resp.predictions.map { pred =>
    pred.valid && (
      pred.bits.attribute.isConditional && pred.bits.taken ||
        pred.bits.attribute.isDirect ||
        pred.bits.attribute.isIndirect
    )
  })

  replacer.io.predictTouch.valid        := s2_fire && s2_takenMask.reduce(_ || _)
  replacer.io.predictTouch.bits.setIdx  := getReplacerSetIndex(s2_startPc)
  replacer.io.predictTouch.bits.wayMask := s2_takenMask.asUInt

  /* *** t1 ***
   * send write req to internal banks (srams)
   */
  private val t1_fire             = w.req.valid
  private val t1_startPc          = w.req.bits.startPc
  private val t1_branches         = w.req.bits.branches
  private val t1_meta             = w.req.bits.meta
  private val t1_mispredictInfo   = w.req.bits.mispredictInfo
  private val t1_setIdx           = getSetIndex(t1_startPc)
  private val t1_internalBankIdx  = getInternalBankIndex(t1_startPc)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx, NumInternalBanks)
  private val t1_alignBankIdx     = getAlignBankIndex(t1_startPc)

  /* *** update entry *** */
  // NOTE: the original rawHit result can be multi-hit (i.e. multiple rawHit && position match), so PriorityEncoderOH
  private val t1_hitMask = PriorityEncoderOH(VecInit(t1_meta.map(_.hit(t1_mispredictInfo.bits))).asUInt)
  private val t1_hit     = t1_hitMask.orR

  // Write entry only when there's a mispredict, and if:
  private val t1_entryNeedWrite = t1_mispredictInfo.valid && (
    // 1. not hit, always write a new entry, use mbtb replacer's victim way.
    !t1_hit ||
      // 2. hit, do write only if:
      //   a. it's an OtherIndirect-type branch (to update target and play the role of Ittage's base table).
      t1_mispredictInfo.bits.attribute.needIttage ||
      //   b. attribute changed, probably indicating a software self-modification.
      !(t1_mispredictInfo.bits.attribute === Mux1H(t1_hitMask, t1_meta.map(_.attribute)))
  )
  // Use hit wayMask if hit, else use replacer's victim way
  private val t1_entryWayMask = Mux(t1_hit, t1_hitMask, replacer.io.victim.wayMask)

  private val t1_entry = Wire(new MainBtbEntry)
  t1_entry.valid           := true.B
  t1_entry.tag             := getTag(t1_startPc)
  t1_entry.position        := t1_mispredictInfo.bits.cfiPosition
  t1_entry.targetLowerBits := getTargetLowerBits(t1_mispredictInfo.bits.target)
  t1_entry.targetCarry     := getTargetCarry(t1_startPc, t1_mispredictInfo.bits.target)
  t1_entry.attribute       := t1_mispredictInfo.bits.attribute

  // similar to s0 case
  assert(!t1_fire || t1_alignBankIdx === alignIdx.U, "MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeEntry.req.valid        := t1_fire && t1_entryNeedWrite && t1_internalBankMask(i)
    b.io.writeEntry.req.bits.setIdx  := t1_setIdx
    b.io.writeEntry.req.bits.wayMask := t1_entryWayMask
    b.io.writeEntry.req.bits.entry   := t1_entry
  }

  // update replacer
  replacer.io.trainTouch.valid        := t1_fire && t1_entryNeedWrite
  replacer.io.trainTouch.bits.setIdx  := getReplacerSetIndex(t1_startPc)
  replacer.io.trainTouch.bits.wayMask := t1_entryWayMask

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

  // write counter anytime when needed
  private val t1_counterNeedWrite = t1_counterWayMask.reduce(_ || _)

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeCounter.req.valid         := t1_fire && t1_counterNeedWrite && t1_internalBankMask(i)
    b.io.writeCounter.req.bits.setIdx   := t1_setIdx
    b.io.writeCounter.req.bits.wayMask  := t1_counterWayMask.asUInt
    b.io.writeCounter.req.bits.counters := t1_newCounters
  }

  /* *** multi-hit detection & flush *** */
  private val s2_multiHitMask = detectMultiHit(s2_hitMask, VecInit(s2_rawEntries.map(_.position)))

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := s2_fire && s2_multiHitMask.orR && s2_internalBankMask(i)
    b.io.flush.req.bits.setIdx  := s2_setIdx
    b.io.flush.req.bits.wayMask := s2_multiHitMask
  }

  XSPerfHistogram("multihit_count", PopCount(s2_multiHitMask), s2_fire, 0, NumWay)

  XSPerfAccumulate(
    "", // no common prefix is needed
    t1_fire && t1_mispredictInfo.valid,
    Seq(
      ("allocate", !t1_hit),
      ("fixTarget", t1_hit && t1_mispredictInfo.bits.attribute.needIttage),
      ("fixAttribute", t1_hit && !(t1_mispredictInfo.bits.attribute === Mux1H(t1_hitMask, t1_meta.map(_.attribute))))
    )
  )

  XSPerfAccumulate("updateCounter", Mux(t1_fire, PopCount(t1_counterWayMask), 0.U))
}
