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
        // s1 response
        val positions: Vec[UInt] = Vec(NumWay, UInt(CfiPositionWidth.W))
        // s2 response
        val predictions: Vec[Valid[Prediction]] = Vec(NumWay, Valid(new Prediction))
        val metas:       Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
      }
      // don't need Valid or Decoupled here, AlignBank's pipeline is coupled with top, so we use stageCtrl to control
      val req: Req = Input(new Req)

      val mbtbResp: Resp = Output(new Resp)
      val vbtbResp: Resp = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val needWrite: Bool = Bool()
        // similar to Read.Req.startPc, calculated in MainBtb top
        val startPc:       Pc                     = new Pc
        val branches:      Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
        val meta:          Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
        val posHigherBits: UInt                   = UInt(AlignBankIdxLen.W)
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
    val s3_takenMask:     Vec[Bool] = Input(Vec(NumWay, Bool()))
    val s3_vbtbTakenMask: Vec[Bool] = Input(Vec(NumWay, Bool()))

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

  // Each InternalBank owns one VBTB and one VBTB replacer. The VBTB catches
  // valid MainBtb entries evicted by miss allocation, reducing effective BTB
  // misses caused by limited MainBtb associativity.
  private val victimBtbs         = Seq.tabulate(NumInternalBanks)(_ => Module(new VictimBtb))
  private val victimBtbReplacers = Seq.tabulate(NumInternalBanks)(_ => Module(new VictimBtbReplacer))

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
   */
  private val s1_fire              = io.stageCtrl.s1_fire
  private val s1_startPc           = RegEnable(s0_startPc, s0_fire)
  private val s1_posHigherBits     = RegEnable(s0_posHigherBits, s0_fire)
  private val s1_crossPage         = RegEnable(s0_crossPage, s0_fire)
  private val s1_internalBankMask  = RegEnable(s0_internalBankMask, s0_fire)
  private val s1_tag               = getTag(s1_startPc)
  private val s1_setIdx            = getSetIndex(s1_startPc)
  private val s1_alignedInstOffset = getAlignedInstOffset(s1_startPc)

  private val s1_rawEntries = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.entries)
  )
  private val s1_rawCounters = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.counters)
  )
  private val s1_vbtbEntries = Mux1H(
    s1_internalBankMask,
    victimBtbs.map(_.io.read.resp.entries)
  )

  private val s1_vbtbRawHitMask = VecInit(s1_vbtbEntries.map { e =>
    e.entry.valid && e.setIdx === s1_setIdx && e.entry.tag === s1_tag // TODO: optimize this
  })
  private val s1_vbtbHitMask = VecInit((s1_vbtbRawHitMask zip s1_vbtbEntries).map { case (raw, e) =>
    raw && e.entry.position >= s1_alignedInstOffset && !s1_crossPage
  })

  io.read.mbtbResp.positions := VecInit(s1_rawEntries.map(e => Cat(s1_posHigherBits, e.position)))
  io.read.vbtbResp.positions := VecInit(s1_vbtbEntries.map(e => Cat(s1_posHigherBits, e.entry.position)))

  victimBtbs.foreach(b => b.io.read.req.setIdx := s1_setIdx.take(VictimBtbSetIdxLen))

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
  private val s2_vbtbEntries      = RegEnable(s1_vbtbEntries, s1_fire)
  private val s2_vbtbRawHitMask   = RegEnable(s1_vbtbRawHitMask, s1_fire)
  private val s2_vbtbHitMask      = RegEnable(s1_vbtbHitMask, s1_fire)

  private val s2_setIdx = getSetIndex(s2_startPc)
  private val s2_tag    = getTag(s2_startPc)

  // NOTE: when we calculate startPc in MainBtb top, we have selected whether lower bits should be masked
  //       (see s0_startPcVec)
  //       so here, if this alignBank is not the first alignBank of the fetch block, we'll get s2_alignedInstOffset = 0
  //       and, we'll do a (e.position >= 0) check later, which is always true
  private val s2_alignedInstOffset = getAlignedInstOffset(s2_startPc)

  // send resp
  (r.mbtbResp.predictions zip r.mbtbResp.metas zip s2_rawEntries zip s2_rawCounters).foreach {
    case (((pred, meta), e), c) =>
      // send rawHit for training
      val rawHit = e.valid && e.tag === s2_tag
      // filter out branches before alignedInstOffset
      // also filter out all entries if crossPage to satisfy Ifu/ICache's requirement
      val hit = rawHit && e.position >= s2_alignedInstOffset && !s2_crossPage
      pred.valid            := hit
      pred.bits.cfiPosition := Cat(s2_posHigherBits, e.position)
      pred.bits.target      := getFullTarget(s2_startPc, e.targetLowerBits, e.targetCarry)
      pred.bits.attribute   := e.attribute
      pred.bits.taken       := c.isPositive

      meta.rawHit    := rawHit
      meta.attribute := e.attribute
      meta.position  := Cat(s2_posHigherBits, e.position)
      meta.counter   := c
  }
  (r.vbtbResp.predictions zip r.vbtbResp.metas).zipWithIndex.foreach {
    case ((pred, meta), i) =>
      // send rawHit for training
      val rawHit = s2_vbtbRawHitMask(i)
      val hit    = s2_vbtbHitMask(i)
      val e      = s2_vbtbEntries(i)
      pred.valid            := hit
      pred.bits.cfiPosition := Cat(s2_posHigherBits, e.entry.position)
      pred.bits.target      := getFullTarget(s2_startPc, e.entry.targetLowerBits, e.entry.targetCarry)
      pred.bits.attribute   := e.entry.attribute
      pred.bits.taken       := e.counter.isPositive

      meta.rawHit    := rawHit
      meta.attribute := e.entry.attribute
      meta.position  := Cat(s2_posHigherBits, e.entry.position)
      meta.counter   := e.counter
  }

  // add an alias for hitMask for later use & debug purpose
  private val s2_hitMask = VecInit(r.mbtbResp.predictions.map(_.valid))
  dontTouch(s2_hitMask)
  dontTouch(s2_vbtbHitMask)

  /* *** s3 ***
   * touch replacer using final takenMask (mbtb + tage + sc)
   */
  private val s3_fire             = io.stageCtrl.s3_fire
  private val s3_internalBankMask = RegEnable(s2_internalBankMask, s2_fire)
  private val s3_setIdx           = RegEnable(s2_setIdx, s2_fire)
  private val s3_replacerSetIdx   = RegEnable(getReplacerSetIndex(s2_startPc), s2_fire)
  private val s3_takenMask        = io.s3_takenMask
  private val s3_vbtbTakenMask    = io.s3_vbtbTakenMask

  victimBtbReplacers.zipWithIndex.foreach { case (r, i) =>
    r.io.predictTouch.valid        := s3_fire && s3_internalBankMask(i) && s3_vbtbTakenMask.reduce(_ || _)
    r.io.predictTouch.bits.setIdx  := s3_setIdx.take(VictimBtbSetIdxLen)
    r.io.predictTouch.bits.wayMask := s3_vbtbTakenMask.asUInt
  }

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
  private val t1_posHigherBits    = w.req.bits.posHigherBits
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

  private val t1_vbtbEntries = Mux1H(t1_internalBankMask, victimBtbs.map(_.io.trainEntryRead.resp.entries))
  private val t1_vbtbHitMask = PriorityEncoderOH(VecInit(t1_vbtbEntries.map { e =>
    e.setIdx === t1_setIdx &&
    e.entry.valid && e.entry.tag === getTag(t1_startPc) && // TODO: optimize this
    Cat(t1_posHigherBits, e.entry.position) === t1_mispredictInfo.bits.cfiPosition
  }).asUInt)
  private val t1_vbtbHit = t1_vbtbHitMask.orR

  // Write entry only when there's a mispredict, and if:
  private val t1_entryNeedWrite = t1_needWrite && !t1_vbtbHit && t1_mispredictInfo.valid && (
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
    b.io.writeEntry.req.bits.hit     := t1_hit
  }

  // write vbtb entry
  private val t1_vbtbEntryNeedWrite = t1_needWrite && t1_vbtbHit && t1_mispredictInfo.valid && (
    t1_mispredictInfo.bits.attribute.needIttage ||
      !(t1_mispredictInfo.bits.attribute === Mux1H(t1_vbtbHitMask, t1_vbtbEntries.map(_.entry.attribute)))
  )
  private val t1_vbtbEntryWayMask = t1_vbtbHitMask

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

  // update vbtb counters
  private val t1_vbtbNewCounters      = Wire(Vec(NumWay, TakenCounter()))
  private val t1_vbtbCounterWayMask   = Wire(Vec(NumWay, Bool()))
  private val t1_vbtbCounterNeedWrite = t1_vbtbCounterWayMask.reduce(_ || _)

  t1_vbtbEntries.zipWithIndex.foreach { case (e, i) =>
    val hitMask = t1_branches.map { branch =>
      e.setIdx === t1_setIdx &&
      e.entry.valid && e.entry.tag === getTag(t1_startPc) && // TODO: optimize this
      branch.valid && branch.bits.attribute.isConditional &&
      Cat(t1_posHigherBits, e.entry.position) === branch.bits.cfiPosition
    }
    val actualTaken     = Mux1H(hitMask, t1_branches.map(_.bits.taken))
    val entryOverridden = t1_vbtbEntryNeedWrite && t1_vbtbEntryWayMask(i)
    t1_vbtbCounterWayMask(i) := hitMask.reduce(_ || _) || entryOverridden
    t1_vbtbNewCounters(i)    := Mux(entryOverridden, TakenCounter.WeakPositive, e.counter.getUpdate(actualTaken))
  }

  /* *** victim btb train *** */
  (victimBtbs zip victimBtbReplacers zip internalBanks).zipWithIndex.foreach {
    case (((vbtb, vbtbReplacer), bank), wayIdx) =>
      // train entry
      val t1_victimNeedTrain = t1_vbtbEntryNeedWrite || t1_vbtbCounterNeedWrite
      vbtb.io.trainEntry.req.valid             := t1_fire && t1_internalBankMask(wayIdx) && t1_victimNeedTrain
      vbtb.io.trainEntry.req.bits.setIdx       := t1_setIdx.take(VictimBtbSetIdxLen)
      vbtb.io.trainEntry.req.bits.entryWayMask := Mux(t1_vbtbEntryNeedWrite, t1_vbtbEntryWayMask, 0.U(NumWay.W))
      vbtb.io.trainEntry.req.bits.counterWayMask := Mux(
        t1_vbtbCounterNeedWrite,
        t1_vbtbCounterWayMask.asUInt,
        0.U(NumWay.W)
      )
      vbtb.io.trainEntry.req.bits.entry    := t1_entry
      vbtb.io.trainEntry.req.bits.counters := t1_vbtbNewCounters

      vbtb.io.trainEntryRead.req.setIdx := t1_setIdx.take(VictimBtbSetIdxLen)

      // Snapshot write path:
      // - evicted is the old MainBtb entry displaced by a miss allocation.
      // - incoming is the new entry written back into MainBtb.
      // The evicted entry is inserted into VBTB unless it is the same logical
      // entry as incoming. Any duplicated incoming copy already in VBTB is flushed.
      val snapshotResp  = bank.io.snapshot.resp
      val victimEntries = vbtb.io.writeEntryRead.resp.entries

      val evictedValid = snapshotResp.bits.evicted.valid
      val evictedHitMask = victimEntries.map { e =>
        evictedValid &&
        e.entry.valid &&
        e.setIdx === snapshotResp.bits.setIdx &&
        e.entry.tag === snapshotResp.bits.evicted.tag && // TODO: optimize this
        e.entry.position === snapshotResp.bits.evicted.position
      }
      val evictedHit = evictedHitMask.reduce(_ || _)

      val incomingValid = snapshotResp.bits.incoming.valid
      val incomingHitMask = victimEntries.map { e =>
        incomingValid &&
        e.entry.valid &&
        e.setIdx === snapshotResp.bits.setIdx &&
        e.entry.tag === snapshotResp.bits.incoming.tag && // TODO: optimize this
        e.entry.position === snapshotResp.bits.incoming.position
      }
      val incomingHit = incomingHitMask.reduce(_ || _)

      // evicted hit incoming only when encountered inflight miss
      val evictedHitIncoming =
        evictedValid && incomingValid &&
          snapshotResp.bits.evicted.tag === snapshotResp.bits.incoming.tag &&
          snapshotResp.bits.evicted.position === snapshotResp.bits.incoming.position

      // Prefer refreshing an existing evicted entry, then reusing the duplicated
      // incoming way, and finally falling back to the VBTB replacer victim way.
      val entryWayMask = PriorityMux(Seq(
        evictedHit  -> VecInit(evictedHitMask).asUInt,
        incomingHit -> VecInit(incomingHitMask).asUInt,
        true.B      -> vbtbReplacer.io.victim.wayMask
      ))

      // write evicted entry when not hit incoming
      val writeEvicted = snapshotResp.valid && evictedValid && !evictedHitIncoming
      // flush duplicated incoming entry
      val flushIncoming = snapshotResp.valid && incomingHit

      vbtb.io.writeEntry.req.valid          := writeEvicted || flushIncoming
      vbtb.io.writeEntry.req.bits.setIdx    := snapshotResp.bits.setIdx
      vbtb.io.writeEntry.req.bits.entry     := snapshotResp.bits.evicted
      vbtb.io.writeEntry.req.bits.wayMask   := Mux(writeEvicted, entryWayMask, 0.U(NumWay.W))
      vbtb.io.writeEntry.req.bits.flushMask := Mux(flushIncoming, VecInit(incomingHitMask).asUInt, 0.U(NumWay.W))

      vbtb.io.writeEntryRead.req.setIdx := snapshotResp.bits.setIdx.take(VictimBtbSetIdxLen)

      vbtbReplacer.io.trainTouch.valid        := writeEvicted
      vbtbReplacer.io.trainTouch.bits.setIdx  := snapshotResp.bits.setIdx.take(VictimBtbSetIdxLen)
      vbtbReplacer.io.trainTouch.bits.wayMask := entryWayMask

      assert(PopCount(VecInit(incomingHitMask)).asUInt <= 1.U, "incoming hit mask should be one-hot")
      assert(PopCount(VecInit(evictedHitMask)).asUInt <= 1.U, "evicted hit mask should be one-hot")
  }

  /* *** multi-hit detection & flush *** */
  private val s2_multiHitMask = detectMultiHit(s2_hitMask, VecInit(s2_rawEntries.map(_.position)))

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := s2_fire && s2_multiHitMask.orR && s2_internalBankMask(i)
    b.io.flush.req.bits.setIdx  := s2_setIdx
    b.io.flush.req.bits.wayMask := s2_multiHitMask
  }

  // Remove VBTB entries that duplicate a MainBtb hit at the same position. Once
  // MainBtb can provide the prediction directly, the victim copy is stale.
  private val s2_vbtbMultiHitMask = Wire(Vec(NumWay, Bool()))
  private val s2_vbtbPositions    = s2_vbtbEntries.map(_.entry.position)
  private val s2_positions        = s2_rawEntries.map(_.position)
  for (i <- 0 until NumWay) {
    val multiHitVec =
      VecInit.tabulate(NumWay)(j => s2_vbtbHitMask(i) && s2_hitMask(j) && s2_vbtbPositions(i) === s2_positions(j))
    s2_vbtbMultiHitMask(i) := multiHitVec.reduce(_ || _)
  }

  victimBtbs.zipWithIndex.foreach { case (vbtb, i) =>
    vbtb.io.flush.req.valid        := s2_fire && s2_vbtbMultiHitMask.reduce(_ || _) && s2_internalBankMask(i)
    vbtb.io.flush.req.bits.setIdx  := s2_setIdx.take(VictimBtbSetIdxLen)
    vbtb.io.flush.req.bits.wayMask := s2_vbtbMultiHitMask.asUInt
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
