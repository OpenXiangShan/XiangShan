// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.StageCtrl

class VictimBtb(implicit p: Parameters) extends MainBtbModule with Helpers {
  class VictimBtbIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val startPc:       GuardedPc = new GuardedPc
        val posHigherBits: UInt      = UInt(AlignBankIdxLen.W)
        val crossPage:     Bool      = Bool()
      }

      class Resp extends Bundle {
        val s1_position: UInt = UInt(CfiPositionWidth.W)

        val s2_prediction: Valid[Prediction] = Valid(new Prediction)
        val s2_meta:       MainBtbMetaEntry  = new MainBtbMetaEntry
      }
      val req:  Req  = Input(new Req)
      val resp: Resp = Output(new Resp)
    }

    class Snapshot extends Bundle {
      class Req extends Bundle {
        val setIdx:          UInt         = UInt(SetIdxLen.W)
        val internalBankIdx: UInt         = UInt(InternalBankIdxLen.W)
        val evicted:         MainBtbEntry = new MainBtbEntry
        val incoming:        MainBtbEntry = new MainBtbEntry
      }
      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class TrainEntry extends Bundle {
      class Req extends Bundle {
        val entryWayMask:   UInt                 = UInt(NumVictimBtbWays.W)
        val counterWayMask: UInt                 = UInt(NumVictimBtbWays.W)
        val entry:          MainBtbEntry         = new MainBtbEntry
        val counters:       Vec[SaturateCounter] = Vec(NumVictimBtbWays, TakenCounter())
      }

      val entries: Vec[VictimBtbEntry] = Output(Vec(NumVictimBtbWays, new VictimBtbEntry))
      val req:     Valid[Req]          = Flipped(Valid(new Req))
    }

    val enable:              Bool      = Input(Bool())
    val stageCtrl:           StageCtrl = Input(new StageCtrl)
    val s2_mainBtbHitMask:   Vec[Bool] = Input(Vec(NumWay, Bool()))
    val s2_mainBtbPositions: Vec[UInt] = Input(Vec(NumWay, UInt(CfiAlignedPositionWidth.W)))
    val s3_vbtbTaken:        Bool      = Input(Bool())

    val read:       Read       = new Read
    val snapshot:   Snapshot   = new Snapshot
    val trainEntry: TrainEntry = new TrainEntry
  }
  val io: VictimBtbIO = IO(new VictimBtbIO)

  private val read       = io.read
  private val snapshot   = io.snapshot
  private val trainEntry = io.trainEntry

  private val entries = Reg(Vec(NumVictimBtbWays, new VictimBtbEntry))
  trainEntry.entries := entries

  private val replacer = Module(new VictimBtbReplacer)
  replacer.io.valids := VecInit(entries.map(_.entry.valid))

  private val readRawHitMask    = getVictimBtbRawHitMask(entries, read.req.startPc)
  private val readAlignedOffset = getAlignedInstOffset(read.req.startPc)
  private val s1_hitMask = VecInit((readRawHitMask zip entries).map { case (rawHit, e) =>
    rawHit && e.entry.position >= readAlignedOffset && !read.req.crossPage
  })

  // A fully-associative VBTB exposes only the earliest matching entry. The
  // non-strict order makes a lower physical way win when positions are equal.
  private val readPositionMatrix = CompareMatrix(
    VecInit(entries.map(_.entry.position)),
    order = (a: UInt, b: UInt) => a <= b
  )
  private val s1_selectOH    = readPositionMatrix.getLeastElementOH(s1_hitMask).asUInt
  private val s1_selectEntry = Mux1H(s1_selectOH, entries)
  private val s1_prediction  = Wire(Valid(new Prediction))

  s1_prediction.valid            := s1_selectOH.orR
  s1_prediction.bits.cfiPosition := Cat(read.req.posHigherBits, s1_selectEntry.entry.position)
  s1_prediction.bits.target := getFullTarget(
    read.req.startPc,
    s1_selectEntry.entry.targetLowerBits,
    s1_selectEntry.entry.targetCarry
  )
  s1_prediction.bits.attribute := s1_selectEntry.entry.attribute
  s1_prediction.bits.taken     := s1_selectEntry.counter.isPositive

  read.resp.s1_position := s1_prediction.bits.cfiPosition

  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire

  private val s2_hitMask     = RegEnable(s1_hitMask, s1_fire)
  private val s2_positions   = RegEnable(VecInit(entries.map(_.entry.position)), s1_fire)
  private val s2_selectOH    = RegEnable(s1_selectOH, s1_fire)
  private val s2_selectEntry = RegEnable(s1_selectEntry, s1_fire)
  private val s2_prediction  = RegEnable(s1_prediction, s1_fire)

  read.resp.s2_prediction     := s2_prediction
  read.resp.s2_meta.rawHit    := s2_selectOH.orR
  read.resp.s2_meta.attribute := s2_selectEntry.entry.attribute
  read.resp.s2_meta.position  := s2_prediction.bits.cfiPosition
  read.resp.s2_meta.counter   := s2_selectEntry.counter

  private val s3_selectOH = RegEnable(s2_selectOH, s2_fire)
  replacer.io.predTouch.valid := io.stageCtrl.s3_fire && io.s3_vbtbTaken && s3_selectOH.orR
  replacer.io.predTouch.bits  := OHToUInt(s3_selectOH)

  private val s2_mainBtbDuplicateMask = VecInit.tabulate(NumVictimBtbWays) { i =>
    VecInit.tabulate(NumWay) { j =>
      s2_hitMask(i) && io.s2_mainBtbHitMask(j) && s2_positions(i) === io.s2_mainBtbPositions(j)
    }.asUInt.orR
  }

  private val snapshotEvictedValid = snapshot.req.valid && snapshot.req.bits.evicted.valid
  private val snapshotEvictedHitMask = VecInit(entries.map { e =>
    snapshotEvictedValid && e.entry.valid &&
    e.internalBankIdx === snapshot.req.bits.internalBankIdx && e.setIdx === snapshot.req.bits.setIdx &&
    e.entry.tag === snapshot.req.bits.evicted.tag && e.entry.position === snapshot.req.bits.evicted.position
  })
  private val snapshotEvictedHit = snapshotEvictedHitMask.asUInt.orR

  private val snapshotIncomingValid = snapshot.req.valid && snapshot.req.bits.incoming.valid
  private val snapshotIncomingHitMask = VecInit(entries.map { e =>
    snapshotIncomingValid && e.entry.valid &&
    e.internalBankIdx === snapshot.req.bits.internalBankIdx && e.setIdx === snapshot.req.bits.setIdx &&
    e.entry.tag === snapshot.req.bits.incoming.tag && e.entry.position === snapshot.req.bits.incoming.position
  })
  private val snapshotIncomingHit = snapshotIncomingHitMask.asUInt.orR

  private val snapshotEvictedMatchesIncoming =
    snapshot.req.bits.evicted.valid && snapshot.req.bits.incoming.valid &&
      snapshot.req.bits.evicted.tag === snapshot.req.bits.incoming.tag &&
      snapshot.req.bits.evicted.position === snapshot.req.bits.incoming.position
  private val snapshotEntryWayMask = PriorityMux(Seq(
    snapshotEvictedHit  -> snapshotEvictedHitMask.asUInt,
    snapshotIncomingHit -> snapshotIncomingHitMask.asUInt,
    true.B              -> replacer.io.victimMask
  ))
  private val snapshotWriteEvicted  = snapshotEvictedValid && !snapshotEvictedMatchesIncoming
  private val snapshotFlushIncoming = snapshotIncomingHit

  // A snapshot allocation is the newest access and wins if it coincides with
  // ordinary VBTB training. Otherwise touch one trained entry, as uBTB does.
  private val trainEntryTouchWay = Mux(
    trainEntry.req.bits.entryWayMask.orR,
    OHToUInt(trainEntry.req.bits.entryWayMask),
    PriorityEncoder(trainEntry.req.bits.counterWayMask)
  )
  replacer.io.trainTouch.valid := snapshotWriteEvicted || trainEntry.req.valid
  replacer.io.trainTouch.bits  := Mux(snapshotWriteEvicted, OHToUInt(snapshotEntryWayMask), trainEntryTouchWay)

  assert(PopCount(snapshotIncomingHitMask) <= 1.U, "incoming hit mask should be one-hot")
  assert(PopCount(snapshotEvictedHitMask) <= 1.U, "evicted hit mask should be one-hot")

  private val vbtbPredReq = s2_fire && io.enable
  XSPerfAccumulate("vbtb_pred_req", vbtbPredReq)
  XSPerfAccumulate("vbtb_pred_hit", vbtbPredReq && s2_selectOH.orR)
  XSPerfAccumulate("vbtb_pred_miss", vbtbPredReq && !s2_selectOH.orR)
  XSPerfHistogram("vbtb_pred_hit_count", PopCount(s2_hitMask), vbtbPredReq, 0, NumVictimBtbWays + 1)
  XSPerfHistogram("vbtb_occupancy", PopCount(entries.map(_.entry.valid)), true.B, 0, NumVictimBtbWays + 1)
  XSPerfAccumulate("vbtb_mainbtb_duplicate_flush", s2_fire && s2_mainBtbDuplicateMask.asUInt.orR)

  XSPerfAccumulate("vbtb_replacer_pred_touch", replacer.io.predTouch.valid)
  XSPerfAccumulate("vbtb_replacer_train_touch", replacer.io.trainTouch.valid)
  XSPerfAccumulate("vbtb_snapshot_write", snapshotWriteEvicted)
  XSPerfAccumulate("vbtb_snapshot_evicted_reuse", snapshotEvictedValid && snapshotEvictedHit)
  XSPerfAccumulate("vbtb_snapshot_incoming_flush", snapshotFlushIncoming)

  // Training can update entry and counters independently:
  // - entryWayMask rewrites the victim BTB entry payload.
  // - counterWayMask updates the taken counter for conditional branches.
  // This allows a VBTB hit to be repaired in place without allocating MainBtb.
  for (w <- 0 until NumVictimBtbWays) {
    when(trainEntry.req.valid) {
      when(trainEntry.req.bits.counterWayMask(w)) {
        entries(w).counter := trainEntry.req.bits.counters(w)
      }
      when(trainEntry.req.bits.entryWayMask(w)) {
        entries(w).entry := trainEntry.req.bits.entry
      }
    }

    // Duplicate flushes override ordinary training updates.
    when(s2_fire && s2_mainBtbDuplicateMask(w)) {
      entries(w).entry.attribute := BranchAttribute.None
    }

    // Snapshot insertion has priority over training and duplicate flushes. This preserves
    // a newly evicted MainBtb entry if the same physical VBTB way is also flushed.
    when(snapshotFlushIncoming && snapshotIncomingHitMask(w)) {
      entries(w).entry.attribute := BranchAttribute.None
    }
    when(snapshotWriteEvicted && snapshotEntryWayMask(w)) {
      entries(w).entry           := snapshot.req.bits.evicted
      entries(w).setIdx          := snapshot.req.bits.setIdx
      entries(w).internalBankIdx := snapshot.req.bits.internalBankIdx
      entries(w).counter         := TakenCounter.WeakPositive
    }
  }

  when(reset.asBool) {
    entries.foreach(e => e.entry.attribute := BranchAttribute.None)
  }
}
