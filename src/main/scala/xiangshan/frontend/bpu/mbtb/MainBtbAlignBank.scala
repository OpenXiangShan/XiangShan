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
import xiangshan.frontend.bpu.SaturateCounter
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
        val startPc:     PrunedAddr             = new PrunedAddr(VAddrBits)
        val predictions: Vec[Valid[Prediction]] = Vec(NumWay, Valid(new Prediction))
        val entries:     Vec[MainBtbEntry]      = Vec(NumWay, new MainBtbEntry)
        val metas:       Vec[MainBtbMetaEntry]  = Vec(NumWay, new MainBtbMetaEntry)
      }

      // don't need Valid or Decoupled here, AlignBank's pipeline is coupled with top, so we use stageCtrl to control
      val req: Req = Input(new Req)

      val resp: Resp = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        // similar to Read.Req.startPc, calculated in MainBtb top
        val startPc: PrunedAddr        = new PrunedAddr(VAddrBits)
        val wayMask: UInt              = UInt(NumWay.W)
        val entry:   MainBtbEntry      = new MainBtbEntry
        val shared:  MainBtbSharedInfo = new MainBtbSharedInfo
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class WriteShared extends Bundle {
      class Req extends Bundle {
        // similar to Read.Req.startPc, calculated in MainBtb top
        val startPc: PrunedAddr             = new PrunedAddr(VAddrBits)
        val wayMask: UInt                   = UInt(NumWay.W)
        val shareds: Vec[MainBtbSharedInfo] = Vec(NumWay, new MainBtbSharedInfo)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }
    class Trace extends Bundle {
      val needWrite: Bool         = Bool()
      val setIdx:    UInt         = UInt(SetIdxLen.W)
      val bankIdx:   UInt         = UInt(log2Ceil(NumInternalBanks).W)
      val wayIdx:    UInt         = UInt(log2Ceil(NumWay).W)
      val entry:     MainBtbEntry = new MainBtbEntry
    }

    val resetDone: Bool      = Output(Bool())
    val stageCtrl: StageCtrl = Input(new StageCtrl)

    val read:        Read        = new Read
    val writeEntry:  WriteEntry  = new WriteEntry
    val writeShared: WriteShared = new WriteShared
    val trace:       Trace       = new Trace
  }

  val io: MainBtbAlignBankIO = IO(new MainBtbAlignBankIO)

  // alias
  private val r  = io.read
  private val we = io.writeEntry
  private val ws = io.writeShared
  private val t  = io.trace

  private val internalBanks = Seq.tabulate(NumInternalBanks) { bankIdx =>
    Module(new MainBtbInternalBank(alignIdx, bankIdx))
  }

//  private val replacer = Module(new MainBtbReplacer)

  io.resetDone := internalBanks.map(_.io.resetDone).reduce(_ && _)

  /* *** s0 ***
   * send read req to entry internal banks (srams)
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
   * send read req to extra info internal banks
   * receive read resp from internal banks
   * select 1 internal bank's resp
   * calculate first jump branch
   */
  private val s1_fire             = io.stageCtrl.s1_fire
  private val s1_startPc          = RegEnable(s0_startPc, s0_fire)
  private val s1_posHigherBits    = RegEnable(s0_posHigherBits, s0_fire)
  private val s1_crossPage        = RegEnable(s0_crossPage, s0_fire)
  private val s1_setIdx           = getSetIndex(s1_startPc)
  private val s1_internalBankIdx  = getInternalBankIndex(s1_startPc)
  private val s1_internalBankMask = UIntToOH(s1_internalBankIdx, NumInternalBanks)
  private val s1_alignBankIdx     = getAlignBankIndex(s1_startPc)

  private val s1_rawEntries = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.entries)
  )

  private val s1_rawShareds = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.shareds)
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
  private val s2_rawShareds       = RegEnable(s1_rawShareds, s1_fire)

  private val s2_setIdx   = getSetIndex(s2_startPc)
  private val s2_tagLower = getTagLower(s2_startPc)
  private val s2_tagFull  = getTagFull(s2_startPc)

  // NOTE: when we calculate startPc in MainBtb top, we have selected whether lower bits should be masked
  //       (see s0_startPcVec)
  //       so here, if this alignBank is not the first alignBank of the fetch block, we'll get s2_alignedInstOffset = 0
  //       and, we'll do a (e.position >= 0) check later, which is always true
  private val s2_alignedInstOffset = getAlignedInstOffset(s2_startPc)

  // send resp
  (s2_rawEntries zip s2_rawShareds).zipWithIndex.foreach { case ((e, s), i) =>
    val pred  = r.resp.predictions(i)
    val meta  = r.resp.metas(i)
    val entry = r.resp.entries(i)

    // reconstruct full tag when entry is a short branch
    val tagHit = Mux(
      e.targetCrossPage,
      e.tagLower === s2_tagLower,
      Cat(s.asShort.tagUpper, e.tagLower) === s2_tagFull
    )
    // send rawHit for training
    val rawHit = e.valid && tagHit
    // filter out branches before alignedInstOffset
    // also filter out all entries if crossPage to satisfy Ifu/ICache's requirement
    val hit = rawHit && e.position >= s2_alignedInstOffset && !s2_crossPage
    pred.valid            := hit
    pred.bits.cfiPosition := Cat(s2_posHigherBits, e.position)
    // temporary use condition target, will be fixed later in MainBtb top
    pred.bits.target :=
      getFullTarget(s2_startPc, e.targetLower, Some(s.asShort.targetCarry))
    pred.bits.attribute := e.attribute
    pred.bits.taken     := s.asShort.counter.isPositive

    meta.rawHit    := rawHit
    meta.attribute := e.attribute
    meta.position  := Cat(s2_posHigherBits, e.position)
    meta.shared    := s

    entry := e
  }
  r.resp.startPc := s2_startPc

  // add an alias for hitMask for later use & debug purpose
  private val s2_hitMask = VecInit(r.resp.predictions.map(_.valid))
  dontTouch(s2_hitMask)

  /* *** t1 ***
   * send write req to internal banks (srams)
   */
  private val t1_entryValid   = we.req.valid
  private val t1_entryStartPc = we.req.bits.startPc
//  private val t1_branches         = w.req.bits.branches
//  private val t1_meta             = w.req.bits.meta
//  private val t1_mispredictInfo   = w.req.bits.mispredictInfo
  private val t1_entryWayMask          = we.req.bits.wayMask
  private val t1_entry                 = we.req.bits.entry
  private val t1_entryShared           = we.req.bits.shared
  private val t1_entrySetIdx           = getSetIndex(t1_entryStartPc)
  private val t1_entryInternalBankIdx  = getInternalBankIndex(t1_entryStartPc)
  private val t1_entryInternalBankMask = UIntToOH(t1_entryInternalBankIdx, NumInternalBanks)
  private val t1_entryAlignBankIdx     = getAlignBankIndex(t1_entryStartPc)

  // similar to s0 case
  assert(!t1_entryValid || t1_entryAlignBankIdx === alignIdx.U, "t1 MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeEntry.req.valid        := t1_entryValid && t1_entryInternalBankMask(i)
    b.io.writeEntry.req.bits.setIdx  := t1_entrySetIdx
    b.io.writeEntry.req.bits.wayMask := t1_entryWayMask
    b.io.writeEntry.req.bits.entry   := t1_entry
    b.io.writeEntry.req.bits.shared  := t1_entryShared
  }

  private val t1_sharedValid            = ws.req.valid
  private val t1_sharedStartPc          = ws.req.bits.startPc
  private val t1_sharedWayMask          = ws.req.bits.wayMask
  private val t1_shareds                = ws.req.bits.shareds
  private val t1_sharedSetIdx           = getSetIndex(t1_sharedStartPc)
  private val t1_sharedInternalBankIdx  = getInternalBankIndex(t1_sharedStartPc)
  private val t1_sharedInternalBankMask = UIntToOH(t1_sharedInternalBankIdx, NumInternalBanks)
  private val t1_sharedAlignBankIdx     = getAlignBankIndex(t1_sharedStartPc)

  assert(!t1_sharedValid || t1_sharedAlignBankIdx === alignIdx.U, "t2 MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeShared.req.valid        := t1_sharedValid && t1_sharedInternalBankMask(i)
    b.io.writeShared.req.bits.setIdx  := t1_sharedSetIdx
    b.io.writeShared.req.bits.wayMask := t1_sharedWayMask
    b.io.writeShared.req.bits.shareds := t1_shareds
  }

  /* *** multi-hit detection & flush *** */
  private val s2_multiHitMask = detectMultiHit(s2_hitMask, VecInit(s2_rawEntries.map(_.position)))

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := s2_fire && s2_multiHitMask.orR && s2_internalBankMask(i)
    b.io.flush.req.bits.setIdx  := s2_setIdx
    b.io.flush.req.bits.wayMask := s2_multiHitMask
  }

  // mainBTB trace bundle
  io.trace.needWrite := t1_entryValid
  io.trace.setIdx    := t1_entrySetIdx
  io.trace.bankIdx   := t1_entryInternalBankIdx
  io.trace.wayIdx    := PriorityEncoder(t1_entryWayMask.asUInt)
  io.trace.entry     := t1_entry
  XSPerfHistogram("multihit_count", PopCount(s2_multiHitMask), s2_fire, 0, NumWay)

}
