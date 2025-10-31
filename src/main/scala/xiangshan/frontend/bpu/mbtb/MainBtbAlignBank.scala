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
import utility.XSPerfHistogram
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.StageCtrl

class MainBtbAlignBank(
    alignIdx: Int
)(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbAlignBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        // NOTE: this startVAddr is not from Bpu top, it's calculated in MainBtb top
        // i.e. vecRotateRight(VecInit.tabulate(NumAlignBanks)(startVAddr + _ * alignSize), startAlignIdx)(alignIdx)
        val startVAddr:    PrunedAddr = new PrunedAddr(VAddrBits)
        val posHigherBits: UInt       = UInt(AlignBankIdxLen.W)
        val crossPage:     Bool       = Bool()
      }

      class Resp extends Bundle {
        val rawHit:    Bool            = Bool() // for training
        val hit:       Bool            = Bool()
        val position:  UInt            = UInt(CfiPositionWidth.W)
        val target:    PrunedAddr      = PrunedAddr(VAddrBits)
        val attribute: BranchAttribute = new BranchAttribute
      }

      // don't need Valid or Decoupled here, AlignBank's pipeline is coupled with top, so we use stageCtrl to control
      val req: Req = Input(new Req)

      val resp: Vec[Resp] = Vec(NumWay, new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        // similar to Read.Req.startVAddr, calculated in MainBtb top
        val startVAddr: PrunedAddr   = new PrunedAddr(VAddrBits)
        val entry:      MainBtbEntry = new MainBtbEntry
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
  private val s0_startVAddr       = r.req.startVAddr
  private val s0_posHigherBits    = r.req.posHigherBits
  private val s0_crossPage        = r.req.crossPage
  private val s0_setIdx           = getSetIndex(s0_startVAddr)
  private val s0_internalBankIdx  = getInternalBankIndex(s0_startVAddr)
  private val s0_internalBankMask = UIntToOH(s0_internalBankIdx, NumInternalBanks)
  private val s0_alignBankIdx     = getAlignBankIndex(s0_startVAddr)

  // mainBtb top is responsible for sending the correct startVAddr to alignBanks,
  // so here we should always see getAlignBankIndex(s0_startVAddr) == physical alignIdx.
  assert(s0_alignBankIdx === alignIdx.U, "MainBtbAlignBank alignIdx mismatch")

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
  private val s1_startVAddr       = RegEnable(s0_startVAddr, s0_fire)
  private val s1_posHigherBits    = RegEnable(s0_posHigherBits, s0_fire)
  private val s1_crossPage        = RegEnable(s0_crossPage, s0_fire)
  private val s1_internalBankMask = RegEnable(s0_internalBankMask, s0_fire)

  private val s1_rawEntries = Mux1H(
    s1_internalBankMask,
    internalBanks.map(_.io.read.resp.entries)
  )

  /* *** s2 ***
   * check entries hit
   * filter-out unneeded entries
   * send resp to top
   */
  private val s2_fire             = io.stageCtrl.s2_fire
  private val s2_startVAddr       = RegEnable(s1_startVAddr, s1_fire)
  private val s2_posHigherBits    = RegEnable(s1_posHigherBits, s1_fire)
  private val s2_crossPage        = RegEnable(s1_crossPage, s1_fire)
  private val s2_internalBankMask = RegEnable(s1_internalBankMask, s0_fire)
  private val s2_rawEntries       = RegEnable(s1_rawEntries, s1_fire)

  private val s2_setIdx = getSetIndex(s2_startVAddr)
  private val s2_tag    = getTag(s2_startVAddr)

  // NOTE: when we calculate startVAddr in MainBtb top, we have selected whether lower bits should be masked
  //       (see s0_startVAddrVec)
  //       so here, if this alignBank is not the first alignBank of the fetch block, we'll get s2_alignedInstOffset = 0
  //       and, we'll do a (e.position >= 0) check later, which is always true
  private val s2_alignedInstOffset = getAlignedInstOffset(s2_startVAddr)

  // send resp
  (r.resp zip s2_rawEntries).foreach { case (resp, e) =>
    // send rawHit for training
    val rawHit = e.valid && e.tag === s2_tag
    // filter out branches before alignedInstOffset
    // also filter out all entries if crossPage to satisfy Ifu/ICache's requirement
    val hit = rawHit && e.position >= s2_alignedInstOffset && !s2_crossPage
    resp.rawHit    := rawHit
    resp.hit       := hit
    resp.position  := Cat(s2_posHigherBits, e.position)
    resp.target    := getFullTarget(s2_startVAddr, e.targetLowerBits, Some(e.targetCarry))
    resp.attribute := e.attribute
  }

  // add an alias for hitMask for later use & debug purpose
  private val s2_hitMask = VecInit(r.resp.map(_.hit))
  dontTouch(s2_hitMask)

  // update replacer
  replacer.io.predictTouch.valid        := s2_fire && s2_hitMask.reduce(_ || _)
  replacer.io.predictTouch.bits.setIdx  := getReplacerSetIndex(s2_startVAddr)
  replacer.io.predictTouch.bits.wayMask := s2_hitMask.asUInt

  /* *** t1 ***
   * send write req to internal banks (srams)
   */
  private val t1_valid            = w.req.valid
  private val t1_startVAddr       = w.req.bits.startVAddr
  private val t1_entry            = w.req.bits.entry
  private val t1_setIdx           = getSetIndex(t1_startVAddr)
  private val t1_internalBankIdx  = getInternalBankIndex(t1_startVAddr)
  private val t1_internalBankMask = UIntToOH(t1_internalBankIdx, NumInternalBanks)
  private val t1_alignBankIdx     = getAlignBankIndex(t1_startVAddr)
  private val t1_wayMask          = replacer.io.victim.wayMask

  // similar to s0 case
  assert(t1_alignBankIdx === alignIdx.U, "MainBtbAlignBank alignIdx mismatch")

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid        := t1_valid && t1_internalBankMask(i)
    b.io.write.req.bits.setIdx  := t1_setIdx
    b.io.write.req.bits.wayMask := t1_wayMask
    b.io.write.req.bits.entry   := t1_entry
  }

  // update replacer
  replacer.io.trainTouch.valid       := t1_valid
  replacer.io.trainTouch.bits.setIdx := getReplacerSetIndex(t1_startVAddr)

  /* *** multi-hit detection & flush *** */
  private val s2_multiHitMask = detectMultiHit(s2_hitMask, VecInit(s2_rawEntries.map(_.position)))

  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.flush.req.valid        := s2_fire && s2_multiHitMask.orR && s2_internalBankMask(i)
    b.io.flush.req.bits.setIdx  := s2_setIdx
    b.io.flush.req.bits.wayMask := s2_multiHitMask
  }

  XSPerfHistogram("multihit_count", PopCount(s2_multiHitMask), s2_fire, 0, NumWay)
}
