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
import utility.ChiselDB
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utils.VecRotate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.TargetCarry

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction specific bundle
    val result: Vec[Valid[Prediction]] = Output(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val meta:   MainBtbMeta            = Output(new MainBtbMeta)
    // fullTarget for unconditional branches
    val s3_firstJumpTarget: PrunedAddr = Output(PrunedAddr(VAddrBits))
    // final taken
    val s3_takenMask:    Vec[Bool] = Input(Vec(NumBtbResultEntries, Bool()))
    val s3_firstTakenOH: Vec[Bool] = Input(Vec(NumBtbResultEntries, Bool()))
  }

  val io: MainBtbIO = IO(new MainBtbIO)

  // print params
  println(f"MainBtb:")
  println(f"  Size(set, way, align, internal): $NumSets * $NumWay * $NumAlignBanks * $NumInternalBanks = $NumEntries")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  /* *** submodules *** */
  private val alignBanks         = Seq.tabulate(NumAlignBanks)(alignIdx => Module(new MainBtbAlignBank(alignIdx)))
  private val alignBankReplacers = Seq.tabulate(NumAlignBanks)(_ => Module(new MainBtbReplacer(NumSets, NumWay)))

  private val pageTable         = Module(new MainBtbPageTable)
  private val pageTableReplacer = Module(new MainBtbReplacer(NumPageTableSet, NumPageTableWay))

//  private val regionTable = Module(new MainBtbRegionTable)
  private val regionTable         = Reg(Vec(NumRegionTableWay, new MainBtbRegionTableEntry))
  private val regionTableReplacer = Module(new MainBtbReplacer(1, NumRegionTableWay))

  io.resetDone := alignBanks.map(_.io.resetDone).reduce(_ && _) && pageTable.io.resetDone

  io.trainReady := true.B

  private val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())
  alignBanks.foreach { b =>
    b.io.stageCtrl.s0_fire := s0_fire
    b.io.stageCtrl.s1_fire := s1_fire
    b.io.stageCtrl.s2_fire := s2_fire
    b.io.stageCtrl.s3_fire := s3_fire
    // alignBank does not care t0, it's using t1 only
    b.io.stageCtrl.t0_fire := false.B
  }

  /* *** s0 ***
   * calculate per-bank startPc and posHigherBits
   * send read request to alignBanks and page table
   */
  s0_fire := io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc
  // rotate read addresses according to the first align bank index
  // e.g. if NumAlignBanks = 4, startPc locates in alignBank 1,
  // startPc + (i << FetchBlockAlignWidth) will be located in alignBank (1 + i) % 4,
  // i.e. we have VecInit.tabulate(...)'s alignBankIdx = (1, 2, 3, 0),
  // they always needs to goes to physical alignBank (0, 1, 2, 3),
  // so we need to rotate it right by 1.
  private val s0_rotator = VecRotate(getAlignBankIndex(s0_startPc))
  private val s0_startPcVec = s0_rotator.rotate(
    VecInit.tabulate(NumAlignBanks) { i =>
      if (i == 0)
        s0_startPc // keep lower bits for the first one
      else
        getAlignedPc(s0_startPc + (i << FetchBlockAlignWidth).U) // use aligned for others
    }
  )
  private val s0_posHigherBitsVec = s0_rotator.rotate(VecInit.tabulate(NumAlignBanks)(_.U(AlignBankIdxLen.W)))
  private val s0_alignCrossPage   = VecInit.tabulate(NumAlignBanks)(i => isCrossPage(s0_startPcVec(i), s0_startPc))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.startPc       := s0_startPcVec(i)
    b.io.read.req.posHigherBits := s0_posHigherBitsVec(i)
    b.io.read.req.crossPage     := s0_alignCrossPage(i)
  }

  /* *** s1 ***
   * just wait alignBanks
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable

  /* *** s2 ***
   * receive read response from alignBanks
   * send out prediction result and meta info
   * read page table if needed
   */
  s2_fire := io.stageCtrl.s2_fire && io.enable

  private val s2_result     = VecInit(alignBanks.flatMap(_.io.read.resp.predictions))
  private val s2_entry      = VecInit(alignBanks.flatMap(_.io.read.resp.entries))
  private val s2_shared     = VecInit(alignBanks.flatMap(_.io.read.resp.metas.map(_.shared)))
  private val s2_startPcVec = VecInit(alignBanks.flatMap(b => VecInit(Seq.fill(NumWay)(b.io.read.resp.startPc))))
  private val s2_meta       = VecInit(alignBanks.map(_.io.read.resp.metas))

  // we don't care about the order of alignBanks' responses,
  // (as s0_posHigherBitsVec is already computed and concatenated to each entry's posLowerBits)
  // (and we care about the full position when searching for a matching entry, not the bank it comes from)
  // so here we just flatten them, without rotating them back to the ori ginal order
  io.result := s2_result
  // we don't need to flatten meta entries, keep the alignBank structure, anyway we just use them per alignBank
  io.meta.entries := s2_meta

  // calculate first jump branch from two alignBanks
  private val s2_jumpMask = VecInit(s2_result.map { r =>
    r.valid && (r.bits.attribute.isDirect || r.bits.attribute.isIndirect)
  })

  private val s2_compareMatrix = CompareMatrix(VecInit(s2_result.map(_.bits.cfiPosition)))
  private val s2_firstJumpOH   = s2_compareMatrix.getLeastElementOH(s2_jumpMask)

  private val s2_firstJumpEntry   = Mux1H(s2_firstJumpOH, s2_entry)
  private val s2_firstJumpShared  = Mux1H(s2_firstJumpOH, s2_shared)
  private val s2_firstJumpStartPc = Mux1H(s2_firstJumpOH, s2_startPcVec)

  private val s2_jumpCrossPage =
    s2_firstJumpOH.asUInt.orR && s2_firstJumpEntry.targetCrossPage

  // read page table if first jump branch jumps cross page
  private val s2_pageIdx    = s2_firstJumpShared.asLong.pageIdx
  private val s2_pageSetIdx = getPageSetIdx(s2_pageIdx)
  private val s2_pageWayIdx = getPageWayIdx(s2_pageIdx)

  pageTable.io.readWay.req.valid        := s2_fire && s2_jumpCrossPage
  pageTable.io.readWay.req.bits.setIdx  := s2_pageSetIdx
  pageTable.io.readWay.req.bits.wayMask := UIntToOH(s2_pageWayIdx, NumPageTableWay)

  /* *** s3 ***
   * receive page table read response
   */
  s3_fire := io.stageCtrl.s3_fire && io.enable

  private val s3_startPcVec    = RegEnable(s2_startPcVec, s2_fire)
  private val s3_startPc       = RegEnable(s2_firstJumpStartPc, s2_fire)
  private val s3_entry         = RegEnable(s2_firstJumpEntry, s2_fire)
  private val s3_shared        = RegEnable(s2_firstJumpShared, s2_fire)
  private val s3_jumpCrossPage = RegEnable(s2_jumpCrossPage, s2_fire)
  private val s3_firstJumpOH   = RegEnable(s2_firstJumpOH, s2_fire)
  private val s3_pageSetIdx    = RegEnable(s2_pageSetIdx, s2_fire)
  private val s3_pageWayIdx    = RegEnable(s2_pageWayIdx, s2_fire)

  private val s3_pageEntry = pageTable.io.readWay.resp.entry

  // Case 1: jump not cross page
  // - use cond target fix helper
  private val s3_condTarget = getFullTarget(
    s3_startPc,
    s3_entry.targetLower,
    Some(s3_shared.asShort.targetCarry)
  ).toUInt
  // Case 2: jump cross page
  // - use full target from page/region table
  private val s3_pageTableTarget = Cat(
    regionTable(s3_pageEntry.regionWay).vpnUpper,
    pageTable.io.readWay.resp.entry.vpnLower,
    s3_entry.targetLower,
    0.U(instOffsetBits.W)
  )

  io.s3_firstJumpTarget := PrunedAddrInit(Mux(
    !s3_jumpCrossPage,
    s3_condTarget,
    s3_pageTableTarget
  ))

  // update replacers
  alignBankReplacers.zipWithIndex.foreach { case (replacer, i) =>
    val startPc   = s3_startPcVec(i * NumWay)
    val takenMask = io.s3_takenMask.slice(i * NumWay, (i + 1) * NumWay)

    // touch taken entries only: not-taken conditional entries are considered not very useful and should be killed first
    replacer.io.predictTouch.valid        := s3_fire && takenMask.reduce(_ || _)
    replacer.io.predictTouch.bits.setIdx  := getReplacerSetIndex(startPc)
    replacer.io.predictTouch.bits.wayMask := VecInit(takenMask).asUInt
  }

  // touch page table only when bpu use first jump branch
  private val s3_firstJumpTaken = io.s3_firstTakenOH.asUInt === s3_firstJumpOH.asUInt
  pageTableReplacer.io.predictTouch.valid        := s3_fire && s3_jumpCrossPage && s3_firstJumpTaken
  pageTableReplacer.io.predictTouch.bits.setIdx  := s3_pageSetIdx
  pageTableReplacer.io.predictTouch.bits.wayMask := UIntToOH(s3_pageWayIdx, NumPageTableWay)

  regionTableReplacer.io.predictTouch.valid        := s3_fire && s3_jumpCrossPage && s3_firstJumpTaken
  regionTableReplacer.io.predictTouch.bits.setIdx  := 0.U
  regionTableReplacer.io.predictTouch.bits.wayMask := UIntToOH(s3_pageEntry.regionWay, NumRegionTableWay)

  /* *** s3 ***
   * touch replacer using final takenMask (mbtb + tage + sc)
   */
  s3_fire := io.enable && io.stageCtrl.s3_fire
  // io.result is flattened, so is s3_takenMask from Bpu top, here we need to slice it back to alignBank structure
//  alignBanks.zipWithIndex.foreach { case (b, i) =>
//    b.io.s3_takenMask := io.s3_takenMask.slice(i * NumWay, (i + 1) * NumWay)
//  }

  /* *** t0 ***
   * receive training data and latch
   * read page table
   */
  private val t0_fire           = io.stageCtrl.t0_fire && io.enable
  private val t0_train          = io.train
  private val t0_mispredictInfo = t0_train.mispredictBranch

  private val t0_mispredNotCond =
    t0_mispredictInfo.valid && !t0_mispredictInfo.bits.attribute.isConditional

  private val t0_pageSetIdx = getPageSetIdx(t0_mispredictInfo.bits.target)

  pageTable.io.readSet.req.valid       := t0_fire && t0_mispredNotCond
  pageTable.io.readSet.req.bits.setIdx := t0_pageSetIdx

  /* *** t1 ***
   * calculate update entry and write to align bank
   * calculate update shared info
   */
  private val t1_fire  = RegNext(t0_fire, init = false.B) && io.enable
  private val t1_train = RegEnable(t0_train, t0_fire)

  private val t1_startPc = t1_train.startPc
  private val t1_rotator = VecRotate(getAlignBankIndex(t1_startPc))
  private val t1_startPcVec = t1_rotator.rotate(
    VecInit.tabulate(NumAlignBanks)(i => getAlignedPc(t1_startPc + (i << FetchBlockAlignWidth).U))
  )
  private val t1_meta           = t1_train.meta.mbtb
  private val t1_mispredictInfo = t1_train.mispredictBranch

  private val t1_writeAlignBankIdx  = getAlignBankIndexFromPosition(t1_mispredictInfo.bits.cfiPosition)
  private val t1_writeAlignBankMask = t1_rotator.rotate(VecInit(UIntToOH(t1_writeAlignBankIdx).asBools))

  private val t1_updateStartPc = Mux1H(t1_writeAlignBankMask, t1_startPcVec)
  private val t1_updateMeta    = Mux1H(t1_writeAlignBankMask, t1_meta.entries)
  private val t1_victimWay     = Mux1H(t1_writeAlignBankMask, alignBankReplacers.map(_.io.victim.wayMask))

  /* *** update entry *** */
  // NOTE: the original rawHit result can be multi-hit (i.e. multiple rawHit && position match), so PriorityEncoderOH
  private val t1_hitOH = VecInit(PriorityEncoderOH(VecInit(t1_updateMeta.map(_.hit(t1_mispredictInfo.bits)))))
  private val t1_hit   = t1_hitOH.reduce(_ || _)

  // Write entry only when there's a mispredict, and if:
  private val t1_entryNeedWrite = t1_mispredictInfo.valid && (
    // 1. not hit, always write a new entry, use mbtb replacer's victim way.
    !t1_hit ||
      // 2. hit, do write only if:
      //   a. it's an OtherIndirect-type branch (to update target and play the role of Ittage's base table).
      t1_mispredictInfo.bits.attribute.needIttage ||
      //   b. it's a Direct-type branch and page table entry is evicted (to update shared info).
      t1_mispredictInfo.bits.attribute.isDirect ||
      //   c. attribute changed, probably indicating a software self-modification.
      !(t1_mispredictInfo.bits.attribute === Mux1H(t1_hitOH, t1_updateMeta.map(_.attribute)))
  )
  // Use hit wayMask if hit, else use replacer's victim way
  private val t1_entryWayMask = Mux(t1_hit, t1_hitOH.asUInt, t1_victimWay)

  private val t1_targetCarry = getTargetCarrySlow(
    t1_updateStartPc,
    t1_mispredictInfo.bits.target
  )

  // fill entry
  private val t1_writeEntry = Wire(new MainBtbEntry)
  t1_writeEntry.valid           := true.B
  t1_writeEntry.tagLower        := getTagLower(t1_updateStartPc)
  t1_writeEntry.attribute       := t1_mispredictInfo.bits.attribute
  t1_writeEntry.position        := t1_mispredictInfo.bits.cfiPosition
  t1_writeEntry.targetCrossPage := t1_targetCarry.isInvalid
  t1_writeEntry.targetLower     := getTargetLower(t1_mispredictInfo.bits.target)

  // page table update
  private val t1_pageEntries    = pageTable.io.readSet.resp.entries
  private val t1_pageSetIdx     = getPageSetIdx(t1_mispredictInfo.bits.target)
  private val t1_targetVpnLower = getVpnLower(t1_mispredictInfo.bits.target)
  private val t1_targetVpnUpper = getVpnUpper(t1_mispredictInfo.bits.target)

  private val t1_vpnUpperMatchVec = VecInit.tabulate(NumRegionTableWay) { i =>
    regionTable(i).vpnUpper === t1_targetVpnUpper
  }
  private val t1_vpnUpperMatchOH = PriorityEncoderOH(t1_vpnUpperMatchVec.asUInt)
  private val t1_vpnUpperMatch   = t1_vpnUpperMatchVec.asUInt.orR

  private val t1_regionWayMask =
    Mux(t1_vpnUpperMatch, t1_vpnUpperMatchOH, regionTableReplacer.io.victim.wayMask)
  private val t1_regionWay = OHToUInt(t1_regionWayMask)

  private val t1_vpnMatchVec = VecInit(
    t1_pageEntries.map { e =>
      t1_vpnUpperMatch &&
      e.vpnLower === t1_targetVpnLower &&
      e.regionWay === OHToUInt(t1_vpnUpperMatchVec)
    }
  )
  private val t1_vpnMatchOH = PriorityEncoderOH(t1_vpnMatchVec.asUInt)
  private val t1_vpnMatch   = t1_vpnMatchVec.asUInt.orR

  private val t1_pageVictimMask = pageTableReplacer.io.victim.wayMask
  private val t1_pageWayMask    = Mux(t1_vpnMatch, t1_vpnMatchOH, t1_pageVictimMask)
  private val t1_pageWayIdx     = OHToUInt(t1_pageWayMask)

  private val t1_writePageEntry = Wire(new MainBtbPageTableEntry)
  t1_writePageEntry.vpnLower  := t1_targetVpnLower
  t1_writePageEntry.regionWay := t1_regionWay

  private val t1_writeRegionEntry = Wire(new MainBtbRegionTableEntry)
  t1_writeRegionEntry.vpnUpper := t1_targetVpnUpper

  // write page table
  pageTable.io.write.req.valid        := t1_fire && t1_entryNeedWrite && t1_targetCarry.isInvalid && !t1_vpnMatch
  pageTable.io.write.req.bits.setIdx  := t1_pageSetIdx
  pageTable.io.write.req.bits.wayMask := t1_pageWayMask
  pageTable.io.write.req.bits.entries := VecInit.fill(NumPageTableWay)(t1_writePageEntry)

  // write region table
  when(t1_fire && t1_entryNeedWrite && !t1_vpnUpperMatch) {
    regionTable(t1_regionWay) := t1_writeRegionEntry
  }

  // update page/region replacers
  pageTableReplacer.io.trainTouch.valid :=
    t1_fire && t1_entryNeedWrite && t1_targetCarry.isInvalid
  pageTableReplacer.io.trainTouch.bits.setIdx  := t1_pageSetIdx
  pageTableReplacer.io.trainTouch.bits.wayMask := t1_vpnMatchOH

  regionTableReplacer.io.trainTouch.valid :=
    t1_fire && t1_entryNeedWrite && t1_targetCarry.isInvalid
  regionTableReplacer.io.trainTouch.bits.setIdx  := 0.U
  regionTableReplacer.io.trainTouch.bits.wayMask := t1_vpnUpperMatchOH

  // shared info override
  private val t1_overrideShortShared = Wire(new ShortSharedInfo)
  t1_overrideShortShared.targetCarry := t1_targetCarry
  t1_overrideShortShared.tagUpper    := getTagUpperFromTagFull(getTagFull(t1_updateStartPc))
  t1_overrideShortShared.counter     := TakenCounter.WeakPositive

  private val t1_overrideLongShared = Wire(new LongSharedInfo)
  t1_overrideLongShared.pageIdx := getPageIdx(t1_pageSetIdx, t1_pageWayIdx)

  private val t1_overrideShared = Wire(new MainBtbSharedInfo)
  t1_overrideShared.bits := Mux(t1_targetCarry.isInvalid, t1_overrideLongShared.asUInt, t1_overrideShortShared.asUInt)

  // write entry to align bank
  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeEntry.req.valid        := t1_fire && t1_writeAlignBankMask(i) && t1_entryNeedWrite
    b.io.writeEntry.req.bits.startPc := t1_startPcVec(i)
    b.io.writeEntry.req.bits.wayMask := t1_entryWayMask
    b.io.writeEntry.req.bits.entry   := t1_writeEntry
    b.io.writeEntry.req.bits.shared  := t1_overrideShared
  }
  // update align bank replacer
  alignBankReplacers.zipWithIndex.foreach { case (r, i) =>
    r.io.trainTouch.valid        := t1_fire && t1_writeAlignBankMask(i) && t1_entryNeedWrite
    r.io.trainTouch.bits.setIdx  := getReplacerSetIndex(t1_startPcVec(i))
    r.io.trainTouch.bits.wayMask := t1_hitOH.asUInt
  }

  // counter update
  private val t1_branches        = t1_train.branches
  private val t1_writeShareds    = Wire(Vec(NumWay, new MainBtbSharedInfo))
  private val t1_writeSharedMask = Wire(Vec(NumWay, Bool()))

  t1_updateMeta.zipWithIndex.foreach { case (meta, i) =>
    val condHitMask = VecInit(t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && meta.hit(branch.bits)
//      meta.position === branch.bits.cfiPosition // TODO: really need meta.hit?
    })

    val condHit     = condHitMask.reduce(_ || _)
    val actualTaken = Mux1H(condHitMask, t1_branches.map(_.bits.taken))

    val metaShortShared = meta.shared.asShort
    val newShortShared  = Wire(new ShortSharedInfo)
    newShortShared.targetCarry := metaShortShared.targetCarry
    newShortShared.tagUpper    := metaShortShared.tagUpper
    newShortShared.counter     := metaShortShared.counter.getUpdate(actualTaken)

    t1_writeSharedMask(i)   := condHit
    t1_writeShareds(i).bits := Mux(condHit, newShortShared.asUInt, meta.shared.bits)
  }

  // write shared info to align bank
  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.writeShared.req.valid        := t1_fire && t1_writeAlignBankMask(i) && t1_writeSharedMask.asUInt.orR
    b.io.writeShared.req.bits.startPc := t1_startPcVec(i)
    b.io.writeShared.req.bits.wayMask := t1_writeSharedMask.asUInt
    b.io.writeShared.req.bits.shareds := t1_writeShareds
  }

  /* --------------------------------------------------------------------------------------------------------------
   MainBTB Trace
   -------------------------------------------------------------------------------------------------------------- */
  private val alignBankTraceVec = alignBanks.map(_.io.trace)
  private val finalTrace        = Mux1H(t1_writeAlignBankMask, alignBankTraceVec)
  private val finalTraceStartPc = Mux1H(t1_writeAlignBankMask, t1_startPcVec)
  private val mbtbTrace         = Wire(new MainBtbTrace)

  mbtbTrace.startPc      := finalTraceStartPc
  mbtbTrace.setIdx       := finalTrace.setIdx
  mbtbTrace.internalIdx  := finalTrace.bankIdx
  mbtbTrace.alignBankIdx := PriorityEncoder(t1_writeAlignBankMask)
  mbtbTrace.wayIdx       := finalTrace.wayIdx
  mbtbTrace.attribute    := finalTrace.entry.attribute
  mbtbTrace.cfiPosition  := finalTrace.entry.position

  private val mbtbTraceDBTable = ChiselDB.createTable("MBTBTrace", new MainBtbTrace(), EnableMainBtbTrace)
  mbtbTraceDBTable.log(
    data = mbtbTrace,
    en = t1_fire && finalTrace.needWrite,
    clock = clock,
    reset = reset
  )
  /* *** statistics *** */
  private val perf_s2HitMask             = VecInit(alignBanks.flatMap(_.io.read.resp.predictions.map(_.valid)))
  private val perf_t1HitMispredictBranch = t1_meta.entries.flatten.map(_.hit(t1_mispredictInfo.bits)).reduce(_ || _)

  XSPerfAccumulate("total_train", t1_fire)
  XSPerfAccumulate("pred_hit", s2_fire && perf_s2HitMask.reduce(_ || _))
  XSPerfHistogram("pred_hit_count", PopCount(perf_s2HitMask), s2_fire, 0, NumWay * NumAlignBanks + 1)
  XSPerfAccumulate("pred_miss", s2_fire && perf_s2HitMask.reduce(!_ && !_))

  XSPerfAccumulate("total_train", t1_fire)
  XSPerfAccumulate("train_has_mispredict", t1_fire && t1_mispredictInfo.valid)
  XSPerfAccumulate("train_hit_mispredict", t1_fire && t1_mispredictInfo.valid && perf_t1HitMispredictBranch)
  XSPerfAccumulate(
    "train", // no common prefix is needed
    t1_fire && t1_mispredictInfo.valid,
    Seq(
      ("allocate", !t1_hit),
      ("fixTarget", t1_hit && t1_mispredictInfo.bits.attribute.needIttage),
      (
        "fixAttribute",
        t1_hit && !(t1_mispredictInfo.bits.attribute === Mux1H(t1_hitOH, t1_updateMeta.map(_.attribute)))
      )
    )
  )
}
