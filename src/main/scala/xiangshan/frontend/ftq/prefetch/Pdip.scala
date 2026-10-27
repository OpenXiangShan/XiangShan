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
//
// Acknowledgement
// This implementation is inspired by several key papers:
// [1] B. R. Godala et al., "PDIP: Priority Directed Instruction Prefetching," in Proceedings of the 29th ACM
//     International Conference on Architectural Support for Programming Languages and Operating Systems, Volume 2,
//     La Jolla CA USA: ACM, Apr. 2024, pp. 846–861. doi: 10.1145/3620665.3640394.

package xiangshan.frontend.ftq.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy
import utility.ValidHold
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import utils.AddrField
import xiangshan.frontend.FecCandidateInfo
import xiangshan.frontend.Pc
import xiangshan.frontend.PcInit
import xiangshan.frontend.ftq.FtqModule
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ftq.HasFtqParameters
import xiangshan.frontend.ftq.PrefetchQueueEntry
import xiangshan.frontend.icache.PrefetchSource

case class PdipParameters(
    NumSet:      Int = 512,
    NumWay:      Int = 2,
    TagWidth:    Int = 10,
    NumAdjacent: Int = 2
) {
  require(isPow2(NumSet), "NumSet must is power of 2")
  require(NumWay >= 1, "NumWay must >= 1")
  require(TagWidth >= 1, "TagWidth must >= 1")
  require(NumAdjacent >= 1, "NumAdjacent must >= 1")
}

trait HasPdipParameters extends HasFtqParameters {
  def pdipParameters: PdipParameters = ftqParameters.pdipParameters

  def BlockOffsetWidth: Int = log2Ceil(frontendParameters.icacheParameters.blockBytes)

  def NumSet:      Int = pdipParameters.NumSet
  def SetIdxWidth: Int = log2Ceil(NumSet)
  def NumWay:      Int = pdipParameters.NumWay
  def TagWidth:    Int = pdipParameters.TagWidth
  def NumAdjacent: Int = pdipParameters.NumAdjacent

  def BlockVAddrWidth: Int = VAddrBits - BlockOffsetWidth

  val addrFields = AddrField(
    Seq(
      ("blockOffset", BlockOffsetWidth),
      ("setIdx", SetIdxWidth),
      ("tag", TagWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      ("blockVAddr", BlockOffsetWidth, BlockVAddrWidth)
    )
  )

  def getSetIdx(pc: Pc): UInt = addrFields.extract("setIdx", pc)

  def getTag(pc: Pc): UInt = addrFields.extract("tag", pc)

  def getBlockVAddr(pc: Pc): UInt = addrFields.extract("blockVAddr", pc)

  def getPc(blockVAddr: UInt): Pc = PcInit(Cat(blockVAddr, 0.U(BlockOffsetWidth.W)))
}

class Pdip(implicit p: Parameters) extends FtqModule with HasPdipParameters {
  class PdipIO extends Bundle {
    class Commit extends Bundle {
      val ptr:     FtqPtr = new FtqPtr
      val startPc: Pc     = Pc()
    }

    class Redirect extends Bundle {
      val ptr:     FtqPtr = new FtqPtr
      val startPc: Pc     = Pc()
    }

    // from Ifu
    val fecCandidates: Vec[Valid[FecCandidateInfo]] = Vec(MaxFetchReqNum, Flipped(Valid(new FecCandidateInfo)))

    // from backend/ftq
    val commit:   Valid[Commit]   = Flipped(Valid(new Commit))
    val redirect: Valid[Redirect] = Flipped(Valid(new Redirect))
    val fetchPtr: FtqPtr          = Input(new FtqPtr)

    // to prefetch queue
    val req: Vec[Valid[PrefetchQueueEntry]] = Vec(NumAdjacent, Valid(new PrefetchQueueEntry))
  }

  val io: PdipIO = IO(new PdipIO)

  println(f"Pdip:")
  println(f"  Size(set, way, align, internal): $NumSet * $NumWay = ${NumSet * NumWay}")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  private class Entry extends Bundle {
    val mask:     UInt = UInt(NumAdjacent.W)
    val tag:      UInt = UInt(TagWidth.W)
    val blkVAddr: UInt = UInt(BlockVAddrWidth.W)

    def valid: Bool = mask.orR
  }

  private val candidates = RegInit(VecInit.fill(FtqSize)(
    VecInit.fill(2)(false.B) // FIXME: magic 2: each Ftq entry can locate in 2 adjacent ICache lines
  ))
  private val entries = Module(new SRAMTemplate(
    new Entry,
    NumSet,
    NumWay,
    singlePort = true,
    shouldReset = true
  ))
  private val replacer = ReplacementPolicy.fromString("setplru", NumWay, NumSet)

  /* *** read *** */
  // when a redirect happens, lookup table and enqueue to prefetch queue
  private val r0_valid   = io.redirect.valid
  private val r0_startPc = io.redirect.bits.startPc
  private val r0_setIdx  = getSetIdx(r0_startPc)
  entries.io.r.req.valid       := r0_valid
  entries.io.r.req.bits.setIdx := r0_setIdx

  private val r1_valid   = RegNext(r0_valid, false.B)
  private val r1_startPc = RegEnable(r0_startPc, r0_valid)
  private val r1_setIdx  = getSetIdx(r1_startPc)
  private val r1_tag     = getTag(r1_startPc)

  private val r1_entries = entries.io.r.resp.data

  // hold for write pipeline reuse, we need control of this, so do not use DataHoldBypass
  private val r1_hitMask = VecInit(r1_entries.map(e => e.valid && e.tag === r1_tag)).asUInt
  private val r1_entry   = Mux1H(r1_hitMask, r1_entries)

  private val r1_hit = r1_hitMask.orR
  assert(!r1_valid || PopCount(r1_hitMask) <= 1.U, "multi-hit")

  private val r1_blkVAddrs = VecInit.tabulate(NumAdjacent)(i => r1_entry.blkVAddr + i.U)

  (io.req zip r1_blkVAddrs).foreach { case (port, blkVAddr) =>
    port.valid       := r1_valid && r1_hit
    port.bits.vAddr  := getPc(blkVAddr).signGuard // for prefetch, we don't care about guard bit
    port.bits.source := PrefetchSource.Pdip
  }

  // save result for write pipeline reuse
  private val r2_valid     = RegEnable(true.B, false.B, r1_valid) // forever true after the first r1_valid
  private val r2_blkVAddrs = RegEnable(r1_blkVAddrs, r1_valid)
  private val r2_entry     = RegEnable(r1_entry, r1_valid)
  private val r2_hitMask   = RegEnable(r1_hitMask, r1_valid)
  private val r2_hit       = r2_hitMask.orR

  /* *** write *** */
  // 1. mark a Ftq entry as FEC candidate when:
  //   1) ICache detects a miss, and
  //   2) the core(backend + IBuffer) is empty when the block reaches Ifu->IBuffer interface
  io.fecCandidates.foreach { port =>
    when(port.valid) {
      candidates(port.bits.ftqIdx.value) := port.bits.miss // see Ifu.scala for this .miss
    }
  }

  // 2. if redirect happens, flush all entries after redirectPtr
  when(io.redirect.valid) {
    candidates.zipWithIndex.foreach { case (reg, i) =>
      when(Mux(
        io.redirect.bits.ptr.flag === io.fetchPtr.flag,
        i.U >= io.redirect.bits.ptr.value && i.U <= io.fetchPtr.value, // same flag, flush between
        i.U >= io.redirect.bits.ptr.value || i.U <= io.fetchPtr.value  // otherwise, flush sides
      )) {
        reg := 0.U.asTypeOf(reg)
      }
    }
  }

  // 3. promote candidate to the PDIP table when the Ftq entry has commited instruction
  // NOTE: pdip.io.commit is not actual backend (rob) commit, which does not guarantee continuity,
  //       the Ftq top is responsible to do commit step-by-step (see the difference between commit and robCommit there)
  // NOTE: entry should be written to the last redirect startPc, which we have cached in read pipeline, reuse
  private val w0_candidates = candidates(io.commit.bits.ptr.value)
  private val w0_valid      = io.commit.valid && w0_candidates.reduce(_ || _) && r2_valid
  private val w0_startPc    = io.commit.bits.startPc

  // clear candidates after it is promoted
  when(w0_valid) {
    candidates(io.commit.bits.ptr.value) := 0.U.asTypeOf(w0_candidates)
  }

  private val w1_fire       = Wire(Bool())
  private val w1_valid      = ValidHold(w0_valid, w1_fire)
  private val w1_candidates = RegEnable(w0_candidates, w0_valid)
  private val w1_startPc    = RegEnable(w0_startPc, w0_valid)

  private val w1_blkVAddr = getBlockVAddr(w1_startPc)
  private val w1_setIdx   = getSetIdx(w1_startPc)

  private val w1_wayMask = Mux(r2_hit, r2_hitMask, replacer.way(w1_setIdx))

  private val w1_blkHitMask = VecInit(r2_blkVAddrs.map(_ === w1_blkVAddr))
  private val w1_blkHit     = w1_blkHitMask.reduce(_ || _)

  private val w1_entry = WireDefault(r2_entry)
  when(!w1_blkHit) { // blkAddr does not match => init a new entry
    w1_entry.blkVAddr := w1_blkVAddr
    w1_entry.mask     := w1_candidates.asUInt
  }.otherwise { // otherwise => save it in mask
    w1_entry.mask := r2_entry.mask | w1_blkHitMask.asUInt
  }

  // if there are 2 continuous cycles have w0_valid, and the previous one have r0_valid also
  // the previous is dropped (no w1_fire, but w1 stage registers are overwritten by w0_valid)
  // PDIP promotion should be rare so we consider this not a big problem, just count it and do nothing
  XSPerfAccumulate("promotionFailure", w0_valid && w1_valid && !w1_fire)
  entries.io.w.req.valid := w1_fire
  entries.io.w.req.bits.apply(
    data = w1_entry,
    setIdx = w1_setIdx,
    waymask = w1_wayMask
  )

  // also update r2 buffer
  when(w1_fire) {
    r2_entry   := w1_entry
    r2_hitMask := w1_wayMask
  }

  w1_fire := w1_valid && !r0_valid // prioritize read

  /* *** replacer update *** */
  private val touchSets = Seq(r1_setIdx, w1_setIdx)
  private val touchWays = Seq.tabulate(2)(_ => // magic number 2: read touch & write touch
    Wire(Valid(UInt(log2Ceil(NumWay).W)))
  )
  touchWays.head.valid := r1_valid && r1_hit
  touchWays.head.bits  := OHToUInt(r1_hitMask)
  touchWays.last.valid := w1_fire
  touchWays.last.bits  := OHToUInt(w1_wayMask)
  replacer.access(touchSets, touchWays)

  XSPerfAccumulate(
    "fecCandidates",
    io.fecCandidates.map(port => Mux(port.valid, PopCount(port.bits.miss), 0.U)).reduce(_ +& _)
  )

  XSPerfAccumulate(
    "fec",
    Mux(io.commit.valid, PopCount(candidates(io.commit.bits.ptr.value)), 0.U)
  )
}
