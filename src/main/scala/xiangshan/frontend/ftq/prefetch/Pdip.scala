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
import utility.DataHoldBypass
import utility.ReplacementPolicy
import utility.ValidHold
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.FecCandidateInfo
import xiangshan.frontend.Pc
import xiangshan.frontend.ftq.FtqModule
import xiangshan.frontend.ftq.FtqPtr

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

    class FtqEnqueue extends Bundle {
      val ptr: FtqPtr = new FtqPtr
    }

    // from Ifu
    val fecCandidates: Vec[Valid[FecCandidateInfo]] = Vec(MaxFetchReqNum, Flipped(Valid(new FecCandidateInfo)))

    // from backend/ftq
    val commit:     Valid[Commit]    = Flipped(Valid(new Commit))
    val redirect:   Valid[Redirect]  = Flipped(Valid(new Redirect))
    val ftqEnqueue: Valid[FtqEnqueue] = Flipped(Valid(new FtqEnqueue))
    val fetchPtr:   FtqPtr           = Input(new FtqPtr)

    // to prefetch queue
    val req: Vec[DecoupledIO[PrefetchQueueEntry]] = Vec(NumAdjacent, DecoupledIO(new PrefetchQueueEntry))
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

  private class TrainingReq extends Bundle {
    val candidates: Vec[Bool] = Vec(2, Bool())
    val blkVAddr:   UInt      = UInt(BlockVAddrWidth.W)
    val setIdx:     UInt      = UInt(SetIdxWidth.W)
    val tag:        UInt      = UInt(TagWidth.W)
  }

  private class TriggerKey extends Bundle {
    val valid:  Bool = Bool()
    val setIdx: UInt = UInt(SetIdxWidth.W)
    val tag:    UInt = UInt(TagWidth.W)
  }

  private class FtqTrigger extends Bundle {
    val entryValid: Bool       = Bool()
    val ftqFlag:    Bool       = Bool()
    val key:        TriggerKey = new TriggerKey
  }

  private val candidates = RegInit(VecInit.fill(FtqSize)(
    VecInit.fill(2)(false.B) // FIXME: magic 2: each Ftq entry can locate in 2 adjacent ICache lines
  ))
  private val activeTrigger = RegInit(0.U.asTypeOf(new TriggerKey))
  private val triggerQueue  = RegInit(VecInit.fill(FtqSize)(0.U.asTypeOf(new FtqTrigger)))
  private val entries = Module(new SRAMTemplate(
    new Entry,
    NumSet,
    NumWay,
    singlePort = true,
    shouldReset = true
  ))
  private val replacer = ReplacementPolicy.fromString("setplru", NumWay, NumSet)

  when(io.redirect.valid) {
    activeTrigger.valid  := true.B
    activeTrigger.setIdx := getSetIdx(io.redirect.bits.startPc)
    activeTrigger.tag    := getTag(io.redirect.bits.startPc)
  }

  /* *** read *** */
  private val r0_valid              = io.redirect.valid
  private val trainSlot             = Reg(new TrainingReq)
  private val trainSlotValid        = RegInit(false.B)
  private val writeBufferValid      = RegInit(false.B)
  private val writeBufferEntry      = Reg(new Entry)
  private val writeBufferSetIdx     = Reg(UInt(SetIdxWidth.W))
  private val writeBufferWayMask    = Reg(UInt(NumWay.W))
  private val writeBufferCandidates = Reg(Vec(2, Bool()))

  private val queryReadFire      = Wire(Bool())
  private val trainReadFire      = Wire(Bool())
  private val trainReadReq       = Wire(Bool())
  private val trainReadRespValid = RegNext(trainReadFire, false.B)
  private val r1_fire            = Wire(Bool())
  private val r1_valid           = ValidHold(queryReadFire, r1_fire)
  private val queryReadReq       = io.redirect.valid
  // The query result has its own hold register, so PQ backpressure does not block training reads.
  trainReadReq                := trainSlotValid && !queryReadReq && !writeBufferValid && !trainReadRespValid
  entries.io.r.req.valid       := queryReadReq || trainReadReq
  entries.io.r.req.bits.setIdx := Mux(queryReadReq, getSetIdx(io.redirect.bits.startPc), trainSlot.setIdx)
  queryReadFire               := entries.io.r.req.fire && queryReadReq
  trainReadFire               := entries.io.r.req.fire && trainReadReq

  private val queryReadRespValid = RegNext(queryReadFire, false.B)
  private val r1_querySetIdx     = RegEnable(getSetIdx(io.redirect.bits.startPc), queryReadFire)
  private val r1_queryTag        = RegEnable(getTag(io.redirect.bits.startPc), queryReadFire)
  private val trainResp          = RegEnable(trainSlot, trainReadFire)

  // Forward the entire pending way, including a replacement with a different tag. Comparing only
  // matching tags here would let a lookup hit the victim that the buffered write has already evicted.
  private val queryWriteBypass    = writeBufferValid && writeBufferSetIdx === r1_querySetIdx
  private val r1_forwardedEntries = Wire(Vec(NumWay, new Entry))
  r1_forwardedEntries := entries.io.r.resp.data
  for (way <- 0 until NumWay) {
    when(queryWriteBypass && writeBufferWayMask(way)) {
      r1_forwardedEntries(way) := writeBufferEntry
    }
  }
  private val r1_entries = DataHoldBypass(r1_forwardedEntries, queryReadRespValid)
  private val r1_hitMask = VecInit(r1_entries.map(e => e.valid && e.tag === r1_queryTag)).asUInt
  private val r1_entry = Mux1H(r1_hitMask, r1_entries)
  private val r1_hit = r1_hitMask.orR
  assert(!r1_valid || PopCount(r1_hitMask) <= 1.U, "multi-hit")
  private val r1_blkVAddrs = VecInit.tabulate(NumAdjacent)(i => r1_entry.blkVAddr + i.U)
  private val r1_sent = io.req.map(port => ValidHold(port.fire, r1_fire, flush = queryReadFire))
  private val r1_needEnqueue = io.req.indices.map(i => r1_hit && r1_entry.mask(i) && !r1_sent(i))

  io.req.zipWithIndex.foreach { case (port, i) =>
    port.valid       := r1_valid && r1_needEnqueue(i)
    port.bits.vAddr  := getPc(r1_blkVAddrs(i)).signGuard
    port.bits.source := PrefetchSource.Pdip
  }
  r1_fire := r1_valid && !r1_needEnqueue.reduce(_ || _)

  // An FTQ slot keeps the trigger snapshot that was active when that generation was enqueued.
  io.fecCandidates.foreach { port =>
    val idx = port.bits.ftqIdx.value
    val sameGeneration = triggerQueue(idx).entryValid && triggerQueue(idx).ftqFlag === port.bits.ftqIdx.flag
    when(port.valid && sameGeneration) {
      candidates(port.bits.ftqIdx.value) := port.bits.miss
    }
  }
  private val redirectFlushMask = VecInit((0 until FtqSize).map { i =>
    io.redirect.valid && Mux(
      io.redirect.bits.ptr.flag === io.fetchPtr.flag,
      i.U >= io.redirect.bits.ptr.value && i.U <= io.fetchPtr.value,
      i.U >= io.redirect.bits.ptr.value || i.U <= io.fetchPtr.value
    )
  })
  when(io.redirect.valid) {
    for (i <- 0 until FtqSize) {
      when(redirectFlushMask(i)) {
        candidates(i) := 0.U.asTypeOf(candidates(i))
        triggerQueue(i).entryValid := false.B
      }
    }
  }

  private val w0_candidates = candidates(io.commit.bits.ptr.value)
  private val commitTrigger = triggerQueue(io.commit.bits.ptr.value)
  private val w0_candidateGenerationMatches = commitTrigger.entryValid &&
    commitTrigger.ftqFlag === io.commit.bits.ptr.flag
  private val w0_triggerGenerationMatches = commitTrigger.key.valid
  private val commitFlushed = redirectFlushMask(io.commit.bits.ptr.value)
  private val w0_valid = io.commit.valid && w0_candidates.asUInt.orR &&
    w0_candidateGenerationMatches && w0_triggerGenerationMatches && !commitFlushed
  private val w0_startPc = io.commit.bits.startPc

  when(io.commit.valid && w0_candidateGenerationMatches) {
    candidates(io.commit.bits.ptr.value) := 0.U.asTypeOf(w0_candidates)
    triggerQueue(io.commit.bits.ptr.value).entryValid := false.B
  }

  private val w1_valid = trainSlotValid
  private val w1_fire = trainReadFire
  when(trainReadFire) {
    trainSlotValid := false.B
  }
  when(w0_valid) {
    trainSlot.candidates := w0_candidates
    trainSlot.blkVAddr   := getBlockVAddr(w0_startPc)
    trainSlot.setIdx     := commitTrigger.key.setIdx
    trainSlot.tag        := commitTrigger.key.tag
    trainSlotValid       := true.B
  }

  when(io.ftqEnqueue.valid) {
    val idx = io.ftqEnqueue.bits.ptr.value
    candidates(idx)              := 0.U.asTypeOf(candidates(idx))
    triggerQueue(idx).entryValid := true.B
    triggerQueue(idx).ftqFlag    := io.ftqEnqueue.bits.ptr.flag
    triggerQueue(idx).key        := activeTrigger
  }

  private val trainHitMask = VecInit(entries.io.r.resp.data.map(e => e.valid && e.tag === trainResp.tag)).asUInt
  private val trainHit = trainHitMask.orR
  private val trainOldEntry = Mux(
    trainHit,
    Mux1H(trainHitMask, entries.io.r.resp.data),
    0.U.asTypeOf(new Entry)
  )
  private val trainOldBlkVAddrs = VecInit.tabulate(NumAdjacent)(i => trainOldEntry.blkVAddr + i.U)
  private val trainBlkHitMask = VecInit(trainOldBlkVAddrs.map(_ === trainResp.blkVAddr))
  private val trainBlkHitIdx = OHToUInt(trainBlkHitMask)
  private val trainBlkHit = trainHit && trainBlkHitMask.asUInt.orR
  private val trainBlkHitPrev = trainHit && (trainResp.blkVAddr + 1.U) === trainOldBlkVAddrs(0)
  private val trainUpdatedEntry = WireDefault(trainOldEntry)
  when(trainBlkHit) {
    trainUpdatedEntry.mask := trainOldEntry.mask | (trainResp.candidates.asUInt << trainBlkHitIdx).asUInt
  }.elsewhen(trainBlkHitPrev) {
    trainUpdatedEntry.blkVAddr := trainResp.blkVAddr
    trainUpdatedEntry.mask     := (trainOldEntry.mask << 1).asUInt | trainResp.candidates.asUInt
  }.otherwise {
    trainUpdatedEntry.blkVAddr := trainResp.blkVAddr
    trainUpdatedEntry.mask     := trainResp.candidates.asUInt
  }
  trainUpdatedEntry.tag := trainResp.tag
  private val trainWayMask = Mux(trainHit, trainHitMask, UIntToOH(replacer.way(trainResp.setIdx), NumWay))
  when(trainReadRespValid) {
    assert(PopCount(trainHitMask) <= 1.U, "multi-hit during PDIP training re-read")
    assert(!writeBufferValid, "PDIP training re-read completed while write buffer was occupied")
    writeBufferEntry      := trainUpdatedEntry
    writeBufferSetIdx     := trainResp.setIdx
    writeBufferWayMask    := trainWayMask
    writeBufferCandidates := trainResp.candidates
    writeBufferValid      := true.B
  }

  private val writeReqValid = writeBufferValid && !entries.io.r.req.valid
  entries.io.w.apply(writeReqValid, writeBufferEntry, writeBufferSetIdx, writeBufferWayMask)
  private val tableWriteFire = entries.io.w.req.fire
  when(tableWriteFire) {
    assert(PopCount(writeBufferWayMask) === 1.U, "PDIP write must select exactly one way")
    writeBufferValid := false.B
  }
  assert(!(entries.io.r.req.fire && tableWriteFire), "PDIP single-port SRAM read/write conflict")

  private val touchSets = Seq(r1_querySetIdx, writeBufferSetIdx)
  private val touchWays = Seq.tabulate(2)(_ => Wire(Valid(UInt(log2Ceil(NumWay).W))))
  touchWays.head.valid := queryReadRespValid && r1_hit
  touchWays.head.bits  := OHToUInt(r1_hitMask)
  touchWays.last.valid := tableWriteFire
  touchWays.last.bits  := OHToUInt(writeBufferWayMask)
  replacer.access(touchSets, touchWays)

  XSPerfAccumulate(
    "fecCandidates",
    io.fecCandidates.map(port => Mux(port.valid, PopCount(port.bits.miss), 0.U)).reduce(_ +& _)
  )

  XSPerfAccumulate(
    "fec",
    Mux(io.commit.valid, PopCount(candidates(io.commit.bits.ptr.value)), 0.U)
  )

  XSPerfAccumulate("hit", queryReadRespValid && r1_hit)

  // A new commit may overwrite the pending training slot, but never an in-flight read or buffered write.
  XSPerfAccumulate("promotionFailure", w0_valid && w1_valid && !w1_fire)

  // same for enqueue
  XSPerfAccumulate("enqueueFailure", r0_valid && r1_valid && !r1_fire)

  // Observe actual SRAM handshakes separately from control completion.
  private val perfBlockedTargets = PopCount(io.req.map(port => port.valid && !port.ready))
  XSPerfAccumulate("query_attempt_events", r0_valid)
  XSPerfAccumulate("table_read_accepted_events", entries.io.r.req.fire)
  XSPerfAccumulate("table_read_not_accepted_events", entries.io.r.req.valid && !entries.io.r.req.ready)
  XSPerfAccumulate("query_read_accepted_events", queryReadFire)
  XSPerfAccumulate("training_read_accepted_events", trainReadFire)
  XSPerfAccumulate("table_read_hit_events", queryReadRespValid && r1_hit)
  XSPerfAccumulate("training_read_hit_events", trainReadRespValid && trainHit)
  XSPerfAccumulate("query_expanded_targets", Mux(queryReadRespValid && r1_hit, PopCount(r1_entry.mask), 0.U))
  XSPerfAccumulate("output_accepted_targets", PopCount(io.req.map(_.fire)))
  XSPerfAccumulate("output_wait_lane_cycles", perfBlockedTargets)
  // A lane accepted on the redirect edge is not lost. Only still-blocked targets are overwritten.
  XSPerfAccumulate("output_overwrite_events", r0_valid && perfBlockedTargets.orR)
  XSPerfAccumulate("output_overwrite_targets", Mux(r0_valid, perfBlockedTargets, 0.U))
  XSPerfAccumulate("training_slot_input_events", w0_valid)
  XSPerfAccumulate("training_slot_input_bits", Mux(w0_valid, PopCount(w0_candidates), 0.U))
  XSPerfAccumulate("training_slot_overwrite_events", w0_valid && w1_valid && !w1_fire)
  XSPerfAccumulate(
    "training_slot_overwrite_bits",
    Mux(w0_valid && w1_valid && !w1_fire, PopCount(trainSlot.candidates), 0.U)
  )
  XSPerfAccumulate("table_write_attempt_events", entries.io.w.req.valid)
  XSPerfAccumulate("table_write_accepted_events", entries.io.w.req.fire)
  XSPerfAccumulate("table_write_not_ready_cycles", entries.io.w.req.valid && !entries.io.w.req.ready)
  // Completion now is the actual SRAM handshake; keep this legacy diagnostic as an invariant zero.
  XSPerfAccumulate("table_write_control_without_accept_events", false.B)
  XSPerfAccumulate("write_buffer_wait_cycles", writeBufferValid && !tableWriteFire)
  // Input candidate bits of an accepted write, not newly installed bits or promotions.
  XSPerfAccumulate("table_write_accepted_input_bits", Mux(tableWriteFire, PopCount(writeBufferCandidates), 0.U))
}
