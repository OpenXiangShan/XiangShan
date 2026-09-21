/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._

class MissTrackLine(implicit p: Parameters) extends DCacheBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
}

class MissTrackResident(implicit p: Parameters) extends MissTrackLine {
  val way = UInt(nWays.W)
}

class MissTrackAlloc(implicit p: Parameters) extends MissTrackLine {
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)
}

class MissTrackInvalidate(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way = UInt(nWays.W)
}

class MissTrackLoadResult(implicit p: Parameters) extends MissTrackResident {
  val hit = Bool() // Accurate tag/permission result, independent of WPU prediction.
  val first_issue = Bool()
  val handled = Bool()
  val merged = Bool()
  val allocated = Bool()
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)
}

// Observation only: no MissTrack output drives a LoadPipe control signal.
class MissTrackLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s0_valid = Bool()
  val s0_vaddr = UInt(VAddrBits.W)
  val s1_valid = Bool()
  val s1_paddr = UInt(PAddrBits.W)
  val s2 = Valid(new MissTrackLoadResult)
}

class MissTrackEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val idx = UInt(idxBits.W)
  val src_hash = UInt(cfg.missTrackHashBits.W)
  val block_paddr = UInt((PAddrBits - blockOffBits).W)
  val is_pending = Bool()
  val way = UInt(nWays.W)
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)
  val age = UInt(3.W)
}

class MissTrackAssessment(implicit p: Parameters) extends DCacheBundle {
  val resident = Bool()
  val pending = Bool()
  val unknown = Bool()
  val pa_fail = Bool()
  val multi_hit = Bool()
  val actual_hit = Bool()
  val way_correct = Bool()
  val owner_match = Bool()
  val mshr_match = Bool()
  val handled = Bool()
  val merged = Bool()
  val new_alloc = Bool()
}

/** Small, lossy history table for speculative-miss eligibility, currently shadow only.
  * Existing records are maintained in parallel. One new record per cycle is sampled
  * by a round-robin arbiter; losing insertion opportunities affects coverage only.
  */
class MissTrack(allocPorts: Int)(implicit p: Parameters) extends DCacheModule {
  require(cfg.missTrackShadow, "MissTrack currently supports shadow operation only")
  require(cfg.missTrackEntries >= 2)
  require(cfg.missTrackHashBits > 0 && cfg.missTrackHashBits <= VAddrBits - untagBits)
  require(allocPorts > 0)

  val io = IO(new Bundle {
    val loads = Input(Vec(LoadPipelineWidth, new MissTrackLoadIO))
    val alloc = Flipped(Vec(allocPorts, Valid(new MissTrackAlloc)))
    val owners = Flipped(Vec(cfg.nMissEntries, Valid(new MissTrackLine)))
    val install = Flipped(Valid(new MissTrackResident))
    // Tag replacement OR meta invalidation, at the actual write handshake.
    val invalidate = Flipped(Valid(new MissTrackInvalidate))
    val clear = Input(Bool())
    val assessment = Output(Vec(LoadPipelineWidth, Valid(new MissTrackAssessment)))
  })

  private val entries = RegInit(VecInit(Seq.fill(cfg.missTrackEntries)(0.U.asTypeOf(new MissTrackEntry))))
  private def block(pa: UInt): UInt = pa(PAddrBits - 1, blockOffBits)
  private def hash(va: UInt): UInt = XORFold(va(VAddrBits - 1, untagBits), cfg.missTrackHashBits)
  private def sameLine(a: MissTrackEntry, b: MissTrackEntry): Bool =
    a.idx === b.idx && a.block_paddr === b.block_paddr
  private def ownerMatches(e: MissTrackEntry): Bool = VecInit(io.owners.zipWithIndex.map { case (o, i) =>
    o.valid && e.mshr_id === i.U && block(o.bits.paddr) === e.block_paddr &&
      get_dcache_idx(o.bits.vaddr) === e.idx
  }).asUInt.orR
  private def overlaps(idx: UInt, way: UInt, inv: Valid[MissTrackInvalidate]): Bool =
    inv.valid && inv.bits.idx === idx && (inv.bits.way & way).orR

  // A hit observed in s1 is trained in s2. Do not resurrect a line invalidated
  // either during that observation or during the training cycle.
  val previousInvalidate = RegNext(io.invalidate, 0.U.asTypeOf(io.invalidate))
  val previousClear = RegNext(io.clear, false.B)
  val events = Wire(Vec(1 + allocPorts + LoadPipelineWidth, Valid(new MissTrackEntry)))
  val lookupEntries = Wire(Vec(cfg.missTrackEntries, new MissTrackEntry))
  events := 0.U.asTypeOf(events)

  def makeEntry(va: UInt, pa: UInt, way: UInt, pending: Bool, id: UInt): MissTrackEntry = {
    val e = WireDefault(0.U.asTypeOf(new MissTrackEntry))
    e.valid := true.B
    e.idx := get_dcache_idx(va)
    e.src_hash := hash(va)
    e.block_paddr := block(pa)
    e.is_pending := pending
    e.way := way
    e.mshr_id := id
    e
  }

  // The installation carries the final MainPipe way, never occupy_way or GrantLast.
  events(0).valid := io.install.valid && PopCount(io.install.bits.way) === 1.U
  events(0).bits := makeEntry(io.install.bits.vaddr, io.install.bits.paddr, io.install.bits.way, false.B, 0.U)
  for (i <- 0 until allocPorts) {
    val a = io.alloc(i)
    events(1 + i).bits := makeEntry(a.bits.vaddr, a.bits.paddr, 0.U, true.B, a.bits.mshr_id)
    events(1 + i).valid := a.valid && ownerMatches(events(1 + i).bits)
  }
  for (w <- 0 until LoadPipelineWidth) {
    val q = io.loads(w)
    val truth = q.s2.bits
    val training = events(1 + allocPorts + w)
    training.bits := makeEntry(truth.vaddr, truth.paddr, truth.way, false.B, 0.U)
    training.valid := q.s2.valid && truth.hit && PopCount(truth.way) === 1.U && !previousClear &&
      !overlaps(get_dcache_idx(truth.vaddr), truth.way, io.invalidate) &&
      !overlaps(get_dcache_idx(truth.vaddr), truth.way, previousInvalidate)

    val matches = VecInit(lookupEntries.map(e => e.valid && e.idx === get_dcache_idx(q.s0_vaddr) &&
      e.src_hash === hash(q.s0_vaddr)))
    val unique = PopCount(matches) === 1.U
    // Selection is well-defined even on a collision; valid rejects the collision.
    val candidate = WireDefault(lookupEntries(PriorityEncoder(matches)))
    candidate.valid := q.s0_valid && unique && !io.clear
    val s1 = RegEnable(candidate, q.s0_valid)
    val s1Vaddr = RegEnable(q.s0_vaddr, q.s0_valid)
    val s1Valid = RegNext(q.s0_valid && !io.clear, false.B) && q.s1_valid
    val s1Multi = RegEnable(PopCount(matches) > 1.U, q.s0_valid)
    val s1PaMatch = s1.valid && s1.block_paddr === block(q.s1_paddr)
    val s2 = RegEnable(s1, s1Valid)
    val s2Vaddr = RegEnable(s1Vaddr, s1Valid)
    val s2ObservedBlock = RegEnable(block(q.s1_paddr), s1Valid)
    val s2Valid = RegNext(s1Valid && !io.clear, false.B) && q.s2.valid && !io.clear
    val s2PaMatch = RegEnable(s1PaMatch, s1Valid)
    val s2Multi = RegEnable(s1Multi, s1Valid)

    val a = io.assessment(w)
    a.valid := s2Valid
    a.bits.resident := s2.valid && s2PaMatch && !s2.is_pending
    a.bits.pending := s2.valid && s2PaMatch && s2.is_pending
    a.bits.unknown := !s2.valid || !s2PaMatch
    a.bits.pa_fail := s2.valid && !s2PaMatch
    a.bits.multi_hit := s2Multi
    a.bits.actual_hit := truth.hit
    a.bits.way_correct := a.bits.resident && truth.hit && s2.way === truth.way
    a.bits.owner_match := a.bits.pending && ownerMatches(s2)
    a.bits.mshr_match := a.bits.pending && truth.handled && s2.mshr_id === truth.mshr_id
    a.bits.handled := truth.handled
    a.bits.merged := truth.handled && truth.merged
    a.bits.new_alloc := truth.handled && truth.allocated
    when (a.valid) {
      assert(PopCount(Seq(a.bits.resident, a.bits.pending, a.bits.unknown)) === 1.U)
      assert(s2.idx === get_dcache_idx(truth.vaddr) || !s2.valid,
        "MissTrack query and load result must refer to the same attempt")
      assert(s2Vaddr(VAddrBits - 1, blockOffBits) === truth.vaddr(VAddrBits - 1, blockOffBits),
        "MissTrack s0 and s2 virtual addresses must refer to the same attempt")
      assert(s2ObservedBlock === block(truth.paddr),
        "MissTrack s1 and s2 physical addresses must refer to the same attempt")
    }
  }

  // Collapse simultaneous observations of the same line before insertion. The
  // events are ordered by semantic priority: installation, allocation, then hit.
  val canonicalEvents = Wire(Vec(events.length, Valid(new MissTrackEntry)))
  for (i <- events.indices) {
    canonicalEvents(i).bits := events(i).bits
    val shadowed = if (i == 0) false.B else VecInit((0 until i).map { j =>
      events(j).valid && sameLine(events(j).bits, events(i).bits)
    }).asUInt.orR
    canonicalEvents(i).valid := events(i).valid && !shadowed
  }

  // Priority is local to a matching line: installation > allocation > hit.
  // Other lines' events cannot suppress maintenance of an existing record.
  val maintained = WireDefault(entries)
  val retired = Wire(Vec(cfg.missTrackEntries, Bool()))
  val invalidated = Wire(Vec(cfg.missTrackEntries, Bool()))
  for (i <- 0 until cfg.missTrackEntries) {
    val e = entries(i)
    retired(i) := e.valid && e.is_pending && !ownerMatches(e)
    invalidated(i) := e.valid && !e.is_pending && overlaps(e.idx, e.way, io.invalidate)
    when (e.valid && e.age =/= 7.U) { maintained(i).age := e.age + 1.U }
    when (retired(i) || invalidated(i)) { maintained(i).valid := false.B }
    val updates = events.map(u => u.valid && e.valid && sameLine(e, u.bits))
    when (VecInit(updates).asUInt.orR) {
      maintained(i) := PriorityMux(updates.zip(events.map(_.bits)))
    }
  }

  val insert = Module(new RRArbiter(new MissTrackEntry, events.length))
  for (i <- events.indices) {
    insert.io.in(i).valid := !io.clear && canonicalEvents(i).valid &&
      !VecInit(maintained.map(e => e.valid && sameLine(e, canonicalEvents(i).bits))).asUInt.orR
    insert.io.in(i).bits := canonicalEvents(i).bits
  }
  insert.io.out.ready := true.B
  val invalids = VecInit(maintained.map(!_.valid))
  val maxAge = maintained.map(_.age).reduce((a, b) => Mux(a > b, a, b))
  val victim = Mux(invalids.asUInt.orR, PriorityEncoder(invalids),
    PriorityEncoder(maintained.map(_.age === maxAge)))
  val nextEntries = WireDefault(maintained)
  when (insert.io.out.fire) { nextEntries(victim) := insert.io.out.bits }
  when (io.clear) { nextEntries.foreach(_.valid := false.B) }
  // Queries see maintenance and accepted training from this cycle. This closes
  // the otherwise avoidable cycle between a MissEntry becoming live and PENDING.
  lookupEntries := nextEntries
  entries := nextEntries

  // Hash collisions are allowed; duplicate (physical block, cache set) records aren't.
  for (i <- entries.indices; j <- 0 until i) {
    assert(!(entries(i).valid && entries(j).valid && sameLine(entries(i), entries(j))))
  }

  XSPerfAccumulate("mtrack_queries", PopCount(io.loads.map(_.s0_valid)))
  XSPerfAccumulate("mtrack_update_hit_seen", PopCount(io.loads.map(q => q.s2.valid && q.s2.bits.hit)))
  XSPerfAccumulate("mtrack_update_alloc_seen", PopCount(io.alloc.map(_.valid)))
  XSPerfAccumulate("mtrack_update_install", io.install.valid)
  XSPerfAccumulate("mtrack_pending_retired", PopCount(retired))
  XSPerfAccumulate("mtrack_resident_invalidated", PopCount(invalidated))
  XSPerfAccumulate("mtrack_insert", insert.io.out.fire)
  XSPerfAccumulate("mtrack_insert_not_selected", PopCount(insert.io.in.map(_.valid)) - insert.io.out.fire.asUInt)
  XSPerfAccumulate("mtrack_event_coalesced", PopCount(events.map(_.valid)) - PopCount(canonicalEvents.map(_.valid)))
  XSPerfAccumulate("mtrack_clear", io.clear)

  // Same truth matrix for all attempts and first issues; PA failures are UNKNOWN.
  for ((prefix, firstOnly) <- Seq("attempt" -> false, "first" -> true)) {
    def count(name: String)(f: MissTrackAssessment => Bool): Unit = {
      XSPerfAccumulate(s"mtrack_${prefix}_$name", PopCount(io.assessment.zipWithIndex.map { case (a, w) =>
        a.valid && (if (firstOnly) io.loads(w).s2.bits.first_issue else true.B) && f(a.bits)
      }))
    }
    count("evaluated")(_ => true.B)
    count("resident_hit")(a => a.resident && a.actual_hit)
    count("resident_miss")(a => a.resident && !a.actual_hit)
    count("resident_way_correct")(_.way_correct)
    count("pending_hit")(a => a.pending && a.actual_hit)
    count("pending_miss")(a => a.pending && !a.actual_hit)
    count("pending_owner_match")(_.owner_match)
    count("pending_merged")(a => a.pending && a.merged)
    count("pending_merged_same_id")(a => a.pending && a.merged && a.mshr_match)
    count("pending_merged_wrong_id")(a => a.pending && a.merged && !a.mshr_match)
    count("pending_new_alloc")(a => a.pending && a.new_alloc)
    count("pending_unhandled_miss")(a => a.pending && !a.actual_hit && !a.handled)
    count("unknown_hit")(a => a.unknown && a.actual_hit)
    count("unknown_miss")(a => a.unknown && !a.actual_hit)
    count("unknown_new_alloc")(a => a.unknown && a.new_alloc)
    count("unknown_merged")(a => a.unknown && a.merged)
    count("unknown_handled_other")(a => a.unknown && !a.actual_hit && a.handled && !a.merged && !a.new_alloc)
    count("unknown_unhandled_miss")(a => a.unknown && !a.actual_hit && !a.handled)
    count("new_alloc")(_.new_alloc)
    count("pa_fail")(_.pa_fail)
    count("multi_hit")(_.multi_hit)
  }
}
