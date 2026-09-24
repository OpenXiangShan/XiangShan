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
}

class MissTrackLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s0_valid = Bool()
  val s0_vaddr = UInt(VAddrBits.W)
  val s1_valid = Bool()
  val s1_paddr = UInt(PAddrBits.W)
  val s2 = Valid(new MissTrackLoadResult)
}

/** S1 qualification for the speculative-miss fast path.  This is only a
  * permission to prepare a request; the accurate S2 tag/PMP/MissQueue result
  * still owns the transaction. */
class MissTrackSpec(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val candidate = Bool()
  val resident = Bool()
  val pending = Bool()
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)
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

object MissTrack {
  def balancedHashBitPairs(addr: UInt, hi: Int, lo: Int, step: Int = 2): UInt = {
    require(hi >= lo)
    require(step > 0)
    if ((hi - lo + 1) <= step) {
      addr(hi, lo)
    } else {
      val remain = (hi - lo + 1) % step
      val chunks = (lo to (hi - remain) by step).map(i => addr(i + step - 1, i))
      val xor = ParallelXOR(chunks)
      if (remain == 0) xor else Cat(xor(step - 1, remain), xor(remain - 1, 0) ^ addr(hi, hi - remain + 1))
    }
  }
}

/** Small, lossy history table for speculative-miss eligibility.
  * Existing records are maintained in parallel. One new record per cycle is sampled
  * by a round-robin arbiter; losing insertion opportunities affects coverage only.
  */
class MissTrack(allocPorts: Int)(implicit p: Parameters) extends DCacheModule {
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
    val spec = Output(Vec(LoadPipelineWidth, new MissTrackSpec))
  })

  private val entries = RegInit(VecInit(Seq.fill(cfg.missTrackEntries)(0.U.asTypeOf(new MissTrackEntry))))
  private def block(pa: UInt): UInt = pa(PAddrBits - 1, blockOffBits)
  private def hash(va: UInt): UInt = XORFold(va(VAddrBits - 1, untagBits), cfg.missTrackHashBits)
  private def index(va: UInt): UInt = modeId match {
    case 1 => Cat(
      MissTrack.balancedHashBitPairs(va, PAddrBits - 1, pgIdxBits),
      va(untagBits - 1 - (untagBits - pgUntagBits), blockOffBits)
    )(idxBits - 1, 0)
    case 2 => va(untagBits - 1, blockOffBits)
    case _ => throw new IllegalArgumentException(s"Invalid MissTrack index modeId: $modeId")
  }
  private def sameLine(a: MissTrackEntry, b: MissTrackEntry): Bool =
    a.idx === b.idx && a.block_paddr === b.block_paddr
  private def ownerMatches(e: MissTrackEntry): Bool = VecInit(io.owners.zipWithIndex.map { case (o, i) =>
    o.valid && e.mshr_id === i.U && block(o.bits.paddr) === e.block_paddr &&
      index(o.bits.vaddr) === e.idx
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
    e.idx := index(va)
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
    val truthIdx = index(truth.vaddr)
    training.valid := q.s2.valid && truth.hit && PopCount(truth.way) === 1.U && !previousClear &&
      !overlaps(truthIdx, truth.way, io.invalidate) &&
      !overlaps(truthIdx, truth.way, previousInvalidate)

    val queryIdx = index(q.s0_vaddr)
    val queryHash = hash(q.s0_vaddr)
    val matches = VecInit(lookupEntries.map(e => e.valid && e.idx === queryIdx && e.src_hash === queryHash))
    val matchCount = PopCount(matches)
    val unique = matchCount === 1.U
    // The payload is ignored on zero or multiple matches.
    val candidate = WireDefault(ParallelMux(matches.zip(lookupEntries)))
    candidate.valid := q.s0_valid && unique && !io.clear
    val s1 = RegEnable(candidate, q.s0_valid)
    val s1Multi = RegEnable(matchCount > 1.U, q.s0_valid)
    val s1PaMatch = s1.valid && s1.block_paddr === block(q.s1_paddr)
    io.spec(w).valid := q.s1_valid && !io.clear && !s1Multi
    io.spec(w).resident := s1.valid && !s1.is_pending && s1PaMatch
    io.spec(w).pending := s1.valid && s1.is_pending && s1PaMatch
    io.spec(w).mshr_id := s1.mshr_id
    // UNKNOWN after PA validation is the only state eligible for a new
    // speculative miss.  A live resident/pending line suppresses duplication.
    io.spec(w).candidate := io.spec(w).valid && !io.spec(w).resident && !io.spec(w).pending && s1PaMatch === false.B
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
  // Keep invalid-first/oldest semantics while balancing both reductions.
  // This removes the 16-entry left-folded max and victim priority chain.
  val invalids = maintained.map(e => !e.valid)
  val hasInvalid = ParallelORR(invalids)
  val invalidVictimOH = UIntToOH(
    ParallelPriorityEncoder(invalids),
    cfg.missTrackEntries
  )
  val maxAge = ParallelMax(maintained.map(_.age))
  val oldest = maintained.map(_.age === maxAge)
  val oldestVictimOH = UIntToOH(
    ParallelPriorityEncoder(oldest),
    cfg.missTrackEntries
  )
  val victimOH = Mux(hasInvalid, invalidVictimOH, oldestVictimOH)
  val nextEntries = WireDefault(maintained)
  for (i <- entries.indices) {
    when (insert.io.out.fire && victimOH(i)) {
      nextEntries(i) := insert.io.out.bits
    }
  }
  when (io.clear) { nextEntries.foreach(_.valid := false.B) }
  // Table maintenance becomes visible to queries after the register boundary.
  lookupEntries := entries
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

}
