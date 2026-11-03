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

class MissTrackLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s0_valid = Bool()
  val s0_vaddr = UInt(VAddrBits.W)
  val s1_valid = Bool()
  val s1_paddr = UInt(PAddrBits.W)
  // Accurate resident-hit training, independent of WPU prediction.
  val s2 = Valid(new MissTrackResident)
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

// Query snapshots retain only the lookup key and physical block.
class MissTrackLookupEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val idx = UInt(idxBits.W)
  val src_hash = UInt(cfg.missTrackHashBits.W)
  val block_paddr = UInt((PAddrBits - blockOffBits).W)
}

/** Small, lossy history table for speculative-miss eligibility.
  * Each (idx, src_hash) retains at most one physical line. A colliding line may
  * replace that hint; the full physical-block check still qualifies suppression.
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
    // S1 permission to prepare a request; accurate S2 checks still own confirmation.
    val spec = Output(Vec(LoadPipelineWidth, Bool()))
  })

  private val entries = RegInit(VecInit(Seq.fill(cfg.missTrackEntries)(0.U.asTypeOf(new MissTrackEntry))))
  private def block(pa: UInt): UInt = pa(PAddrBits - 1, blockOffBits)
  private def hash(va: UInt): UInt = XORFold(va(VAddrBits - 1, untagBits), cfg.missTrackHashBits)
  private def sameLine(a: MissTrackEntry, b: MissTrackEntry): Bool =
    a.idx === b.idx && a.block_paddr === b.block_paddr
  private def sameKey(a: MissTrackEntry, b: MissTrackEntry): Bool =
    a.idx === b.idx && a.src_hash === b.src_hash
  private val ownerIndices = VecInit(io.owners.map(o => get_dcache_idx(o.bits.vaddr)))
  private def ownerMatches(e: MissTrackEntry): Bool = {
    val owner = io.owners(e.mshr_id)
    owner.valid && block(owner.bits.paddr) === e.block_paddr &&
      ownerIndices(e.mshr_id) === e.idx
  }
  private def overlaps(idx: UInt, way: UInt, inv: Valid[MissTrackInvalidate]): Bool =
    inv.valid && inv.bits.idx === idx && (inv.bits.way & way).orR

  // A hit observed in s1 is trained in s2. Do not resurrect a line invalidated
  // either during that observation or during the training cycle.
  val previousInvalidate = RegNext(io.invalidate, 0.U.asTypeOf(io.invalidate))
  val previousClear = RegNext(io.clear, false.B)
  val previousAllocs = RegNext(io.alloc, 0.U.asTypeOf(io.alloc))
  val events = Wire(Vec(1 + allocPorts + LoadPipelineWidth, Valid(new MissTrackEntry)))
  events := 0.U.asTypeOf(events)

  // All lanes sample the same pre-maintenance table. Never use nextEntries or
  // the live S1 table here: same-edge allocation/retirement must not affect the
  // S0 observation. This register-to-register snapshot has no query-VA enable.
  val lookupTable = Wire(Vec(cfg.missTrackEntries, new MissTrackLookupEntry))
  for (i <- entries.indices) {
    lookupTable(i).valid := entries(i).valid
    lookupTable(i).idx := entries(i).idx
    lookupTable(i).src_hash := entries(i).src_hash
    lookupTable(i).block_paddr := entries(i).block_paddr
  }
  val lookupSnapshot = RegNext(lookupTable)

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
    // MissQueue emits alloc only after the allocation commits. The registered
    // owner arrives one cycle later and owns liveness checks from then on.
    events(1 + i).valid := a.valid
  }
  for (a <- previousAllocs) {
    val owner = io.owners(a.bits.mshr_id)
    when (a.valid) {
      assert(owner.valid && block(owner.bits.paddr) === block(a.bits.paddr) &&
        get_dcache_idx(owner.bits.vaddr) === get_dcache_idx(a.bits.vaddr),
        "MissTrack allocation must match its registered owner on the following cycle")
    }
  }
  for (w <- 0 until LoadPipelineWidth) {
    val q = io.loads(w)
    val truth = q.s2.bits
    val training = events(1 + allocPorts + w)
    training.bits := makeEntry(truth.vaddr, truth.paddr, truth.way, false.B, 0.U)
    val truthIdx = get_dcache_idx(truth.vaddr)
    training.valid := q.s2.valid && PopCount(truth.way) === 1.U && !previousClear &&
      !overlaps(truthIdx, truth.way, io.invalidate) &&
      !overlaps(truthIdx, truth.way, previousInvalidate)

    val queryIdx = RegNext(get_dcache_idx(q.s0_vaddr))
    val queryHash = RegNext(hash(q.s0_vaddr))
    val sampled = RegNext(q.s0_valid, false.B)
    val matches = VecInit(lookupSnapshot.map(e => e.valid && e.idx === queryIdx && e.src_hash === queryHash))
    val hit = ParallelORR(matches)
    // Unique query keys make the masked-OR selection one-hot.
    val blockGroups = matches.zip(lookupSnapshot.map(_.block_paddr)).grouped(4).map(ParallelMux(_)).toSeq
    val selectedBlock = ParallelOR(blockGroups)
    val paMatch = hit && !previousClear && selectedBlock === block(q.s1_paddr)
    // UNKNOWN after PA validation is the only state eligible for a new
    // speculative miss.  A live resident/pending line suppresses duplication.
    io.spec(w) := q.s1_valid && sampled && !io.clear && !paMatch
  }

  // Events must agree on one winner for both physical-line and query-key ties.
  // Priority is installation, allocation, then hit. Losing observations only
  // reduce history coverage; they never change the actual cache or MSHR state.
  val canonicalEvents = Wire(Vec(events.length, Valid(new MissTrackEntry)))
  for (i <- events.indices) {
    canonicalEvents(i).bits := events(i).bits
    val shadowed = if (i == 0) false.B else VecInit((0 until i).map { j =>
      events(j).valid && (sameLine(events(j).bits, events(i).bits) ||
        sameKey(events(j).bits, events(i).bits))
    }).asUInt.orR
    canonicalEvents(i).valid := events(i).valid && !shadowed
  }

  // Physical-line matches maintain state in place, including alias/hash updates.
  // Key matches are separate: they may refer to a different physical line.
  val eventMatches = canonicalEvents.map(u => entries.map(e => e.valid && sameLine(e, u.bits)))
  val eventKeyMatches = canonicalEvents.map(u => entries.map(e => e.valid && sameKey(e, u.bits)))
  val eventPresent = eventMatches.map(ParallelORR(_))
  val refreshing = canonicalEvents.indices.map(j => canonicalEvents(j).valid && eventPresent(j))
  val maintained = WireDefault(entries)
  val retired = Wire(Vec(cfg.missTrackEntries, Bool()))
  val invalidated = Wire(Vec(cfg.missTrackEntries, Bool()))
  val updated = Wire(Vec(cfg.missTrackEntries, Bool()))
  for (i <- 0 until cfg.missTrackEntries) {
    val e = entries(i)
    retired(i) := e.valid && e.is_pending && !ownerMatches(e)
    invalidated(i) := e.valid && !e.is_pending && overlaps(e.idx, e.way, io.invalidate)
    // Canonical events matching this entry are already mutually exclusive.
    val updates = canonicalEvents.indices.map(j => canonicalEvents(j).valid && eventMatches(j)(i))
    updated(i) := ParallelORR(updates)
    // A physical-line refresh may move to another hash through a VA alias.
    // Drop the old holder of that destination key unless it is also moving.
    val keyDisplaced = !updated(i) && ParallelORR(canonicalEvents.indices.map(j =>
      refreshing(j) && eventKeyMatches(j)(i)
    ))
    when (e.valid && e.age =/= 7.U) { maintained(i).age := e.age + 1.U }
    when (retired(i) || invalidated(i) || keyDisplaced) { maintained(i).valid := false.B }
    when (updated(i)) {
      maintained(i) := ParallelMux(updates.zip(canonicalEvents.map(_.bits)))
      // These fields already equal the winning event; do not put the event
      // priority network in front of another wide line-address comparison.
      maintained(i).idx := e.idx
      maintained(i).block_paddr := e.block_paddr
    }
  }

  val insert = Module(new RRArbiter(new MissTrackEntry, events.length))
  for (i <- events.indices) {
    insert.io.in(i).valid := !io.clear && canonicalEvents(i).valid &&
      !ParallelORR(eventMatches(i))
    insert.io.in(i).bits := canonicalEvents(i).bits
  }
  insert.io.out.ready := true.B
  // Preserve invalid-first, then oldest, with lowest entry index breaking ties.
  // Generate one-hot choices directly instead of max -> equality -> priority
  // encoder -> decoder. Parallel 3-bit comparisons trade area for fewer levels.
  val invalids = maintained.map(e => !e.valid)
  val hasInvalid = ParallelORR(invalids)
  val invalidVictimOH = VecInit(invalids.indices.map { i =>
    invalids(i) && (if (i == 0) true.B else !ParallelORR(invalids.take(i)))
  }).asUInt
  val oldestVictimOH = VecInit(maintained.indices.map { i =>
    ParallelANDR(maintained.indices.filter(_ != i).map { j =>
      if (j < i) maintained(i).age > maintained(j).age
      else maintained(i).age >= maintained(j).age
    })
  }).asUInt
  // A selected new physical line replaces the old holder of its query key.
  // An entry refreshed to another key is no longer a conflict. Reuse the
  // pre-maintenance key comparisons instead of comparing the arbiter payload.
  val keyVictimOH = VecInit(entries.indices.map { i =>
    !updated(i) && ParallelORR(canonicalEvents.indices.map(j =>
      insert.io.in(j).fire && eventKeyMatches(j)(i)
    ))
  }).asUInt
  val victimOH = Mux(keyVictimOH.orR, keyVictimOH, Mux(hasInvalid, invalidVictimOH, oldestVictimOH))
  val nextEntries = WireDefault(maintained)
  for (i <- entries.indices) {
    when (insert.io.out.fire && victimOH(i)) {
      nextEntries(i) := insert.io.out.bits
    }
  }
  when (io.clear) { nextEntries.foreach(_.valid := false.B) }
  // Table maintenance becomes visible to following S0 queries after this edge.
  entries := nextEntries

  // Both query selection and physical-line maintenance must stay one-hot.
  for (i <- entries.indices; j <- 0 until i) {
    assert(!(entries(i).valid && entries(j).valid && sameLine(entries(i), entries(j))))
    assert(!(entries(i).valid && entries(j).valid && sameKey(entries(i), entries(j))))
  }

  XSPerfAccumulate("mtrack_queries", PopCount(io.loads.map(_.s0_valid)))
  XSPerfAccumulate("mtrack_update_hit_seen", PopCount(io.loads.map(_.s2.valid)))
  XSPerfAccumulate("mtrack_update_alloc_seen", PopCount(io.alloc.map(_.valid)))
  XSPerfAccumulate("mtrack_update_install", io.install.valid)
  XSPerfAccumulate("mtrack_pending_retired", PopCount(retired))
  XSPerfAccumulate("mtrack_resident_invalidated", PopCount(invalidated))
  XSPerfAccumulate("mtrack_insert", insert.io.out.fire)
  XSPerfAccumulate("mtrack_insert_not_selected", PopCount(insert.io.in.map(_.valid)) - insert.io.out.fire.asUInt)
  XSPerfAccumulate("mtrack_event_coalesced", PopCount(events.map(_.valid)) - PopCount(canonicalEvents.map(_.valid)))
  XSPerfAccumulate("mtrack_clear", io.clear)

}
