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
  private def ownerMatches(e: MissTrackEntry): Bool = {
    val owner = io.owners(e.mshr_id)
    owner.valid && block(owner.bits.paddr) === e.block_paddr &&
      get_dcache_idx(owner.bits.vaddr) === e.idx
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

  // A matched entry has a second match iff another valid entry has its key.
  // Share each unordered comparison and capture collisions with the old table,
  // removing PopCount from the arriving query's path on all lanes.
  val sameKeys = (for (i <- entries.indices; j <- 0 until i) yield {
    (i, j) -> (entries(i).idx === entries(j).idx && entries(i).src_hash === entries(j).src_hash)
  }).toMap
  val keyCollisions = VecInit(entries.indices.map { i =>
    ParallelORR(entries.indices.filter(_ != i).map { j =>
      entries(j).valid && sameKeys((i max j, i min j))
    })
  })
  val collisionSnapshot = RegNext(keyCollisions)

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
    training.valid := q.s2.valid && truth.hit && PopCount(truth.way) === 1.U && !previousClear &&
      !overlaps(truthIdx, truth.way, io.invalidate) &&
      !overlaps(truthIdx, truth.way, previousInvalidate)

    val queryIdx = RegNext(get_dcache_idx(q.s0_vaddr))
    val queryHash = RegNext(hash(q.s0_vaddr))
    val sampled = RegNext(q.s0_valid, false.B)
    val matches = VecInit(lookupSnapshot.map(e => e.valid && e.idx === queryIdx && e.src_hash === queryHash))
    val multi = ParallelORR(matches.zip(collisionSnapshot).map { case (hit, collision) => hit && collision })
    val unique = ParallelORR(matches) && !multi
    // Four-entry local muxes preserve masked-OR selection of the physical block.
    val blockGroups = matches.zip(lookupSnapshot.map(_.block_paddr)).grouped(4).map(ParallelMux(_)).toSeq
    val selectedBlock = ParallelOR(blockGroups)
    val paMatch = unique && !previousClear && selectedBlock === block(q.s1_paddr)
    // UNKNOWN after PA validation is the only state eligible for a new
    // speculative miss.  A live resident/pending line suppresses duplication.
    io.spec(w) := q.s1_valid && sampled && !io.clear && !multi && !paMatch
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
  // Compare against register Q once. Maintenance never changes an existing
  // entry's (idx, block_paddr), and any matching valid event restores valid,
  // even if that entry is simultaneously retired or invalidated. Thus an
  // active event is already present after maintenance iff it matches here.
  val eventMatches = events.map(u => entries.map(e => e.valid && sameLine(e, u.bits)))
  val maintained = WireDefault(entries)
  val retired = Wire(Vec(cfg.missTrackEntries, Bool()))
  val invalidated = Wire(Vec(cfg.missTrackEntries, Bool()))
  for (i <- 0 until cfg.missTrackEntries) {
    val e = entries(i)
    retired(i) := e.valid && e.is_pending && !ownerMatches(e)
    invalidated(i) := e.valid && !e.is_pending && overlaps(e.idx, e.way, io.invalidate)
    when (e.valid && e.age =/= 7.U) { maintained(i).age := e.age + 1.U }
    when (retired(i) || invalidated(i)) { maintained(i).valid := false.B }
    val updates = events.indices.map(j => events(j).valid && eventMatches(j)(i))
    val updateOH = updates.indices.map { j =>
      updates(j) && (if (j == 0) true.B else !ParallelORR(updates.take(j)))
    }
    when (ParallelORR(updates)) {
      maintained(i) := ParallelMux(updateOH.zip(events.map(_.bits)))
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
  val victimOH = Mux(hasInvalid, invalidVictimOH, oldestVictimOH)
  val nextEntries = WireDefault(maintained)
  for (i <- entries.indices) {
    when (insert.io.out.fire && victimOH(i)) {
      nextEntries(i) := insert.io.out.bits
    }
  }
  when (io.clear) { nextEntries.foreach(_.valid := false.B) }
  // Table maintenance becomes visible to following S0 queries after this edge.
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
