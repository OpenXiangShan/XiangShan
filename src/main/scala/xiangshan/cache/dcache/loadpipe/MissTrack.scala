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

class MissTrackResident(implicit p: Parameters) extends DCacheBundle {
  val vaddr = UInt(VAddrBits.W)
  val way = UInt(nWays.W)
}

class MissTrackInvalidate(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way = UInt(nWays.W)
}

class MissTrackLoadIO(implicit p: Parameters) extends DCacheBundle {
  val s0_valid = Bool()
  val s0_vaddr = UInt(VAddrBits.W)
  val s1_valid = Bool()
  // Accurate resident-hit training, independent of WPU prediction.
  val s2 = Valid(new MissTrackResident)
}

class MissTrackEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val idx = UInt(idxBits.W)
  val src_hash = UInt(cfg.missTrackHashBits.W)
  val way_index = UInt(wayBits.W)
}

// Approximate queries need only the lookup key, not the physical block.
class MissTrackLookupEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val idx = UInt(idxBits.W)
  val src_hash = UInt(cfg.missTrackHashBits.W)
}

/** Small, lossy history table of confirmed L1 residents.
  * Existing records are maintained in parallel. One new record per cycle is sampled
  * by a round-robin arbiter; losing insertion opportunities affects coverage only.
  * In-flight transactions are tracked and deduplicated exclusively by MissQueue.
  * Queries use only set/hash: a unique key can suppress an unrelated physical
  * line's speculative candidate. The normal S2 miss path still handles that line.
  */
class MissTrack(implicit p: Parameters) extends DCacheModule {
  require(cfg.missTrackEntries >= 2)
  require(cfg.missTrackHashBits > 0 && cfg.missTrackHashBits <= VAddrBits - untagBits)

  val io = IO(new Bundle {
    val loads = Input(Vec(LoadPipelineWidth, new MissTrackLoadIO))
    val install = Flipped(Valid(new MissTrackResident))
    // Tag replacement OR meta invalidation, at the actual write handshake.
    val invalidate = Flipped(Valid(new MissTrackInvalidate))
    val clear = Input(Bool())
    // S1 permission to prepare a request; accurate S2 checks still own confirmation.
    val spec = Output(Vec(LoadPipelineWidth, Bool()))
  })

  private val entries = RegInit(VecInit(Seq.fill(cfg.missTrackEntries)(0.U.asTypeOf(new MissTrackEntry))))
  private def hash(va: UInt): UInt = XORFold(va(VAddrBits - 1, untagBits), cfg.missTrackHashBits)
  private def sameSlot(a: MissTrackEntry, b: MissTrackEntry): Bool =
    a.idx === b.idx && a.way_index === b.way_index
  private def overlaps(idx: UInt, way: UInt, inv: Valid[MissTrackInvalidate]): Bool =
    inv.valid && inv.bits.idx === idx && (inv.bits.way & way).orR

  // A hit observed in s1 is trained in s2. Do not resurrect a line invalidated
  // either during that observation or during the training cycle.
  val previousInvalidate = RegNext(io.invalidate, 0.U.asTypeOf(io.invalidate))
  val previousClear = RegNext(io.clear, false.B)
  val events = Wire(Vec(1 + LoadPipelineWidth, Valid(new MissTrackEntry)))
  events := 0.U.asTypeOf(events)
  // Count existing S1 decisions without a separate monitor or truth interface.
  val queryValid = Wire(Vec(LoadPipelineWidth, Bool()))
  val filterHit = Wire(Vec(LoadPipelineWidth, Bool()))

  // All lanes sample the same pre-maintenance table. Never use nextEntries or
  // the live S1 table here: same-edge training/invalidation must not affect the
  // S0 observation. This register-to-register snapshot has no query-VA enable.
  val lookupTable = Wire(Vec(cfg.missTrackEntries, new MissTrackLookupEntry))
  for (i <- entries.indices) {
    lookupTable(i).valid := entries(i).valid
    lookupTable(i).idx := entries(i).idx
    lookupTable(i).src_hash := entries(i).src_hash
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

  def makeEntry(va: UInt, way: UInt): MissTrackEntry = {
    val e = WireDefault(0.U.asTypeOf(new MissTrackEntry))
    e.valid := true.B
    e.idx := get_dcache_idx(va)
    e.src_hash := hash(va)
    e.way_index := OHToUInt(way)
    e
  }

  // The wrapper delays a completed MainPipe installation by one cycle. A newer
  // invalidation wins; the previous invalidation may be this install's own write.
  events(0).valid := io.install.valid && PopCount(io.install.bits.way) === 1.U &&
    !overlaps(get_dcache_idx(io.install.bits.vaddr), io.install.bits.way, io.invalidate)
  events(0).bits := makeEntry(io.install.bits.vaddr, io.install.bits.way)
  for (w <- 0 until LoadPipelineWidth) {
    val q = io.loads(w)
    val truth = q.s2.bits
    val training = events(1 + w)
    training.bits := makeEntry(truth.vaddr, truth.way)
    val truthIdx = get_dcache_idx(truth.vaddr)
    training.valid := q.s2.valid && PopCount(truth.way) === 1.U && !previousClear &&
      !overlaps(truthIdx, truth.way, io.invalidate) &&
      !overlaps(truthIdx, truth.way, previousInvalidate)

    val queryIdx = RegNext(get_dcache_idx(q.s0_vaddr))
    val queryHash = RegNext(hash(q.s0_vaddr))
    val sampled = RegNext(q.s0_valid, false.B)
    val matches = VecInit(lookupSnapshot.map(e => e.valid && e.idx === queryIdx && e.src_hash === queryHash))
    val multi = ParallelORR(matches.zip(collisionSnapshot).map { case (hit, collision) => hit && collision })
    val unique = ParallelORR(matches) && !multi
    val keyMatch = unique && !previousClear
    // A unique key predicts a resident without waiting for the S1 physical address.
    // Hash aliases can lose an early reservation; normal S2 miss handling remains.
    // Ambiguous keys bypass this filter; MissQueue owns duplicate checks.
    queryValid(w) := q.s1_valid && sampled && !io.clear
    io.spec(w) := queryValid(w) && !keyMatch
    filterHit(w) := queryValid(w) && keyMatch
  }

  // Select one event per cache slot for both maintenance and insertion. The events
  // are ordered by semantic priority: installation, then hit.
  val canonicalEvents = Wire(Vec(events.length, Valid(new MissTrackEntry)))
  for (i <- events.indices) {
    canonicalEvents(i).bits := events(i).bits
    val shadowed = if (i == 0) false.B else VecInit((0 until i).map { j =>
      events(j).valid && sameSlot(events(j).bits, events(i).bits)
    }).asUInt.orR
    canonicalEvents(i).valid := events(i).valid && !shadowed
  }

  // Priority is local to a matching cache slot: installation > hit.
  val eventMatches = canonicalEvents.map(u => entries.map(e => e.valid && sameSlot(e, u.bits)))
  val maintained = WireDefault(entries)
  val invalidated = Wire(Vec(cfg.missTrackEntries, Bool()))
  for (i <- 0 until cfg.missTrackEntries) {
    val e = entries(i)
    invalidated(i) := e.valid && io.invalidate.valid &&
      io.invalidate.bits.idx === e.idx && io.invalidate.bits.way(e.way_index)
    when (invalidated(i)) { maintained(i).valid := false.B }
    // Canonical events matching this entry are already mutually exclusive.
    val updates = canonicalEvents.indices.map(j => canonicalEvents(j).valid && eventMatches(j)(i))
    when (ParallelORR(updates)) {
      maintained(i) := ParallelMux(updates.zip(canonicalEvents.map(_.bits)))
      // Slot identity is unchanged; refresh only its query key.
      maintained(i).idx := e.idx
      maintained(i).way_index := e.way_index
    }
  }

  val insert = Module(new RRArbiter(new MissTrackEntry, events.length))
  for (i <- events.indices) {
    insert.io.in(i).valid := !io.clear && canonicalEvents(i).valid &&
      !ParallelORR(eventMatches(i))
    insert.io.in(i).bits := canonicalEvents(i).bits
  }
  insert.io.out.ready := true.B
  // Prefer the first invalid record, otherwise rotate through the full table.
  // Refreshes do not move the pointer; each insertion advances past its victim.
  val nextVictim = RegInit(0.U(log2Ceil(cfg.missTrackEntries).W))
  val invalids = maintained.map(e => !e.valid)
  val hasInvalid = ParallelORR(invalids)
  val invalidVictimOH = VecInit(invalids.indices.map { i =>
    invalids(i) && (if (i == 0) true.B else !ParallelORR(invalids.take(i)))
  }).asUInt
  val victimOH = Mux(hasInvalid, invalidVictimOH, UIntToOH(nextVictim, cfg.missTrackEntries))
  when (io.clear) {
    nextVictim := 0.U
  }.elsewhen (insert.io.out.fire) {
    val victim = OHToUInt(victimOH)
    nextVictim := Mux(victim === (cfg.missTrackEntries - 1).U, 0.U, victim + 1.U)
  }
  assert(nextVictim < cfg.missTrackEntries.U)
  val nextEntries = WireDefault(maintained)
  for (i <- entries.indices) {
    when (insert.io.out.fire && victimOH(i)) {
      nextEntries(i) := insert.io.out.bits
    }
  }
  when (io.clear) { nextEntries.foreach(_.valid := false.B) }
  // Table maintenance becomes visible to following S0 queries after this edge.
  entries := nextEntries

  // Hash collisions are allowed; each cache set/way has at most one record.
  for (i <- entries.indices; j <- 0 until i) {
    assert(!(entries(i).valid && entries(j).valid && sameSlot(entries(i), entries(j))))
  }

  XSPerfAccumulate("mtrack_queries", PopCount(io.loads.map(_.s0_valid)))
  XSPerfAccumulate("mtrack_s1_queries", PopCount(queryValid))
  XSPerfAccumulate("mtrack_hits", PopCount(filterHit))
  XSPerfAccumulate("mtrack_eligible", PopCount(io.spec))
  XSPerfAccumulate("mtrack_update_hit_seen", PopCount(io.loads.map(_.s2.valid)))
  XSPerfAccumulate("mtrack_update_install", io.install.valid)
  XSPerfAccumulate("mtrack_resident_invalidated", PopCount(invalidated))
  XSPerfAccumulate("mtrack_insert", insert.io.out.fire)
  XSPerfAccumulate("mtrack_insert_not_selected", PopCount(insert.io.in.map(_.valid)) - insert.io.out.fire.asUInt)
  XSPerfAccumulate("mtrack_event_coalesced", PopCount(events.map(_.valid)) - PopCount(canonicalEvents.map(_.valid)))
  XSPerfAccumulate("mtrack_clear", io.clear)
}
