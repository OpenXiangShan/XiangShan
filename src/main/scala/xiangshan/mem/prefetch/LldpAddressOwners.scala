package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.cache.MissTrackInvalidate
import xiangshan.mem.prefetch.temporal.{L1TemporalLoad, L1TemporalPrefetcher}

/** Attribute address candidates only through their accepted MSHR and installed L1 way. */
private[prefetch] class LldpAddressOwners(implicit p: Parameters) extends PrefetchModule {
  private val slots = nSets * nWays
  val io = IO(new Bundle {
    val context = Input(UInt(L1TemporalPrefetcher.ContextBits.W))
    val prefetch = Flipped(Valid(new LldpPrefetchResult))
    val refill = Flipped(Valid(new LldpRefill))
    val invalidate = Flipped(Valid(new MissTrackInvalidate))
    val demand = Flipped(Vec(LD_TRAIN_WIDTH, Valid(new L1TemporalLoad)))
    val dropped = Flipped(Vec(2, Valid(new LldpAddressRef)))
    val feedback = Decoupled(new LldpAddressFeedback)
  })

  private class Owner extends Bundle {
    val line = UInt((PAddrBits - blockOffBits).W)
    val set = UInt(idxBits.W)
    val context = UInt(L1TemporalPrefetcher.ContextBits.W)
    val ref = new LldpAddressRef
  }

  private val misses = Reg(Vec(cfg.nMissEntries, new Owner))
  private val missValid = RegInit(VecInit.fill(cfg.nMissEntries)(false.B))
  private val resident = Reg(Vec(slots, new Owner))
  private val residentValid = RegInit(VecInit.fill(slots)(false.B))
  private val events = scala.collection.mutable.ArrayBuffer.empty[ValidIO[LldpAddressFeedback]]

  private def emit(valid: Bool, ref: LldpAddressRef, outcome: LldpAddressOutcome.Type): Unit = {
    val event = Wire(Valid(new LldpAddressFeedback))
    event.valid := valid
    event.bits.ref := ref
    event.bits.outcome := outcome
    events += event
  }

  private def slot(set: UInt, way: UInt): UInt =
    ((set * nWays.U) + OHToUInt(way))(log2Ceil(slots) - 1, 0)

  private val result = io.prefetch.bits
  private val allocated = io.prefetch.valid && result.allocated
  private val addressOwner = result.trigger.addressRef
  private val tracksAddress = allocated && addressOwner.valid
  when(allocated) {
    missValid(result.mshr) := tracksAddress
    misses(result.mshr).line := result.paddr >> blockOffBits
    misses(result.mshr).set := addr_to_dcache_set(result.vaddr)
    misses(result.mshr).context := result.trigger.context
    misses(result.mshr).ref := addressOwner.bits
  }
  emit(io.prefetch.valid && addressOwner.valid && !allocated, addressOwner.bits,
    Mux(result.merged, LldpAddressOutcome.Merged, LldpAddressOutcome.Late))
  for (drop <- io.dropped) {
    emit(drop.valid, drop.bits, LldpAddressOutcome.Dropped)
  }

  private val useSlots = io.demand.map(load => slot(addr_to_dcache_set(load.bits.vaddr), load.bits.way.get))
  private val readyHit = io.demand.zip(useSlots).map { case (load, index) =>
    val owner = resident(index)
    load.valid && !load.bits.miss && load.bits.way.get.orR && residentValid(index) &&
      owner.line === (load.bits.paddr >> blockOffBits) && owner.context === io.context
  }
  private val missIds = io.demand.map(_.bits.mshr.get)
  private val joined = io.demand.zip(missIds).map { case (load, id) =>
    val owner = misses(id)
    load.valid && load.bits.missAccepted.get && missValid(id) &&
      owner.line === (load.bits.paddr >> blockOffBits) &&
      owner.set === addr_to_dcache_set(load.bits.vaddr) && owner.context === io.context
  }
  private def anyMatch(valid: Seq[Bool], indices: Seq[UInt], index: UInt): Bool =
    valid.zip(indices).map { case (v, i) => v && i === index }.foldLeft(false.B)(_ || _)

  for (lane <- 0 until LD_TRAIN_WIDTH) {
    val index = useSlots(lane)
    val id = missIds(lane)
    val hit = readyHit(lane) && !anyMatch(readyHit.take(lane), useSlots.take(lane), index)
    val merge = joined(lane) && !anyMatch(joined.take(lane), missIds.take(lane), id)
    when(hit) {
      residentValid(index) := false.B
    }
    when(merge) {
      missValid(id) := false.B
    }
    emit(hit, resident(index).ref, LldpAddressOutcome.Useful)
    emit(merge, misses(id).ref, LldpAddressOutcome.Merged)
  }

  private val evicted = slot(io.invalidate.bits.idx, io.invalidate.bits.way)
  private val eviction = io.invalidate.valid && io.invalidate.bits.way.orR && residentValid(evicted)
  when(eviction) {
    residentValid(evicted) := false.B
  }
  emit(eviction && !anyMatch(readyHit, useSlots, evicted), resident(evicted).ref, LldpAddressOutcome.Unused)

  private val refill = io.refill.bits
  private val filled = misses(refill.mshr)
  private val filledSet = addr_to_dcache_set(refill.vaddr)
  private val filledSlot = slot(filledSet, refill.way)
  private val ownedRefill = io.refill.valid && missValid(refill.mshr) &&
    filled.line === (refill.paddr >> blockOffBits) && filled.set === filledSet &&
    !anyMatch(joined, missIds, refill.mshr)
  when(io.refill.valid) {
    missValid(refill.mshr) := false.B
  }
  when(ownedRefill && refill.readable) {
    residentValid(filledSlot) := true.B
    resident(filledSlot) := filled
  }
  emit(ownedRefill && !refill.readable, filled.ref, LldpAddressOutcome.Dropped)

  private val arb = Module(new RRArbiter(new LldpAddressFeedback, events.size))
  for (((event, output), index) <- events.toSeq.zip(arb.io.in).zipWithIndex) {
    val queue = Module(new Queue(new LldpAddressFeedback, 8))
    queue.io.enq.valid := event.valid
    queue.io.enq.bits := event.bits
    output <> queue.io.deq
    XSPerfAccumulate(s"lldp_address_feedback_overflow_$index", event.valid && !queue.io.enq.ready)
  }
  io.feedback <> arb.io.out
}
