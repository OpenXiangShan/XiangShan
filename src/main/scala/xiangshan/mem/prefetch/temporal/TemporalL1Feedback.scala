package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.cache._

private[xiangshan] class TemporalL1Result(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val mshr = UInt(log2Up(cfg.nMissEntries).W)
  val cacheHit = Bool()
  val allocated = Bool()
  val merged = Bool()
  val owner = new TemporalOwner(L1TemporalPrefetcher.ContextBits)
}

/** Match feedback to the exact L1 MSHR and resident set/way selected by the cache. */
private[xiangshan] class TemporalL1Feedback(implicit p: Parameters) extends DCacheModule {
  private val contextBits = L1TemporalPrefetcher.ContextBits
  private val mshrBits = log2Up(cfg.nMissEntries)
  private val lineBits = PAddrBits - blockOffBits
  private val residentSlots = nSets * nWays

  val io = IO(new Bundle {
    val context = Input(UInt(contextBits.W))
    val clear = Input(Bool())
    val result = Flipped(Valid(new TemporalL1Result))
    val alloc = Flipped(Vec(MissReqPortCount, Valid(UInt(mshrBits.W))))
    val sent = Flipped(Vec(cfg.numMemChannels, Valid(new Bundle {
      val mshr = UInt(mshrBits.W)
      val paddr = UInt(PAddrBits.W)
    })))
    val refill = Flipped(Valid(UInt(mshrBits.W)))
    val install = Flipped(Valid(new MissTrackResident))
    val invalidate = Flipped(Valid(new MissTrackInvalidate))
    val storeHit = Flipped(Valid(new MissTrackResident))
    val loads = Flipped(Vec(LoadPipelineWidth, Valid(new MissTrackLoadResult)))
    val loadMiss = Flipped(Vec(LoadPipelineWidth, Valid(UInt(mshrBits.W))))
    val feedback = Decoupled(new TemporalFeedback(contextBits))
  })

  private class Owner extends Bundle {
    val id = new TemporalOwner(contextBits)
    val line = UInt(lineBits.W)
    val set = UInt(idxBits.W)
    val sent = Bool()
    val settled = Bool()
    val late = Bool()
    val read = Bool()
  }

  private val inFlight = Reg(Vec(cfg.nMissEntries, new Owner))
  private val inflightValid = RegInit(VecInit.fill(cfg.nMissEntries)(false.B))
  private val resident = Reg(Vec(residentSlots, new Owner))
  private val residentValid = RegInit(VecInit.fill(residentSlots)(false.B))
  private val events = scala.collection.mutable.ArrayBuffer.empty[ValidIO[TemporalFeedback]]

  private def slot(set: UInt, way: UInt): UInt = {
    val idx = (set * nWays.U) + OHToUInt(way)
    idx(log2Ceil(residentSlots) - 1, 0)
  }

  private def emit(valid: Bool, owner: TemporalOwner): TemporalFeedback = {
    val event = Wire(Valid(new TemporalFeedback(contextBits)))
    event.valid := valid && !io.clear
    event.bits := 0.U.asTypeOf(event.bits)
    event.bits.owner := owner
    events += event
    event.bits
  }

  for (alloc <- io.alloc) {
    when(alloc.valid) {
      inflightValid(alloc.bits) := false.B
    }
  }

  private val result = io.result.bits
  private val newOwner = inFlight(result.mshr)
  when(io.result.valid && result.allocated) {
    inflightValid(result.mshr) := true.B
    newOwner := 0.U.asTypeOf(newOwner)
    newOwner.id := result.owner
    newOwner.line := result.paddr >> blockOffBits
    newOwner.set := addr_to_dcache_set(result.vaddr)
  }
  emit(io.result.valid && result.merged, result.owner).legacyUseful := true.B
  XSPerfAccumulate("tp_cache_duplicate", io.result.valid && result.cacheHit)
  XSPerfAccumulate("tp_mshr_allocated", io.result.valid && result.allocated)
  XSPerfAccumulate("tp_mshr_duplicate", io.result.valid && result.merged)
  XSPerfAccumulate("tp_l1_rejected", io.result.valid && !result.cacheHit && !result.allocated && !result.merged)

  for ((sent, channel) <- io.sent.zipWithIndex) {
    val id = sent.bits.mshr
    val owner = inFlight(id)
    val matches = inflightValid(id) && owner.line === (sent.bits.paddr >> blockOffBits)
    val first = sent.valid && matches && !owner.sent
    when(first) {
      owner.sent := true.B
    }
    emit(first, owner.id).sent := true.B
    XSPerfAccumulate(s"tp_l2_sent_$channel", first)
  }

  private val hitSlots = io.loads.map(load => slot(addr_to_dcache_set(load.bits.vaddr), load.bits.way))
  private val readHits = io.loads.zip(hitSlots).map { case (load, index) =>
    val owner = resident(index)
    load.valid && load.bits.hit && load.bits.way.orR && residentValid(index) &&
      owner.line === (load.bits.paddr >> blockOffBits) && owner.id.context === io.context && !owner.read
  }
  private val lateHits = io.loads.zip(io.loadMiss).map { case (load, miss) =>
    val owner = inFlight(miss.bits)
    load.valid && miss.valid && inflightValid(miss.bits) &&
      owner.line === (load.bits.paddr >> blockOffBits) &&
      owner.set === addr_to_dcache_set(load.bits.vaddr) && owner.id.context === io.context && !owner.late
  }
  private val sameCycleLate = io.loads.zip(io.loadMiss).map { case (load, miss) =>
    load.valid && miss.valid && miss.bits === result.mshr &&
      get_block_addr(load.bits.paddr) === get_block_addr(result.paddr) &&
      addr_to_dcache_set(load.bits.vaddr) === addr_to_dcache_set(result.vaddr)
  }.reduce(_ || _)
  private val allocatedLate = io.result.valid && result.allocated && sameCycleLate
  when(allocatedLate) {
    newOwner.settled := true.B
    newOwner.late := true.B
  }
  private val allocLateFeedback = emit(allocatedLate, result.owner)
  allocLateFeedback.legacyUseful := true.B
  allocLateFeedback.adjustRecord := true.B
  allocLateFeedback.priority := 1.U

  for (lane <- 0 until LoadPipelineWidth) {
    val index = hitSlots(lane)
    val owner = resident(index)
    val earlierRead = (0 until lane).map(i => readHits(i) && hitSlots(i) === index).foldLeft(false.B)(_ || _)
    val read = readHits(lane) && !earlierRead
    val timely = !owner.late
    when(read) {
      owner.read := true.B
      owner.settled := true.B
    }
    val feedback = emit(read, owner.id)
    feedback.useful := owner.sent
    feedback.timely := owner.sent && timely
    feedback.legacyUseful := !owner.settled
    feedback.legacyTimely := !owner.settled && timely
    feedback.adjustRecord := !owner.settled
    feedback.priority := Mux(timely, 0.U, 1.U)
    XSPerfAccumulate(s"tp_read_timely_$lane", read && timely)
    XSPerfAccumulate(s"tp_read_delayed_$lane", read && !timely)

    val miss = io.loadMiss(lane).bits
    val missOwner = inFlight(miss)
    val earlierLate = (0 until lane).map(i => lateHits(i) && io.loadMiss(i).bits === miss).foldLeft(false.B)(_ || _)
    val late = lateHits(lane) && !earlierLate
    when(late) {
      missOwner.settled := true.B
      missOwner.late := true.B
    }
    val lateFeedback = emit(late && !missOwner.settled, missOwner.id)
    lateFeedback.legacyUseful := true.B
    lateFeedback.adjustRecord := true.B
    lateFeedback.priority := 1.U
    XSPerfAccumulate(s"tp_demand_merge_$lane", late)
  }

  private val storeSlot = slot(addr_to_dcache_set(io.storeHit.bits.vaddr), io.storeHit.bits.way)
  private val storeOwner = resident(storeSlot)
  private val readWithStore = readHits.zip(hitSlots).map { case (hit, index) =>
    hit && index === storeSlot
  }.reduce(_ || _)
  private val store = io.storeHit.valid && io.storeHit.bits.way.orR && residentValid(storeSlot) &&
    storeOwner.line === (io.storeHit.bits.paddr >> blockOffBits) &&
    storeOwner.id.context === io.context && !storeOwner.settled && !readWithStore
  when(store) {
    storeOwner.settled := true.B
  }
  private val storeFeedback = emit(store, storeOwner.id)
  storeFeedback.legacyUseful := true.B
  storeFeedback.legacyTimely := true.B
  storeFeedback.adjustRecord := true.B

  private val evictSlot = slot(io.invalidate.bits.idx, io.invalidate.bits.way)
  private val evicted = resident(evictSlot)
  private val readWithEvict = readHits.zip(hitSlots).map { case (hit, index) =>
    hit && index === evictSlot
  }.reduce(_ || _)
  private val evict = io.invalidate.valid && io.invalidate.bits.way.orR && residentValid(evictSlot)
  when(evict) {
    residentValid(evictSlot) := false.B
  }
  private val unused = evict && !evicted.read && !readWithEvict
  private val evictFeedback = emit(unused && !evicted.settled && !(store && storeSlot === evictSlot), evicted.id)
  evictFeedback.adjustRecord := true.B
  evictFeedback.priority := 3.U
  XSPerfAccumulate("tp_unused", unused)

  private val filled = inFlight(io.refill.bits)
  private val filledSet = addr_to_dcache_set(io.install.bits.vaddr)
  private val filledSlot = slot(filledSet, io.install.bits.way)
  private val lateWithFill = lateHits.zip(io.loadMiss).map { case (hit, miss) =>
    hit && miss.bits === io.refill.bits
  }.reduce(_ || _)
  when(io.refill.valid) {
    inflightValid(io.refill.bits) := false.B
    when(io.install.valid && inflightValid(io.refill.bits) &&
      filled.line === (io.install.bits.paddr >> blockOffBits) && filled.set === filledSet) {
      residentValid(filledSlot) := true.B
      resident(filledSlot) := filled
      resident(filledSlot).late := filled.late || lateWithFill
      resident(filledSlot).settled := filled.settled || lateWithFill
    }
  }

  when(io.clear) {
    inflightValid.foreach(_ := false.B)
    residentValid.foreach(_ := false.B)
  }

  private val arb = Module(new RRArbiter(new TemporalFeedback(contextBits), events.size))
  for (((event, output), index) <- events.toSeq.zip(arb.io.in).zipWithIndex) {
    val queue = Module(new Queue(new TemporalFeedback(contextBits), 8, hasFlush = true))
    queue.io.flush.get := io.clear
    queue.io.enq.valid := event.valid
    queue.io.enq.bits := event.bits
    output <> queue.io.deq
    XSPerfAccumulate(s"tp_feedback_queue_full_$index", event.valid && !queue.io.enq.ready)
  }
  io.feedback <> arb.io.out
}
