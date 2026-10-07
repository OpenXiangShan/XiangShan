package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility._
import xiangshan.XSCoreParamsKey

case class PDBVictimObserverParameters(entries: Int = 256, bankEntries: Int = 32) {
  require(entries > 0 && bankEntries > 0)
}
case object PDBVictimObserverKey extends Field[PDBVictimObserverParameters](PDBVictimObserverParameters())

class PDBVictim(val blockAddrBits: Int) extends Bundle {
  val blockAddr = UInt(blockAddrBits.W)
  val used = Bool()
  // Keep all capacity victims observable, and separate Stream evidence for H3.
  val stream = Bool()
}

class PDBVictimObservation(entries: Int, blockAddrBits: Int, lanes: Int) extends Bundle {
  val victim = Valid(new PDBVictim(blockAddrBits))
  val demand = Vec(lanes, Valid(UInt(blockAddrBits.W)))
  val usedHits = UInt(log2Ceil(lanes + 1).W)
  val unusedHits = UInt(log2Ceil(lanes + 1).W)
  val streamUsedHits = UInt(log2Ceil(lanes + 1).W)
  val streamUnusedHits = UInt(log2Ceil(lanes + 1).W)
  val duplicate = Bool()
  val eviction = Valid(new PDBVictim(blockAddrBits))
  val occupancy = UInt(log2Ceil(entries + 1).W)
}

/** Ordered, passive event pipeline. No ready, age, epoch or delayed slot writeback.
  *
  * S0 captures all lanes and the victim together. S1 compares every valid entry
  * against every lane, consumes hits, removes a duplicate, then appends the
  * victim. S2 exposes registered bank counts and the completed event batch.
  * A demand sees only victims from earlier input cycles. In a same-cycle batch,
  * demands may consume an old copy, but cannot consume the newly inserted copy.
  */
class PDBVictimObserver(
  entries: Int, blockAddrBits: Int, lanes: Int = 3, bankEntries: Int = 32
) extends Module {
  require(entries > 0 && blockAddrBits > 0 && lanes > 0 && bankEntries > 0)
  val io = IO(new Bundle {
    val victim = Input(Valid(new PDBVictim(blockAddrBits)))
    val demand = Input(Vec(lanes, Valid(UInt(blockAddrBits.W))))
    val result = Output(new PDBVictimObservation(entries, blockAddrBits, lanes))
  })

  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val data = Reg(Vec(entries, new PDBVictim(blockAddrBits)))
  val s1Victim = RegInit(0.U.asTypeOf(Valid(new PDBVictim(blockAddrBits))))
  val s1Demand = RegInit(0.U.asTypeOf(Vec(lanes, Valid(UInt(blockAddrBits.W)))))
  val s2Victim = RegInit(0.U.asTypeOf(Valid(new PDBVictim(blockAddrBits))))
  val s2Demand = RegInit(0.U.asTypeOf(Vec(lanes, Valid(UInt(blockAddrBits.W)))))
  val s2Duplicate = RegInit(false.B)
  val s2Eviction = RegInit(0.U.asTypeOf(Valid(new PDBVictim(blockAddrBits))))
  private val banks = (0 until entries).grouped(bankEntries).toSeq
  private val countBits = log2Ceil(lanes + 1)
  val bankCounts = RegInit(VecInit(Seq.fill(banks.size)(VecInit(Seq.fill(4)(0.U(countBits.W))))))

  // Physical order is FIFO order, ignoring holes. All comparisons use the same
  // pre-update table, so later inserts and shifted/reused slots cannot cause ABA.
  val laneMatch = s1Demand.map(query => VecInit((0 until entries).map(i =>
    query.valid && valid(i) && query.bits === data(i).blockAddr)))
  val hit = VecInit((0 until entries).map(i => laneMatch.map(_(i)).reduce(_ || _)))
  val duplicate = VecInit((0 until entries).map(i =>
    s1Victim.valid && valid(i) && data(i).blockAddr === s1Victim.bits.blockAddr))
  val remaining = VecInit((0 until entries).map(i => valid(i) && !hit(i) && !duplicate(i)))
  val free = ~remaining.asUInt
  val fullEviction = s1Victim.valid && !free.orR
  // Reuse the oldest hole; only a full table discards its oldest valid entry.
  val removeId = Mux(free.orR, PriorityEncoder(free), 0.U)
  val nextValid = WireInit(remaining)
  val nextData = WireInit(data)
  when (s1Victim.valid) {
    for (i <- 0 until entries - 1) {
      when (removeId <= i.U) {
        nextValid(i) := remaining(i + 1)
        nextData(i) := data(i + 1)
      }
    }
    nextValid(entries - 1) := true.B
    nextData(entries - 1) := s1Victim.bits
  }

  s1Victim := io.victim
  s1Demand := io.demand
  valid := nextValid
  when (s1Victim.valid) { data := nextData }
  s2Victim := s1Victim
  s2Demand := s1Demand
  s2Duplicate := duplicate.asUInt.orR
  s2Eviction.valid := fullEviction
  when (fullEviction) { s2Eviction.bits := data(0) }
  for ((bank, b) <- banks.zipWithIndex) {
    bankCounts(b)(0) := PopCount(bank.map(i => hit(i) && data(i).used))
    bankCounts(b)(1) := PopCount(bank.map(i => hit(i) && !data(i).used))
    bankCounts(b)(2) := PopCount(bank.map(i => hit(i) && data(i).used && data(i).stream))
    bankCounts(b)(3) := PopCount(bank.map(i => hit(i) && !data(i).used && data(i).stream))
  }

  io.result.victim := s2Victim
  io.result.demand := s2Demand
  io.result.duplicate := s2Duplicate
  io.result.eviction := s2Eviction
  io.result.occupancy := PopCount(valid)
  io.result.usedHits := bankCounts.map(_(0)).reduce(_ +& _)
  io.result.unusedHits := bankCounts.map(_(1)).reduce(_ +& _)
  io.result.streamUsedHits := bankCounts.map(_(2)).reduce(_ +& _)
  io.result.streamUnusedHits := bankCounts.map(_(3)).reduce(_ +& _)

  laneMatch.foreach(matches => assert(PopCount(matches) <= 1.U,
    "Victim address must have at most one valid copy"))
  assert(PopCount(duplicate) <= 1.U, "Duplicate victim copies in FIFO")
  assert(PopCount(hit) <= lanes.U, "More consumed victims than demand lanes")
  assert(!fullEviction || remaining(0), "FIFO must evict a valid oldest entry")
}

/** Persistent observation, statistics and optional trace. H3 consumes Stream hits. */
class PDBVictimMonitor(implicit p: Parameters) extends DCacheModule {
  private val params = p(PDBVictimObserverKey)
  private val addressBits = PAddrBits - blockOffBits
  val io = IO(new Bundle {
    val victim = Input(Valid(new PDBVictim(addressBits)))
    val demand = Input(Vec(LoadPipelineWidth, Valid(UInt(addressBits.W))))
    val streamUsedHits, streamUnusedHits = Output(UInt(log2Ceil(LoadPipelineWidth + 1).W))
  })
  val hart = p(XSCoreParamsKey).HartId
  val enabled = Constantin.createRecord(s"enablePDBVictimObserver$hart", initValue = true)
  val traceEnabled = Constantin.createRecord(s"tracePDBVictimObserver$hart", initValue = true)
  val observer = Module(new PDBVictimObserver(params.entries, addressBits, LoadPipelineWidth, params.bankEntries))
  observer.io.victim := io.victim
  observer.io.victim.valid := enabled && io.victim.valid
  observer.io.demand := io.demand
  observer.io.demand.zip(io.demand).foreach { case (out, in) => out.valid := enabled && in.valid }
  val result = observer.io.result
  io.streamUsedHits := result.streamUsedHits
  io.streamUnusedHits := result.streamUnusedHits
  // Preserve the hardware observer even when performance printing is disabled.
  dontTouch(result.usedHits)
  dontTouch(result.unusedHits)
  println(s"PDB victim observer: entries=${params.entries}, lanes=$LoadPipelineWidth, bankEntries=${params.bankEntries}, defaultEnable=true")

  XSPerfAccumulate("victims", result.victim.valid)
  XSPerfAccumulate("used_victims", result.victim.valid && result.victim.bits.used)
  XSPerfAccumulate("unused_victims", result.victim.valid && !result.victim.bits.used)
  XSPerfAccumulate("usedVictimHits", result.usedHits)
  XSPerfAccumulate("unusedVictimHits", result.unusedHits)
  XSPerfAccumulate("streamUsedVictimHits", result.streamUsedHits)
  XSPerfAccumulate("streamUnusedVictimHits", result.streamUnusedHits)
  XSPerfAccumulate("demand_queries", PopCount(result.demand.map(_.valid)))
  XSPerfAccumulate("duplicate_victims", result.duplicate)
  XSPerfAccumulate("fifo_full_evictions", result.eviction.valid)
  XSPerfAccumulate("fifo_entry_cycles", result.occupancy)
  PrefetchPerfBoundary("fifo_occupancy", result.occupancy)

  val table = ChiselDB.createTable(s"PDBVictimObserver$hart", chiselTypeOf(result), basicDB = true)
  table.log(data = result,
    en = traceEnabled && (result.victim.valid || result.demand.map(_.valid).reduce(_ || _)),
    site = s"PDBVictimMonitor$hart", clock = clock, reset = reset)
}
