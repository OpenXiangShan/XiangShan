package xiangshan.cache

import chisel3._
import chisel3.util._

class PABRefillObservation(implicit p: org.chipsalliance.cde.config.Parameters) extends DCacheBundle {
  val blockAddr = UInt((PAddrBits - blockOffBits).W)
  val missId = UInt(log2Up(cfg.nMissEntries).W)
  val source = UInt(3.W)
}

class PABLine(blockAddrBits: Int) extends Bundle {
  val blockAddr = UInt(blockAddrBits.W)
  val source = UInt(3.W)
  val used = Bool()
}

/** Metadata-only residency model. No ready, data, cache-way or coherence ownership. */
class PrefetchAddrBuffer(entries: Int, blockAddrBits: Int, lanes: Int = 3) extends Module {
  require(entries > 0 && blockAddrBits > 0 && lanes > 0)
  val io = IO(new Bundle {
    val refill = Input(Valid(new PABLine(blockAddrBits)))
    val demand = Input(Vec(lanes, Valid(UInt(blockAddrBits.W))))
    // Completed Store/AMO or Probe leaves the virtual buffer without a capacity victim.
    val retire = Input(Valid(UInt(blockAddrBits.W)))
    val inserted = Output(Valid(new PABLine(blockAddrBits)))
    val victim = Output(Valid(new PABLine(blockAddrBits)))
    val observedDemand = Output(Vec(lanes, Valid(UInt(blockAddrBits.W))))
    val occupancy = Output(UInt(log2Ceil(entries + 1).W))
    val demandHits = Output(UInt(log2Ceil(lanes + 1).W))
  })
  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val data = Reg(Vec(entries, new PABLine(blockAddrBits)))
  val retiring = VecInit((0 until entries).map(i =>
    valid(i) && io.retire.valid && data(i).blockAddr === io.retire.bits))
  val matches = io.demand.map(d => VecInit((0 until entries).map(i =>
    d.valid && valid(i) && data(i).blockAddr === d.bits)))
  val touched = VecInit((0 until entries).map(i => matches.map(_(i)).reduce(_ || _)))
  val free = VecInit((0 until entries).map(i => !valid(i) || retiring(i)))
  val duplicate = VecInit((0 until entries).map(i =>
    valid(i) && !retiring(i) && data(i).blockAddr === io.refill.bits.blockAddr))
  val insert = io.refill.valid && !duplicate.asUInt.orR &&
    !(io.retire.valid && io.retire.bits === io.refill.bits.blockAddr)

  // Exact pairwise LRU; simultaneous touches order higher indices as newer.
  val pairs = for (i <- 0 until entries; j <- i + 1 until entries) yield (i, j)
  val older = pairs.map(pair => pair -> RegInit(true.B)).toMap
  val oldest = VecInit((0 until entries).map { i =>
    valid(i) && !retiring(i) && (0 until entries).filter(_ != i).map { j =>
      !valid(j) || retiring(j) || (if (i < j) older((i, j)) else !older((j, i)))
    }.foldLeft(true.B)(_ && _)
  })
  val target = Mux(free.asUInt.orR, PriorityEncoder(free), OHToUInt(oldest))
  val touch = VecInit((0 until entries).map(i => touched(i) || insert && target === i.U))
  for ((i, j) <- pairs) {
    when (touch(i) || touch(j)) { older((i, j)) := touch(j) }
  }
  val incoming = WireInit(io.refill.bits)
  incoming.used := io.refill.bits.used || io.demand.map(d =>
    d.valid && d.bits === io.refill.bits.blockAddr).reduce(_ || _)
  val outgoing = WireInit(data(target))
  outgoing.used := data(target).used || touched(target)
  for (i <- 0 until entries) {
    when (touched(i)) { data(i).used := true.B }
    when (retiring(i)) { valid(i) := false.B }
    when (insert && target === i.U) {
      valid(i) := true.B
      data(i) := incoming
    }
  }
  io.inserted.valid := RegNext(insert, false.B)
  io.inserted.bits := RegEnable(incoming, insert)
  val evict = insert && !free.asUInt.orR
  io.victim.valid := RegNext(evict, false.B)
  io.victim.bits := RegEnable(outgoing, evict)
  io.observedDemand := RegNext(io.demand, 0.U.asTypeOf(io.demand))
  io.occupancy := PopCount(valid)
  io.demandHits := RegNext(PopCount(matches.map(_.asUInt.orR)), 0.U)
  assert(!evict || PopCount(oldest) === 1.U)
  assert(PopCount(duplicate) <= 1.U)
  matches.foreach(m => assert(PopCount(m) <= 1.U))
}
