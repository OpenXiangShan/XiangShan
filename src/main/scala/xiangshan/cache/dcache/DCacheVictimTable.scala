package xiangshan.cache

import chisel3._
import chisel3.util._

/** History of real, unused Stream evictions. It cannot backpressure the cache.
  * Only qualifying victims allocate. Demand consumes once; refill supersedes
  * an old lifetime, including a same-cycle demand. Queries precede insertion.
  */
class DCacheVictimTable(entries: Int, blockAddrBits: Int, lanes: Int = 3) extends Module {
  require(entries > 0 && blockAddrBits > 0 && lanes > 0)
  val io = IO(new Bundle {
    val victim = Input(Valid(new PABLine(blockAddrBits)))
    val refill = Input(Valid(UInt(blockAddrBits.W)))
    val demand = Input(Vec(lanes, Valid(UInt(blockAddrBits.W))))
    val hits = Output(UInt(log2Ceil(lanes + 1).W))
    val occupancy = Output(UInt(log2Ceil(entries + 1).W))
    val inserted, duplicate, fullEviction, refillRemoval = Output(Bool())
  })
  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val addresses = Reg(Vec(entries, UInt(blockAddrBits.W)))
  val victim = RegNext(io.victim, 0.U.asTypeOf(io.victim))
  val refill = RegNext(io.refill, 0.U.asTypeOf(io.refill))
  val demand = RegNext(io.demand, 0.U.asTypeOf(io.demand))
  val insert = victim.valid && victim.bits.source === 3.U && !victim.bits.used
  val refilled = VecInit((0 until entries).map(i =>
    valid(i) && refill.valid && addresses(i) === refill.bits))
  val matches = demand.map(d => VecInit((0 until entries).map(i =>
    d.valid && valid(i) && !refilled(i) && d.bits === addresses(i))))
  val hit = VecInit((0 until entries).map(i => matches.map(_(i)).reduce(_ || _)))
  val duplicate = VecInit((0 until entries).map(i =>
    insert && valid(i) && addresses(i) === victim.bits.blockAddr))
  val remaining = VecInit((0 until entries).map(i => valid(i) && !hit(i) && !refilled(i) && !duplicate(i)))
  val free = ~remaining.asUInt
  val remove = Mux(free.orR, PriorityEncoder(free), 0.U)
  val nextValid = WireInit(remaining)
  val nextAddresses = WireInit(addresses)
  // An installation of this same address supersedes even a simultaneous victim.
  val allocate = insert && !(refill.valid && refill.bits === victim.bits.blockAddr)
  when (allocate) {
    for (i <- 0 until entries - 1) {
      when (remove <= i.U) {
        nextValid(i) := remaining(i + 1)
        nextAddresses(i) := addresses(i + 1)
      }
    }
    nextValid(entries - 1) := true.B
    nextAddresses(entries - 1) := victim.bits.blockAddr
  }
  valid := nextValid
  when (allocate) { addresses := nextAddresses }
  // Two capture edges from input observations to controller feedback.
  val banks = (0 until entries).grouped(32).toSeq
  val counts = RegInit(VecInit(Seq.fill(banks.size)(0.U(log2Ceil(lanes + 1).W))))
  for ((bank, b) <- banks.zipWithIndex) { counts(b) := PopCount(bank.map(hit(_))) }
  io.hits := counts.reduce(_ +& _)
  io.occupancy := PopCount(valid)
  io.inserted := RegNext(allocate, false.B)
  io.duplicate := RegNext(allocate && duplicate.asUInt.orR, false.B)
  io.fullEviction := RegNext(allocate && !free.orR, false.B)
  io.refillRemoval := RegNext(refilled.asUInt.orR, false.B)
  matches.foreach(m => assert(PopCount(m) <= 1.U))
  assert(PopCount(hit) <= lanes.U && PopCount(duplicate) <= 1.U)
}
