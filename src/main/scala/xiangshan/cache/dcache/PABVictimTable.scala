package xiangshan.cache

import chisel3._
import chisel3.util._

/** Persistent FIFO of virtual capacity victims; a demand consumes each record once.
  * Demand queries precede insertion within a batch, including same-address batches.
  */
class PABVictimTable(entries: Int, blockAddrBits: Int, lanes: Int = 3) extends Module {
  require(entries > 0 && blockAddrBits > 0 && lanes > 0)
  val io = IO(new Bundle {
    val victim = Input(Valid(new PABLine(blockAddrBits)))
    val demand = Input(Vec(lanes, Valid(UInt(blockAddrBits.W))))
    val streamUsedHits, streamUnusedHits = Output(UInt(log2Ceil(lanes + 1).W))
    val occupancy = Output(UInt(log2Ceil(entries + 1).W))
    val duplicate, fullEviction = Output(Bool())
  })
  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val data = Reg(Vec(entries, new PABLine(blockAddrBits)))
  val victim = RegNext(io.victim, 0.U.asTypeOf(io.victim))
  val demand = RegNext(io.demand, 0.U.asTypeOf(io.demand))
  val matches = demand.map(d => VecInit((0 until entries).map(i =>
    d.valid && valid(i) && d.bits === data(i).blockAddr)))
  val hit = VecInit((0 until entries).map(i => matches.map(_(i)).reduce(_ || _)))
  val duplicate = VecInit((0 until entries).map(i =>
    victim.valid && valid(i) && data(i).blockAddr === victim.bits.blockAddr))
  val remaining = VecInit((0 until entries).map(i => valid(i) && !hit(i) && !duplicate(i)))
  val free = ~remaining.asUInt
  val remove = Mux(free.orR, PriorityEncoder(free), 0.U)
  val nextValid = WireInit(remaining)
  val nextData = WireInit(data)
  when (victim.valid) {
    for (i <- 0 until entries - 1) {
      when (remove <= i.U) {
        nextValid(i) := remaining(i + 1)
        nextData(i) := data(i + 1)
      }
    }
    nextValid(entries - 1) := true.B
    nextData(entries - 1) := victim.bits
  }
  valid := nextValid
  when (victim.valid) { data := nextData }
  // Register bank partial counts to keep the 256-way lookup out of the controller path.
  val banks = (0 until entries).grouped(32).toSeq
  val counts = RegInit(VecInit(Seq.fill(banks.size)(VecInit(Seq.fill(2)(0.U(log2Ceil(lanes + 1).W))))))
  for ((bank, b) <- banks.zipWithIndex) {
    counts(b)(0) := PopCount(bank.map(i => hit(i) && data(i).source === 3.U && data(i).used))
    counts(b)(1) := PopCount(bank.map(i => hit(i) && data(i).source === 3.U && !data(i).used))
  }
  io.streamUsedHits := counts.map(_(0)).reduce(_ +& _)
  io.streamUnusedHits := counts.map(_(1)).reduce(_ +& _)
  io.occupancy := PopCount(valid)
  io.duplicate := RegNext(duplicate.asUInt.orR, false.B)
  io.fullEviction := RegNext(victim.valid && !free.orR, false.B)
  matches.foreach(m => assert(PopCount(m) <= 1.U))
  assert(PopCount(hit) <= lanes.U && PopCount(duplicate) <= 1.U)
}
