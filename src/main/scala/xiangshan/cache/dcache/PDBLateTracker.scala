package xiangshan.cache

import chisel3._
import chisel3.util._

class PDBLateAllocation extends Bundle {
  val stream = Bool()
  val demand = Bool()
}

/** Preserve demand ownership separately from legacy prefetch flags, which raw
  * rejected queries can clear. Two bits per MSHR cover both pending allocation
  * registers and resident entries; all address matches come from MissQueue.
  */
class PDBLateTracker(entries: Int) extends Module {
  val io = IO(new Bundle {
    val allocate = Input(Vec(entries, Valid(new PDBLateAllocation)))
    val live = Input(Vec(entries, Bool()))
    val demandHit = Input(Vec(entries, Bool()))
    val streamHit = Input(Vec(entries, Bool()))
    val demandLate = Output(UInt(log2Ceil(entries + 1).W))
    val prefetchLate = Output(UInt(log2Ceil(entries + 1).W))
  })
  val stream = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val demand = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val demandLate = Wire(Vec(entries, Bool()))
  val prefetchLate = Wire(Vec(entries, Bool()))
  for (i <- 0 until entries) {
    // A terminal Stream PF attempt hitting an existing demand MSHR is late
    // even when MQ drops it. Each real attempt counts; PF-only hits do not.
    prefetchLate(i) := io.live(i) && demand(i) && io.streamHit(i)
    // Same-address accepted demands count once per prefetch lifetime, including
    // compressed demands in the allocation cycle and merges in the pipe stage.
    demandLate(i) := io.live(i) && stream(i) && !demand(i) && io.demandHit(i)
    when (!io.live(i)) { stream(i) := false.B; demand(i) := false.B }
    when (io.live(i) && io.demandHit(i)) { demand(i) := true.B }
    when (io.allocate(i).valid) {
      stream(i) := io.allocate(i).bits.stream
      demand(i) := io.allocate(i).bits.demand || io.demandHit(i)
      demandLate(i) := io.allocate(i).bits.stream && io.demandHit(i)
      prefetchLate(i) := false.B
    }
  }
  io.demandLate := PopCount(demandLate)
  io.prefetchLate := PopCount(prefetchLate)
  assert(PopCount(io.streamHit) <= 1.U, "One terminal PF cannot match multiple MSHR lifetimes")
}
