package xiangshan.cache

import chisel3._
import chisel3.util._

class PABMissOrigin(blockAddrBits: Int) extends Bundle {
  val blockAddr = UInt(blockAddrBits.W)
  val source = UInt(3.W)
  val eligible = Bool()
}

/** Observation only: acceptance and MSHR lifetime inputs never depend on this state. */
class PABMissTracker(entries: Int, blockAddrBits: Int) extends Module {
  val io = IO(new Bundle {
    val active = Input(Vec(entries, Bool()))
    val allocation = Input(Vec(entries, Valid(new PABMissOrigin(blockAddrBits))))
    val allocationPrefetch = Input(Vec(entries, Bool()))
    val demand = Input(Vec(entries, Bool()))
    val streamAttempt = Input(Valid(UInt(blockAddrBits.W)))
    val origin = Output(Vec(entries, new PABMissOrigin(blockAddrBits)))
    val late = Output(UInt(4.W))
  })
  val origin = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new PABMissOrigin(blockAddrBits)))))
  val wasPrefetch = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val consumed = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val firstDemand = Wire(Vec(entries, Bool()))
  for (i <- 0 until entries) {
    val alloc = io.allocation(i)
    firstDemand(i) := io.demand(i) && Mux(alloc.valid,
      alloc.bits.eligible && alloc.bits.source === 3.U,
      io.active(i) && origin(i).eligible && origin(i).source === 3.U && !consumed(i))
    when (alloc.valid) {
      origin(i) := alloc.bits
      wasPrefetch(i) := io.allocationPrefetch(i)
      consumed(i) := io.demand(i)
    }.elsewhen (io.active(i) && io.demand(i)) { consumed(i) := true.B }
    io.origin(i) := origin(i)
    io.origin(i).eligible := io.active(i) && origin(i).eligible && !consumed(i) && !io.demand(i)
  }
  val lateAttempt = io.streamAttempt.valid && (0 until entries).map(i =>
    io.active(i) && origin(i).blockAddr === io.streamAttempt.bits &&
      (!wasPrefetch(i) || consumed(i))).reduce(_ || _)
  io.late := PopCount(firstDemand) +& lateAttempt.asUInt
}
