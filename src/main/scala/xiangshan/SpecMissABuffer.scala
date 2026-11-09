package xiangshan

import chisel3._
import chisel3.util._

class SpecMissBufferedA[T <: Data](gen: T, nOwners: Int, idBits: Int) extends Bundle {
  val data = gen.cloneType
  val speculative = Bool()
  val committed = Bool()
  val owner = UInt(math.max(1, log2Ceil(nOwners)).W)
  val id = UInt(idBits.W)
}

class SpecMissABuffer[T <: Data](gen: T, depth: Int, nOwners: Int, idBits: Int) extends Module {
  require(depth >= 2)
  require(nOwners >= 1)

  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new SpecMissBufferedA(gen, nOwners, idBits)))
    val deq = Decoupled(gen.cloneType)
    // Existing head origin, used only to count actual speculative A handshakes.
    val deq_speculative = Output(Bool())
    val resolve = Input(Vec(nOwners, Valid(new Bundle {
      val id = UInt(idBits.W)
      val commit = Bool()
    })))
    val matched = Output(Vec(nOwners, Bool()))
    val committed = Output(Vec(nOwners, Bool()))
  })

  val entries = Reg(Vec(depth, new SpecMissBufferedA(gen, nOwners, idBits)))
  val count = RegInit(0.U(log2Ceil(depth + 1).W))
  val occupied = VecInit((0 until depth).map(i => i.U < count))
  // Compare each owner's resolution directly, without selecting it before the ID comparison.
  val resolveMatches = VecInit((0 until depth).map { i =>
    val waiting = occupied(i) && entries(i).speculative && !entries(i).committed
    VecInit((0 until nOwners).map { w =>
      waiting && entries(i).owner === w.U && io.resolve(w).valid &&
        entries(i).id === io.resolve(w).bits.id
    })
  })
  val resolved = VecInit(resolveMatches.map(_.asUInt.orR))
  val canceled = VecInit((0 until depth).map(i =>
    VecInit((0 until nOwners).map(w => resolveMatches(i)(w) && !io.resolve(w).bits.commit)).asUInt.orR))

  val headCommittedNow = VecInit((0 until nOwners).map(w =>
    resolveMatches(0)(w) && io.resolve(w).bits.commit)).asUInt.orR
  io.deq.valid := count =/= 0.U && (!entries(0).speculative || entries(0).committed || headCommittedNow)
  io.deq.bits := entries(0).data
  io.deq_speculative := entries(0).speculative
  io.enq.ready := count < depth.U
  when(io.deq.valid && entries(0).speculative) {
    assert(entries(0).committed || headCommittedNow, "uncommitted SpecMiss reached TileLink A")
  }

  for (w <- 0 until nOwners) {
    val matches = VecInit((0 until depth).map(i => resolveMatches(i)(w)))
    io.matched(w) := matches.asUInt.orR
    io.committed(w) := io.resolve(w).bits.commit && matches.asUInt.orR
    when(io.resolve(w).valid) {
      assert(PopCount(matches) <= 1.U, "specmiss resolution matched multiple candidates in one slice")
    }
  }

  val keep = VecInit((0 until depth).map(i => occupied(i) && !canceled(i) &&
    !(if (i == 0) io.deq.fire else false.B)))
  val surviving = PopCount(keep)
  val nextEntries = Wire(Vec(depth, chiselTypeOf(entries(0))))
  nextEntries := entries
  for (i <- 0 until depth) {
    val rank = if (i == 0) 0.U else PopCount(keep.take(i))
    val updated = Wire(chiselTypeOf(entries(i)))
    updated := entries(i)
    when(resolved(i)) {
      updated.committed := true.B
    }
    when(keep(i)) {
      nextEntries(rank) := updated
    }
  }
  when(io.enq.fire) {
    nextEntries(surviving(log2Ceil(depth) - 1, 0)) := io.enq.bits
  }
  entries := nextEntries
  count := surviving + io.enq.fire.asUInt
}
