package xiangshan.cache

import chisel3._
import chisel3.util._

// Exact recency among eligible entries without a wrapping cycle timestamp.
class PDBLRU(entries: Int) extends Module {
  require(entries > 0)
  val io = IO(new Bundle {
    val touch = Input(Vec(entries, Bool()))
    val eligible = Input(Vec(entries, Bool()))
    val victim = Valid(UInt(log2Ceil(entries max 2).W))
  })

  // One bit per unordered pair: true means the lower index is older.
  // Simultaneous touches are ordered by index, with the higher index most recent.
  private val pairs = for (i <- 0 until entries; j <- i + 1 until entries) yield (i, j)
  private val older = pairs.map(pair => pair -> RegInit(true.B)).toMap
  private val oldest = VecInit((0 until entries).map { i =>
    val precedes = (0 until entries).filter(_ != i).map { j =>
      !io.eligible(j) || (if (i < j) older((i, j)) else !older((j, i)))
    }
    io.eligible(i) && precedes.foldLeft(true.B)(_ && _)
  })

  for ((i, j) <- pairs) {
    when (io.touch(i) || io.touch(j)) { older((i, j)) := io.touch(j) }
  }

  io.victim.valid := io.eligible.asUInt.orR
  io.victim.bits := OHToUInt(oldest)
  assert(PopCount(oldest) === io.victim.valid.asUInt, "PDB LRU must select exactly one eligible victim")
}
