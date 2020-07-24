// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package utils

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := false.B
  val lfsr = LFSR(16, replace)

  def way = Random(ways, lfsr)
  def miss = replace := true.B
  def hit = {}
}

abstract class SeqReplacementPolicy {
  def access(set: UInt): Unit
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt): Unit
  def way: UInt
}

class SeqRandom(n_ways: Int) extends SeqReplacementPolicy {
  val logic = new RandomReplacement(n_ways)
  def access(set: UInt) = { }
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    when (valid && !hit) { logic.miss }
  }
  def way = logic.way
}

class PseudoLRU(n: Int)
{
  private val state_reg = Reg(UInt((n-1).W))
  def access(way: UInt) {
    state_reg := get_next_state(state_reg,way)
  }
  def access(ways: Seq[ValidIO[UInt]]) {
    state_reg := ways.foldLeft(state_reg)((prev, way) => Mux(way.valid, get_next_state(prev, way.bits), prev))
  }
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(n-1, 1)
  }
  def replace = get_replace_way(state_reg)
  def get_replace_way(state: UInt) = {
    val shifted_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val in_bounds = Cat(idx, (BigInt(1) << i).U)(log2Up(n)-1, 0) < n.U
      idx = Cat(idx, in_bounds && shifted_state(idx))
    }
    idx(log2Up(n)-1,0)
  }
}

class SeqPLRU(n_sets: Int, n_ways: Int) extends SeqReplacementPolicy {
  val state = SyncReadMem(n_sets, UInt((n_ways-1).W))
  val logic = new PseudoLRU(n_ways)
  val current_state = Wire(UInt())
  val plru_way = logic.get_replace_way(current_state)
  val next_state = Wire(UInt())

  def access(set: UInt) = {
    current_state := state.read(set)
  }

  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    val update_way = Mux(hit, way, plru_way)
    next_state := logic.get_next_state(current_state, update_way)
    when (valid) { state.write(set, next_state) }
  }

  def way = plru_way
}
