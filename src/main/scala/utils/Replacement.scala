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

class TrueLRU(n_ways: Int) {
  // True LRU replacement policy, using a triangular matrix to track which sets are more recently used than others.
  // The matrix is packed into a single UInt (or Bits).  Example 4-way (6-bits):
  // [5] - 3 more recent than 2
  // [4] - 3 more recent than 1
  // [3] - 2 more recent than 1
  // [2] - 3 more recent than 0
  // [1] - 2 more recent than 0
  // [0] - 1 more recent than 0
  def nBits = (n_ways * (n_ways-1)) / 2
  private val state_reg = RegInit(0.U(nBits.W))
  def state_read = WireDefault(state_reg)

  private def extractMRUVec(state: UInt): Seq[UInt] = {
    // Extract per-way information about which higher-indexed ways are more recently used
    val moreRecentVec = Wire(Vec(n_ways-1, UInt(n_ways.W)))
    var lsb = 0
    for (i <- 0 until n_ways-1) {
      moreRecentVec(i) := Cat(state(lsb+n_ways-i-2,lsb), 0.U((i+1).W))
      lsb = lsb + (n_ways - i - 1)
    }
    moreRecentVec
  }

  def get_next_state(state: UInt, touch_way: UInt): UInt = {
    val nextState     = Wire(Vec(n_ways-1, UInt(n_ways.W)))
    val moreRecentVec = extractMRUVec(state)  // reconstruct lower triangular matrix
    val wayDec        = UIntToOH(touch_way, n_ways)

    // Compute next value of triangular matrix
    // set the touched way as more recent than every other way
    nextState.zipWithIndex.foreach { case (e, i) =>
      e := Mux(i.U === touch_way, 0.U(n_ways.W), moreRecentVec(i) | wayDec)
    }

    nextState.zipWithIndex.tail.foldLeft((nextState.head.apply(n_ways-1,1),0)) { case ((pe,pi),(ce,ci)) => (Cat(ce.apply(n_ways-1,ci+1), pe), ci) }._1
  }


  def get_next_state(state: UInt, touch_ways: Seq[Valid[UInt]]): UInt = {
    touch_ways.foldLeft(state)((prev, touch_way) => Mux(touch_way.valid, get_next_state(prev, touch_way.bits), prev))
  }

  def access(touch_way: UInt) {
    state_reg := get_next_state(state_reg, touch_way)
  }
  def access(touch_ways: Seq[Valid[UInt]]) {
    when (ParallelOR(touch_ways.map(_.valid))) {
      state_reg := get_next_state(state_reg, touch_ways)
    }
//    for (i <- 1 until touch_ways.size) {
//      cover(PopCount(touch_ways.map(_.valid)) === i.U, s"LRU_UpdateCount$i", s"LRU Update $i simultaneous")
//    }
  }

  def get_replace_way(state: UInt): UInt = {
    val moreRecentVec = extractMRUVec(state)  // reconstruct lower triangular matrix
    // For each way, determine if all other ways are more recent
    val mruWayDec     = (0 until n_ways).map { i =>
      val upperMoreRecent = (if (i == n_ways-1) true.B else moreRecentVec(i).apply(n_ways-1,i+1).andR)
      val lowerMoreRecent = (if (i == 0)        true.B else moreRecentVec.map(e => !e(i)).reduce(_ && _))
      upperMoreRecent && lowerMoreRecent
    }
    OHToUInt(mruWayDec)
  }

  def way = get_replace_way(state_reg)
  def miss = access(way)
  def hit = {}
  def flush() = { state_reg := 0.U(nBits.W) }
  @deprecated("replace 'replace' with 'way' from abstract class ReplacementPolicy","Rocket Chip 2020.05")
  def replace: UInt = way
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
//      next_state = next_state.bitSet(idx, !bit)
      next_state = Mux(bit, next_state & (~UIntToOH(idx)), next_state | UIntToOH(idx))
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

class SbufferLRU(n_ways: Int) {

  def nBits = n_ways * n_ways
  private val state_reg = RegInit(0.U(nBits.W))
  def state_read = WireDefault(state_reg)



  // set the row touched with 1, column with 0
  def get_next_state(state: UInt, touch_ways: Seq[Valid[UInt]]): UInt = {
    val nextState     = Wire(Vec(n_ways, UInt(n_ways.W)))
    val moreRecentVec = state.asTypeOf(Vec(n_ways, UInt(n_ways.W)))
    val wayDecs       = touch_ways.map( w => Mux(w.valid, UIntToOH(w.bits, n_ways), 0.U) )
    val wayDec        = ParallelOR(wayDecs)
    val wayUpd        = (~wayDec).asUInt()

    nextState.zipWithIndex.foreach { case (e, i) =>
      e := Mux(wayDec(i), wayUpd, moreRecentVec(i) & wayUpd )
    }
    nextState.asUInt()
  }

  // update the stateRect
  def access(touch_ways: Seq[Valid[UInt]]) {
    when (ParallelOR(touch_ways.map(_.valid))) {
      state_reg := get_next_state(state_reg, touch_ways)
    }
  }

  // get the index of the smallest value from a set of numbers
  def get_min_value(xs: Seq[(UInt,UInt)]): (UInt,UInt)= {
    xs match {
      case Seq(a) => a
      case Seq(a, b) => (Mux(a._1<b._1,a._1,b._1),Mux(a._1<b._1,a._2,b._2))
      case _ =>
        get_min_value(Seq(get_min_value(xs take xs.size/2), get_min_value(xs drop xs.size/2)))
    }
  }

  // get the way which is valid and has the least 1
  def get_replace_way(state: UInt, sbufferState:Seq[Bool]): UInt = {
    val moreRecentVec = state.asTypeOf(Vec(n_ways, UInt(n_ways.W)))
    val count = Wire(Vec(n_ways, UInt(log2Up(n_ways).W)))
    for(i <- 0 until n_ways){
      count(i) := Mux(sbufferState(i), PopCount(moreRecentVec(i)), ((1<<n_ways)-1).U)
    }
    count.zip((0 until n_ways).map(_.U))
    get_min_value(count.zip((0 until n_ways).map(_.U)))._2
  }

  def way(sbufferState:Seq[Bool]) = get_replace_way(state_reg, sbufferState)
  def hit = {}
  def flush() = { state_reg := 0.U(nBits.W) }
}