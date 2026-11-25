package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._

private[prefetch] object TemporalAddress {
  val PcBits = 22
  val RecordLength = 12

  def pcHash(pc: UInt): UInt = {
    val halfword = pc >> 1
    val folded = (halfword ^ (halfword >> 22) ^ (halfword >> 44)).pad(PcBits)(PcBits - 1, 0)
    folded ^ (folded >> 11) ^ Cat(folded(8, 0), 0.U(13.W)) ^ (folded >> 5)
  }

  def placementPC(pc: UInt): UInt = pc ^ (pc >> 7) ^ (pc >> 13)

  def metaHash(pc: UInt, trigger: UInt): UInt =
    trigger ^ (trigger >> 10) ^ (trigger >> 20) ^ (trigger >> 30) ^ (pc << 5) ^ (pc >> 6)
}

private[prefetch] class TemporalLine(lineBits: Int, aliasBits: Int) extends Bundle {
  val line = UInt(lineBits.W)
  val alias = UInt(aliasBits.W)
}

private[prefetch] class TemporalPair(lineBits: Int, aliasBits: Int, contextBits: Int) extends Bundle {
  val pc = UInt(TemporalAddress.PcBits.W)
  val context = UInt(contextBits.W)
  val roles = UInt(2.W)
  val kind = UInt(2.W)
  val base = new TemporalLine(lineBits, aliasBits)
  val target = new TemporalLine(lineBits, aliasBits)
}

private[prefetch] class TemporalRecord(lineBits: Int, aliasBits: Int, contextBits: Int, pcBits: Int)
    extends Bundle {
  val pc = UInt(pcBits.W)
  val context = UInt(contextBits.W)
  val roles = UInt(2.W)
  val kind = UInt(2.W)
  val trigger = UInt(lineBits.W)
  val length = UInt(log2Ceil(TemporalAddress.RecordLength + 1).W)
  val targets = Vec(TemporalAddress.RecordLength, new TemporalLine(lineBits, aliasBits))
}

/** gem5 v3.4.2 Recorder. One synchronous row access at a time, with one completed record held for Meta. */
private[prefetch] class TemporalRecorder(sets: Int, lineBits: Int, aliasBits: Int, contextBits: Int)
    extends Module {
  import TemporalAddress._

  require(sets >= 2 && isPow2(sets))
  private val setBits = log2Ceil(sets)
  require(setBits < PcBits)
  private val tagBits = PcBits - setBits

  val io = IO(new Bundle {
    val pair = Flipped(Decoupled(new TemporalPair(lineBits, aliasBits, contextBits)))
    val record = Decoupled(new TemporalRecord(lineBits, aliasBits, contextBits, PcBits))
  })

  private val rows = SyncReadMem(sets, Vec(2, new TemporalRecord(lineBits, aliasBits, contextBits, tagBits)))
  private val valid = RegInit(VecInit(Seq.fill(sets)(0.U(2.W))))
  private val victim = RegInit(VecInit(Seq.fill(sets)(false.B)))

  private val idle :: update :: send :: Nil = Enum(3)
  private val state = RegInit(idle)
  private val pair = RegEnable(io.pair.bits, io.pair.fire)
  private val record = Reg(chiselTypeOf(io.record.bits))
  private val set = pair.pc(setBits - 1, 0)
  private val tag = pair.pc(PcBits - 1, setBits)

  private val rowWrite = Wire(Vec(2, new TemporalRecord(lineBits, aliasBits, contextBits, tagBits)))
  private val read = rows.read(io.pair.bits.pc(setBits - 1, 0), io.pair.fire)

  io.pair.ready := state === idle
  io.record.valid := state === send
  io.record.bits := record

  private val hits = VecInit((0 until 2).map { way =>
    valid(set)(way) && read(way).pc === tag && read(way).context === pair.context
  })
  private val hit = hits.asUInt.orR
  private val way = Mux(hit, hits(1), valid(set)(0) && (!valid(set)(1) || victim(set)))
  private val previousRecord = Mux(way, read(1), read(0))

  private val addBase = !(0 until RecordLength).map { i =>
    i.U < previousRecord.length && previousRecord.targets(i).line === pair.base.line
  }.reduce(_ || _)
  private val addTarget = !(0 until RecordLength).map { i =>
    i.U < previousRecord.length && previousRecord.targets(i).line === pair.target.line
  }.reduce(_ || _)
  private val appendBase = addBase && previousRecord.length < RecordLength.U
  private val afterBase = previousRecord.length + appendBase
  private val appendTarget = addTarget && afterBase < RecordLength.U
  private val full = afterBase + appendTarget === RecordLength.U
  private val carry = appendBase && !appendTarget && addTarget && previousRecord.length === (RecordLength - 1).U

  private val appended = WireInit(previousRecord)
  appended.roles := previousRecord.roles | pair.roles
  appended.length := afterBase + appendTarget
  for (i <- 0 until RecordLength) {
    when(appendBase && previousRecord.length === i.U) {
      appended.targets(i) := pair.base
    }
    when(appendTarget && afterBase === i.U) {
      appended.targets(i) := pair.target
    }
  }

  private val start = WireDefault(0.U.asTypeOf(previousRecord))
  start.pc := tag
  start.context := pair.context
  start.roles := pair.roles
  start.kind := pair.kind
  start.trigger := pair.base.line
  start.length := 1.U
  start.targets.head := pair.target

  private val continuation = WireInit(start)
  continuation.roles := appended.roles
  continuation.kind := previousRecord.kind
  private val nextRecord = Mux(!hit, start, Mux(full && carry, continuation, appended))
  for (i <- rowWrite.indices) {
    rowWrite(i) := Mux(way === i.U, nextRecord, read(i))
  }

  when(io.pair.fire) {
    state := update
  }
  when(state === update) {
    rows.write(set, rowWrite)
    assert(PopCount(hits) <= 1.U)
    val selected = UIntToOH(way, 2)
    valid(set) := Mux(!hit || !full || carry, valid(set) | selected, valid(set) & ~selected)
    victim(set) := !way
    val evict = !hit && valid(set)(way) && previousRecord.length >= 9.U
    val complete = hit && full
    state := Mux(evict || complete, send, idle)
    when(evict || complete) {
      val finished = Mux(complete, appended, previousRecord)
      record.pc := Cat(finished.pc, set)
      record.context := finished.context
      record.roles := finished.roles
      record.kind := finished.kind
      record.trigger := finished.trigger
      record.length := finished.length
      record.targets := finished.targets
    }
  }
  when(io.record.fire) {
    state := idle
  }
}
