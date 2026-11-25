package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._
import utility.ReplacementPolicy

/** Synchronous one-port set-associative training table with lowest-empty-way allocation and tree PLRU. */
private[prefetch] class TemporalTrainingTable[T <: Data, K <: Data](
  sets: Int,
  ways: Int,
  entry: T,
  key: K,
  matches: (T, K) => Bool
) extends Module {
  val io = IO(new Bundle {
    val read = Flipped(Valid(new Bundle {
      val set = UInt(log2Ceil(sets).W)
      val key = keyType
    }))
    val hit = Output(Bool())
    val selectedEntry = Output(entry.cloneType)
    val touch = Input(Bool())
    val write = Flipped(Valid(entry.cloneType))
  })

  private def keyType: K = key.cloneType
  private val valid = RegInit(VecInit(Seq.fill(sets)(0.U(ways.W))))
  private val replace = ReplacementPolicy.fromString("setplru", ways, sets)
  private val addr = RegEnable(io.read.bits.set, io.read.valid)
  private val readKey = RegEnable(io.read.bits.key, io.read.valid)
  private val mem = SyncReadMem(sets, Vec(ways, entry.cloneType))
  private val update = Wire(Vec(ways, entry.cloneType))
  // Separate ports keep the prior read result visible to GSIM during row writeback.
  private val row = mem.read(io.read.bits.set, io.read.valid)
  private val hits = VecInit(row.zipWithIndex.map { case (value, way) => valid(addr)(way) && matches(value, readKey) })
  private val mask = ~valid(addr)
  private val victim = Mux(mask.orR, PriorityEncoder(mask), replace.way(addr))
  private val way = Mux(io.hit, OHToUInt(hits), victim)
  io.hit := hits.asUInt.orR
  io.selectedEntry := row(way)
  for (i <- 0 until ways) {
    update(i) := Mux(way === i.U, io.write.bits, row(i))
  }

  private val readLastCycle = RegNext(io.read.valid, false.B)
  when(io.touch || io.write.valid) {
    assert(readLastCycle)
  }
  assert(!(io.read.valid && io.write.valid))
  when(io.touch) {
    replace.access(addr, way)
  }
  when(io.write.valid) {
    mem.write(addr, update)
    valid(addr) := valid(addr) | UIntToOH(way, ways)
  }
}

private[prefetch] class TemporalTrainingLoad(lineBits: Int, aliasBits: Int, contextBits: Int) extends Bundle {
  val pc = UInt(TemporalAddress.PcBits.W)
  val context = UInt(contextBits.W)
  val target = new TemporalLine(lineBits, aliasBits)
  val roles = UInt(2.W)
  val kind = UInt(2.W)
  val bypassSampler = Bool()
}

/** Filter and Sampler of the production gem5 TP. Metadata lookup and Conf are handled separately. */
private[prefetch] class TemporalTraining(lineBits: Int, aliasBits: Int, contextBits: Int,
  samplerSets: Int, samplerWays: Int) extends Module {
  import TemporalAddress._

  private val filterSets = 32
  private val filterIndexBits = log2Ceil(filterSets)
  private val baseIndexBits = log2Ceil(samplerSets)

  val io = IO(new Bundle {
    val load = Flipped(Decoupled(new TemporalTrainingLoad(lineBits, aliasBits, contextBits)))
    val pair = Decoupled(new TemporalPair(lineBits, aliasBits, contextBits))
    val bypass = Output(Bool())
    val unchanged = Output(Bool())
  })

  private class FilterKey extends Bundle {
    val pcTag = UInt((PcBits - filterIndexBits).W)
    val context = UInt(contextBits.W)
  }
  private class FilterEntry extends Bundle {
    val key = new FilterKey
    val last = new TemporalLine(lineBits, aliasBits)
    val count = UInt(6.W)
    val epoch = UInt(4.W)
    val roles = UInt(2.W)
  }
  private class SamplerKey extends Bundle {
    val baseTag = UInt((lineBits - baseIndexBits).W)
    val context = UInt(contextBits.W)
  }
  private class SamplerEntry extends Bundle {
    val key = new SamplerKey
    val pc = UInt(PcBits.W)
    val target = new TemporalLine(lineBits, aliasBits)
    val stability = UInt(3.W)
    val roles = UInt(2.W)
  }

  private val filter = Module(new TemporalTrainingTable(
    filterSets, 8, new FilterEntry, new FilterKey, (entry: FilterEntry, key: FilterKey) =>
      entry.key.pcTag === key.pcTag && entry.key.context === key.context
  ))
  private val sampler = Module(new TemporalTrainingTable(
    samplerSets, samplerWays, new SamplerEntry, new SamplerKey, (entry: SamplerEntry, key: SamplerKey) =>
      entry.key.baseTag === key.baseTag && entry.key.context === key.context
  ))

  private val idle :: filterLookup :: samplerLookup :: send :: Nil = Enum(4)
  private val state = RegInit(idle)
  private val accepted = RegInit(0.U(19.W))
  private val load = RegEnable(io.load.bits, io.load.fire)
  private val pair = Reg(chiselTypeOf(io.pair.bits))
  private val priorCount = Reg(UInt(6.W))
  private val epoch = accepted(18, 15)
  private val filterOld = filter.io.selectedEntry
  private val samplerOld = sampler.io.selectedEntry
  private val count = Mux(filterOld.epoch === epoch, filterOld.count, filterOld.count >> 1)
  private val roles = Mux(filter.io.hit, filterOld.roles | load.roles, load.roles)

  io.load.ready := state === idle
  io.pair.valid := state === send
  io.pair.bits := pair
  io.bypass := state === filterLookup && filter.io.hit && load.bypassSampler

  filter.io.read.valid := io.load.fire
  filter.io.read.bits.set := placementPC(io.load.bits.pc)(filterIndexBits - 1, 0)
  filter.io.read.bits.key.pcTag := io.load.bits.pc(PcBits - 1, filterIndexBits)
  filter.io.read.bits.key.context := io.load.bits.context
  filter.io.touch := state === filterLookup
  filter.io.write.valid := state === filterLookup
  filter.io.write.bits.key.pcTag := load.pc(PcBits - 1, filterIndexBits)
  filter.io.write.bits.key.context := load.context
  filter.io.write.bits.last := load.target
  filter.io.write.bits.count := Mux(filter.io.hit, count + !count.andR, 0.U)
  filter.io.write.bits.epoch := epoch
  filter.io.write.bits.roles := roles

  sampler.io.read.valid := state === filterLookup && filter.io.hit && !load.bypassSampler
  sampler.io.read.bits.set := filterOld.last.line(baseIndexBits - 1, 0)
  sampler.io.read.bits.key.baseTag := filterOld.last.line(lineBits - 1, baseIndexBits)
  sampler.io.read.bits.key.context := load.context
  sampler.io.touch := state === samplerLookup
  private val sameTarget = samplerOld.target.line === load.target.line
  private val matchingPair = sampler.io.hit && sameTarget && samplerOld.pc === load.pc
  private val record = (sampler.io.hit && (matchingPair || samplerOld.stability.orR)) || priorCount >= 31.U
  private val stable = samplerOld.stability.andR && matchingPair && samplerOld.roles === pair.roles &&
    samplerOld.target.alias === load.target.alias
  io.unchanged := state === samplerLookup && stable
  sampler.io.write.valid := state === samplerLookup && (!sampler.io.hit || record) && !stable
  sampler.io.write.bits.key.baseTag := pair.base.line(lineBits - 1, baseIndexBits)
  sampler.io.write.bits.key.context := load.context
  sampler.io.write.bits.pc := load.pc
  sampler.io.write.bits.target := load.target
  sampler.io.write.bits.roles := pair.roles
  sampler.io.write.bits.stability := Mux(
    sampler.io.hit,
    Mux(matchingPair, samplerOld.stability + !samplerOld.stability.andR,
      samplerOld.stability - (!sameTarget && samplerOld.stability.orR)),
    0.U
  )

  when(io.load.fire) {
    accepted := accepted + 1.U
    state := filterLookup
  }
  when(state === filterLookup) {
    pair.pc := load.pc
    pair.context := load.context
    pair.roles := roles
    pair.kind := load.kind
    pair.base := filterOld.last
    pair.target := load.target
    priorCount := count
    state := Mux(filter.io.hit, Mux(load.bypassSampler, send, samplerLookup), idle)
  }
  when(state === samplerLookup) {
    state := Mux(record, send, idle)
  }
  when(io.pair.fire) {
    state := idle
  }
}
