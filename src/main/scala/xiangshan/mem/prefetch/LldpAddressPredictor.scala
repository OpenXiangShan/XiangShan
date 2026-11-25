package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._

private[prefetch] class LldpAddressKey(pcBits: Int, paddrBits: Int, contextBits: Int) extends Bundle {
  val producer = UInt(pcBits.W)
  val consumer = UInt(pcBits.W)
  val context = UInt(contextBits.W)
  val producerAddress = UInt(paddrBits.W)
  val ruleGeneration = UInt(32.W)
}

private[xiangshan] class LldpAddressRef(indexBits: Int = 8) extends Bundle {
  val index = UInt(indexBits.W)
  val generation = UInt(32.W)
}

private[xiangshan] object LldpAddressOutcome extends ChiselEnum {
  val Useful, Merged, Late, Unused, Dropped = Value
}

private[xiangshan] class LldpAddressFeedback(indexBits: Int = 8) extends Bundle {
  val ref = new LldpAddressRef(indexBits)
  val outcome = LldpAddressOutcome()
}

private[prefetch] class LldpAddressTarget(paddrBits: Int, aliasBits: Int) extends Bundle {
  val paddr = UInt(paddrBits.W)
  val alias = UInt(aliasBits.W)
}

private[prefetch] class LldpAddressSample(pcBits: Int, paddrBits: Int, contextBits: Int, aliasBits: Int)
    extends Bundle {
  val key = new LldpAddressKey(pcBits, paddrBits, contextBits)
  val target = new LldpAddressTarget(paddrBits, aliasBits)
}

/** Byte-address Sampler and MetaTable from the v3.4.2 LLDP. */
private[prefetch] class LldpAddressPredictor(
  pcBits: Int,
  paddrBits: Int,
  contextBits: Int,
  aliasBits: Int,
  sets: Int = 256
) extends Module {
  require(sets >= 2 && isPow2(sets) && sets <= 4096)

  private val indexBits = log2Ceil(sets)
  private def keyType = new LldpAddressKey(pcBits, paddrBits, contextBits)
  private def sampleType = new LldpAddressSample(pcBits, paddrBits, contextBits, aliasBits)

  val io = IO(new Bundle {
    val train = Flipped(Decoupled(sampleType))
    val lookup = Flipped(Decoupled(keyType))
    val candidate = Valid(new Bundle {
      val target = new LldpAddressTarget(paddrBits, aliasBits)
      val ref = new LldpAddressRef(indexBits)
    })
    val admit = Input(Bool())
    val feedback = Flipped(Decoupled(new LldpAddressFeedback(indexBits)))
    val observe = Input(UInt(4.W))
    val promoted = Output(Bool())
    val blocked = Output(Bool())
    val staleFeedback = Output(Bool())
  })

  private class SamplerEntry extends Bundle {
    val sample = sampleType
    val stable = UInt(3.W)
    val sincePromotion = UInt(3.W)
    val rrpv = UInt(2.W)
  }
  private class MetaEntry extends Bundle {
    val sample = sampleType
    val generation = UInt(32.W)
    val train = UInt(3.W)
    val quality = UInt(3.W)
    val tokens = UInt(4.W)
    val busy = Bool()
    val deadline = UInt(4.W)
  }

  private val sampleValid = RegInit(VecInit.fill(sets)(0.U(2.W)))
  private val metaValid = RegInit(VecInit.fill(sets)(false.B))
  private val sampler = SyncReadMem(sets, Vec(2, new SamplerEntry))
  private val meta = SyncReadMem(sets, new MetaEntry)
  private val generation = RegInit(0.U(32.W))
  private val epoch = RegInit(0.U(16.W))
  private val nextEpoch = epoch + io.observe
  private val nextBatch = nextEpoch(15, 12)
  private val scanPending = RegInit(false.B)
  private val scanIndex = RegInit(0.U(indexBits.W))
  private val scanBatch = Reg(UInt(4.W))
  private val ageDeadline = epoch(15, 12) + epoch(11, 0).orR + 8.U
  epoch := nextEpoch
  when(nextBatch =/= epoch(15, 12)) {
    scanPending := true.B
    scanIndex := 0.U
    scanBatch := nextBatch
  }

  private def index(address: UInt): UInt = (address ^ (address >> 6))(indexBits - 1, 0)
  private def metaIndex(key: LldpAddressKey): UInt = index(key.producerAddress ^ key.producer ^ key.consumer)

  private def sameKey(a: LldpAddressKey, b: LldpAddressKey): Bool =
    a.producer === b.producer && a.consumer === b.consumer && a.context === b.context &&
      a.producerAddress === b.producerAddress

  private def increase(value: UInt): UInt = value + !value.andR
  private def add(value: UInt, amount: UInt): UInt = {
    val sum = value +& amount
    Mux(sum(value.getWidth), Fill(value.getWidth, true.B), sum(value.getWidth - 1, 0))
  }
  private def sub(value: UInt, amount: UInt): UInt = Mux(value > amount, value - amount, 0.U)

  private val Seq(idle, sampleResult, promote, promoteResult, lookupResult, feedbackResult, ageResult) = Enum(7)
  private val state = RegInit(idle)
  private val train = RegEnable(io.train.bits, io.train.fire)
  private val lookup = RegEnable(io.lookup.bits, io.lookup.fire)
  private val feedback = RegEnable(io.feedback.bits, io.feedback.fire)
  private val sampleSet = RegEnable(index(io.train.bits.key.producerAddress), io.train.fire)
  private val metaSet = Reg(UInt(indexBits.W))
  private val promoteSample = Reg(sampleType)
  private val scanTurn = RegInit(false.B)
  private val scan = state === idle && scanPending && (scanTurn || !io.lookup.valid)
  io.feedback.ready := state === idle && !scan
  private val preferTraining = RegInit(false.B)
  io.lookup.ready := state === idle && !scan && !io.feedback.valid && !(preferTraining && io.train.valid)
  io.train.ready := state === idle && !scan && !io.feedback.valid && (!io.lookup.valid || preferTraining)
  when(io.train.fire || io.lookup.fire) {
    preferTraining := io.lookup.fire
  }

  private val readingMeta = scan || io.feedback.fire || io.lookup.fire || state === promote
  private val readSet = MuxCase(scanIndex, Seq(
    io.feedback.fire -> io.feedback.bits.ref.index,
    io.lookup.fire -> metaIndex(io.lookup.bits),
    (state === promote) -> metaIndex(promoteSample.key)
  ))
  when(readingMeta) {
    metaSet := readSet
  }
  when(state === idle && scanPending) {
    scanTurn := !scanTurn
  }

  private val updatedSamples = Wire(Vec(2, new SamplerEntry))
  private val sampleRow = sampler.read(index(io.train.bits.key.producerAddress), io.train.fire)
  private val sampleWays = sampleValid(sampleSet)
  private val hits = VecInit(sampleRow.zipWithIndex.map { case (entry, way) =>
    sampleWays(way) && sameKey(entry.sample.key, train.key)
  }).asUInt
  private val victim = Mux(!sampleWays.andR, Mux(sampleWays(0), 1.U, 0.U),
    (sampleRow(1).rrpv >= sampleRow(0).rrpv).asUInt)
  private val way = Mux(hits.orR, OHToUInt(hits), victim)
  private val oldSample = sampleRow(way)
  private val rebind = oldSample.sample.key.ruleGeneration =/= train.key.ruleGeneration
  private val sameTarget = oldSample.sample.target.paddr === train.target.paddr
  private val mismatch = oldSample.stable - oldSample.stable.orR
  private val confirmed = increase(oldSample.stable)
  private val promoteNow = hits.orR && !rebind && sameTarget &&
    (confirmed === 2.U || (confirmed > 2.U && oldSample.sincePromotion.andR))

  private val updatedSample = Wire(new SamplerEntry)
  updatedSample.sample := train
  updatedSample.stable := 1.U
  updatedSample.rrpv := 3.U
  updatedSample.sincePromotion := 0.U
  when(hits.orR && rebind) {
    updatedSample.rrpv := 2.U
  }
  when(hits.orR && !rebind) {
    updatedSample.stable := Mux(sameTarget, confirmed, Mux(mismatch.orR, mismatch, 1.U))
    updatedSample.rrpv := Mux(sameTarget, 0.U, 3.U)
    updatedSample.sincePromotion := Mux(promoteNow, 0.U, oldSample.sincePromotion + sameTarget)
    when(!sameTarget && mismatch.orR) {
      updatedSample.sample.target := oldSample.sample.target
    }
  }
  private val age = !hits.orR && sampleWays.andR
  private val ageDelta = 3.U - oldSample.rrpv
  for (i <- updatedSamples.indices) {
    val previousSample = WireInit(sampleRow(i))
    when(age) {
      previousSample.rrpv := sampleRow(i).rrpv + ageDelta
    }
    updatedSamples(i) := Mux(way === i.U, updatedSample, previousSample)
  }

  private val metaWrite = WireDefault(false.B)
  private val nextMeta = Wire(new MetaEntry)
  private val oldMeta = meta.read(readSet, readingMeta)
  when(metaWrite) {
    meta.write(metaSet, nextMeta)
  }
  assert(!(readingMeta && metaWrite))
  nextMeta := oldMeta
  private val lookupHit = metaValid(metaSet) && sameKey(oldMeta.sample.key, lookup) &&
    oldMeta.sample.key.ruleGeneration === lookup.ruleGeneration
  private val eligible = oldMeta.train >= 3.U && oldMeta.quality >= 2.U && oldMeta.tokens.orR && !oldMeta.busy
  io.candidate.valid := state === lookupResult && lookupHit && eligible
  io.candidate.bits.target := oldMeta.sample.target
  io.candidate.bits.ref.index := metaSet
  io.candidate.bits.ref.generation := generation + 1.U
  io.blocked := state === lookupResult && lookupHit && !eligible
  io.promoted := state === promoteResult
  private val ownerMatches = metaValid(metaSet) && oldMeta.busy && oldMeta.generation === feedback.ref.generation
  io.staleFeedback := state === feedbackResult && !ownerMatches

  switch(state) {
    is(idle) {
      when(scan) {
        state := ageResult
      }.elsewhen(io.feedback.fire) {
        state := feedbackResult
      }.elsewhen(io.lookup.fire) {
        state := lookupResult
      }.elsewhen(io.train.fire) {
        state := sampleResult
      }
    }
    is(sampleResult) {
      sampler.write(sampleSet, updatedSamples)
      sampleValid(sampleSet) := sampleWays | UIntToOH(way, 2)
      when(promoteNow) {
        promoteSample := train
      }
      state := Mux(promoteNow, promote, idle)
    }
    is(promote) {
      state := promoteResult
    }
    is(promoteResult) {
      metaWrite := true.B
      when(metaValid(metaSet) && sameKey(oldMeta.sample.key, promoteSample.key) &&
        oldMeta.sample.key.ruleGeneration === promoteSample.key.ruleGeneration) {
        when(oldMeta.sample.target.paddr === promoteSample.target.paddr) {
          nextMeta.sample.target.alias := promoteSample.target.alias
          nextMeta.train := increase(oldMeta.train)
          nextMeta.deadline := ageDeadline
        }.otherwise {
          nextMeta.train := sub(oldMeta.train, 2.U)
          nextMeta.tokens := 0.U
        }
      }.otherwise {
        generation := generation + 1.U
        nextMeta.sample := promoteSample
        nextMeta.generation := generation + 1.U
        nextMeta.train := 3.U
        nextMeta.quality := 4.U
        nextMeta.tokens := 2.U
        nextMeta.busy := false.B
        nextMeta.deadline := ageDeadline
        metaValid(metaSet) := true.B
      }
      state := idle
    }
    is(lookupResult) {
      when(io.candidate.valid && io.admit) {
        generation := generation + 1.U
        metaWrite := true.B
        nextMeta.generation := generation + 1.U
        nextMeta.tokens := oldMeta.tokens - 1.U
        nextMeta.busy := true.B
      }
      state := idle
    }
    is(feedbackResult) {
      when(ownerMatches) {
        metaWrite := true.B
        nextMeta.busy := false.B
        switch(feedback.outcome) {
          is(LldpAddressOutcome.Useful) {
            nextMeta.quality := add(oldMeta.quality, 2.U)
            nextMeta.tokens := add(oldMeta.tokens, 4.U)
          }
          is(LldpAddressOutcome.Merged) {
            nextMeta.quality := increase(oldMeta.quality)
            nextMeta.tokens := increase(oldMeta.tokens)
          }
          is(LldpAddressOutcome.Late) {
            nextMeta.tokens := sub(oldMeta.tokens, 1.U)
          }
          is(LldpAddressOutcome.Unused) {
            nextMeta.quality := sub(oldMeta.quality, 2.U)
            nextMeta.tokens := 0.U
            when(oldMeta.quality <= 2.U) {
              metaValid(metaSet) := false.B
            }
          }
          is(LldpAddressOutcome.Dropped) {
            nextMeta.tokens := increase(oldMeta.tokens)
          }
        }
      }
      state := idle
    }
    is(ageResult) {
      when(metaValid(metaSet) && oldMeta.deadline === scanBatch) {
        metaWrite := true.B
        nextMeta.train := sub(oldMeta.train, 1.U)
        nextMeta.deadline := scanBatch + 8.U
        nextMeta.busy := false.B
        when(oldMeta.train <= 1.U) {
          metaValid(metaSet) := false.B
        }
      }
      scanPending := !scanIndex.andR
      scanIndex := scanIndex + 1.U
      state := idle
    }
  }
}
