package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._
import utility.ReplacementPolicy

private[xiangshan] class LldpDataType extends Bundle {
  val size = UInt(2.W)
  val sext = Bool()
  val floating = Bool()
}

private[xiangshan] class LldpDependence(pcBits: Int) extends Bundle {
  val producer = UInt(pcBits.W)
  val consumer = UInt(pcBits.W)
  val descriptor = new LldpReplayDescriptor
  val consumerType = new LldpDataType
}

private[xiangshan] object LldpValueSource extends ChiselEnum {
  val Demand, Spatial, Chain = Value
}

private[prefetch] class LldpValueHint(rowBits: Int, generationBits: Int, offsetBits: Int) extends Bundle {
  val row = UInt(rowBits.W)
  val generation = UInt(generationBits.W)
  val dataType = new LldpDataType
  val offset = UInt(offsetBits.W)
  val source = LldpValueSource()
}

private[prefetch] class LldpTable(pcBits: Int, contextBits: Int, offsetBits: Int = 6, rows: Int = 64)
    extends Module {
  require(rows >= 2 && isPow2(rows))
  private val rowBits = log2Ceil(rows)
  private val generationBits = 32

  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new Bundle {
      val dependence = new LldpDependence(pcBits)
      val context = UInt(contextBits.W)
    }))
    val role = Decoupled(new Bundle {
      val pc = UInt(pcBits.W)
      val context = UInt(contextBits.W)
      val roles = UInt(2.W)
    })
    val lookup = Flipped(Valid(new Bundle {
      val pc = UInt(pcBits.W)
      val context = UInt(contextBits.W)
      val dataType = new LldpDataType
      val offset = UInt(offsetBits.W)
      val source = LldpValueSource()
      val depth = Bool()
    }))
    val hint = Valid(new LldpValueHint(rowBits, generationBits, offsetBits))
    val addressRule = Valid(new Bundle {
      val consumer = UInt(pcBits.W)
      val dataType = new LldpDataType
      val generation = UInt(generationBits.W)
    })
    val addressTrainKey = Input(new Bundle {
      val producer = UInt(pcBits.W)
      val consumer = UInt(pcBits.W)
      val context = UInt(contextBits.W)
    })
    val addressTrainGeneration = Valid(UInt(generationBits.W))
    val returned = Flipped(Decoupled(new Bundle {
      val hint = new LldpValueHint(rowBits, generationBits, offsetBits)
      val bytes = UInt(64.W)
    }))
    val candidate = Decoupled(new Bundle {
      val vaddr = UInt(64.W)
      val context = UInt(contextBits.W)
      val consumer = UInt(pcBits.W)
      val dataType = new LldpDataType
      val source = LldpValueSource()
    })
  })

  private class Entry extends Bundle {
    val dependence = new LldpDependence(pcBits)
    val context = UInt(contextBits.W)
    val generation = UInt(generationBits.W)
    val producerConf = UInt(3.W)
    val consumerConf = UInt(3.W)
    val offsetConf = UInt(3.W)
  }

  private val rowsValid = RegInit(0.U(rows.W))
  private val entries = Reg(Vec(rows, new Entry))
  private val generation = RegInit(0.U(generationBits.W))
  private val replace = ReplacementPolicy.fromString("plru", rows)
  private val train = Module(new Queue(chiselTypeOf(io.train.bits), 16))
  train.io.enq <> io.train

  private val roles = Reg(chiselTypeOf(io.train.bits))
  private val noRole :: producerRole :: consumerRole :: Nil = Enum(3)
  private val roleState = RegInit(noRole)
  private val roleLast = roleState === consumerRole || roles.dependence.producer === roles.dependence.consumer
  io.role.valid := roleState =/= noRole
  io.role.bits.context := roles.context
  io.role.bits.pc := Mux(roleState === producerRole, roles.dependence.producer, roles.dependence.consumer)
  io.role.bits.roles := Mux(roles.dependence.producer === roles.dependence.consumer, 3.U,
    Mux(roleState === producerRole, 1.U, 2.U))
  train.io.deq.ready := roleState === noRole || (io.role.fire && roleLast)
  when(io.role.fire) {
    roleState := Mux(roleLast, noRole, consumerRole)
  }

  private def matches(pc: UInt, context: UInt): UInt = VecInit(entries.zipWithIndex.map { case (entry, index) =>
    rowsValid(index) && entry.context === context && entry.dependence.producer === pc
  }).asUInt

  private def increase(value: UInt): UInt = value + !value.andR

  private val sample = train.io.deq.bits
  private val trainHits = matches(sample.dependence.producer, sample.context)
  private val trainHit = trainHits.orR
  private val trainRow = Mux(trainHit, OHToUInt(trainHits),
    Mux(rowsValid.andR, replace.way, PriorityEncoder(~rowsValid)))
  private val trainOld = entries(trainRow)
  private val sameConsumer = trainHit && trainOld.dependence.consumer === sample.dependence.consumer
  private val sameOffset = trainOld.dependence.descriptor.displacement === sample.dependence.descriptor.displacement
  when(train.io.deq.fire) {
    val entry = entries(trainRow)
    entry.dependence := sample.dependence
    entry.context := sample.context
    entry.producerConf := Mux(!trainHit, 2.U,
      Mux(sameConsumer, trainOld.producerConf, increase(trainOld.producerConf)))
    entry.consumerConf := Mux(sameConsumer, increase(trainOld.consumerConf), 1.U)
    entry.offsetConf := Mux(!sameConsumer, 1.U,
      Mux(sameOffset, increase(trainOld.offsetConf), trainOld.offsetConf - trainOld.offsetConf.orR))
    when(!sameConsumer) {
      generation := generation + 1.U
      entry.generation := generation + 1.U
    }
    when(!trainHit) {
      rowsValid := rowsValid | UIntToOH(trainRow, rows)
    }
    roles := sample
    roleState := producerRole
  }

  private val lookup = io.lookup.bits
  private val lookupHits = matches(lookup.pc, lookup.context)
  private val lookupRow = OHToUInt(lookupHits)
  private val lookupEntry = entries(lookupRow)
  private val dataFits = (lookup.offset +& (1.U(8.W) << lookup.dataType.size)) <= (1 << offsetBits).U
  private val depthFits = lookup.source =/= LldpValueSource.Chain || !lookup.depth
  io.hint.valid := io.lookup.valid && lookupHits.orR && lookupEntry.producerConf >= 2.U &&
    dataFits && depthFits && !lookup.dataType.floating
  io.hint.bits.row := lookupRow
  io.hint.bits.generation := lookupEntry.generation
  io.hint.bits.dataType := lookup.dataType
  io.hint.bits.offset := lookup.offset
  io.hint.bits.source := lookup.source
  io.addressRule.valid := io.lookup.valid && lookupHits.orR && lookupEntry.producerConf >= 2.U &&
    lookupEntry.consumerConf >= 3.U && dataFits && depthFits
  io.addressRule.bits.consumer := lookupEntry.dependence.consumer
  io.addressRule.bits.dataType := lookupEntry.dependence.consumerType
  io.addressRule.bits.generation := lookupEntry.generation
  private val addressTrainHits = matches(io.addressTrainKey.producer, io.addressTrainKey.context)
  private val addressTrainEntry = entries(OHToUInt(addressTrainHits))
  io.addressTrainGeneration.valid := addressTrainHits.orR &&
    addressTrainEntry.dependence.consumer === io.addressTrainKey.consumer
  io.addressTrainGeneration.bits := addressTrainEntry.generation

  private val lookupTouch = io.lookup.valid && lookupHits.orR && depthFits
  when(train.io.deq.fire || lookupTouch) {
    replace.access(Mux(train.io.deq.fire, trainRow, lookupRow))
  }

  private val replay = Module(new LldpReplay(sourceBits = 2))
  private val hint = io.returned.bits.hint
  private val returnedEntry = entries(hint.row)
  private val isReplacing = train.io.deq.fire && !sameConsumer && trainRow === hint.row
  private val current = rowsValid(hint.row) && returnedEntry.generation === hint.generation && !isReplacing
  private val replayable = current && returnedEntry.consumerConf >= 3.U && returnedEntry.offsetConf >= 3.U &&
    returnedEntry.dependence.descriptor.length =/= 0.U && !hint.dataType.floating
  io.returned.ready := !replayable || replay.io.input.ready
  replay.io.input.valid := io.returned.valid && replayable
  replay.io.input.bits.bytes := io.returned.bits.bytes
  replay.io.input.bits.size := hint.dataType.size
  replay.io.input.bits.sext := hint.dataType.sext
  replay.io.input.bits.floating := hint.dataType.floating
  replay.io.input.bits.source := hint.source.asUInt
  replay.io.input.bits.descriptor := returnedEntry.dependence.descriptor

  private val replayEntry = RegEnable(returnedEntry, replay.io.input.fire)
  private val replaySource = RegEnable(hint.source, replay.io.input.fire)
  io.candidate.valid := replay.io.output.valid
  io.candidate.bits.vaddr := replay.io.output.bits.vaddr
  io.candidate.bits.context := replayEntry.context
  io.candidate.bits.consumer := replayEntry.dependence.consumer
  io.candidate.bits.dataType := replayEntry.dependence.consumerType
  io.candidate.bits.source := replaySource
  replay.io.output.ready := io.candidate.ready
}
