package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._

private[prefetch] object TemporalPoolRef {
  val SlotBits = 15
  val Invalid = (1 << SlotBits) - 1
}

private[prefetch] class TemporalPoolRef extends Bundle {
  val slot = UInt(TemporalPoolRef.SlotBits.W)
  val compact = Bool()
  // Completed records have 9 to 12 targets.
  val lengthCode = UInt(2.W)
  def length: UInt = lengthCode +& 9.U
  def validSlot: Bool = slot =/= TemporalPoolRef.Invalid.U
}

/** Compact (signed 16-bit line offsets) and full (physical line numbers) SRAMs.
  * Each slot stores the target aliases alongside the offsets or line numbers.
  * An unused slot links to the next released slot in its first word.
  */
private[prefetch] class TemporalRecordPools(
  lineBits: Int,
  aliasBits: Int,
  compactSlots: Int,
  fullSlots: Int
) extends Module {
  import TemporalAddress.RecordLength

  require(lineBits >= 16 && aliasBits >= 1)
  require(compactSlots > 1 && compactSlots < TemporalPoolRef.Invalid)
  require(fullSlots > 1 && fullSlots < TemporalPoolRef.Invalid)
  private val lineType = new TemporalLine(lineBits, aliasBits)
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val install = Bool()
      val previous = new TemporalPoolRef
      val trigger = UInt(lineBits.W)
      val length = UInt(log2Ceil(RecordLength + 1).W)
      val targets = Vec(RecordLength, lineType.cloneType)
    }))
    val response = Decoupled(new Bundle {
      val success = Bool()
      val ref = new TemporalPoolRef
      val targets = Vec(RecordLength, lineType.cloneType)
    })
  })

  private val compactRow = Vec(RecordLength, new TemporalLine(16, aliasBits))
  private val fullRow = Vec(RecordLength, lineType.cloneType)
  private val compact = SyncReadMem(compactSlots, compactRow)
  private val full = SyncReadMem(fullSlots, fullRow)
  private val compactHead = RegInit(TemporalPoolRef.Invalid.U(TemporalPoolRef.SlotBits.W))
  private val fullHead = RegInit(TemporalPoolRef.Invalid.U(TemporalPoolRef.SlotBits.W))
  private val compactNext = RegInit(0.U(log2Ceil(compactSlots + 1).W))
  private val fullNext = RegInit(0.U(log2Ceil(fullSlots + 1).W))

  private val idle :: lookup :: readPayload :: popFree :: writePayload :: release :: respond :: Nil = Enum(7)
  private val state = RegInit(idle)
  private val request = RegEnable(io.request.bits, io.request.fire)
  private val response = Reg(chiselTypeOf(io.response.bits))
  private val newRef = Reg(new TemporalPoolRef)

  io.request.ready := state === idle
  io.response.valid := state === respond
  io.response.bits := response

  private val deltas = request.targets.map(target => target.line.zext - request.trigger.zext)
  private val encodable = deltas.zipWithIndex.map { case (delta, index) =>
    index.U >= request.length || (delta >= (-32768).S && delta <= 32767.S)
  }.reduce(_ && _)
  private val compactReleased = compactHead =/= TemporalPoolRef.Invalid.U
  private val fullReleased = fullHead =/= TemporalPoolRef.Invalid.U
  private val reuseCompact = request.previous.validSlot && request.previous.compact
  private val reuseFull = request.previous.validSlot && !request.previous.compact
  private val selectCompact = encodable && (reuseCompact || compactReleased || compactNext < compactSlots.U)
  private val reuse = Mux(selectCompact, reuseCompact, reuseFull)
  private val freeHead = Mux(selectCompact, compactHead, fullHead)
  private val released = Mux(selectCompact, compactReleased, fullReleased)
  private val next = Mux(selectCompact, compactNext, fullNext)
  private val allocSlot = Mux(reuse, request.previous.slot, Mux(released, freeHead, next))
  private val available = selectCompact || reuseFull || fullReleased || fullNext < fullSlots.U
  private val pop = state === lookup && request.install && available && !reuse && released
  private val read = state === lookup && !request.install
  private val write = state === writePayload
  private val free = state === release

  private val compactW = WireDefault(0.U.asTypeOf(compactRow))
  private val fullW = WireDefault(0.U.asTypeOf(fullRow))
  when(free) {
    compactW.head.line := compactHead
    fullW.head.line := fullHead
  }.otherwise {
    for (index <- 0 until RecordLength) {
      compactW(index).line := deltas(index).asUInt(15, 0)
      compactW(index).alias := request.targets(index).alias
      fullW(index) := request.targets(index)
    }
  }

  private val accessSlot = Mux(free || read, request.previous.slot, Mux(pop, freeHead, newRef.slot))
  private val accessCompact = Mux(free || read, request.previous.compact, Mux(pop, selectCompact, newRef.compact))
  private val access = pop || read || write || free
  private val compactRead = compact.readWrite(
    accessSlot(log2Ceil(compactSlots) - 1, 0),
    compactW,
    access && accessCompact,
    write || free
  )
  private val fullRead = full.readWrite(
    accessSlot(log2Ceil(fullSlots) - 1, 0),
    fullW,
    access && !accessCompact,
    write || free
  )

  when(io.request.fire) {
    state := lookup
  }
  when(state === lookup) {
    when(!request.install) {
      assert(request.previous.validSlot)
      assert(request.previous.slot < Mux(request.previous.compact, compactSlots.U, fullSlots.U))
      state := readPayload
    }.elsewhen(available) {
      assert(request.length >= 9.U && request.length <= RecordLength.U)
      newRef.slot := allocSlot
      newRef.compact := selectCompact
      newRef.lengthCode := request.length - 9.U
      when(pop) {
        state := popFree
      }.otherwise {
        when(!reuse && selectCompact) {
          compactNext := compactNext + 1.U
        }
        when(!reuse && !selectCompact) {
          fullNext := fullNext + 1.U
        }
        state := writePayload
      }
    }.otherwise {
      response.success := false.B
      response.ref := request.previous
      response.targets := 0.U.asTypeOf(response.targets)
      state := respond
    }
  }

  when(state === readPayload) {
    response.success := true.B
    response.ref := request.previous
    for (index <- 0 until RecordLength) {
      val decoded = request.trigger + compactRead(index).line.asSInt.pad(lineBits).asUInt
      response.targets(index).line := Mux(request.previous.compact, decoded, fullRead(index).line)
      response.targets(index).alias := Mux(request.previous.compact, compactRead(index).alias, fullRead(index).alias)
    }
    state := respond
  }

  when(state === popFree) {
    when(newRef.compact) {
      compactHead := compactRead.head.line(TemporalPoolRef.SlotBits - 1, 0)
    }.otherwise {
      fullHead := fullRead.head.line(TemporalPoolRef.SlotBits - 1, 0)
    }
    state := writePayload
  }

  when(write) {
    val releaseOld = request.previous.validSlot &&
      (request.previous.compact =/= newRef.compact || request.previous.slot =/= newRef.slot)
    response.success := true.B
    response.ref := newRef
    response.targets := 0.U.asTypeOf(response.targets)
    state := Mux(releaseOld, release, respond)
  }

  when(free) {
    when(request.previous.compact) {
      compactHead := request.previous.slot
    }.otherwise {
      fullHead := request.previous.slot
    }
    state := respond
  }
  when(io.response.fire) {
    state := idle
  }
}
