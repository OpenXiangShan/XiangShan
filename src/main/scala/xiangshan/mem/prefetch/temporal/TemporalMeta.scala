package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._

private[prefetch] object TemporalMetaOp extends ChiselEnum {
  val Query, Install, Feedback = Value
}

private[xiangshan] class TemporalRecordRef extends Bundle {
  val slot = UInt(15.W)
  val generation = UInt(15.W)
}

/** Completes a directory transaction before accepting the next one so a slot never names a partially written record. */
private[prefetch] class TemporalMeta(
  lineBits: Int,
  aliasBits: Int,
  contextBits: Int,
  sets: Int = 1536,
  ways: Int = 16,
  compactSlots: Int = 18432,
  fullSlots: Int = 4608
) extends Module {
  import TemporalAddress._
  import TemporalMetaOp._

  require(sets > 1 && ways > 1 && isPow2(ways) && sets * ways <= (1 << 15))
  private val setBits = log2Ceil(sets)
  private val wayBits = log2Ceil(ways)
  private val implicitBits = Integer.numberOfTrailingZeros(sets)
  require(implicitBits < lineBits && implicitBits <= 10)

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val op = TemporalMetaOp()
      val record = new TemporalRecord(lineBits, aliasBits, contextBits, PcBits)
      val allowReplay = Bool()
      val ref = new TemporalRecordRef
      val priority = UInt(2.W)
    }))
    val response = Decoupled(new Bundle {
      val hit = Bool()
      val success = Bool()
      val record = new TemporalRecord(lineBits, aliasBits, contextBits, PcBits)
      val ref = new TemporalRecordRef
    })
    val generationWrap = Decoupled(UInt(15.W))
  })

  private class Entry extends Bundle {
    val pc = UInt(PcBits.W)
    val tag = UInt((lineBits - implicitBits).W)
    val context = UInt(contextBits.W)
    val roles = UInt(2.W)
    val kind = UInt(2.W)
    val payload = new TemporalPoolRef
    val generation = UInt(15.W)
    val rrpv = UInt(2.W)
  }

  private val pool = Module(new TemporalRecordPools(lineBits, aliasBits, compactSlots, fullSlots))
  private val directory = SyncReadMem(sets, Vec(ways, new Entry))
  private val valid = RegInit(VecInit(Seq.fill(sets)(0.U(ways.W))))
  private val idle :: inspect :: loadOld :: install :: write :: wrap :: respond :: Nil = Enum(7)
  private val state = RegInit(idle)
  private val request = RegEnable(io.request.bits, io.request.fire)
  private val response = Reg(chiselTypeOf(io.response.bits))
  private val pendingRow = Reg(Vec(ways, new Entry))
  private val pendingWay = Reg(UInt(wayBits.W))
  private val readSet = Wire(UInt(setBits.W))
  readSet := Mux(
    io.request.bits.op === Feedback,
    io.request.bits.ref.slot >> wayBits,
    metaHash(io.request.bits.record.pc, io.request.bits.record.trigger) % sets.U
  )
  private val pendingSet = RegEnable(readSet, io.request.fire)
  private val samePayload = RegInit(false.B)
  private val installSent = RegInit(false.B)
  private val directoryWrite = state === write
  private val row = directory.readWrite(
    Mux(directoryWrite, pendingSet, readSet),
    pendingRow,
    io.request.fire || directoryWrite,
    directoryWrite
  )

  private val key = request.record
  private val tag = key.trigger(lineBits - 1, implicitBits)
  private val hits = VecInit(row.zipWithIndex.map { case (entry, i) =>
    valid(pendingSet)(i) && entry.pc === key.pc && entry.context === key.context && entry.tag === tag
  })
  private val hit = hits.asUInt.orR
  private val emptyMask = ~valid(pendingSet)
  private val maxPriority = row.map(_.rrpv).reduce((a, b) => Mux(a >= b, a, b))
  private val oldest = VecInit(row.map(_.rrpv === maxPriority)).asUInt
  private val needsAging = request.op === Install && !hit && !emptyMask.orR
  private val way = Mux(
    request.op === Feedback,
    request.ref.slot(wayBits - 1, 0),
    Mux(hit, OHToUInt(hits), Mux(emptyMask.orR, PriorityEncoder(emptyMask), PriorityEncoder(oldest)))
  )
  private val slot = Cat(pendingSet, way).pad(15)
  private val entry = row(way)
  private val entryValid = valid(pendingSet)(way)

  io.request.ready := state === idle
  io.response.valid := state === respond
  io.response.bits := response
  io.generationWrap.valid := state === wrap
  io.generationWrap.bits := response.ref.slot

  private val readPayload = state === inspect && hit &&
    ((request.op === Query && request.allowReplay) || request.op === Install)
  pool.io.request.valid := readPayload || (state === install && !installSent)
  pool.io.request.bits.install := state === install
  pool.io.request.bits.previous := Mux(state === install, pendingRow(pendingWay).payload, entry.payload)
  pool.io.request.bits.trigger := key.trigger
  pool.io.request.bits.length := key.length
  pool.io.request.bits.targets := key.targets
  pool.io.response.ready := state === loadOld || (state === install && installSent)

  when(io.request.fire) {
    when(io.request.bits.op === Feedback) {
      assert(io.request.bits.ref.slot < (sets * ways).U)
    }
    state := inspect
  }
  when(state === inspect) {
    pendingRow := row
    pendingWay := way
    response := 0.U.asTypeOf(response)
    response.ref.slot := slot
    response.ref.generation := Mux(entryValid, entry.generation, 0.U)
    response.hit := hit && request.op =/= Feedback
    response.record := key
    samePayload := false.B
    installSent := false.B

    when(request.op === Feedback) {
      val current = entryValid && entry.generation === request.ref.generation
      response.success := current
      when(current) {
        pendingRow(way).rrpv := request.priority
      }
      state := Mux(current, write, respond)
    }.elsewhen(readPayload) {
      assert(pool.io.request.ready)
      response.record.roles := entry.roles | key.roles
      response.record.kind := entry.kind
      state := loadOld
    }.elsewhen(request.op === Install) {
      when(!entryValid) {
        pendingRow(way).payload.slot := TemporalPoolRef.Invalid.U
        pendingRow(way).generation := 0.U
      }
      when(needsAging) {
        for (i <- 0 until ways) {
          pendingRow(i).rrpv := row(i).rrpv + (3.U - maxPriority)
        }
      }
      state := install
    }.otherwise {
      state := respond
    }
  }

  when(state === loadOld && pool.io.response.fire) {
    val length = pool.io.response.bits.ref.length
    val targets = pool.io.response.bits.targets
    when(request.op === Query) {
      response.success := true.B
      response.record.length := length
      response.record.targets := targets
      state := respond
    }.otherwise {
      val matches = targets.zip(key.targets).zipWithIndex.map { case ((old, next), i) =>
        i.U >= key.length || old.line === next.line
      }
      samePayload := length === key.length && matches.reduce(_ && _)
      state := install
    }
  }

  when(state === install && pool.io.request.fire) {
    installSent := true.B
  }
  when(state === install && pool.io.response.fire) {
    val next = pendingRow(pendingWay)
    response.success := pool.io.response.bits.success
    when(pool.io.response.bits.success) {
      next.pc := key.pc
      next.tag := tag
      next.context := key.context
      next.roles := key.roles
      next.kind := key.kind
      next.payload := pool.io.response.bits.ref
      val generation = next.generation + 1.U
      val reuseGeneration = next.generation.andR && !samePayload
      when(!samePayload) {
        next.generation := generation
        next.rrpv := 2.U
        response.ref.generation := generation
      }
      valid(pendingSet) := valid(pendingSet) | UIntToOH(pendingWay, ways)
      state := Mux(reuseGeneration, wrap, write)
    }.otherwise {
      // Keep the old key and payload but persist aging from the attempted allocation.
      state := Mux(!response.hit && !emptyMask.orR, write, respond)
    }
  }

  when(io.generationWrap.fire) {
    state := write
  }
  when(directoryWrite) {
    state := respond
  }
  when(io.response.fire) {
    state := idle
  }
}
