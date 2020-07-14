package xiangshan.utils

import chisel3._
import chisel3.util._  // can't use chisel3_ version because of compile order

class FlushableQueueIO[T <: Data](private val gen: T, entries: Int) extends QueueIO(gen, entries) {
  val flush = Input(Bool())
}

class FlushableQueue[T <: Data](gen: T, val entries: Int,
  pipe: Boolean = false, flow: Boolean = false) extends Module() {
  val genType = gen

  val io = IO(new FlushableQueueIO(genType, entries))

  private val ram = Mem(entries, genType)
  private val enq_ptr = Counter(entries)
  private val deq_ptr = Counter(entries)
  private val maybe_full = RegInit(false.B)

  private val ptr_match = enq_ptr.value === deq_ptr.value
  private val empty = ptr_match && !maybe_full
  private val full = ptr_match && maybe_full
  private val do_enq = WireInit(io.enq.fire())
  private val do_deq = WireInit(io.deq.fire())

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  when (io.flush) {
    if (entries > 1) {
      enq_ptr.value := 0.U
      deq_ptr.value := 0.U
    }
    maybe_full := false.B
  }

  private val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  } else {
    io.count := Mux(ptr_match,
                    Mux(maybe_full,
                      entries.asUInt, 0.U),
                    Mux(deq_ptr.value > enq_ptr.value,
                      entries.asUInt + ptr_diff, ptr_diff))
  }
}

object FlushableQueue {
  /** Create a queue and supply a DecoupledIO containing the product. */
  def apply[T <: Data](enq: ReadyValidIO[T], flush: Bool, entries: Int = 2,
      pipe: Boolean = false, flow: Boolean = false): DecoupledIO[T] = {
    if (entries == 0) {
      val deq = Wire(new DecoupledIO(enq.bits))
      deq.valid := enq.valid
      deq.bits := enq.bits
      enq.ready := deq.ready
      deq
    } else {
      require(entries > 0)
      val q = Module(new FlushableQueue(chiselTypeOf(enq.bits), entries, pipe, flow))
      q.io.enq.valid := enq.valid // not using <> so that override is allowed
      q.io.enq.bits := enq.bits
      q.io.flush := flush
      enq.ready := q.io.enq.ready
      TransitName(q.io.deq, q)
    }
  }
}
