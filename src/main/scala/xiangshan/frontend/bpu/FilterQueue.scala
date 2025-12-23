package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._

class FilterQueue[T <: Data](
    gen:          T,
    numEntries:   Int,
    pipe:         Boolean = false,
    flow:         Boolean = false,
    hasFilter:    Boolean = false,
    hasOverrider: Boolean = false,
    filter:       (T, T) => Bool = (oldData: T, newData: T) => false.B,
    overrider:    (T, T) => T = (oldData: T, newData: T) => oldData
) extends Module {
  require(numEntries >= 1, "FilterQueue numEntries should be at least 1")

  class FilterQueueIO extends Bundle {
    val enq:    DecoupledIO[T] = Flipped(Decoupled(gen))
    val deq:    DecoupledIO[T] = Decoupled(gen)
    val filter: Valid[T]       = Input(Valid(gen))
  }

  val io: FilterQueueIO = IO(new FilterQueueIO)

  private val ram     = Reg(Vec(numEntries, gen))
  private val valMask = RegInit(VecInit(Seq.fill(numEntries)(false.B)))

  private val enqPtr    = Counter(numEntries)
  private val deqPtr    = Counter(numEntries)
  private val maybeFull = RegInit(false.B)

  private val ptrMatch = enqPtr.value === deqPtr.value
  private val empty    = ptrMatch && !maybeFull
  private val full     = ptrMatch && maybeFull

  private val headIsBubble = !empty && !valMask(deqPtr.value)

  private val doDeq  = io.deq.ready && io.deq.valid
  private val doEnq  = io.enq.ready && io.enq.valid
  private val doSkip = headIsBubble

  private val advDeqPtr = doDeq || doSkip

  // --- Enqueue Logic ---
  when(doEnq) {
    ram(enqPtr.value)     := io.enq.bits
    valMask(enqPtr.value) := true.B
    enqPtr.inc()
  }

  // --- Filter Logic ---
  when(io.filter.valid) {
    for (i <- 0 until numEntries) {
      val isEnqHit   = doEnq && (enqPtr.value === i.U)
      val filterData = Mux(isEnqHit, io.enq.bits, ram(i))
      when(filter(filterData, io.filter.bits)) {
        if (hasFilter) {
          valMask(i) := false.B
        }
        if (hasOverrider) {
          ram(i) := overrider(filterData, io.filter.bits)
        }
      }
    }
  }

  // --- Dequeue Logic ---
  when(advDeqPtr) {
    deqPtr.inc()
  }

  // --- Empty/Full ---
  when(doEnq =/= advDeqPtr) {
    maybeFull := doEnq
  }

  // --- Output Assignments ---
  io.enq.ready := !full
  io.deq.valid := !empty && !headIsBubble
  io.deq.bits  := ram(deqPtr.value)

  // --- Pipe & Flow Support ---
  if (pipe) {
    io.enq.ready := !full || advDeqPtr
  }

  if (flow) {
    when(empty) {
      val hitFilter = io.filter.valid && filter(io.enq.bits, io.filter.bits)
      val flowValid = io.enq.valid && !(hitFilter && hasFilter.B)
      io.deq.valid := flowValid
      io.deq.bits  := Mux(hitFilter && hasOverrider.B, overrider(io.enq.bits, io.filter.bits), io.enq.bits)
    }
  }
}
