package xiangshan

import chisel3._
import chisel3.util._
import xscache.coupledL2.SpecMissAPreview

class SpecMissABuffer[T <: Data](gen: T, depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new Bundle {
      val data = gen.cloneType
      val speculative = Bool()
    }))
    val decision = Flipped(Valid(Bool()))
    val preview = new SpecMissAPreview(gen.cloneType)
    val deq = Decoupled(gen.cloneType)
    val deq_speculative = Output(Bool())
  })

  // L2 S1 sees every FIFO head, independently of the MissQueue decision.
  val head = Queue(io.enq, depth)
  io.preview.a.valid := head.valid
  io.preview.a.bits := head.bits.data
  io.preview.speculative := head.bits.speculative
  head.ready := io.preview.a.ready

  // At most one candidate enters this bank per cycle, with decisions arriving
  // in enqueue order. A one-bit FIFO retains decisions across S1 backpressure.
  val decisions = Module(new Queue(Bool(), depth, flow = true, pipe = true))
  decisions.io.enq.valid := io.decision.valid
  decisions.io.enq.bits := io.decision.bits
  assert(!io.decision.valid || decisions.io.enq.ready)
  val specS2 = RegNext(head.fire && head.bits.speculative, false.B)
  decisions.io.deq.ready := specS2
  val retry = RegNext(head.valid && head.bits.speculative && !head.ready, false.B)
  when(specS2 || retry) { assert(decisions.io.deq.valid) }
  io.preview.commit := decisions.io.deq.bits

  // S1 has already reserved capacity. Publish exactly the confirmed transfer
  // in S2, with its own payload, even while S1 is looking at the next head.
  io.deq.valid := RegNext(head.fire, false.B) && (!specS2 || decisions.io.deq.bits)
  io.deq.bits := RegEnable(head.bits.data, head.fire)
  io.deq_speculative := specS2
  when(io.deq.valid) { assert(io.deq.ready) }
}
