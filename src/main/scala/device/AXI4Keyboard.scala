package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class KeyboardIO extends Bundle {
  val ps2Clk = Input(Bool())
  val ps2Data = Input(Bool())
}

// this Module is not tested
class AXI4Keyboard extends AXI4SlaveModule(new AXI4Lite, new KeyboardIO) {
  val buf = Reg(UInt(10.W))
  val ps2ClkLatch = RegNext(io.extra.get.ps2Clk)
  val negedge = RegNext(ps2ClkLatch) && ~ps2ClkLatch
  when (negedge) { buf := Cat(io.extra.get.ps2Data, buf(9,1)) }

  val cnt = Counter(negedge, 10)
  val queue = Module(new Queue(UInt(8.W), 8))
  queue.io.enq.valid := cnt._2 && !buf(0) && io.extra.get.ps2Data && buf(9,1).xorR
  queue.io.enq.bits := buf(8,1)
  queue.io.deq.ready := in.r.ready

  in.r.bits.data := Mux(queue.io.deq.valid, queue.io.deq.bits, 0.U)
}
