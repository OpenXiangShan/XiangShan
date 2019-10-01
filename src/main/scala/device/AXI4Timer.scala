package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class AXI4Timer extends AXI4SlaveModule(new AXI4Lite) {
  val clk = 40000 // 40MHz / 1000
  val tick = Counter(true.B, clk)._2
  val ms = Counter(tick, 0x40000000)._1
  in.r.bits.data := ms
}
