package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class TimerIO extends Bundle {
  val mtip = Output(Bool())
}

class AXI4Timer(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite, new TimerIO) {
  val mtime = RegInit(0.U(64.W))  // unit: ms
  val mtimecmp = RegInit(0.U(64.W))

  val clk = (if (!sim) 40000 /* 40MHz / 1000 */ else 10000)
  val tick = Counter(true.B, clk)._2
  when (tick) { mtime := mtime + 1.U }

  val mapping = Map(
    RegMap(0x0, mtime),
    RegMap(0x8, mtimecmp)
  )

  RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
    waddr(3,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.mtip := RegNext(mtime >= mtimecmp)
}
