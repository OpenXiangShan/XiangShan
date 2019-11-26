package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class TimerIO extends Bundle {
  val mtip = Output(Bool())
}

class AXI4Timer(sim: Boolean = false) extends AXI4SlaveModule(new AXI4Lite, new TimerIO) {
  val mtime = RegInit(0.U(64.W))  // unit: us
  val mtimecmp = RegInit(0.U(64.W))

  val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 10000)
  val tick = Counter(true.B, clk)._2
  when (tick) { mtime := mtime + 1.U }

  val mapping = Map(
    RegMap(0x4000, mtimecmp),
    RegMap(0xbff8, mtime)
  )
  def getOffset(addr: UInt) = addr(15,0)

  RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
    getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

  io.extra.get.mtip := RegNext(mtime >= mtimecmp)
}
