// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import bus.axi4._
import utils._

class AXI4RAM[T <: AXI4Lite](_type: T = new AXI4,
  memByte: Int, beatBytes: Int = 4, dataFile: String = "") extends AXI4SlaveModule(_type) {
  val mem = Mem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))
  if (dataFile != "") loadMemoryFromFile(mem, dataFile)

  def index(addr: UInt) = addr >> log2Ceil(beatBytes)

  val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
  when (in.w.fire()) {
    mem.write(index(waddr) + writeBeatCnt, wdata, in.w.bits.strb.toBools)
  }
  in.b.valid := BoolStopWatch(in.w.fire() && wLast, in.b.fire(), startHighPriority = true)

  val ren = in.ar.fire() || (in.r.fire() && !rLast)
  in.r.bits.data := RegEnable(Cat(mem.read(index(raddr) + readBeatCnt).reverse), ren)
  in.r.valid := BoolStopWatch(ren && (in.ar.fire() || r_busy), in.r.fire(), startHighPriority = true)
}
