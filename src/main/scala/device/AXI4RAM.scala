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

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(beatBytes)
  def inRange(idx: UInt) = idx < (memByte / 4).U

  val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
  val wIdx = index(waddr) + writeBeatCnt
  when (in.w.fire() && inRange(wIdx)) {
    mem.write(wIdx, wdata, in.w.bits.strb.toBools)
  }

  in.r.bits.data := RegEnable(Cat(mem.read(index(raddr) + readBeatCnt).reverse), ren)
}
