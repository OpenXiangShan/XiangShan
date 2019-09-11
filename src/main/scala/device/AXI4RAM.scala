// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import bus.axi4._
import utils._

class RAMHelper(memByte: Int) extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rIdx = Input(UInt(64.W))
    val rdata = Output(UInt(64.W))
    val wIdx = Input(UInt(64.W))
    val wdata = Input(UInt(64.W))
    val wmask = Input(UInt(64.W))
    val wen = Input(Bool())
  })
}

class AXI4RAM[T <: AXI4Lite](_type: T = new AXI4, memByte: Int, beatBytes: Int = 8,
  useBlackBox: Boolean = false) extends AXI4SlaveModule(_type) {

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(beatBytes)
  def inRange(idx: UInt) = idx < (memByte / 8).U

  val wIdx = index(waddr) + writeBeatCnt
  val rIdx = index(raddr) + readBeatCnt
  val wen = in.w.fire() && inRange(wIdx)

  val rdata = if (useBlackBox) {
    val mem = Module(new RAMHelper(memByte))
    mem.io.clk := clock
    mem.io.rIdx := rIdx
    mem.io.wIdx := wIdx
    mem.io.wdata := in.w.bits.data
    mem.io.wmask := Cat(in.w.bits.strb.asBools.map(Mux(_, 0xff.U, 0.U)).reverse)
    mem.io.wen := wen
    mem.io.rdata
  } else {
    val mem = Mem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))

    val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (wen) { mem.write(wIdx, wdata, in.w.bits.strb.asBools) }

    Cat(mem.read(rIdx).reverse)
  }

  in.r.bits.data := RegEnable(rdata, ren)
}
