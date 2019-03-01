// See LICENSE.SiFive for license details.

package device

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import bus.axi4._

sealed abstract class RAM[T <: AXI4Lite](_type: T,
  memByte: Int, beatBytes: Int = 4, dataFile: String = "") extends Module {
  val io = IO(new Bundle{
    val in = Flipped(_type)
  })

  val in = io.in
  val mem = Mem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))
  if (dataFile != "") loadMemoryFromFile(mem, dataFile)

  val r_addr = in.ar.bits.addr >> log2Ceil(beatBytes)
  val w_addr = in.aw.bits.addr >> log2Ceil(beatBytes)

  val w_full = RegInit(false.B)
  when (in. b.fire()) { w_full := false.B }
  when (in.aw.fire()) { w_full := true.B }

  val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
  when (in.aw.fire()) {
    mem.write(w_addr, wdata, in.w.bits.strb.toBools)
  }

  in. b.valid := w_full
  in.aw.ready := in. w.valid && (in.b.ready || !w_full)
  in. w.ready := in.aw.valid && (in.b.ready || !w_full)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY

  val r_full = RegInit(false.B)
  when (in. r.fire()) { r_full := false.B }
  when (in.ar.fire()) { r_full := true.B }

  def holdUnless[T <: Data](x: T, enable: Bool): T = Mux(enable, x, RegEnable(x, enable))

  val ren = in.ar.fire()
  val rdata = RegEnable(mem.read(r_addr), ren)

  in. r.valid := r_full
  in.ar.ready := in.r.ready || !r_full
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.data := Cat(rdata.reverse)
}

class AXI4LiteRAM(memByte: Int, beatBytes: Int = 4, dataFile: String = "")
  extends RAM(new AXI4Lite, memByte, beatBytes, dataFile)

class AXI4RAM(memByte: Int, beatBytes: Int = 4, dataFile: String = "")
  extends RAM(new AXI4, memByte, beatBytes, dataFile) {

  in.b.bits.id := RegEnable(in.aw.bits.id, in.aw.fire())
  in.b.bits.user := RegEnable(in.aw.bits.user, in.aw.fire())
  in.r.bits.id := RegEnable(in.ar.bits.id, in.ar.fire())
  in.r.bits.user := RegEnable(in.ar.bits.user, in.ar.fire())
  in.r.bits.last := true.B
}
