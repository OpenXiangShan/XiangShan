// See LICENSE.SiFive for license details.

package bus.axi4

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class AXI4RAM(memByte: Int, beatBytes: Int = 4, dataFile: String = "") extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new AXI4)
  })

  val in = io.in
  val mem = SeqMem(memByte / beatBytes, Vec(beatBytes, UInt(8.W)))
  if (dataFile != "") loadMemoryFromFile(mem, dataFile)

  val r_addr = in.ar.bits.addr >> log2Ceil(beatBytes)
  val w_addr = in.aw.bits.addr >> log2Ceil(beatBytes)

  val w_full = RegInit(false.B)
  val w_id   = Reg(UInt())
  val w_user = Reg(UInt())

  when (in. b.fire()) { w_full := false.B }
  when (in.aw.fire()) { w_full := true.B }

  when (in.aw.fire()) {
    w_id := in.aw.bits.id
    w_user := in.aw.bits.user
  }

  val wdata = VecInit.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
  when (in.aw.fire()) {
    mem.write(w_addr, wdata, in.w.bits.strb.toBools)
  }

  in. b.valid := w_full
  in.aw.ready := in. w.valid && (in.b.ready || !w_full)
  in. w.ready := in.aw.valid && (in.b.ready || !w_full)

  in.b.bits.id   := w_id
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
  in.b.bits.user := w_user

  val r_full = RegInit(false.B)
  val r_id   = Reg(UInt())
  val r_user = Reg(UInt())

  when (in. r.fire()) { r_full := false.B }
  when (in.ar.fire()) { r_full := true.B }

  when (in.ar.fire()) {
    r_id := in.ar.bits.id
    r_user := in.ar.bits.user
  }

  def holdUnless[T <: Data](x: T, enable: Bool): T = Mux(enable, x, RegEnable(x, enable))

  val ren = in.ar.fire()
  val rdata = holdUnless(mem.read(r_addr, ren), RegNext(ren))

  in. r.valid := r_full
  in.ar.ready := in.r.ready || !r_full

  in.r.bits.id   := r_id
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.data := Cat(rdata.reverse)
  in.r.bits.user := r_user
  in.r.bits.last := true.B
}
