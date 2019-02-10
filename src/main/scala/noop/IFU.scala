package noop

import chisel3._
import chisel3.util._

trait HasResetVector {
  val resetVector = 0x100000
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new MemIO
    val out = new PcInstrIO
    val br = Flipped(new BranchIO)
  })

  val pc = RegInit(resetVector.U(32.W))
  pc := Mux(io.br.isTaken, io.br.target, pc + 4.U)

  io.imem := DontCare
  io.imem.a.valid := true.B
  io.imem.a.bits.addr := pc
  io.imem.a.bits.size := "b10".U
  io.imem.w.valid := false.B

  io.out.instr := io.imem.r.bits.data
  io.out.pc := pc
}
