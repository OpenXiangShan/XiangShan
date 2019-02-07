package core

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

  io.imem.out.valid := true.B
  io.imem.out.bits.addr := pc
  io.imem.out.bits.wen := false.B
  io.imem.out.bits.wdata := DontCare

  io.out.instr := io.imem.in.rdata
  io.out.pc := pc
}
