package core

import chisel3._
import chisel3.util._

class IFU extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val out = new PcInstrIO
  })

  val pc = RegInit(0.U(32.W))
  pc := pc + 4.U

  io.imem.out.valid := true.B
  io.imem.out.bits.addr := pc
  io.imem.out.bits.wen := false.B
  io.imem.out.bits.wdata := DontCare

  io.out.instr := io.imem.in.rdata
  io.out.pc := pc
}
