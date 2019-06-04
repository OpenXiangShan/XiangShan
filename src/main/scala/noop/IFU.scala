package noop

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus.SimpleBus

trait HasResetVector {
  val resetVector = 0x80100000L
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new SimpleBus
    val pc = Input(UInt(32.W))
    val out = Decoupled(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val flushVec = Output(UInt(4.W))
    val imemStall = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))
  pc := Mux(io.br.isTaken, io.br.target, Mux(io.imem.req.fire(), pc + 4.U, pc))

  io.flushVec := Mux(io.br.isTaken, "b1111".U, 0.U)

  io.imem := DontCare
  io.imem.req.valid := io.out.ready
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.valid := io.imem.resp.valid && !io.flushVec(0)
  io.out.bits.instr := io.imem.resp.bits.rdata

  io.out.bits.pc := io.pc

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.req.valid, io.imem.resp.fire())
}
