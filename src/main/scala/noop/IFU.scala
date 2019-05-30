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
    val out = Decoupled(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val csrjmp = Flipped(new BranchIO)
    val flushVec = Output(UInt(5.W))
    val imemStall = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))
  pc := Mux(io.csrjmp.isTaken, io.csrjmp.target,
          Mux(io.br.isTaken, io.br.target,
            Mux(io.imem.req.fire(), pc + 4.U, pc)))

  io.flushVec := Mux(RegNext(io.csrjmp.isTaken || io.br.isTaken), "b00111".U, 0.U)

  val pcInflight = RegEnable(pc, io.imem.req.fire())
  val inflight = RegInit(false.B)
  when (io.imem.resp.fire() || io.flushVec(0)) { inflight := false.B }
  when (io.imem.req.fire()) { inflight := true.B }

  io.imem := DontCare
  io.imem.req.valid := io.out.ready
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.resp.ready := io.out.ready || io.flushVec(0) //true.B

  io.out.valid := io.imem.resp.valid && inflight && !io.flushVec(0)
  io.out.bits.instr := io.imem.resp.bits.rdata

  io.out.bits.pc := pcInflight

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.req.valid, io.imem.resp.fire())
}
