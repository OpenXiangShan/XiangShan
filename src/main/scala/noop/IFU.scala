package noop

import chisel3._
import chisel3.util._

import memory.MemIO

trait HasResetVector {
  val resetVector = 0x80100000L
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new MemIO
    val out = Valid(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val writeback = Input(Bool())
  })

  val pc = RegInit(resetVector.U(32.W))
  when (io.writeback) {
    pc := Mux(io.br.isTaken, io.br.target, pc + 4.U)
  }

  val s_idle :: s_req :: s_wait_resp :: Nil = Enum(3)
  val state = RegInit(s_req)

  switch (state) {
    is (s_idle) {
      when (io.writeback) { state := s_req }
    }

    is (s_req) {
      when (io.imem.a.fire()) { state := s_wait_resp }
    }

    is (s_wait_resp) {
      when (io.imem.r.fire()) { state := Mux(io.writeback, s_req, s_idle) }
    }
  }

  io.imem := DontCare
  io.imem.a.valid := (state === s_req)
  io.imem.a.bits.addr := pc
  io.imem.a.bits.size := "b10".U
  io.imem.r.ready := (state === s_wait_resp)
  io.imem.w.valid := false.B

  io.out.valid := io.imem.r.fire()
  io.out.bits.instr := Mux(io.out.valid, io.imem.r.bits.data, Instructions.NOP)
  io.out.bits.pc := pc
}
