package noop

import chisel3._
import chisel3.util._

import memory.MemIO
import utils._

trait HasResetVector {
  val resetVector = 0x80100000L
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new MemIO
    val out = Valid(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val csrjmp = Flipped(new BranchIO)
    val writeback = Input(Bool())
    val imemStall = Output(Bool())
  })

  val pc = RegInit(resetVector.U(32.W))
  when (io.writeback) {
    pc := Mux(io.csrjmp.isTaken, io.csrjmp.target,
            Mux(io.br.isTaken, io.br.target, pc + 4.U)
          )
  }

  val s_executing :: s_req :: s_wait_resp :: Nil = Enum(3)
  val state = RegInit(s_req)

  switch (state) {
    is (s_executing) {
      when (io.writeback) { state := s_req }
    }

    is (s_req) {
      when (io.imem.a.fire()) {
        state := Mux(io.imem.r.fire(), Mux(io.writeback, s_req, s_executing), s_wait_resp)
      }
    }

    is (s_wait_resp) {
      when (io.imem.r.fire()) { state := Mux(io.writeback, s_req, s_executing) }
    }
  }

  io.imem := DontCare
  io.imem.a.valid := (state === s_req)
  io.imem.a.bits.addr := pc
  io.imem.a.bits.size := "b10".U
  io.imem.r.ready := true.B
  io.imem.w.valid := false.B

  val instrReg = RegInit(Instructions.NOP)
  when (io.writeback) { instrReg := Instructions.NOP }
  .elsewhen (io.imem.r.fire()) { instrReg := io.imem.r.bits.data }

  io.out.valid := io.imem.r.fire() || (state === s_executing)
  io.out.bits.instr := Mux(io.imem.r.fire(), io.imem.r.bits.data, instrReg)

  when (io.out.valid) {
    assert(io.out.bits.instr(1, 0) === 3.U,
      "%d: pc = 0x%x, bad instr = 0x%x\n", GTimer(), pc, io.out.bits.instr)
  }

  io.out.bits.pc := pc

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.a.valid, io.imem.r.fire())
}
