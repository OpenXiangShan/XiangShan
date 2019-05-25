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
    val imem = new SimpleBus(512)
    val out = Valid(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val csrjmp = Flipped(new BranchIO)
    val writeback = Input(Bool())
    val imemStall = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))
  val npc = Mux(io.csrjmp.isTaken, io.csrjmp.target,
              Mux(io.br.isTaken, io.br.target, pc + 4.U))
  when (io.writeback) { pc := npc }

  // instruction buffer
  def pcTag(pc: UInt): UInt = pc(31, 6)
  val ibuf = Reg(UInt(512.W))
  val ibufPcTag = RegInit(0.U((32 - 6).W))

  val ibufHit = (pcTag(pc) === ibufPcTag)

  io.out.valid := ibufHit
  io.out.bits.instr := ibuf.asTypeOf(Vec(512 / 32, UInt(32.W)))(pc(5, 2))

  // state machine
  val s_idle :: s_req :: s_wait_resp :: Nil = Enum(3)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) {
      when (io.writeback && !ibufHit) { state := s_req }
    }

    is (s_req) {
      when (io.imem.req.fire()) {
        state := Mux(io.imem.resp.fire(), Mux(io.writeback && !ibufHit, s_req, s_idle), s_wait_resp)
      }
    }

    is (s_wait_resp) {
      when (io.imem.resp.fire()) { state := Mux(io.writeback && !ibufHit, s_req, s_idle) }
    }
  }

  io.imem := DontCare
  io.imem.req.valid := (state === s_idle) && !ibufHit
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.resp.ready := true.B

  when (io.imem.resp.fire()) {
    ibuf := io.imem.resp.bits.rdata
    ibufPcTag := pcTag(pc)
  }

  when (io.out.valid) {
    assert(io.out.bits.instr(1, 0) === 3.U,
      "%d: pc = 0x%x, bad instr = 0x%x\n", GTimer(), pc, io.out.bits.instr)
  }

  when (io.writeback) {
//    printf("%d: pc = 0x%x, instr = 0x%x\n", GTimer(), pc, io.out.bits.instr)
  }

  io.out.bits.pc := pc

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.req.valid, io.imem.resp.fire())
}
