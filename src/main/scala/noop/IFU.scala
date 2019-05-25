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
            Mux(io.out.fire(), pc + 4.U, pc)))

  io.flushVec := Mux(io.csrjmp.isTaken || io.br.isTaken, "b00111".U, 0.U)

  // instruction buffer
  def pcTag(pc: UInt): UInt = pc(31, 6)
  val ibuf = Reg(UInt(512.W))
  val ibufPcTag = RegInit(0.U((32 - 6).W))

  val ibufHit = (pcTag(pc) === ibufPcTag)

  io.out.valid := ibufHit && !io.flushVec(0)
  io.out.bits.instr := ibuf.asTypeOf(Vec(512 / 32, UInt(32.W)))(pc(5, 2))

  // state machine
  val s_idle :: s_req :: s_wait_resp :: Nil = Enum(3)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) {
      when (!ibufHit) { state := s_req }
    }

    is (s_req) {
      when (io.imem.req.fire()) {
        state := Mux(io.imem.resp.fire(), Mux(!ibufHit, s_req, s_idle), s_wait_resp)
      }
    }

    is (s_wait_resp) {
      when (io.imem.resp.fire()) { state := Mux(!ibufHit, s_req, s_idle) }
    }
  }

  io.imem := DontCare
  io.imem.req.valid := !ibufHit
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.resp.ready := true.B

  val pcInflight = RegEnable(pc, io.imem.req.fire())

  when (io.imem.resp.fire()) {
    ibuf := io.imem.resp.bits.rdata
    ibufPcTag := pcTag(pcInflight)
  }

  io.out.bits.pc := pc

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.req.valid, io.imem.resp.fire())
}
