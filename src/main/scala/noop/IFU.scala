package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus.SimpleBus

trait HasResetVector {
  val resetVector = 0x80100000L
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new SimpleBus(userBits = 32)
    val pc = Input(UInt(32.W))
    val out = Decoupled(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val bpu1Update = Input(new BRUIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))
  val pcUpdate = io.br.isTaken || io.imem.req.fire()
  val snpc = pc + 4.U  // sequential next pc

  val bp1 = Module(new BPU1)
  // predicted next pc
  val pnpc = bp1.io.out.target
  val npc = Mux(io.br.isTaken, io.br.target, Mux(bp1.io.out.isTaken, pnpc, snpc))

  bp1.io.in.pc.valid := pcUpdate // only predict when pc is updated
  bp1.io.in.pc.bits := npc  // predict one cycle early
  bp1.io.update := io.bpu1Update

  val bp2 = Module(new BPU2)
  bp2.io.in.bits := io.out.bits
  bp2.io.in.valid := io.imem.resp.fire()

  when (pcUpdate) { pc := npc }

  io.flushVec := Mux(io.br.isTaken, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem := DontCare
  io.imem.req.valid := io.out.ready
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.req.bits.user.map(_ := npc)
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.valid := io.imem.resp.valid && !io.flushVec(0)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map(io.out.bits.npc := _)

  io.out.bits.pc := io.pc

  BoringUtils.addSource(BoolStopWatch(io.imem.req.valid, io.imem.resp.fire()), "perfCntCondMimemStall")
  BoringUtils.addSource(io.flushVec.orR, "perfCntCondMifuFlush")
}
