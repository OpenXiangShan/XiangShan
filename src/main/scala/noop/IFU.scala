package noop

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus.SimpleBus

trait HasResetVector {
  val resetVector = 0x80100000L
}

class BPU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new PcInstrIO))
    val out = new BranchIO
  })

  val instr = io.in.bits.instr
  val immJ = Cat(Fill(12, instr(31)), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
  val immB = Cat(Fill(20, instr(31)), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W))
  val table = Array(
    BRUInstr.JAL  -> List(immJ, true.B),
    BRUInstr.BNE  -> List(immB, instr(31)),
    BRUInstr.BEQ  -> List(immB, instr(31)),
    BRUInstr.BLT  -> List(immB, instr(31)),
    BRUInstr.BGE  -> List(immB, instr(31)),
    BRUInstr.BLTU -> List(immB, instr(31)),
    BRUInstr.BGEU -> List(immB, instr(31))
  )
  val default = List(immB, false.B)
  val offset :: predict :: Nil = ListLookup(instr, default, table)

  io.out.target := io.in.bits.pc + offset
  io.out.isTaken := io.in.valid && predict(0)
}

class IFU extends Module with HasResetVector {
  val io = IO(new Bundle {
    val imem = new SimpleBus
    val pc = Input(UInt(32.W))
    val out = Decoupled(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val imemStall = Output(Bool())
  })

  val bp = Module(new BPU)
  bp.io.in.bits := io.out.bits
  bp.io.in.valid := io.imem.resp.fire()

  // pc
  val pc = RegInit(resetVector.U(32.W))
  pc := Mux(io.br.isTaken, io.br.target,
    Mux(bp.io.out.isTaken, bp.io.out.target,
      Mux(io.imem.req.fire(), pc + 4.U, pc)))

  io.flushVec := Mux(io.br.isTaken, "b1111".U, 0.U)
  io.bpFlush := bp.io.out.isTaken

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
