package noop

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus.SimpleBus

trait HasResetVector {
  val resetVector = 0x80100000L
}

class TableAddr(idxBits: Int) extends Bundle {
  def tagBits = 32 - 2 - idxBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val pad = UInt(2.W)

  override def cloneType = new TableAddr(idxBits).asInstanceOf[this.type]
}

class BPU1 extends Module with HasBRUOpType {
  val io = IO(new Bundle {
    val pc = Input(UInt(32.W))
    val update = Input(new BRUIO)
    val out = new BranchIO
  })

  // BTB
  val NRbtb = 512
  val btbAddr = new TableAddr(log2Up(NRbtb))
  val btbEntry = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val offset = UInt(12.W)
    val isTaken = Bool()
  }

  val btb = Mem(NRbtb, btbEntry)
  val btbRead = btb.read(io.pc.asTypeOf(btbAddr).idx)
  val btbHit = btbRead.tag === io.pc.asTypeOf(btbAddr).tag
  val btbTarget = io.pc + Cat(Fill(20, btbRead.offset(11)), btbRead.offset)
  val btbTaken = btbHit && btbRead.isTaken

  // jump table
  val NRjtb = 128
  val jtbAddr = new TableAddr(log2Up(NRjtb))
  val jtbEntry = new Bundle {
    val tag = UInt(jtbAddr.tagBits.W)
    val offset = UInt(20.W)
  }

  val jtb = Mem(NRjtb, jtbEntry)
  val jtbRead = jtb.read(io.pc.asTypeOf(jtbAddr).idx)
  val jtbHit = jtbRead.tag === io.pc.asTypeOf(jtbAddr).tag
  val jtbTarget = io.pc + Cat(Fill(12, jtbRead.offset(19)), jtbRead.offset)

  // update
  when (io.update.in.valid) {
    when (io.update.in.bits.func === BruJal) {
      val jtbWrite = Wire(jtbEntry)
      jtbWrite.tag := io.update.pc.asTypeOf(jtbAddr).tag
      jtbWrite.offset := io.update.offset(19, 0)
      jtb.write(io.update.pc.asTypeOf(jtbAddr).idx, jtbWrite)
    }
    when (isBranch(io.update.in.bits.func)) {
      val btbWrite = Wire(btbEntry)
      btbWrite.tag := io.update.pc.asTypeOf(btbAddr).tag
      btbWrite.offset := io.update.offset(11, 0)
      btbWrite.isTaken := btbWrite.offset(11)  // static prediction
      btb.write(io.update.pc.asTypeOf(btbAddr).idx, btbWrite)
    }
  }

  io.out.target := Mux(jtbHit, jtbTarget, btbTarget)
  io.out.isTaken := jtbHit || btbTaken
  assert(!(jtbHit && btbHit), "should not both hit in BTB and JBT")
}

class BPU2 extends Module {
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
    val imem = new SimpleBus(userBits = 1)
    val pc = Input(UInt(32.W))
    val out = Decoupled(new PcInstrIO)
    val br = Flipped(new BranchIO)
    val bpu1Update = Input(new BRUIO)
    val flushVec = Output(UInt(4.W))
    val bpFlush = Output(Bool())
    val imemStall = Output(Bool())
  })

  // pc
  val pc = RegInit(resetVector.U(32.W))

  val bp1 = Module(new BPU1)
  bp1.io.pc := pc
  bp1.io.update := io.bpu1Update

  val bp2 = Module(new BPU2)
  bp2.io.in.bits := io.out.bits
  bp2.io.in.valid := io.imem.resp.fire()

  pc := Mux(io.br.isTaken, io.br.target,
      Mux(bp1.io.out.isTaken && io.imem.req.fire(), bp1.io.out.target,
        Mux(io.imem.req.fire(), pc + 4.U, pc)))//)

  io.flushVec := Mux(io.br.isTaken, "b1111".U, 0.U)
  io.bpFlush := false.B

  io.imem := DontCare
  io.imem.req.valid := io.out.ready
  io.imem.req.bits.addr := pc
  io.imem.req.bits.size := "b10".U
  io.imem.req.bits.wen := false.B
  io.imem.req.bits.user.map(_ := bp1.io.out.isTaken)
  io.imem.resp.ready := io.out.ready || io.flushVec(0)

  io.out.valid := io.imem.resp.valid && !io.flushVec(0)
  io.out.bits.instr := io.imem.resp.bits.rdata
  io.imem.resp.bits.user.map(io.out.bits.isBranchTaken := _)

  io.out.bits.pc := io.pc

  // perfcnt
  io.imemStall := BoolStopWatch(io.imem.req.valid, io.imem.resp.fire())
}
