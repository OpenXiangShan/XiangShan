package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

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
    val in = new Bundle { val pc = Flipped(Valid((UInt(32.W)))) }
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
  val btbRead = btb.read(io.in.pc.bits.asTypeOf(btbAddr).idx)
  val btbHit = btbRead.tag === io.in.pc.bits.asTypeOf(btbAddr).tag
  val btbTarget = io.in.pc.bits + Cat(Fill(20, btbRead.offset(11)), btbRead.offset)
  val btbTaken = btbHit && btbRead.isTaken

  // jump table
  val NRjtb = 128
  val jtbAddr = new TableAddr(log2Up(NRjtb))
  val jtbEntry = new Bundle {
    val tag = UInt(jtbAddr.tagBits.W)
    val offset = UInt(20.W)
  }

  val jtb = Mem(NRjtb, jtbEntry)
  val jtbRead = jtb.read(io.in.pc.bits.asTypeOf(jtbAddr).idx)
  val jtbHit = jtbRead.tag === io.in.pc.bits.asTypeOf(jtbAddr).tag
  val jtbTarget = io.in.pc.bits + Cat(Fill(12, jtbRead.offset(19)), jtbRead.offset)

  // RAS

  // store pc table
  val NRrasPctb = 64
  val rasPcAddr = new TableAddr(log2Up(NRjtb))
  val rasPcTable = Mem(NRrasPctb, UInt(32.W))
  val rasPcTableHit = rasPcTable.read(io.in.pc.bits.asTypeOf(rasPcAddr).idx) === io.in.pc.bits

  val NRras = 16

  val ras = Mem(NRras, UInt(32.W))
  val sp = Counter(NRras)
  val rasTarget = ras.read(sp.value)

  // update
  when (io.update.in.valid) {
    when (io.update.in.bits.func === BruJal || io.update.in.bits.func === BruCall) {
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
    when (io.update.in.bits.func === BruCall) {
      ras.write(sp.value + 1.U, io.update.pc + 4.U)
      sp.value := sp.value + 1.U
    }
    when (io.update.in.bits.func === BruRet) {
      sp.value := sp.value - 1.U
      rasPcTable.write(io.update.pc.asTypeOf(rasPcAddr).idx, io.update.pc)
    }
  }


  io.out.target := RegEnable(Mux(jtbHit, jtbTarget, Mux(rasPcTableHit, rasTarget, btbTarget)), io.in.pc.valid)
  io.out.isTaken := RegEnable(jtbHit || btbTaken || rasPcTableHit, init = false.B, io.in.pc.valid)
  assert(jtbHit + btbHit + rasPcTableHit <= 1.U, "should not both hit in BTB and JBT")
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
