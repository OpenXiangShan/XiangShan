package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class TableAddr(idxBits: Int) extends Bundle {
  def tagBits = 32 - 2 - idxBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val pad = UInt(2.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(32.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx

  override def cloneType = new TableAddr(idxBits).asInstanceOf[this.type]
}

class BPU1 extends Module with HasBRUOpType {
  val io = IO(new Bundle {
    val in = new Bundle { val pc = Flipped(Valid((UInt(32.W)))) }
    val update = Input(new BRUIO)
    val out = new BranchIO
  })

  def btbTypeB = "b00".U  // branch
  def btbTypeJ = "b01".U  // jump
  def btbTypeI = "b10".U  // indirect
  def btbTypeR = "b11".U  // return

  // BTB
  val NRbtb = 512
  val btbAddr = new TableAddr(log2Up(NRbtb))
  val btbEntry = new Bundle {
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(32.W)
  }

  val btb = Module(new ArrayTemplate(btbEntry, set = NRbtb, holdRead = true))
  btb.io.r.req.valid := io.in.pc.valid
  btb.io.r.req.idx := btbAddr.getIdx(io.in.pc.bits)

  val btbRead = Wire(btbEntry)
  btbRead := btb.io.r.entry
  // since there is one cycle latency to read SyncReadMem,
  // we should latch the input pc for one cycle
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbHit = btbRead.tag === btbAddr.getTag(pcLatch)

  // direction prediction table for branch
  val dpt = Mem(NRbtb, Bool())
  val dptTaken = dpt.read(btbAddr.getIdx(pcLatch))

  // RAS

  val NRras = 16
  val ras = Mem(NRras, UInt(32.W))
  val sp = Counter(NRras)
  val rasTarget = RegEnable(ras.read(sp.value), io.in.pc.valid)

  val table = List(
      BruJal  -> btbTypeJ,
      BruCall -> btbTypeJ,
      BruJalr -> btbTypeI,
      BruRet  -> btbTypeR,
      BruBeq  -> btbTypeB,
      BruBne  -> btbTypeB,
      BruBlt  -> btbTypeB,
      BruBge  -> btbTypeB,
      BruBltu -> btbTypeB,
      BruBgeu -> btbTypeB
  )
  // update
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry))
  btbWrite.tag := btbAddr.getTag(io.update.pc)
  BoringUtils.addSink(btbWrite.target, "btbTarget")
  btbWrite._type := LookupTree(io.update.in.bits.func, table)
  btb.io.w.req.valid := io.update.in.valid
  btb.io.w.req.idx := btbAddr.getIdx(io.update.pc)
  btb.io.w.wordIndex := 0.U // ???
  btb.io.w.entry := btbWrite

  when (io.update.in.valid) {
    when (isBranch(io.update.in.bits.func)) {
      dpt.write(btbAddr.getIdx(io.update.pc), io.update.offset(31))
    }
    when (io.update.in.bits.func === BruCall) {
      ras.write(sp.value + 1.U, io.update.pc + 4.U)
      sp.value := sp.value + 1.U
    }
    .elsewhen (io.update.in.bits.func === BruRet) {
      sp.value := sp.value - 1.U
    }
  }

  io.out.target := Mux(btbRead._type === btbTypeR, rasTarget, btbRead.target)
  io.out.isTaken := btbHit && Mux(btbRead._type === btbTypeB, dptTaken, true.B)
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
