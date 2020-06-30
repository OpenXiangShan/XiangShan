package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle {
  def tagBits = VAddrBits - idxBits - 2

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(2.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = getIdx(x)(log2Up(banks) - 1, 0)
  def getBankIdx(x: UInt) = getIdx(x)(idxBits - 1, log2Up(banks))
}

class BPU extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    // val out = new Bundle { val redirect = Valid(UInt(VAddrBits.W)) }
    val predMask = Output(Vec(FetchWidth, Bool()))
    val predTargets = Output(Vec(FetchWidth, UInt(VAddrBits.W)))
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)
  
  // BTB makes a quick prediction for branch and direct jump, which is
  // 4-way set-associative, and each way is divided into 4 banks. 
  val btbAddr = new TableAddr(log2Up(BtbSets), BtbBanks)
  def btbEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(btbAddr.tagBits.W)
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
    val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
  }

  val btb = List.fill(BtbBanks)(List.fill(BtbWays)(
    Module(new SRAMTemplate(btbEntry(), set = BtbSets / BtbBanks, shouldReset = true, holdRead = true, singlePort = true))))

  // val fetchPkgAligned = btbAddr.getBank(io.in.pc.bits) === 0.U
  val HeadBank = btbAddr.getBank(io.in.pc.bits)
  val TailBank = btbAddr.getBank(io.in.pc.bits + FetchWidth.U << 2.U - 4.U)
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btb(b)(w).reset := reset.asBool
      btb(b)(w).io.r.req.valid := io.in.pc.valid && Mux(TailBank > HeadBank, b.U >= HeadBank && b.U <= TailBank, b.U >= TailBank || b.U <= HeadBank)
      btb(b)(w).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
    }
  }
  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbRead = Wire(Vec(BtbBanks, Vec(BtbWays, btbEntry())))
  val btbHits = Wire(Vec(FetchWidth, Bool()))
  val btbTargets = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  val btbTypes = Wire(Vec(FetchWidth, UInt(2.W)))
  // val btbPreds = Wire(Vec(FetchWidth, UInt(2.W)))
  val btbTakens = Wire(Vec(FetchWidth, Bool()))
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btbRead(b)(w) := btb(b)(w).io.r.resp.data(0)
    }
  }
  for (i <- 0 until FetchWidth) {
    btbHits(i) := false.B
    for (b <- 0 until BtbBanks) {
      when (b.U === btbAddr.getBank(pcLatch)) {
        for (w <- 0 until BtbWays) {
          when (btbRead(b)(w).valid && btbRead(b)(w).tag === btbAddr.getTag(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2)) {
            btbHits(i) := !flush && RegNext(btb(b)(w).io.r.req.fire(), init = false.B)
            btbTargets(i) := btbRead(b)(w).target
            btbTypes(i) := btbRead(b)(w)._type
            // btbPreds(i) := btbRead(b)(w).pred
            btbTakens(i) := (btbRead(b)(w).pred)(1).asBool
          }
        }
      }
    }
  }

  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W)
    val target = UInt(VAddrBits.W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = true)))

  (0 until JbtacBanks).map(i => jbtac(i).reset := reset.asBool)
  (0 until JbtacBanks).map(i => jbtac(i).io.r.req.valid := io.in.pc.valid)
  (0 until JbtacBanks).map(i => jbtac(i).io.r.req.bits.setIdx := jbtacAddr.getBankIdx(Cat((io.in.pc.bits)(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2))

  val jbtacRead = Wire(Vec(JbtacBanks, jbtacEntry()))
  (0 until JbtacBanks).map(i => jbtacRead(i) := jbtac(i).io.r.resp.data(0))
  val jbtacHits = Wire(Vec(FetchWidth, Bool()))
  val jbtacTargets = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  val jbtacHeadBank = jbtacAddr.getBank(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)))
  for (i <- 0 until FetchWidth) {
    jbtacHits(i) := false.B
    for (b <- 0 until JbtacBanks) {
      when (jbtacHeadBank + i.U === b.U) {
        jbtacHits(i) := jbtacRead(b).valid && jbtacRead(b).tag === jbtacAddr.getTag(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2) &&
          !flush && RegNext(jbtac(b).io.r.req.fire(), init = false.B)
        jbtacTargets(i) := jbtacRead(b).target
      }
    }
  }

  // redirect based on BTB and JBTAC
  /*
  val redirectMask = Wire(Vec(FetchWidth, Bool()))
  val redirectTarget = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  (0 until FetchWidth).map(i => redirectMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  (0 until FetchWidth).map(i => redirectTarget(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))
  io.out.redirect.valid := redirectMask.asUInt.orR
  io.out.redirect.bits := PriorityMux(redirectMask, redirectTarget)
  */
  (0 until FetchWidth).map(i => io.predMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  (0 until FetchWidth).map(i => io.predTargets(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))

}
