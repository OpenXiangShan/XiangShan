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
    // val flush = Input(Bool())
    // update bpu based on redirect signals from brq
    val redirect = Flipped(ValidIO(new Redirect))
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    val predMask = Output(Vec(FetchWidth, Bool()))
    val predTargets = Output(Vec(FetchWidth, UInt(VAddrBits.W)))
  })

  val flush = BoolStopWatch(io.redirect.valid, io.in.pc.valid, startHighPriority = true)
  
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
      for (w <- 0 until BtbWays) {
        when (b.U === btbAddr.getBank(pcLatch) && btbRead(b)(w).valid && btbRead(b)(w).tag === btbAddr.getTag(Cat(pcLatch(VAddrBits - 1, 2), 0.U(2.W)) + i.U << 2)) {
          btbHits(i) := !flush && RegNext(btb(b)(w).io.r.req.fire(), init = false.B)
          btbTargets(i) := btbRead(b)(w).target
          btbTypes(i) := btbRead(b)(w)._type
          // btbPreds(i) := btbRead(b)(w).pred
          btbTakens(i) := (btbRead(b)(w).pred)(1).asBool
        }.otherwise {
          btbHits(i) := false.B
          btbTargets(i) := DontCare
          btbTypes(i) := DontCare
          btbTakens(i) := DontCare
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
      }.otherwise {
        jbtacHits(i) := false.B
        jbtacTargets(i) := DontCare
      }
    }
  }

  // redirect based on BTB and JBTAC
  (0 until FetchWidth).map(i => io.predMask(i) := btbHits(i) && Mux(btbTypes(i) === BTBtype.B, btbTakens(i), true.B) || jbtacHits(i))
  (0 until FetchWidth).map(i => io.predTargets(i) := Mux(btbHits(i) && !(btbTypes(i) === BTBtype.B && !btbTakens(i)), btbTargets(i), jbtacTargets(i)))


  // update bpu, including BTB, JBTAC...
  // 1. update BTB
  // 1.1 read the selected bank
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      btb(b)(w).io.r.req.valid := io.redirect.valid && btbAddr.getBank(io.redirect.bits.pc) === b.U
      btb(b)(w).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.redirect.bits.pc)
    }
  }

  // 1.2 match redirect pc tag with the 4 tags in a btb line, find a way to write
  // val redirectLatch = RegEnable(io.redirect.bits, io.redirect.valid)
  val redirectLatch = RegNext(io.redirect.bits, init = 0.U.asTypeOf(new Redirect))
  val bankLatch = btbAddr.getBank(redirectLatch.pc)
  val btbUpdateRead = Wire(Vec(BtbWays, btbEntry()))
  val btbValids = Wire(Vec(BtbWays, Bool()))
  val btbUpdateTagHits = Wire(Vec(BtbWays, Bool()))
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      when (b.U === bankLatch) {
        btbUpdateRead(w) := btb(b)(w).io.r.resp.data(0)
        btbValids(w) := btbUpdateRead(w).valid && RegNext(btb(b)(w).io.r.req.fire(), init = false.B)
      }.otherwise {
        btbUpdateRead(w) := 0.U.asTypeOf(btbEntry())
        btbValids(w) := false.B
      }
    }
  }
  (0 until BtbWays).map(w => btbUpdateTagHits(w) := btbValids(w) && btbUpdateRead(w).tag === btbAddr.getTag(redirectLatch.pc))
  // val btbWriteWay = Wire(Vec(BtbWays, Bool()))
  val btbWriteWay = Wire(UInt(BtbWays.W))
  val btbInvalids = ~ btbValids.asUInt
  when (btbUpdateTagHits.asUInt.orR) {
    // tag hits
    btbWriteWay := btbUpdateTagHits.asUInt
  }.elsewhen (!btbValids.asUInt.andR) {
    // no tag hits but there are free entries
    btbWriteWay := Mux(btbInvalids >= 8.U, "b1000".U,
      Mux(btbInvalids >= 4.U, "b0100".U,
      Mux(btbInvalids >= 2.U, "b0010".U, "b0001".U)))
  }.otherwise {
    // no tag hits and no free entry, select a victim way
    btbWriteWay := UIntToOH(LFSR64()(log2Up(BtbWays) - 1, 0))
  }

  // 1.3 calculate new 2-bit counter value
  val btbWrite = WireInit(0.U.asTypeOf(btbEntry()))
  btbWrite.valid := true.B
  btbWrite.tag := btbAddr.getTag(redirectLatch.pc)
  btbWrite._type := redirectLatch._type
  btbWrite.target := redirectLatch.brTarget
  val oldPred = WireInit("b01".U)
  oldPred := PriorityMux(btbWriteWay.asTypeOf(Vec(BtbWays, Bool())), btbUpdateRead.map{ e => e.pred })
  val newPred = Mux(redirectLatch.taken, Mux(oldPred === "b11".U, "b11".U, oldPred + 1.U),
    Mux(oldPred === "b00".U, "b00".U, oldPred - 1.U))
  btbWrite.pred := Mux(btbUpdateTagHits.asUInt.orR && redirectLatch._type === BTBtype.B, newPred, "b01".U)
  
  // 1.4 write BTB
  for (b <- 0 until BtbBanks) {
    for (w <- 0 until BtbWays) {
      when (b.U === bankLatch) {
        btb(b)(w).io.w.req.valid := OHToUInt(btbWriteWay) === w.U &&
          RegNext(io.redirect.valid, init = false.B) &&
          (redirectLatch._type === BTBtype.B || redirectLatch._type === BTBtype.J)
        btb(b)(w).io.w.req.bits.setIdx := btbAddr.getBankIdx(redirectLatch.pc)
        btb(b)(w).io.w.req.bits.data := btbWrite
      }.otherwise {
        btb(b)(w).io.w.req.valid := false.B
        btb(b)(w).io.w.req.bits.setIdx := DontCare
        btb(b)(w).io.w.req.bits.data := DontCare
      }
    }
  }

  // 2. update JBTAC
  val jbtacWrite = WireInit(0.U.asTypeOf(jbtacEntry()))
  jbtacWrite.valid := true.B
  jbtacWrite.tag := jbtacAddr.getTag(io.redirect.bits.pc)
  jbtacWrite.target := io.redirect.bits.target
  (0 until JbtacBanks).map(b =>
    jbtac(b).io.w.req.valid := io.redirect.valid &&
      b.U === jbtacAddr.getBank(io.redirect.bits.pc) &&
      io.redirect.bits._type === BTBtype.I)
  (0 until JbtacBanks).map(b => jbtac(b).io.w.req.bits.setIdx := jbtacAddr.getBankIdx(io.redirect.bits.pc))
  (0 until JbtacBanks).map(b => jbtac(b).io.w.req.bits.data := jbtacWrite)
}
