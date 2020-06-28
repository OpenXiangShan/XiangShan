package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class TableAddr(val idxBits: Int, val wayBanks: Int) extends XSBundle {
  def wayBankBits = log2Up(wayBanks)
  def tagBits = VAddrBits - idxBits - wayBankBits - 2

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val bank = UInt(wayBankBits.W)
  val offset = UInt(2.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getIdx(x: UInt) = fromUInt(x).idx
  // def getLineBank(x: UInt) = getIdx(x)(0)
  def getWayBank(x: UInt) = fromUInt(x).bank
  def getTag(x: UInt) = fromUInt(x).tag
  def getLineOffset(x: UInt) = Cat(fromUInt(x).bank, fromUInt(x).offset)
}

class BPU extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    val out = new Bundle { val redirect = Valid(UInt(VAddrBits.W)) }
  })

  val flush = BoolStopWatch(io.flush, io.in.pc.valid, startHighPriority = true)

  // BTB
  val btbAddr = new TableAddr(log2Up(BtbSets), BtbWayBanks)
  def btbMeta() = new Bundle {
    val valid = Bool()
    val tag = UInt(btbAddr.tagBits.W)
  }
  def btbEntry() = new Bundle {
    val _type = UInt(2.W)
    val target = UInt(VAddrBits.W)
  }

  val meta = RegInit(0.U.asTypeOf(Vec(BtbSets, btbMeta())))
  val btb = List.fill(BtbWayBanks)(Module(new SRAMTemplate(btbEntry(), set = BtbSets, shouldReset = true, holdRead = true, singlePort = true)))

  // PHT, which has the same complete association structure as BTB's
  val pht = List.fill(BtbWayBanks)(Mem(BtbSets, UInt(2.W)))
  val phtRead = Wire(Vec(FetchWidth, UInt(2.W)))

  val fetchPkgBank = btbAddr.getWayBank(io.in.pc.bits)
  val fetchPkgAligned = btbAddr.getLineOffset(io.in.pc.bits) === 0.U // whether fetch package is 32B aligned or not
  val loPkgTag = btbAddr.getTag(io.in.pc.bits)
  val hiPkgTag = loPkgTag + 1.U
  val loMetaHits = Wire(Vec(BtbSets, Bool()))
  val hiMetaHits = Wire(Vec(BtbSets, Bool()))
  // val loMetaHits = meta.map{ m => (m.valid && m.tag === loPkgTag) }
  // val hiMetaHits = meta.map{ m => (m.valid && m.tag === hiPkgTag) }
  (0 until BtbSets).map(i => loMetaHits(i) := meta(i).valid && meta(i).tag === loPkgTag)
  (0 until BtbSets).map(i => hiMetaHits(i) := meta(i).valid && meta(i).tag === hiPkgTag)
  val loMetaHit = io.in.pc.valid && loMetaHits.reduce(_||_)
  val hiMetaHit = io.in.pc.valid && hiMetaHits.reduce(_||_) && !fetchPkgAligned
  val loMetaHitIdx = PriorityEncoder(loMetaHits.asUInt)
  val hiMetaHitIdx = PriorityEncoder(hiMetaHits.asUInt)

  (0 until BtbWayBanks).map(i => btb(i).io.r.req.valid := Mux(i.U < fetchPkgBank, hiMetaHit, loMetaHit))
  (0 until BtbWayBanks).map(i => btb(i).io.r.req.bits.setIdx := Mux(i.U < fetchPkgBank, hiMetaHitIdx, loMetaHitIdx))
  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  val btbRead = Wire(Vec(FetchWidth, btbEntry()))
  val btbHits = Wire(Vec(FetchWidth, Bool()))
  for (i <- 0 until FetchWidth) {
    for (j <- 0 until BtbWayBanks) {
      when (j.U === RegEnable(fetchPkgBank, io.in.pc.valid)) {
        val isLoPkg = i.U + j.U < BtbWayBanks.U
        btbRead(i) := Mux(isLoPkg, btb(i+j).io.r.resp.data(0), btb(i+j-BtbWayBanks).io.r.resp.data(0))
        btbHits(i) := !flush &&
          Mux(isLoPkg, RegNext(loMetaHit), RegNext(hiMetaHit)) &&
          Mux(isLoPkg, RegNext(btb(i+j).io.r.req.fire(), init = false.B), RegNext(btb(i+j-BtbWayBanks).io.r.req.fire(), init = false.B))
        phtRead(i) := RegEnable(Mux(isLoPkg, pht(i+j).read(loMetaHitIdx), pht(i+j-BtbWayBanks).read(hiMetaHitIdx)), io.in.pc.valid)
      }
    }
  }
  val phtTaken = phtRead.map { ctr => ctr(1).asBool }

  // RAS
  def rasEntry() = new Bundle {
    val target = UInt(VAddrBits.W)
    val layer = UInt(3.W) // layer of nested function
  }
  val ras = Mem(RasSize, rasEntry())
  val sp = Counter(RasSize)
  val rasRead = ras.read(sp.value)
  val retAddr = RegEnable(rasRead.target, io.in.pc.valid)

  // JBTAC
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    val target = UInt(VAddrBits.W)
  }
  val jbtacAddr = new TableAddr(log2Up(JbtacSets), JbtacBanks)
  val jbtac = List.fill(JbtacBanks)(new SRAMTemplate(jbtacEntry(), set = JbtacSets, shouldReset = true, holdRead = true, singlePort = true))
  (0 until JbtacBanks).map(i => jbtac(i).io.r.req.valid := io.in.pc.valid)
  (0 until JbtacBanks).map(i =>
    jbtac(i).io.r.req.bits.setIdx := jbtacAddr.getIdx(io.in.pc.bits) + Mux(i.U >= jbtacAddr.getWayBank(io.in.pc.bits), 0.U, 1.U)
  )
  val jbtacRead = Wire(Vec(JbtacBanks, jbtacEntry()))
  for (i <- 0 until JbtacBanks) {
    for (j <- 0 until JbtacBanks) {
      when (j.U === jbtacAddr.getWayBank(io.in.pc.bits)) {
        jbtacRead(i) := Mux(j.U + i.U < JbtacBanks.U, jbtac(i+j).io.r.resp.data(0), jbtac(i+j-JbtacBanks).io.r.resp.data(0))
      }
    }
  }

  // redirect based on BTB, PHT, RAS and JBTAC
  // io.out.redirect.valid := false.B
  // io.out.redirect.bits := DontCare
  val redirectIdx = Wire(Vec(FetchWidth, Bool()))
  val redirectTarget = Wire(Vec(FetchWidth, UInt(VAddrBits.W)))
  (0 until FetchWidth).map(i =>
    redirectIdx(i) := btbHits(i)
      && Mux(btbRead(i)._type === BTBtype.B, phtTaken(i), true.B)
      && Mux(btbRead(i)._type === BTBtype.I, jbtacRead(i).valid, true.B)
  )
  (0 until FetchWidth).map(i =>
    redirectTarget(i) := Mux(btbRead(i)._type === BTBtype.I, jbtacRead(i).target,
      Mux(btbRead(i)._type === BTBtype.R, retAddr, btbRead(i).target))
  )
  io.out.redirect.valid := redirectIdx.asUInt.orR
  io.out.redirect.bits := PriorityMux(redirectIdx, redirectTarget)
}