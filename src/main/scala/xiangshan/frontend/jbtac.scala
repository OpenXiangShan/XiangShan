package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend.ALUOpType
import utils._


class JBTACUpdateBundle extends XSBundle {
  val fetchPC = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(FetchWidth).W)
  val hist = UInt(HistoryLength.W)
  val target = UInt(VAddrBits.W)
  val _type = UInt(2.W)
  val misPred = Bool()
}

class JBTACPred extends XSBundle {
  val hit = Bool()
  val target = UInt(VAddrBits.W)
  val hitIdx = UInt(log2Up(FetchWidth).W)
}

class JBTAC extends XSModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val pc = Flipped(Decoupled(UInt(VAddrBits.W)))
      val pcLatch = Input(UInt(VAddrBits.W))
      val hist = Input(UInt(HistoryLength.W))
    }
    val redirectValid = Input(Bool())
    val flush = Input(Bool())
    val update = Input(new JBTACUpdateBundle)

    val out = Output(new JBTACPred)
  })

  io.in.pc.ready := true.B
  // JBTAC, divided into 8 banks, makes prediction for indirect jump except ret.
  val jbtacAddr = new TableAddr(log2Up(JbtacSize), JbtacBanks)
  def jbtacEntry() = new Bundle {
    val valid = Bool()
    // TODO: don't need full length of tag and target
    val tag = UInt(jbtacAddr.tagBits.W)
    val target = UInt(VAddrBits.W)
    val offset = UInt(log2Up(FetchWidth).W)
  }

  val jbtac = List.fill(JbtacBanks)(Module(new SRAMTemplate(jbtacEntry(), set = JbtacSize / JbtacBanks, shouldReset = true, holdRead = true, singlePort = false)))

  val readEntries = Wire(Vec(JbtacBanks, jbtacEntry()))

  val readFire = Reg(Vec(JbtacBanks, Bool()))
  // Only read one bank
  val histXORAddr = io.in.pc.bits ^ Cat(io.in.hist, 0.U(2.W))(VAddrBits - 1, 0)
  val histXORAddrLatch = RegEnable(histXORAddr, io.in.pc.valid)
  readFire := 0.U.asTypeOf(Vec(JbtacBanks, Bool()))
  (0 until JbtacBanks).map(
    b => {
      jbtac(b).reset := reset.asBool
      jbtac(b).io.r.req.valid := io.in.pc.fire() && b.U === jbtacAddr.getBank(histXORAddr)
      jbtac(b).io.r.req.bits.setIdx := jbtacAddr.getBankIdx(histXORAddr)
      readFire(b) := jbtac(b).io.r.req.fire()
      readEntries(b) := jbtac(b).io.r.resp.data(0)
    }
  )

  val bank = jbtacAddr.getBank(histXORAddrLatch)

  io.out.hit := readEntries(bank).valid && readEntries(bank).tag === jbtacAddr.getTag(io.in.pcLatch) && !io.flush && readFire(bank)
  io.out.hitIdx := readEntries(bank).offset
  io.out.target := readEntries(bank).target

    // 2. update jbtac
  val writeEntry = Wire(jbtacEntry())
  // val updateHistXORAddr = updatefetchPC ^ Cat(r.hist, 0.U(2.W))(VAddrBits - 1, 0)
  val updateHistXORAddr = io.update.fetchPC ^ Cat(io.update.hist, 0.U(2.W))(VAddrBits - 1, 0)
  writeEntry.valid := true.B
  // writeEntry.tag := jbtacAddr.getTag(updatefetchPC)
  writeEntry.tag := jbtacAddr.getTag(io.update.fetchPC)
  writeEntry.target := io.update.target
  // writeEntry.offset := updateFetchIdx
  writeEntry.offset := io.update.fetchIdx
  for (b <- 0 until JbtacBanks) {
    when (b.U === jbtacAddr.getBank(updateHistXORAddr)) {
      jbtac(b).io.w.req.valid := io.redirectValid && io.update.misPred && io.update._type === BTBtype.I
      jbtac(b).io.w.req.bits.setIdx := jbtacAddr.getBankIdx(updateHistXORAddr)
      jbtac(b).io.w.req.bits.data := writeEntry
    }.otherwise {
      jbtac(b).io.w.req.valid := false.B
      jbtac(b).io.w.req.bits.setIdx := DontCare
      jbtac(b).io.w.req.bits.data := DontCare
    }
  }
}