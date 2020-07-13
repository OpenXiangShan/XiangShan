package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils._
import xiangshan.backend.ALUOpType
import utils._

class BTBUpdateBundle extends XSBundle {
  val fetchPC = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(FetchWidth).W)
  val hit = Bool()
  val misPred = Bool()
  val writeWay = UInt(log2Up(BtbWays).W)
  val oldCtr = UInt(2.W)
  val taken = Bool()
  val target = UInt(VAddrBits.W)
  val _type = UInt(2.W)
}

class BTBPred extends XSBundle {
  val hit = Bool()
  val taken = Bool()
  val takenIdx = UInt(log2Up(FetchWidth).W)
  val target = UInt(VAddrBits.W)

  val writeWay = UInt(log2Up(BtbWays).W)
  val notTakens = Vec(FetchWidth, Bool())
  val dEntries = Vec(FetchWidth, btbDataEntry())
}

case class btbDataEntry() extends XSBundle {
  val valid = Bool()
  val target = UInt(VAddrBits.W)
  val pred = UInt(2.W) // 2-bit saturated counter as a quick predictor
  val _type = UInt(2.W)
}

case class btbMetaEntry() extends XSBundle {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Up(BtbSets) - 2).W)
}

class BTB extends XSModule {
  val io = IO(new Bundle() {
    // Input
    val in = new Bundle {
      val pc = Flipped(Decoupled(UInt(VAddrBits.W)))
      val pcLatch = Input(UInt(VAddrBits.W))
    }
    val redirectValid = Input(Bool())
    val flush = Input(Bool())
    val update = Input(new BTBUpdateBundle)
    // Output
    val out = Output(new BTBPred)
  })

  io.in.pc.ready := true.B


  val btbAddr = new TableAddr(log2Up(BtbSets), BtbBanks)

  // SRAMs to store BTB meta & data
  val btbMeta = List.fill(BtbWays)(List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbMetaEntry(), set = BtbSets / BtbBanks, way = 1, shouldReset = true, holdRead = true))
  ))
  val btbData = List.fill(BtbWays)(List.fill(BtbBanks)(
    Module(new SRAMTemplate(btbDataEntry(), set = BtbSets / BtbBanks, way = FetchWidth, shouldReset = true, holdRead = true))
  ))

  // BTB read requests
  // read addr comes from pc[6:2]
  // read 4 ways in parallel
  (0 until BtbWays).map(
    w => (0 until BtbBanks).map(
      b => {
        btbMeta(w)(b).reset := reset.asBool
        btbMeta(w)(b).io.r.req.valid := io.in.pc.valid && b.U === btbAddr.getBank(io.in.pc.bits)
        btbMeta(w)(b).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
        btbData(w)(b).reset := reset.asBool
        btbData(w)(b).io.r.req.valid := io.in.pc.valid && b.U === btbAddr.getBank(io.in.pc.bits)
        btbData(w)(b).io.r.req.bits.setIdx := btbAddr.getBankIdx(io.in.pc.bits)
      }
    )
  )

  // latch pc for 1 cycle latency when reading SRAM
  val pcLatch = RegEnable(io.in.pc.bits, io.in.pc.valid)
  // Entries read from SRAM
  val metaRead = Wire(Vec(BtbWays, btbMetaEntry()))
  val dataRead = Wire(Vec(BtbWays, Vec(FetchWidth, btbDataEntry())))
  val readFire = Wire(Vec(BtbWays, Vec(BtbBanks, Bool())))
  
  metaRead := DontCare
  dataRead := DontCare

  val readBankIdx = btbAddr.getBank(pcLatch)

  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      when (b.U === readBankIdx) {
        metaRead(w) := btbMeta(w)(b).io.r.resp.data(0)
        (0 until FetchWidth).map(i => dataRead(w)(i) := btbData(w)(b).io.r.resp.data(i))
      }
    }
  }

  // 1/4 hit intended
  val wayHits = Wire(Vec(BtbWays, Bool()))
  val hitWayIdx = Wire(UInt(log2Up(BtbWays).W))

  // #(FetchWidth) results
  val dataEntries = Wire(Vec(FetchWidth, btbDataEntry()))

  wayHits := 0.U.asTypeOf(Vec(BtbWays, Bool()))
  dataEntries.map(_.valid := false.B)
  dataEntries.map(_.pred := DontCare)
  dataEntries.map(_.target := DontCare)
  dataEntries.map(_._type := DontCare)


  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) { readFire(w)(b) := btbMeta(w)(b).io.r.req.fire() && btbData(w)(b).io.r.req.fire() }
    when (metaRead(w).valid && metaRead(w).tag === btbAddr.getTag(pcLatch)) {
      wayHits(w) := !io.flush && RegNext(readFire(w)(readBankIdx), init = false.B)
      for (i <- 0 until FetchWidth) {
        dataEntries(i).valid := dataRead(w)(i).valid
        dataEntries(i)._type := dataRead(w)(i)._type
        dataEntries(i).pred := dataRead(w)(i).pred
        dataEntries(i).target := dataRead(w)(i).target
      }
    }
  }

  val hit = wayHits.reduce(_||_)
  hitWayIdx := OHToUInt(HighestBit(wayHits.asUInt, BtbWays))

  // taken branches of jumps from a valid entry
  val predTakens = Wire(Vec(FetchWidth, Bool()))
  // not taken branches from a valid entry
  val notTakenBranches = Wire(Vec(FetchWidth, Bool()))
  for (i <- 0 until FetchWidth) {
    predTakens(i) := dataEntries(i).valid && (dataEntries(i)._type === BTBtype.J || dataEntries(i)._type === BTBtype.B && dataEntries(i).pred(1).asBool)
    notTakenBranches(i) := dataEntries(i).valid && dataEntries(i)._type === BTBtype.B && !dataEntries(i).pred(1).asBool
  }

  val isTaken       = predTakens.reduce(_||_)
  // Priority mux which corresponds with inst orders
  // BTB only produce one single prediction
  val takenTarget = MuxCase(0.U, predTakens zip dataEntries.map(_.target))
  val takenType   = MuxCase(0.U, predTakens zip dataEntries.map(_._type))
  // Record which inst is predicted taken
  val takenIdx = MuxCase(0.U, predTakens zip (0 until FetchWidth).map(_.U))

  // choose one way to write BTB
  // If the read hits, choose hitWay, else choose an invalid way(random way if no invalid ways)
  val wayInvalids = Cat(metaRead.map(e => !e.valid)).asUInt
  val writeWay = Mux(hit, hitWayIdx, Mux(wayInvalids.orR, OHToUInt(LowestBit(wayInvalids, BtbWays)), LFSR64()(log2Up(BtbWays) - 1, 0)))

  // Update logic
  // 1 calculate new 2-bit saturated counter value
  val u = io.update
  val newCtr = Mux(!u.hit, "b01".U, Mux(u.taken, Mux(u.oldCtr === "b11".U, "b11".U, u.oldCtr + 1.U),
                                                           Mux(u.oldCtr === "b00".U, "b00".U, u.oldCtr - 1.U)))
                                                      
  val updateOnSaturated = u.taken && u.oldCtr === "b11".U || !u.taken && u.oldCtr === "b00".U

  // 2 write btb
  val updateBank = btbAddr.getBank(u.fetchPC)
  val updateBankIdx = btbAddr.getBankIdx(u.fetchPC)
  val updateWaymask = UIntToOH(u.fetchIdx)
  val btbMetaWrite = Wire(btbMetaEntry())
  btbMetaWrite.valid := true.B
  btbMetaWrite.tag := btbAddr.getTag(u.fetchPC)
  val btbDataWrite = Wire(btbDataEntry())
  btbDataWrite.valid := true.B
  btbDataWrite.target := u.target
  btbDataWrite.pred := newCtr
  btbDataWrite._type := u._type

  val isBr = u._type === BTBtype.B
  val isJ = u._type === BTBtype.J
  val notBrOrJ = u._type =/= BTBtype.B && u._type =/= BTBtype.J

  // Do not update BTB on indirect or return, or correctly predicted J or saturated counters
  val noNeedToUpdate = (!u.misPred && (isBr && updateOnSaturated || isJ)) || (u.misPred && notBrOrJ)

  // do not update on saturated ctrs
  val btbWriteValid = io.redirectValid && !noNeedToUpdate

  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      // println(s"${btbData(w)(b).io.w.req.bits.waymask.nonEmpty}")
      when (b.U === updateBank && w.U === u.writeWay) {
        btbMeta(w)(b).io.w.req.valid := btbWriteValid
        btbMeta(w)(b).io.w.req.bits.setIdx := updateBankIdx
        btbMeta(w)(b).io.w.req.bits.data := btbMetaWrite
        btbData(w)(b).io.w.req.valid := btbWriteValid
        btbData(w)(b).io.w.req.bits.setIdx := updateBankIdx
        btbData(w)(b).io.w.req.bits.waymask.map(_ := updateWaymask)
        btbData(w)(b).io.w.req.bits.data := btbDataWrite
        XSDebug(btbWriteValid, "write btb: setIdx=%x meta.tag=%x updateWaymask=%d target=%x _type=%b predCtr=%b\n",
          updateBankIdx, btbMetaWrite.tag, updateWaymask, btbDataWrite.target, btbDataWrite._type, btbDataWrite.pred)
      }.otherwise {
        btbMeta(w)(b).io.w.req.valid := false.B
        btbMeta(w)(b).io.w.req.bits.setIdx := DontCare
        btbMeta(w)(b).io.w.req.bits.data := DontCare
        btbData(w)(b).io.w.req.valid := false.B
        btbData(w)(b).io.w.req.bits.setIdx := DontCare
        btbData(w)(b).io.w.req.bits.waymask.map(_ := 0.U)
        btbData(w)(b).io.w.req.bits.data := DontCare
      }
    }
  }

  // write and read bypass
  for ( w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      when (RegNext(updateBank) === btbAddr.getBank(io.in.pcLatch) && RegNext(updateBankIdx) === btbAddr.getBankIdx(io.in.pcLatch)) {
        when (RegNext(btbWriteValid && io.in.pc.valid) && w.U === RegNext(u.writeWay) && b.U === RegNext(updateBank)) {
          metaRead(u.writeWay) := RegNext(btbMetaWrite)
          (0 until FetchWidth).map(i => dataRead(RegNext(u.writeWay))(i.U) := Mux(RegNext(updateWaymask(i)), RegNext(btbDataWrite), btbData(w)(b).io.r.resp.data(i)))

          XSDebug(true.B, "BTB write & read bypass hit!\n")
        }
      }
    }
  }

  io.out.hit := hit
  io.out.taken := isTaken
  io.out.takenIdx := takenIdx
  io.out.target := takenTarget
  io.out.writeWay := writeWay
  io.out.notTakens := notTakenBranches
  io.out.dEntries := dataEntries
}