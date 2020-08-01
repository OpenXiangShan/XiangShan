package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap

trait BTBParams extends HasXSParameter {
  val nRows = BtbSize / (PredictWidth * BtbWays)
  val offsetLen = 13
  val extendedNRows = nRows
}

case class BtbDataEntry() extends XSBundle with BTBParams {
  val offset = SInt(offsetLen.W)
  val extended = Bool()
  // def apply(offset: SInt, extended: Bool) = {
  //   this.offset := offset
  //   this.extended := extended
  //   this
  // }
}

case class BtbMetaEntry() extends XSBundle with BTBParams {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Up(BtbSize) - 1).W)
  val btbType = UInt(2.W)
  val isRVC = Bool()
  // def apply(tag: UInt, btbType: UInt, isRVC: Bool) = {
  //   this.valid := true.B
  //   this.tag := tag
  //   this.btbType := btbType
  //   this.isRVC := isRVC
  //   this
  // }
}

class BTB extends BasePredictor with BTBParams{
  class BTBResp extends Resp {
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val types = Vec(PredictWidth, UInt(2.W))
    val isRVC = Vec(PredictWidth, Bool())
  }
  class BTBMeta extends Meta {
    val writeWay =  Vec(PredictWidth, UInt(log2Up(BtbWays).W))
  }
  class BTBFromOthers extends FromOthers {}
  
  class BTBIO extends DefaultBasePredictorIO {
    val resp = Output(new BTBResp)
    val meta = Output(new BTBMeta)
  }
  override val io = IO(new BTBIO)
  val btbAddr = new TableAddr(log2Up(BtbSize), BtbBanks)

  val pcLatch = RegEnable(io.pc.bits, io.pc.valid)

  val data = List.fill(BtbWays) {
    List.fill(BtbBanks) {
      Module(new SRAMTemplate(BtbDataEntry(), set = nRows, shouldReset = true, holdRead = true))
    }
  }
  val meta = List.fill(BtbWays) {
    List.fill(BtbBanks) {
      Module(new SRAMTemplate(BtbMetaEntry(), set = nRows, shouldReset = true, holdRead = true))
    }
  }
  val edata = Module(new SRAMTemplate(UInt(VAddrBits.W), set = extendedNRows, shouldReset = true, holdRead = true))

  // BTB read requests
  val baseBank = btbAddr.getBank(io.pc.bits)

  val realMask = circularShiftRight(io.inMask, BtbBanks, baseBank)

  // those banks whose indexes are less than baseBank are in the next row
  val isInNextRow = VecInit((0 until BtbBanks).map(_.U < baseBank))

  val baseRow = btbAddr.getBankIdx(io.pc.bits)

  val nextRowStartsUp = baseRow.andR

  val realRow = VecInit((0 until BtbBanks).map(b => Mux(isInNextRow(b.U), (baseRow+1.U)(log2Up(nRows)-1, 0), baseRow)))

  val realRowLatch = VecInit(realRow.map(RegEnable(_, enable=io.pc.valid)))

  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      meta(w)(b).reset                := reset.asBool
      meta(w)(b).io.r.req.valid       := realMask(b) && io.pc.valid
      meta(w)(b).io.r.req.bits.setIdx := realRow(b)
      data(w)(b).reset                := reset.asBool
      data(w)(b).io.r.req.valid       := realMask(b) && io.pc.valid
      data(w)(b).io.r.req.bits.setIdx := realRow(b)
    }
  }
  edata.reset                := reset.asBool
  edata.io.r.req.valid       := io.pc.valid
  edata.io.r.req.bits.setIdx := realRow(0) // Use the baseRow

  // Entries read from SRAM
  val metaRead = VecInit((0 until BtbWays).map(w => VecInit((0 until BtbBanks).map( b => meta(w)(b).io.r.resp.data(0)))))
  val dataRead = VecInit((0 until BtbWays).map(w => VecInit((0 until BtbBanks).map( b => data(w)(b).io.r.resp.data(0)))))
  val edataRead = edata.io.r.resp.data(0)

  val baseBankLatch = btbAddr.getBank(pcLatch)
  val baseTag = btbAddr.getTag(pcLatch)

  val tagIncremented = VecInit((0 until BtbBanks).map(b => RegEnable(isInNextRow(b.U) && nextRowStartsUp, io.pc.valid)))

  val totalHits = VecInit((0 until BtbBanks).map( b => 
    VecInit((0 until BtbWays).map( w =>
      metaRead(w)(b).tag === Mux(tagIncremented(b), baseTag + 1.U, baseTag) && metaRead(w)(b).valid
    ))
  ))
  val bankHits = VecInit(totalHits.map(_.reduce(_||_)))
  val bankHitWays = VecInit(totalHits.map(PriorityEncoder(_)))

  val writeWay = VecInit((0 until BtbBanks).map(
    b => Mux(bankHits(b), bankHitWays(b), LFSR64()(0))
  ))

  // e.g: baseBank == 5 => (5, 6,..., 15, 0, 1, 2, 3, 4)
  val bankIdxInOrder = VecInit((0 until BtbBanks).map(b => (baseBankLatch +& b.U)(log2Up(BtbBanks)-1,0)))


  for (b <- 0 until BtbBanks) {
    val meta_entry = metaRead(bankHitWays(bankIdxInOrder(b)))(bankIdxInOrder(b))
    val data_entry = dataRead(bankHitWays(bankIdxInOrder(b)))(bankIdxInOrder(b))
    // Use real pc to calculate the target
    io.resp.targets(b) := Mux(data_entry.extended, edataRead, (pcLatch.asSInt + (bankIdxInOrder(b.U) << 1).asSInt + data_entry.offset).asUInt)
    io.resp.hits(b)  := bankHits(bankIdxInOrder(b))
    io.resp.types(b) := meta_entry.btbType
    io.resp.isRVC(b) := meta_entry.isRVC
    io.meta.writeWay(b) := writeWay(bankIdxInOrder(b))
  }

  def pdInfoToBTBtype(pd: PreDecodeInfo) = {
    val t = WireInit(0.U(2.W))
    when (pd.isJalr) { t := BTBtype.I}
    when (pd.isRet)  { t := BTBtype.R}
    when (pd.isJal)  { t := BTBtype.J}
    when (pd.isBr)   { t := BTBtype.B}
    t
  }
  val u = io.update.bits
  
  val max_offset = Cat(0.B, ~(0.U((offsetLen-1).W))).asSInt
  val min_offset = Cat(1.B,  (0.U((offsetLen-1).W))).asSInt
  val new_target = Mux(u.pd.isBr, u.brTarget, u.target)
  val new_offset = (new_target.asSInt -
    u.pc.asSInt)
  val new_extended = (new_offset > max_offset || new_offset < min_offset)


  val updateWay = u.brInfo.btbWriteWay
  val updateBankIdx = btbAddr.getBank(u.pc)
  val updateRow = btbAddr.getBankIdx(u.pc)
  val metaWrite = Wire(BtbMetaEntry())
  metaWrite.valid   := true.B
  metaWrite.tag     := btbAddr.getTag(u.pc)
  metaWrite.btbType := pdInfoToBTBtype(u.pd)
  metaWrite.isRVC   := u.pd.isRVC
  val dataWrite = Wire(BtbDataEntry())
  dataWrite.offset   := new_offset
  dataWrite.extended := new_extended

  val updateValid = io.update.valid
  // Update btb
  for (w <- 0 until BtbWays) {
    for (b <- 0 until BtbBanks) {
      meta(w)(b).io.w.req.valid := updateValid && b.U === updateBankIdx && w.U === updateWay
      meta(w)(b).io.w.req.bits.setIdx := updateRow
      meta(w)(b).io.w.req.bits.data := metaWrite
      data(w)(b).io.w.req.valid := updateValid && b.U === updateBankIdx && w.U === updateWay
      data(w)(b).io.w.req.bits.setIdx := updateRow
      data(w)(b).io.w.req.bits.data := dataWrite
    }
  }

  edata.io.w.req.valid := updateValid && new_extended
  edata.io.w.req.bits.setIdx := updateRow
  edata.io.w.req.bits.data := u.target

  val debug_verbose = true

  val validLatch = RegNext(io.pc.valid)
  XSDebug(io.pc.valid, "read: pc=0x%x, baseBank=%d, realMask=%b\n", io.pc.bits, baseBank, realMask)
  XSDebug(validLatch, "read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
    pcLatch, btbAddr.getIdx(pcLatch))
  if (debug_verbose) {
    for (i <- 0 until BtbBanks){
      for (j <- 0 until BtbWays) {
        XSDebug(validLatch && metaRead(j)(i).valid, "read_resp[w=%d][b=%d][r=%d] is valid, tag=0x%x, offset=0x%x, type=%d, isExtend=%d, isRVC=%d\n",
        j.U, i.U, realRowLatch(i), metaRead(j)(i).tag, dataRead(j)(i).offset, metaRead(j)(i).btbType, dataRead(j)(i).extended, metaRead(j)(i).isRVC)
      }
    }
  }
  for (i <- 0 until BtbBanks) {
    val idx = bankIdxInOrder(i)
    XSDebug(validLatch && bankHits(i), "resp(%d): bank(%d) hits, tgt=%x, isRVC=%d, type=%d\n",
      i.U, idx, io.resp.targets(i), io.resp.isRVC(i), io.resp.types(i))
  }
  XSDebug(updateValid, "update_req: pc=0x%x, target=0x%x, offset=%x, extended=%d, way=%d, bank=%d, row=0x%x\n",
    u.pc, new_target, new_offset, new_extended, updateWay, updateBankIdx, updateRow)
}



// class BTB extends XSModule {
//   val io = IO(new Bundle() {
//     // Input
//     val in = new Bundle {
//       val pc = Flipped(Decoupled(UInt(VAddrBits.W)))
//       val pcLatch = Input(UInt(VAddrBits.W))
//       val mask = Input(UInt(PredictWidth.W))
//     }
//     val redirectValid = Input(Bool())
//     val flush = Input(Bool())
//     val update = Input(new BTBUpdateBundle)
//     // Output
//     val out = Output(new BTBPred)
//   })

//   io.in.pc.ready := true.B
//   val fireLatch = RegNext(io.in.pc.fire())
//   val maskLatch = RegEnable(io.in.mask, io.in.pc.fire())

//   val btbAddr = new TableAddr(log2Up(BtbSize), BtbBanks)

//   // SRAMs to store BTB meta & data
//   val btbMeta = List.fill(BtbBanks)(
//     Module(new SRAMTemplate(btbMetaEntry(), set = BtbSize / BtbBanks, shouldReset = true, holdRead = true)))
//   val btbData = List.fill(BtbBanks)(
//     Module(new SRAMTemplate(btbDataEntry(), set = BtbSize / BtbBanks, shouldReset = true, holdRead = true)))

//   // BTB read requests
//   val baseBank = btbAddr.getBank(io.in.pc.bits)
//   // circular shifting
//   def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
//     val res = Wire(UInt(len.W))
//     val higher = source << shamt
//     val lower = source >> (len.U - shamt)
//     res := higher | lower
//     res
//   }
//   val realMask = circularShiftLeft(io.in.mask, BtbBanks, baseBank)

//   // those banks whose indexes are less than baseBank are in the next row
//   val isInNextRow = VecInit((0 until BtbBanks).map(_.U < baseBank))

//   val baseRow = btbAddr.getBankIdx(io.in.pc.bits)
//   // this row is the last row of a bank
//   val nextRowStartsUp = baseRow.andR
//   val realRow = VecInit((0 until BtbBanks).map(b => Mux(isInNextRow(b.U), Mux(nextRowStartsUp, 0.U, baseRow+1.U), baseRow)))
//   val realRowLatch = VecInit(realRow.map(RegNext(_)))

//   for (b <- 0 until BtbBanks) {
//     btbMeta(b).reset := reset.asBool
//     btbMeta(b).io.r.req.valid := realMask(b) && io.in.pc.valid
//     btbMeta(b).io.r.req.bits.setIdx := realRow(b)
//     btbData(b).reset := reset.asBool
//     btbData(b).io.r.req.valid := realMask(b) && io.in.pc.valid
//     btbData(b).io.r.req.bits.setIdx := realRow(b)
//   }




//   // Entries read from SRAM
//   val metaRead = Wire(Vec(BtbBanks, btbMetaEntry()))
//   val dataRead = Wire(Vec(BtbBanks, btbDataEntry()))
//   val readFire = Wire(Vec(BtbBanks, Bool()))
//   for (b <- 0 until BtbBanks) {
//     readFire(b) := btbMeta(b).io.r.req.fire() && btbData(b).io.r.req.fire()
//     metaRead(b) := btbMeta(b).io.r.resp.data(0)
//     dataRead(b) := btbData(b).io.r.resp.data(0)
//   }

//   val baseBankLatch = btbAddr.getBank(io.in.pcLatch)
//   // val isAlignedLatch = baseBankLatch === 0.U
//   val baseTag = btbAddr.getTag(io.in.pcLatch)
//   // If the next row starts up, the tag needs to be incremented as well
//   val tagIncremented = VecInit((0 until BtbBanks).map(b => RegEnable(isInNextRow(b.U) && nextRowStartsUp, io.in.pc.valid)))

//   val bankHits = Wire(Vec(BtbBanks, Bool()))
//   for (b <- 0 until BtbBanks) {
//     bankHits(b) := metaRead(b).valid &&
//       (Mux(tagIncremented(b), baseTag+1.U, baseTag) === metaRead(b).tag) && !io.flush && RegNext(readFire(b), init = false.B)
//   }

//   // taken branches of jumps from a valid entry
//   val predTakens = Wire(Vec(BtbBanks, Bool()))
//   // not taken branches from a valid entry
//   val notTakenBranches = Wire(Vec(BtbBanks, Bool()))
//   for (b <- 0 until BtbBanks) {
//     predTakens(b) := bankHits(b) && (dataRead(b).btbType === BTBtype.J || dataRead(b).btbType === BTBtype.B && dataRead(b).pred(1).asBool)
//     notTakenBranches(b) := bankHits(b) && dataRead(b).btbType === BTBtype.B && !dataRead(b).pred(1).asBool
//   }

//   // e.g: baseBank == 5 => (5, 6,..., 15, 0, 1, 2, 3, 4)
//   val bankIdxInOrder = VecInit((0 until BtbBanks).map(b => (baseBankLatch + b.U) % BtbBanks.U))

//   val isTaken       = predTakens.reduce(_||_)
//   // Priority mux which corresponds with inst orders
//   // BTB only produce one single prediction
//   val takenTarget = MuxCase(0.U, bankIdxInOrder.map(b => (predTakens(b), dataRead(b).target)))
//   val takenType   = MuxCase(0.U, bankIdxInOrder.map(b => (predTakens(b), dataRead(b).btbType)))
//   // Record which inst is predicted taken
//   val takenIdx = MuxCase(0.U, (0 until BtbBanks).map(b => (predTakens(bankIdxInOrder(b)), b.U)))

//   // Update logic
//   // 1 calculate new 2-bit saturated counter value
//   def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
//     val oldSatTaken = old === ((1 << len)-1).U
//     val oldSatNotTaken = old === 0.U
//     Mux(oldSatTaken && taken, ((1 << len)-1).U,
//       Mux(oldSatNotTaken && !taken, 0.U,
//         Mux(taken, old + 1.U, old - 1.U)))
//   }

//   val u = io.update
//   val newCtr = Mux(!u.hit, "b10".U, satUpdate(u.oldCtr, 2, u.taken))

//   val updateOnSaturated = u.taken && u.oldCtr === "b11".U || !u.taken && u.oldCtr === "b00".U

//   // 2 write btb
//   val updateBankIdx = btbAddr.getBank(u.pc)
//   val updateRow = btbAddr.getBankIdx(u.pc)
//   val btbMetaWrite = Wire(btbMetaEntry())
//   btbMetaWrite.valid := true.B
//   btbMetaWrite.tag := btbAddr.getTag(u.pc)
//   val btbDataWrite = Wire(btbDataEntry())
//   btbDataWrite.target := u.target
//   btbDataWrite.pred := newCtr
//   btbDataWrite.btbType := u.btbType
//   btbDataWrite.isRVC := u.isRVC

//   val isBr = u.btbType === BTBtype.B
//   val isJ = u.btbType === BTBtype.J
//   val notBrOrJ = u.btbType =/= BTBtype.B && u.btbType =/= BTBtype.J

//   // Do not update BTB on indirect or return, or correctly predicted J or saturated counters
//   val noNeedToUpdate = (!u.misPred && (isBr && updateOnSaturated || isJ)) || notBrOrJ

//   // do not update on saturated ctrs
//   val btbWriteValid = io.redirectValid && !noNeedToUpdate

//   for (b <- 0 until BtbBanks) {
//     btbMeta(b).io.w.req.valid := btbWriteValid && b.U === updateBankIdx
//     btbMeta(b).io.w.req.bits.setIdx := updateRow
//     btbMeta(b).io.w.req.bits.data := btbMetaWrite
//     btbData(b).io.w.req.valid := btbWriteValid && b.U === updateBankIdx
//     btbData(b).io.w.req.bits.setIdx := updateRow
//     btbData(b).io.w.req.bits.data := btbDataWrite
//   }

//   // io.out.hit := bankHits.reduce(_||_)
//   io.out.taken := isTaken
//   io.out.takenIdx := takenIdx
//   io.out.target := takenTarget
//   // io.out.writeWay := writeWay
//   io.out.notTakens := VecInit((0 until BtbBanks).map(b => notTakenBranches(bankIdxInOrder(b))))
//   io.out.dEntries := VecInit((0 until BtbBanks).map(b => dataRead(bankIdxInOrder(b))))
//   io.out.hits := VecInit((0 until BtbBanks).map(b => bankHits(bankIdxInOrder(b))))
//   io.out.isRVILateJump := io.out.taken && takenIdx === OHToUInt(HighestBit(maskLatch, PredictWidth)) && !dataRead(bankIdxInOrder(takenIdx)).isRVC

//   // read-after-write bypass
//   val rawBypassHit = Wire(Vec(BtbBanks, Bool()))
//   for (b <- 0 until BtbBanks) {
//     when (b.U === updateBankIdx && realRow(b) === updateRow) { // read and write to the same address
//       when (realMask(b) && io.in.pc.valid && btbWriteValid) {  // both read and write valid
//         rawBypassHit(b) := true.B
//         btbMeta(b).io.r.req.valid := false.B
//         btbData(b).io.r.req.valid := false.B
//         // metaRead(b) := RegNext(btbMetaWrite)
//         // dataRead(b) := RegNext(btbDataWrite)
//         readFire(b) := true.B
//         XSDebug("raw bypass hits: bank=%d, row=%d, meta: %d %x, data: tgt=%x pred=%b btbType=%b isRVC=%d\n",
//           b.U, updateRow,
//           btbMetaWrite.valid, btbMetaWrite.tag,
//           btbDataWrite.target, btbDataWrite.pred, btbDataWrite.btbType, btbDataWrite.isRVC)
//       }.otherwise {
//         rawBypassHit(b) := false.B
//       }
//     }.otherwise {
//       rawBypassHit(b) := false.B
//     }

//     when (RegNext(rawBypassHit(b))) {
//       metaRead(b) := RegNext(btbMetaWrite)
//       dataRead(b) := RegNext(btbDataWrite)
//     }
//   }

//   XSDebug(io.pc.valid, "read: pc=0x%x, baseBank=%d, realMask=%b\n", io.pc.bits, baseBank, realMask)
//   XSDebug(fireLatch, "read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
//     io.in.pcLatch, btbAddr.getIdx(io.in.pcLatch))
//   for (i <- 0 until BtbBanks){
//     XSDebug(fireLatch, "read_resp[b=%d][r=%d]: valid=%d, tag=0x%x, target=0x%x, type=%d, ctr=%d\n",
//     i.U, realRowLatch(i), metaRead(i).valid, metaRead(i).tag, dataRead(i).target, dataRead(i).btbType, dataRead(i).pred)
//   }
//   XSDebug("out: taken=%d takenIdx=%d tgt=%x notTakens=%b hits=%b isRVILateJump=%d\n",
//     io.out.taken, io.out.takenIdx, io.out.target, io.out.notTakens.asUInt, io.out.hits.asUInt, io.out.isRVILateJump)
//   XSDebug(fireLatch, "bankIdxInOrder:")
//   for (i <- 0 until BtbBanks){ XSDebug(fireLatch, "%d ", bankIdxInOrder(i))}
//   XSDebug(fireLatch, "\n")
//   XSDebug(io.redirectValid, "update_req: pc=0x%x, hit=%d, misPred=%d, oldCtr=%d, taken=%d, target=0x%x, btbType=%d\n",
//     u.pc, u.hit, u.misPred, u.oldCtr, u.taken, u.target, u.btbType)
//   XSDebug(io.redirectValid, "update: noNeedToUpdate=%d, writeValid=%d, bank=%d, row=%d, newCtr=%d\n",
//     noNeedToUpdate, btbWriteValid, updateBankIdx, updateRow, newCtr)
// }