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

class BtbDataEntry extends XSBundle with BTBParams {
  val offset = SInt(offsetLen.W)
  val extended = Bool()
}

object BtbDataEntry {
  def apply(offset: SInt, extended: Bool) = {
    val e = Wire(new BtbDataEntry)
    e.offset := offset
    e.extended := extended
    e
  }
}

class BtbMetaEntry() extends XSBundle with BTBParams {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Up(BtbSize) - 1).W)
  val btbType = UInt(2.W)
  val isRVC = Bool()
}

object BtbMetaEntry {
  def apply(tag: UInt, btbType: UInt, isRVC: Bool) = {
    val e = Wire(new BtbMetaEntry)
    e.valid := true.B
    e.tag := tag
    e.btbType := btbType
    e.isRVC := isRVC
    e
  }
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
      Module(new SRAMTemplate(new BtbDataEntry, set = nRows, shouldReset = true, holdRead = true))
    }
  }
  val meta = List.fill(BtbWays) {
    List.fill(BtbBanks) {
      Module(new SRAMTemplate(new BtbMetaEntry, set = nRows, shouldReset = true, holdRead = true))
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
    io.resp.targets(b) := Mux(data_entry.extended, edataRead, (pcLatch.asSInt + (b << 1).S + data_entry.offset).asUInt)
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
  val u = io.update.bits.ui
  
  val max_offset = Cat(0.B, ~(0.U((offsetLen-1).W))).asSInt
  val min_offset = Cat(1.B,  (0.U((offsetLen-1).W))).asSInt
  val new_target = Mux(u.pd.isBr, u.brTarget, u.target)
  val new_offset = (new_target.asSInt -
    u.pc.asSInt)
  val new_extended = (new_offset > max_offset || new_offset < min_offset)


  val updateWay = u.brInfo.btbWriteWay
  val updateBankIdx = btbAddr.getBank(u.pc)
  val updateRow = btbAddr.getBankIdx(u.pc)
  val metaWrite = BtbMetaEntry(btbAddr.getTag(u.pc), pdInfoToBTBtype(u.pd), u.pd.isRVC)
  val dataWrite = BtbDataEntry(new_offset, new_extended)

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
    XSDebug(validLatch && bankHits(bankIdxInOrder(i)), "resp(%d): bank(%d) hits, tgt=%x, isRVC=%d, type=%d\n",
      i.U, idx, io.resp.targets(i), io.resp.isRVC(i), io.resp.types(i))
  }
  XSDebug(updateValid, "update_req: pc=0x%x, target=0x%x, offset=%x, extended=%d, way=%d, bank=%d, row=0x%x\n",
    u.pc, new_target, new_offset, new_extended, updateWay, updateBankIdx, updateRow)
}