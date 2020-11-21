package xiangshan.frontend

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import xiangshan.backend.decode.XSTrap

import scala.math.min

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
    val hitJal = Vec(PredictWidth, Bool())
  }
  class BTBFromOthers extends FromOthers {}
  
  class BTBIO extends DefaultBasePredictorIO {
    val resp = Output(new BTBResp)
    val meta = Output(new BTBMeta)
  }
  override val debug = true
  override val io = IO(new BTBIO)
  val btbAddr = new TableAddr(log2Up(BtbSize/BtbWays), BtbBanks)

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

  val realMask = circularShiftLeft(io.inMask, BtbBanks, baseBank)

  val realMaskLatch = RegEnable(realMask, io.pc.valid)

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
  val realTags = VecInit((0 until BtbBanks).map(b => Mux(tagIncremented(b), baseTag + 1.U, baseTag)))

  val totalHits = VecInit((0 until BtbBanks).map( b => 
    VecInit((0 until BtbWays).map( w =>
      // This should correspond to the real mask from last valid cycle!
      metaRead(w)(b).tag === realTags(b) && metaRead(w)(b).valid && realMaskLatch(b)
    ))
  ))
  val bankHits = VecInit(totalHits.map(_.reduce(_||_)))
  val bankHitWays = VecInit(totalHits.map(PriorityEncoder(_)))


  def allocWay(valids: UInt, meta_tags: UInt, req_tag: UInt) = {
    val randomAlloc = true
    if (BtbWays > 1) {
      val w = Wire(UInt(log2Up(BtbWays).W))
      val valid = WireInit(valids.andR)
      val tags = Cat(meta_tags, req_tag)
      val l = log2Up(BtbWays)
      val nChunks = (tags.getWidth + l - 1) / l
      val chunks = (0 until nChunks).map( i => 
        tags(min((i+1)*l, tags.getWidth)-1, i*l)
      )
      w := Mux(valid, chunks.reduce(_^_), (if (randomAlloc) {LFSR64()(log2Up(BtbWays)-1,0)} else {PriorityEncoder(~valids)}))
      w
    } else {
      val w = WireInit(0.U)
      w
    }
  }
  val allocWays = VecInit((0 until BtbBanks).map(b => 
    allocWay(VecInit(metaRead.map(w => w(b).valid)).asUInt,
             VecInit(metaRead.map(w => w(b).tag)).asUInt,
             realTags(b))))

  val writeWay = VecInit((0 until BtbBanks).map(
    b => Mux(bankHits(b), bankHitWays(b), allocWays(b))
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
    io.meta.hitJal(b)   := bankHits(bankIdxInOrder(b)) && meta_entry.btbType === BTBtype.J
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
  val new_offset = (new_target.asSInt - u.pc.asSInt)
  val new_extended = (new_offset > max_offset || new_offset < min_offset)


  val updateWay = u.brInfo.btbWriteWay
  val updateBankIdx = btbAddr.getBank(u.pc)
  val updateRow = btbAddr.getBankIdx(u.pc)
  val updateType = pdInfoToBTBtype(u.pd)
  val metaWrite = BtbMetaEntry(btbAddr.getTag(u.pc), updateType, u.pd.isRVC)
  val dataWrite = BtbDataEntry(new_offset, new_extended)

  val jalFirstEncountered = !u.isMisPred && !u.brInfo.btbHitJal && updateType === BTBtype.J
  val updateValid = io.update.valid && (u.isMisPred || jalFirstEncountered || !u.isMisPred && u.pd.isBr)
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


  if (BPUDebug && debug) {
    val debug_verbose = true
    
    XSDebug("isInNextRow: ")
    (0 until BtbBanks).foreach(i => {
      XSDebug(false, true.B, "%d ", isInNextRow(i))
      if (i == BtbBanks-1) { XSDebug(false, true.B, "\n") }
    })

    val validLatch = RegNext(io.pc.valid)
    XSDebug(io.pc.valid, "read: pc=0x%x, baseBank=%d, realMask=%b\n", io.pc.bits, baseBank, realMask)
    XSDebug(validLatch, "read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
      pcLatch, btbAddr.getIdx(pcLatch))
    if (debug_verbose) {
      for (i <- 0 until BtbBanks){
        for (j <- 0 until BtbWays) {
          XSDebug(validLatch, "read_resp[w=%d][b=%d][r=%d] is valid(%d) mask(%d), tag=0x%x, offset=0x%x, type=%d, isExtend=%d, isRVC=%d\n",
          j.U, i.U, realRowLatch(i), metaRead(j)(i).valid, realMaskLatch(i), metaRead(j)(i).tag, dataRead(j)(i).offset, metaRead(j)(i).btbType, dataRead(j)(i).extended, metaRead(j)(i).isRVC)
        }
      }
    }
    for (i <- 0 until BtbBanks) {
      val idx = bankIdxInOrder(i)
      XSDebug(validLatch && bankHits(bankIdxInOrder(i)), "resp(%d): bank(%d) hits, tgt=%x, isRVC=%d, type=%d\n",
        i.U, idx, io.resp.targets(i), io.resp.isRVC(i), io.resp.types(i))
    }
    XSDebug(updateValid, "update_req: cycle=%d, pc=0x%x, target=0x%x, misPred=%d, offset=%x, extended=%d, way=%d, bank=%d, row=0x%x\n",
      u.brInfo.debug_btb_cycle, u.pc, new_target, u.isMisPred, new_offset, new_extended, updateWay, updateBankIdx, updateRow)
    for (i <- 0 until BtbBanks) {
      // Conflict when not hit and allocating a valid entry
      val conflict = metaRead(allocWays(i))(i).valid && !bankHits(i)
      XSDebug(conflict, "bank(%d) is trying to allocate a valid way(%d)\n", i.U, allocWays(i))
      // There is another circumstance when a branch is on its way to update while another
      // branch chose the same way to udpate, then after the first branch is wrote in, 
      // the second branch will overwrite the first branch
  }

  }
}