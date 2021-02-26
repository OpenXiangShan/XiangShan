package xiangshan.frontend

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.experimental.chiselName


import scala.math.min

trait BTBParams extends HasXSParameter with HasIFUConst {
  val nRows = BtbSize / (PredictWidth * BtbWays)
  val offsetLen = 13
  val lowerBitsSize = 13
  val extendedNRows = nRows
}

class BtbDataEntry extends XSBundle with BTBParams {
  val lower = UInt(lowerBitsSize.W)
  val extended = Bool()
}

object BtbDataEntry {
  def apply(lower: UInt, extended: Bool) = {
    val e = Wire(new BtbDataEntry)
    e.lower := lower
    e.extended := extended
    e
  }
}

class BtbMetaEntry() extends XSBundle with BTBParams {
  val valid = Bool()
  // TODO: don't need full length of tag
  val tag = UInt((VAddrBits - log2Ceil(nRows) - log2Ceil(PredictWidth) - instOffsetBits).W)
  val isBr = Bool()
  val isRVC = Bool()
}

object BtbMetaEntry {
  def apply(tag: UInt, isBr: UInt, isRVC: Bool) = {
    val e = Wire(new BtbMetaEntry)
    e.valid := true.B
    e.tag := tag
    e.isBr := isBr
    e.isRVC := isRVC
    e
  }
}

class BTB extends BasePredictor with BTBParams{
  class BTBResp extends Resp {
    val targets = Vec(PredictWidth, UInt(VAddrBits.W))
    val hits = Vec(PredictWidth, Bool())
    val isBrs = Vec(PredictWidth, Bool())
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
  override val debug = true
  override val io = IO(new BTBIO)
  val btbAddr = new TableAddr(log2Up(BtbSize/BtbWays), BtbBanks)

  val if1_packetAlignedPC = packetAligned(io.pc.bits)

  val if2_pc = RegEnable(if1_packetAlignedPC, io.pc.valid)

  // layout: way 0 bank 0, way 0 bank 1, ..., way 0 bank BtbBanks-1, way 1 bank 0, ..., way 1 bank BtbBanks-1
  val data = Module(new SRAMTemplate(new BtbDataEntry, set = nRows, way=BtbWays*BtbBanks, shouldReset = true, holdRead = true))
  val meta = Module(new SRAMTemplate(new BtbMetaEntry, set = nRows, way=BtbWays*BtbBanks, shouldReset = true, holdRead = true))

  val edata = Module(new SRAMTemplate(UInt(VAddrBits.W), set = extendedNRows, shouldReset = true, holdRead = true))

  val if1_mask = io.inMask
  val if2_mask = RegEnable(if1_mask, io.pc.valid)
  val if1_row = btbAddr.getBankIdx(if1_packetAlignedPC)
  val if2_row = RegEnable(if1_row, io.pc.valid)
  
  // BTB read requests
  meta.io.r.req.valid  := io.pc.valid
  data.io.r.req.valid  := io.pc.valid
  edata.io.r.req.valid := io.pc.valid
  meta.io.r.req.bits.setIdx  := if1_row
  data.io.r.req.bits.setIdx  := if1_row
  edata.io.r.req.bits.setIdx := if1_row
  

  // Entries read from SRAM
  val if2_metaRead =
    VecInit((0 until BtbWays).map(
      w => VecInit((0 until BtbBanks).map(
        b => meta.io.r.resp.data(w*BtbBanks+b)))))
  val if2_dataRead =
    VecInit((0 until BtbWays).map(
      w => VecInit((0 until BtbBanks).map(
        b => data.io.r.resp.data(w*BtbBanks+b)))))
  val if2_edataRead = edata.io.r.resp.data(0)

  val if2_tag = btbAddr.getTag(if2_pc)

  val if2_totalHits = VecInit((0 until BtbBanks).map( b =>
    VecInit((0 until BtbWays).map( w =>
      // This should correspond to the real mask from last valid cycle!
      if2_metaRead(w)(b).tag === if2_tag && if2_metaRead(w)(b).valid && if2_mask(b)
    ))
  ))
  val if2_bankHits = VecInit(if2_totalHits.map(_.reduce(_||_)))
  val if2_bankHitWays = VecInit(if2_totalHits.map(PriorityEncoder(_)))


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
      w := Mux(valid, if (randomAlloc) {LFSR64()(log2Up(BtbWays)-1,0)} else {chunks.reduce(_^_)}, PriorityEncoder(~valids))
      w
    } else {
      val w = WireInit(0.U)
      w
    }
  }
  val allocWays = VecInit((0 until BtbBanks).map(b =>
    allocWay(VecInit(if2_metaRead.map(w => w(b).valid)).asUInt,
             VecInit(if2_metaRead.map(w => w(b).tag)).asUInt,
             if2_tag)))

  val writeWay = VecInit((0 until BtbBanks).map(
    b => Mux(if2_bankHits(b), if2_bankHitWays(b), allocWays(b))
  ))



  for (b <- 0 until BtbBanks) {
    val meta_entry = if2_metaRead(if2_bankHitWays(b))(b)
    val data_entry = if2_dataRead(if2_bankHitWays(b))(b)
    // Use real pc to calculate the target
    io.resp.targets(b) := Mux(data_entry.extended, if2_edataRead, Cat(if2_pc(VAddrBits-1, lowerBitsSize+instOffsetBits), data_entry.lower, 0.U(instOffsetBits.W)))
    io.resp.hits(b)  := if2_bankHits(b)
    io.resp.isBrs(b) := meta_entry.isBr
    io.resp.isRVC(b) := meta_entry.isRVC
    io.meta.writeWay(b) := writeWay(b)
    // io.meta.hitJal(b)   := if2_bankHits(b) && meta_entry.btbType === BTBtype.J
  }

  def pdInfoToBTBtype(pd: PreDecodeInfo) = {
    val t = WireInit(0.U(2.W))
    when (pd.isJalr) { t := BTBtype.I}
    when (pd.isRet)  { t := BTBtype.R}
    when (pd.isJal)  { t := BTBtype.J}
    when (pd.isBr)   { t := BTBtype.B}
    t
  }
  
  val do_update = RegNext(io.update)
  val u = do_update.bits

  val cfi_pc = packetAligned(u.ftqPC) + (u.cfiIndex.bits << instOffsetBits)
  val new_target = u.target
  val new_lower = new_target(lowerBitsSize+instOffsetBits-1, instOffsetBits)
  val update_pc_higher     = cfi_pc(VAddrBits-1, lowerBitsSize+instOffsetBits)
  val update_target_higher = new_target(VAddrBits-1, lowerBitsSize+instOffsetBits)
  val higher_identical = update_pc_higher === update_target_higher
  val new_extended = !higher_identical


  val updateWay = u.metas(u.cfiIndex.bits).btbWriteWay
  val updateBank = u.cfiIndex.bits
  val updateRow = btbAddr.getBankIdx(cfi_pc)
  val updateIsBr = u.br_mask(u.cfiIndex.bits)
  val updateTaken = u.cfiIndex.valid
  // TODO: remove isRVC
  val metaWrite = BtbMetaEntry(btbAddr.getTag(cfi_pc), updateIsBr, u.cfiIsRVC)
  val dataWrite = BtbDataEntry(new_lower, new_extended)
  

  val updateValid = do_update.valid && updateTaken
  // Update btb
  require(isPow2(BtbBanks))
  // this is one hot, since each fetch bundle has at most 1 taken instruction
  val updateWayMask = UIntToOH(Cat(updateWay, updateBank))
  meta.io.w.apply(updateValid, metaWrite, updateRow, updateWayMask)
  data.io.w.apply(updateValid, dataWrite, updateRow, updateWayMask)
  edata.io.w.apply(updateValid && new_extended, u.target, updateRow, "b1".U)

  if (BPUDebug && debug) {
    val debug_verbose = true
    val validLatch = RegNext(io.pc.valid)
    XSDebug(io.pc.valid, "read: pc=0x%x, mask=%b\n", if1_packetAlignedPC, if1_mask)
    XSDebug(validLatch, "read_resp: pc=0x%x, readIdx=%d-------------------------------\n",
      if2_pc, btbAddr.getIdx(if2_pc))
    if (debug_verbose) {
      for (i <- 0 until BtbBanks){
        for (j <- 0 until BtbWays) {
          XSDebug(validLatch, "read_resp[w=%d][b=%d][r=%d] is valid(%d) mask(%d), tag=0x%x, lower=0x%x, isBr=%d, isExtend=%d, isRVC=%d\n",
          j.U, i.U, if2_row, if2_metaRead(j)(i).valid, if2_mask(i), if2_metaRead(j)(i).tag, if2_dataRead(j)(i).lower, if2_metaRead(j)(i).isBr, if2_dataRead(j)(i).extended, if2_metaRead(j)(i).isRVC)
        }
      }
    }

    for (i <- 0 until BtbBanks) {
      XSDebug(validLatch && if2_bankHits(i), "resp(%d): bank(%d) hits, tgt=%x, isRVC=%d, isBr=%d\n",
        i.U, i.U, io.resp.targets(i), io.resp.isRVC(i), io.resp.isBrs(i))
    }
    XSDebug(updateValid, "update_req: cycle=%d, pc=0x%x, target=0x%x, misPred=%d, lower=%x, extended=%d, way=%d, bank=%d, row=0x%x\n",
      u.metas(u.cfiIndex.bits).debug_btb_cycle, cfi_pc, new_target, u.mispred(u.cfiIndex.bits), new_lower, new_extended, updateWay, updateBank, updateRow)
    for (i <- 0 until BtbBanks) {
      // Conflict when not hit and allocating a valid entry
      val conflict = if2_metaRead(allocWays(i))(i).valid && !if2_bankHits(i)
      XSDebug(conflict, "bank(%d) is trying to allocate a valid way(%d)\n", i.U, allocWays(i))
      // There is another circumstance when a branch is on its way to update while another
      // branch chose the same way to udpate, then after the first branch is wrote in,
      // the second branch will overwrite the first branch
  }

  }
}