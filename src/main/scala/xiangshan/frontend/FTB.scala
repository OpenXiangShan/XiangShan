/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

import scala.math.min
import os.copy


trait FTBParams extends HasXSParameter with HasBPUConst {
  val numEntries = 4096
  val numWays    = 4
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20

  

  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20
}

class FtbSlot(val offsetLen: Int, val subOffsetLen: Int = 0)(implicit p: Parameters) extends XSBundle with FTBParams {
  require(subOffsetLen <= offsetLen)
  val offset  = UInt(log2Ceil(PredictWidth).W)
  val lower   = UInt(offsetLen.W)
  val tarStat = UInt(TAR_STAT_SZ.W)
  val sharing = Bool()
  val valid   = Bool()

  def setLowerStatByTarget(pc: UInt, target: UInt, isShare: Boolean) = {
    def getTargetStatByHigher(pc_higher: UInt, target_higher: UInt) =
      Mux(target_higher > pc_higher, TAR_OVF,
        Mux(target_higher < pc_higher, TAR_UDF, TAR_FIT))
    def getLowerByTarget(target: UInt, offsetLen: Int) = target(offsetLen, 1)
    val offLen = if (isShare) this.subOffsetLen else this.offsetLen
    val pc_higher = pc(VAddrBits-1, offLen+1)
    val target_higher = target(VAddrBits-1, offLen+1)
    val stat = getTargetStatByHigher(pc_higher, target_higher)
    val lower = ZeroExt(getLowerByTarget(target, offLen), this.offsetLen)
    this.lower := lower
    this.tarStat := stat
    this.sharing := isShare.B
  }

  def getTarget(pc: UInt) = {
    def getTarget(offLen: Int)(pc: UInt, lower: UInt, stat: UInt) = {
      val higher = pc(VAddrBits-1, offLen+1)
      val target =
        Cat(
          Mux(stat === TAR_OVF, higher+1.U,
            Mux(stat === TAR_UDF, higher-1.U, higher)),
          lower(offLen-1, 0), 0.U(1.W)
        )
      require(target.getWidth == VAddrBits)
      require(offLen != 0)
      target
    }
    if (subOffsetLen != 0)
      Mux(sharing,
        getTarget(subOffsetLen)(pc, lower, tarStat),
        getTarget(offsetLen)(pc, lower, tarStat)
      )
    else
      getTarget(offsetLen)(pc, lower, tarStat)
  }
  def fromAnotherSlot(that: FtbSlot) = {
    require(
      this.offsetLen > that.offsetLen && that.offsetLen == this.subOffsetLen ||
      this.offsetLen == that.offsetLen
    )
    this.offset := that.offset
    this.tarStat := that.tarStat
    this.sharing := (this.offsetLen > that.offsetLen && that.offsetLen == this.subOffsetLen).B
    this.valid := that.valid
    this.lower := ZeroExt(that.lower, this.offsetLen)
  }
  
}

class FTBEntry(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  
  
  val valid       = Bool()

  val brSlots = Vec(numBrSlot, new FtbSlot(BR_OFFSET_LEN))

  // if shareTailSlot is set, this slot can hold a branch or a jal/jalr
  // else this slot holds only jal/jalr
  val tailSlot = new FtbSlot(JMP_OFFSET_LEN, BR_OFFSET_LEN)

  // Partial Fall-Through Address
  val pftAddr     = UInt((log2Up(PredictWidth)+1).W)
  val carry       = Bool()

  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  // 
  val oversize    = Bool()

  val last_is_rvc = Bool()
  
  val always_taken = Vec(numBr, Bool())

  def getSlotForBr(idx: Int): FtbSlot = {
    require(
      idx < numBr-1 || idx == numBr-1 && !shareTailSlot ||
      idx == numBr-1 && shareTailSlot
    )
    (idx, numBr, shareTailSlot) match {
      case (i, n, true) if i == n-1 => this.tailSlot
      case _ => this.brSlots(idx)
    }
  }
  def allSlotsForBr = {
    (0 until numBr).map(getSlotForBr(_))
  }
  def setByBrTarget(brIdx: Int, pc: UInt, target: UInt) = {
    val slot = getSlotForBr(brIdx)
    slot.setLowerStatByTarget(pc, target, shareTailSlot && brIdx == numBr-1)
  }
  def setByJmpTarget(pc: UInt, target: UInt) = {
    this.tailSlot.setLowerStatByTarget(pc, target, false)
  }

  def getTargetVec(pc: UInt) = {
    VecInit((brSlots :+ tailSlot).map(_.getTarget(pc)))
  }

  def getOffsetVec = VecInit(brSlots.map(_.offset) :+ tailSlot.offset)
  def isJal = !isJalr
  def getFallThrough(pc: UInt) = getFallThroughAddr(pc, carry, pftAddr)
  def hasBr(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset}.reduce(_||_) ||
    (shareTailSlot.B && tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)

  def getBrMaskByOffset(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset } ++
    (if (shareTailSlot) Seq(tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing) else Nil)
    
  def getBrRecordedVec(offset: UInt) = {
    VecInit(
      brSlots.map(s => s.valid && s.offset === offset) ++
      (if (shareTailSlot) Seq(tailSlot.valid && tailSlot.offset === offset && tailSlot.sharing) else Nil)
    )
  }
    
  def brIsSaved(offset: UInt) = getBrRecordedVec(offset).reduce(_||_)

  def onNotHit(pc: UInt) = {
    pftAddr := pc(instOffsetBits + log2Ceil(PredictWidth), instOffsetBits) ^ (1 << log2Ceil(PredictWidth)).U
    carry := pc(instOffsetBits + log2Ceil(PredictWidth)).asBool
    oversize := false.B
  }

  def brValids = {
    VecInit(
      brSlots.map(_.valid) ++
      (if (shareTailSlot) Seq(tailSlot.valid && tailSlot.sharing) else Nil)
    )
  }

  def noEmptySlotForNewBr = {
    VecInit(
      brSlots.map(_.valid) ++
      (if (shareTailSlot) Seq(tailSlot.valid) else Nil)
    ).reduce(_&&_)
  }

  def newBrCanNotInsert(offset: UInt) = {
    val lastSlotForBr = if (shareTailSlot) tailSlot else brSlots.last
    lastSlotForBr.valid && lastSlotForBr.offset < offset
  }

  def jmpValid = {
    tailSlot.valid && (!shareTailSlot.B || !tailSlot.sharing)
  }

  def brOffset = {
    VecInit(
      brSlots.map(_.offset) ++
      (if (shareTailSlot) Seq(tailSlot.offset) else Nil)
    )
  }


  def display(cond: Bool): Unit = {
    XSDebug(cond, p"-----------FTB entry----------- \n")
    XSDebug(cond, p"v=${valid}\n")
    for(i <- 0 until numBr) {
      XSDebug(cond, p"[br$i]: v=${allSlotsForBr(i).valid}, offset=${allSlotsForBr(i).offset}," +
        p"lower=${Hexadecimal(allSlotsForBr(i).lower)}\n")
    }
    XSDebug(cond, p"[tailSlot]: v=${tailSlot.valid}, offset=${tailSlot.offset}," +
      p"lower=${Hexadecimal(tailSlot.lower)}, sharing=${tailSlot.sharing}}\n")
    XSDebug(cond, p"pftAddr=${Hexadecimal(pftAddr)}, carry=$carry\n")
    XSDebug(cond, p"isCall=$isCall, isRet=$isRet, isjalr=$isJalr\n")
    XSDebug(cond, p"oversize=$oversize, last_is_rvc=$last_is_rvc\n")
    XSDebug(cond, p"------------------------------- \n")
  }

}

class FTBEntryWithTag(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  val entry = new FTBEntry
  val tag = UInt(tagSize.W)
  def display(cond: Bool): Unit = {
    entry.display(cond)
    XSDebug(cond, p"tag is ${Hexadecimal(tag)}\n------------------------------- \n")
  }
}

class FTBMeta(implicit p: Parameters) extends XSBundle with FTBParams {
  val writeWay = UInt(numWays.W)
  val hit = Bool()
  val pred_cycle = UInt(64.W) // TODO: Use Option
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool, pred_cycle: UInt)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    e.pred_cycle := pred_cycle
    e
  }
}

class FTB(implicit p: Parameters) extends BasePredictor with FTBParams with BPUUtils {
  override val meta_size = WireInit(0.U.asTypeOf(new FTBMeta)).getWidth

  val ftbAddr = new TableAddr(log2Up(numSets), 1)

  class FTBBank(val numSets: Int, val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val read_resp = Output(new FTBEntry)

      // when ftb hit, read_hits.valid is true, and read_hits.bits is OH of hit way
      // when ftb not hit, read_hits.valid is false, and read_hits is OH of allocWay
      val read_hits = Valid(Vec(numWays, Bool()))

      val update_pc = Input(UInt(VAddrBits.W))
      val update_write_data = Flipped(Valid(new FTBEntryWithTag))
      val update_write_mask = Input(UInt(numWays.W))
    })

    val ftb = Module(new SRAMTemplate(new FTBEntryWithTag, set = numSets, way = numWays, shouldReset = true, holdRead = true, singlePort = true))

    ftb.io.r.req.valid := io.req_pc.valid // io.s0_fire
    ftb.io.r.req.bits.setIdx := ftbAddr.getIdx(io.req_pc.bits) // s0_idx

    io.req_pc.ready := ftb.io.r.req.ready

    val req_tag = RegEnable(ftbAddr.getTag(io.req_pc.bits)(tagSize-1, 0), io.req_pc.valid)

    val read_entries = ftb.io.r.resp.data.map(_.entry)
    val read_tags    = ftb.io.r.resp.data.map(_.tag)

    val total_hits = VecInit((0 until numWays).map(b => read_tags(b) === req_tag && read_entries(b).valid))
    val hit = total_hits.reduce(_||_)
    val hit_way_1h = VecInit(PriorityEncoderOH(total_hits))

    def allocWay(valids: UInt, meta_tags: UInt, req_tag: UInt) = {
      val randomAlloc = false
      if (numWays > 1) {
        val w = Wire(UInt(log2Up(numWays).W))
        val valid = WireInit(valids.andR)
        val tags = Cat(meta_tags, req_tag)
        val l = log2Up(numWays)
        val nChunks = (tags.getWidth + l - 1) / l
        val chunks = (0 until nChunks).map( i =>
          tags(min((i+1)*l, tags.getWidth)-1, i*l)
        )
        w := Mux(valid, if (randomAlloc) {LFSR64()(log2Up(numWays)-1,0)} else {chunks.reduce(_^_)}, PriorityEncoder(~valids))
        w
      } else {
        val w = WireInit(0.U)
        w
      }
    }

    val allocWriteWay = allocWay(
      VecInit(read_entries.map(_.valid)).asUInt,
      VecInit(read_tags).asUInt,
      req_tag
    )

    io.read_resp := PriorityMux(total_hits, read_entries) // Mux1H
    io.read_hits.valid := hit
    io.read_hits.bits := Mux(hit, hit_way_1h, VecInit(UIntToOH(allocWriteWay).asBools()))

    // Update logic
    val u_valid = io.update_write_data.valid
    val u_data = io.update_write_data.bits
    val u_idx = ftbAddr.getIdx(io.update_pc)
    val u_mask = io.update_write_mask

    ftb.io.w.apply(u_valid, u_data, u_idx, u_mask)

    // print hit entry info
    PriorityMux(total_hits, ftb.io.r.resp.data).display(true.B)
  } // FTBBank

  val ftbBank = Module(new FTBBank(numSets, numWays))

  ftbBank.io.req_pc.valid := io.s0_fire
  ftbBank.io.req_pc.bits := s0_pc

  io.s1_ready := ftbBank.io.req_pc.ready //  && !io.redirect.valid

  val ftb_entry = RegEnable(ftbBank.io.read_resp, io.s1_fire)
  val s1_hit = ftbBank.io.read_hits.valid
  val s2_hit = RegEnable(s1_hit, io.s1_fire)
  val writeWay = ftbBank.io.read_hits.bits

  val fallThruAddr = getFallThroughAddr(s2_pc, ftb_entry.carry, ftb_entry.pftAddr)

  // io.out.bits.resp := RegEnable(io.in.bits.resp_in(0), 0.U.asTypeOf(new BranchPredictionResp), io.s1_fire)
  io.out.resp := io.in.bits.resp_in(0)

  val s1_latch_call_is_rvc   = DontCare // TODO: modify when add RAS

  io.out.resp.s2.preds.hit           := s2_hit
  io.out.resp.s2.pc                  := s2_pc
  io.out.resp.s2.ftb_entry           := ftb_entry
  io.out.resp.s2.preds.fromFtbEntry(ftb_entry, s2_pc)

  io.out.s3_meta := RegEnable(RegEnable(FTBMeta(writeWay.asUInt(), s1_hit, GTimer()).asUInt(), io.s1_fire), io.s2_fire)

  when(!s2_hit) {
    io.out.resp.s2.ftb_entry.onNotHit(s2_pc)
  }

  // always taken logic
  when (s2_hit) {
    for (i <- 0 until numBr) {
      when (ftb_entry.always_taken(i)) {
        io.out.resp.s2.preds.br_taken_mask(i) := true.B
      }
    }
  }

  // Update logic
  val has_update = RegInit(VecInit(Seq.fill(64)(0.U(VAddrBits.W))))
  val has_update_ptr = RegInit(0.U(log2Up(64)))

  val update = RegNext(io.update.bits)

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  val u_valid = RegNext(io.update.valid && !io.update.bits.old_entry)
  val u_way_mask = u_meta.writeWay

  val ftb_write = Wire(new FTBEntryWithTag)
  ftb_write.entry := update.ftb_entry
  ftb_write.tag   := ftbAddr.getTag(update.pc)(tagSize-1, 0)

  ftbBank.io.update_write_data.valid := u_valid
  ftbBank.io.update_write_data.bits := ftb_write
  ftbBank.io.update_pc := update.pc
  ftbBank.io.update_write_mask := u_way_mask

  val r_updated = (0 until 64).map(i => has_update(i) === s1_pc).reduce(_||_)
  val u_updated = (0 until 64).map(i => has_update(i) === update.pc).reduce(_||_)

  when(u_valid) {
    when(!u_updated) { has_update(has_update_ptr) := update.pc }

    has_update_ptr := has_update_ptr + !u_updated
  }

  XSDebug("req_v=%b, req_pc=%x, ready=%b (resp at next cycle)\n", io.s0_fire, s0_pc, ftbBank.io.req_pc.ready)
  XSDebug("s2_hit=%b, hit_way=%b\n", s2_hit, writeWay.asUInt)
  XSDebug("s2_br_taken_mask=%b, s2_real_taken_mask=%b\n",
    io.in.bits.resp_in(0).s2.preds.br_taken_mask.asUInt, io.out.resp.s2.real_slot_taken_mask().asUInt)
  XSDebug("s2_target=%x\n", io.out.resp.s2.target)

  XSDebug(u_valid, "Update from ftq\n")
  XSDebug(u_valid, "update_pc=%x, tag=%x, update_write_way=%b, pred_cycle=%d\n",
    update.pc, ftbAddr.getTag(update.pc), u_way_mask, u_meta.pred_cycle)





  XSPerfAccumulate("ftb_first_miss", u_valid && !u_updated && !update.preds.hit)
  XSPerfAccumulate("ftb_updated_miss", u_valid && u_updated && !update.preds.hit)

  XSPerfAccumulate("ftb_read_first_miss", RegNext(io.s0_fire) && !s1_hit && !r_updated)
  XSPerfAccumulate("ftb_read_updated_miss", RegNext(io.s0_fire) && !s1_hit && r_updated)

  XSPerfAccumulate("ftb_read_hits", RegNext(io.s0_fire) && s1_hit)
  XSPerfAccumulate("ftb_read_misses", RegNext(io.s0_fire) && !s1_hit)

  XSPerfAccumulate("ftb_commit_hits", io.update.valid && io.update.bits.preds.hit)
  XSPerfAccumulate("ftb_commit_misses", io.update.valid && !io.update.bits.preds.hit)

  XSPerfAccumulate("ftb_update_req", io.update.valid)
  XSPerfAccumulate("ftb_update_ignored", io.update.valid && io.update.bits.old_entry)
  XSPerfAccumulate("ftb_updated", u_valid)
}
