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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

import scala.math.min
import scala.{Tuple2 => &}
import os.copy


trait FTBParams extends HasXSParameter with HasBPUConst {
  val numEntries = FtbSize
  val numWays    = FtbWays
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20



  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20

  def FTBCLOSE_THRESHOLD_SZ = log2Ceil(500)
  def FTBCLOSE_THRESHOLD = 500.U(FTBCLOSE_THRESHOLD_SZ.W) //can be modified
}

class FtbSlot_FtqMem(implicit p: Parameters) extends XSBundle with FTBParams {
  val offset  = UInt(log2Ceil(PredictWidth).W)
  val sharing = Bool()
  val valid   = Bool()
}

class FtbSlot(val offsetLen: Int, val subOffsetLen: Option[Int] = None)(implicit p: Parameters) extends FtbSlot_FtqMem with FTBParams {
  if (subOffsetLen.isDefined) {
    require(subOffsetLen.get <= offsetLen)
  }
  val lower   = UInt(offsetLen.W)
  val tarStat = UInt(TAR_STAT_SZ.W)

  def setLowerStatByTarget(pc: UInt, target: UInt, isShare: Boolean) = {
    def getTargetStatByHigher(pc_higher: UInt, target_higher: UInt) =
      Mux(target_higher > pc_higher, TAR_OVF,
        Mux(target_higher < pc_higher, TAR_UDF, TAR_FIT))
    def getLowerByTarget(target: UInt, offsetLen: Int) = target(offsetLen, 1)
    val offLen = if (isShare) this.subOffsetLen.get else this.offsetLen
    val pc_higher = pc(VAddrBits-1, offLen+1)
    val target_higher = target(VAddrBits-1, offLen+1)
    val stat = getTargetStatByHigher(pc_higher, target_higher)
    val lower = ZeroExt(getLowerByTarget(target, offLen), this.offsetLen)
    this.lower := lower
    this.tarStat := stat
    this.sharing := isShare.B
  }

  def getTarget(pc: UInt, last_stage: Option[Tuple2[UInt, Bool]] = None) = {
    def getTarget(offLen: Int)(pc: UInt, lower: UInt, stat: UInt,
      last_stage: Option[Tuple2[UInt, Bool]] = None) = {
      val h                = pc(VAddrBits - 1, offLen + 1)
      val higher           = Wire(UInt((VAddrBits - offLen - 1).W))
      val higher_plus_one  = Wire(UInt((VAddrBits - offLen - 1).W))
      val higher_minus_one = Wire(UInt((VAddrBits-offLen-1).W))

      // Switch between previous stage pc and current stage pc
      // Give flexibility for timing
      if (last_stage.isDefined) {
        val last_stage_pc = last_stage.get._1
        val last_stage_pc_h = last_stage_pc(VAddrBits-1, offLen+1)
        val stage_en = last_stage.get._2
        higher := RegEnable(last_stage_pc_h, stage_en)
        higher_plus_one := RegEnable(last_stage_pc_h+1.U, stage_en)
        higher_minus_one := RegEnable(last_stage_pc_h-1.U, stage_en)
      } else {
        higher := h
        higher_plus_one := h + 1.U
        higher_minus_one := h - 1.U
      }
      val target =
        Cat(
          Mux1H(Seq(
            (stat === TAR_OVF, higher_plus_one),
            (stat === TAR_UDF, higher_minus_one),
            (stat === TAR_FIT, higher),
          )),
          lower(offLen-1, 0), 0.U(1.W)
        )
      require(target.getWidth == VAddrBits)
      require(offLen != 0)
      target
    }
    if (subOffsetLen.isDefined)
      Mux(sharing,
        getTarget(subOffsetLen.get)(pc, lower, tarStat, last_stage),
        getTarget(offsetLen)(pc, lower, tarStat, last_stage)
      )
    else
      getTarget(offsetLen)(pc, lower, tarStat, last_stage)
  }
  def fromAnotherSlot(that: FtbSlot) = {
    require(
      this.offsetLen > that.offsetLen && this.subOffsetLen.map(_ == that.offsetLen).getOrElse(true) ||
      this.offsetLen == that.offsetLen
    )
    this.offset := that.offset
    this.tarStat := that.tarStat
    this.sharing := (this.offsetLen > that.offsetLen && that.offsetLen == this.subOffsetLen.get).B
    this.valid := that.valid
    this.lower := ZeroExt(that.lower, this.offsetLen)
  }

  def slotConsistent(that: FtbSlot) = {
    VecInit(
      this.offset  === that.offset,
      this.lower   === that.lower,
      this.tarStat === that.tarStat,
      this.sharing === that.sharing,
      this.valid   === that.valid
    ).reduce(_&&_)
  }

}


class FTBEntry_part(implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  def isJal = !isJalr
}

class FTBEntry_FtqMem(implicit p: Parameters) extends FTBEntry_part with FTBParams with BPUUtils {

  val brSlots = Vec(numBrSlot, new FtbSlot_FtqMem)
  val tailSlot = new FtbSlot_FtqMem

  def jmpValid = {
    tailSlot.valid && !tailSlot.sharing
  }

  def getBrRecordedVec(offset: UInt) = {
    VecInit(
      brSlots.map(s => s.valid && s.offset === offset) :+
      (tailSlot.valid && tailSlot.offset === offset && tailSlot.sharing)
    )
  }

  def brIsSaved(offset: UInt) = getBrRecordedVec(offset).reduce(_||_)

  def getBrMaskByOffset(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset } :+
    (tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)
  
  def newBrCanNotInsert(offset: UInt) = {
    val lastSlotForBr = tailSlot
    lastSlotForBr.valid && lastSlotForBr.offset < offset
  }

}

class FTBEntry(implicit p: Parameters) extends FTBEntry_part with FTBParams with BPUUtils {


  val valid       = Bool()

  val brSlots = Vec(numBrSlot, new FtbSlot(BR_OFFSET_LEN))

  val tailSlot = new FtbSlot(JMP_OFFSET_LEN, Some(BR_OFFSET_LEN))

  // Partial Fall-Through Address
  val pftAddr     = UInt(log2Up(PredictWidth).W)
  val carry       = Bool()

  val last_may_be_rvi_call = Bool()

  val always_taken = Vec(numBr, Bool())

  def getSlotForBr(idx: Int): FtbSlot = {
    require(idx <= numBr-1)
    (idx, numBr) match {
      case (i, n) if i == n-1 => this.tailSlot
      case _ => this.brSlots(idx)
    }
  }
  def allSlotsForBr = {
    (0 until numBr).map(getSlotForBr(_))
  }
  def setByBrTarget(brIdx: Int, pc: UInt, target: UInt) = {
    val slot = getSlotForBr(brIdx)
    slot.setLowerStatByTarget(pc, target, brIdx == numBr-1)
  }
  def setByJmpTarget(pc: UInt, target: UInt) = {
    this.tailSlot.setLowerStatByTarget(pc, target, false)
  }

  def getTargetVec(pc: UInt, last_stage: Option[Tuple2[UInt, Bool]] = None) = {
    /*
    Previous design: Use the getTarget function of FTBSlot to calculate three sets of targets separately;
    During this process, nine sets of registers will be generated to register the values of the higher plus one minus one
    Current design: Reuse the duplicate parts of the original nine sets of registers,
    calculate the common high bits last_stage_pc_higher of brtarget and jmptarget,
    and the high bits last_stage_pc_middle that need to be added and subtracted from each other,
    and then concatenate them according to the carry situation to obtain brtarget and jmptarget
    */
    val h_br                = pc(VAddrBits - 1,  BR_OFFSET_LEN + 1)
    val higher_br           = Wire(UInt((VAddrBits - BR_OFFSET_LEN - 1).W))
    val higher_plus_one_br  = Wire(UInt((VAddrBits - BR_OFFSET_LEN - 1).W))
    val higher_minus_one_br = Wire(UInt((VAddrBits - BR_OFFSET_LEN - 1).W))
    val h_tail                = pc(VAddrBits - 1,  JMP_OFFSET_LEN + 1)
    val higher_tail           = Wire(UInt((VAddrBits - JMP_OFFSET_LEN - 1).W))
    val higher_plus_one_tail  = Wire(UInt((VAddrBits - JMP_OFFSET_LEN - 1).W))
    val higher_minus_one_tail = Wire(UInt((VAddrBits - JMP_OFFSET_LEN - 1).W))
    if (last_stage.isDefined) {
      val last_stage_pc = last_stage.get._1
      val stage_en = last_stage.get._2
      val last_stage_pc_higher = RegEnable(last_stage_pc(VAddrBits - 1, JMP_OFFSET_LEN + 1), stage_en)
      val last_stage_pc_middle = RegEnable(last_stage_pc(JMP_OFFSET_LEN, BR_OFFSET_LEN + 1), stage_en)
      val last_stage_pc_higher_plus_one  = RegEnable(last_stage_pc(VAddrBits - 1, JMP_OFFSET_LEN + 1) + 1.U, stage_en)
      val last_stage_pc_higher_minus_one = RegEnable(last_stage_pc(VAddrBits - 1, JMP_OFFSET_LEN + 1) - 1.U, stage_en)
      val last_stage_pc_middle_plus_one  = RegEnable(Cat(0.U(1.W), last_stage_pc(JMP_OFFSET_LEN, BR_OFFSET_LEN + 1)) + 1.U, stage_en)
      val last_stage_pc_middle_minus_one = RegEnable(Cat(0.U(1.W), last_stage_pc(JMP_OFFSET_LEN, BR_OFFSET_LEN + 1)) - 1.U, stage_en)

      higher_br := Cat(last_stage_pc_higher, last_stage_pc_middle)
      higher_plus_one_br := Mux(
          last_stage_pc_middle_plus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN),
          Cat(last_stage_pc_higher_plus_one, last_stage_pc_middle_plus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN-1, 0)),
          Cat(last_stage_pc_higher, last_stage_pc_middle_plus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN-1, 0)))
      higher_minus_one_br := Mux(
          last_stage_pc_middle_minus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN),
          Cat(last_stage_pc_higher_minus_one, last_stage_pc_middle_minus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN-1, 0)),
          Cat(last_stage_pc_higher, last_stage_pc_middle_minus_one(JMP_OFFSET_LEN - BR_OFFSET_LEN-1, 0)))

      higher_tail := last_stage_pc_higher
      higher_plus_one_tail := last_stage_pc_higher_plus_one
      higher_minus_one_tail := last_stage_pc_higher_minus_one
    }else{
      higher_br := h_br
      higher_plus_one_br := h_br + 1.U
      higher_minus_one_br := h_br - 1.U
      higher_tail := h_tail
      higher_plus_one_tail := h_tail + 1.U
      higher_minus_one_tail := h_tail - 1.U
    }
    val br_slots_targets = VecInit(brSlots.map(s =>
      Cat(
          Mux1H(Seq(
            (s.tarStat === TAR_OVF, higher_plus_one_br),
            (s.tarStat === TAR_UDF, higher_minus_one_br),
            (s.tarStat === TAR_FIT, higher_br),
          )),
          s.lower(s.offsetLen-1, 0), 0.U(1.W)
        )
    ))
    val tail_target = Wire(UInt(VAddrBits.W))
    if(tailSlot.subOffsetLen.isDefined){
      tail_target := Mux(tailSlot.sharing,
        Cat(
          Mux1H(Seq(
            (tailSlot.tarStat === TAR_OVF, higher_plus_one_br),
            (tailSlot.tarStat === TAR_UDF, higher_minus_one_br),
            (tailSlot.tarStat === TAR_FIT, higher_br),
          )),
          tailSlot.lower(tailSlot.subOffsetLen.get-1, 0), 0.U(1.W)
        ),
        Cat(
          Mux1H(Seq(
            (tailSlot.tarStat === TAR_OVF, higher_plus_one_tail),
            (tailSlot.tarStat === TAR_UDF, higher_minus_one_tail),
            (tailSlot.tarStat === TAR_FIT, higher_tail),
          )),
          tailSlot.lower(tailSlot.offsetLen-1, 0), 0.U(1.W)
        )
      )
    }else{
      tail_target := Cat(
          Mux1H(Seq(
            (tailSlot.tarStat === TAR_OVF, higher_plus_one_tail),
            (tailSlot.tarStat === TAR_UDF, higher_minus_one_tail),
            (tailSlot.tarStat === TAR_FIT, higher_tail),
          )),
          tailSlot.lower(tailSlot.offsetLen-1, 0), 0.U(1.W)
        )
    }

    br_slots_targets.map(t => require(t.getWidth == VAddrBits))
    require(tail_target.getWidth == VAddrBits)
    val targets = VecInit(br_slots_targets :+ tail_target)
    targets
  }

  def getOffsetVec = VecInit(brSlots.map(_.offset) :+ tailSlot.offset)
  def getFallThrough(pc: UInt, last_stage_entry: Option[Tuple2[FTBEntry, Bool]] = None) = {
    if (last_stage_entry.isDefined) {
      var stashed_carry = RegEnable(last_stage_entry.get._1.carry, last_stage_entry.get._2)
      getFallThroughAddr(pc, stashed_carry, pftAddr)
    } else {
      getFallThroughAddr(pc, carry, pftAddr)
    }
  }

  def hasBr(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset}.reduce(_||_) ||
    (tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)

  def getBrMaskByOffset(offset: UInt) =
    brSlots.map{ s => s.valid && s.offset <= offset } :+
    (tailSlot.valid && tailSlot.offset <= offset && tailSlot.sharing)

  def getBrRecordedVec(offset: UInt) = {
    VecInit(
      brSlots.map(s => s.valid && s.offset === offset) :+
      (tailSlot.valid && tailSlot.offset === offset && tailSlot.sharing)
    )
  }

  def brIsSaved(offset: UInt) = getBrRecordedVec(offset).reduce(_||_)

  def brValids = {
    VecInit(
      brSlots.map(_.valid) :+ (tailSlot.valid && tailSlot.sharing)
    )
  }

  def noEmptySlotForNewBr = {
    VecInit(brSlots.map(_.valid) :+ tailSlot.valid).reduce(_&&_)
  }

  def newBrCanNotInsert(offset: UInt) = {
    val lastSlotForBr = tailSlot
    lastSlotForBr.valid && lastSlotForBr.offset < offset
  }

  def jmpValid = {
    tailSlot.valid && !tailSlot.sharing
  }

  def brOffset = {
    VecInit(brSlots.map(_.offset) :+ tailSlot.offset)
  }

  def entryConsistent(that: FTBEntry) = {
    val validDiff     = this.valid === that.valid
    val brSlotsDiffSeq  : IndexedSeq[Bool] =
      this.brSlots.zip(that.brSlots).map{
        case(x, y) => x.slotConsistent(y)
      }
    val tailSlotDiff  = this.tailSlot.slotConsistent(that.tailSlot)
    val pftAddrDiff   = this.pftAddr === that.pftAddr
    val carryDiff     = this.carry   === that.carry
    val isCallDiff    = this.isCall  === that.isCall
    val isRetDiff     = this.isRet   === that.isRet
    val isJalrDiff    = this.isJalr  === that.isJalr
    val lastMayBeRviCallDiff = this.last_may_be_rvi_call === that.last_may_be_rvi_call
    val alwaysTakenDiff : IndexedSeq[Bool] =
      this.always_taken.zip(that.always_taken).map{
        case(x, y) => x === y
      }
    VecInit(
      validDiff,
      brSlotsDiffSeq.reduce(_&&_),
      tailSlotDiff,
      pftAddrDiff,
      carryDiff,
      isCallDiff,
      isRetDiff,
      isJalrDiff,
      lastMayBeRviCallDiff,
      alwaysTakenDiff.reduce(_&&_)
    ).reduce(_&&_)
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
    XSDebug(cond, p"last_may_be_rvi_call=$last_may_be_rvi_call\n")
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
  val writeWay = UInt(log2Ceil(numWays).W)
  val hit = Bool()
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool, pred_cycle: UInt)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    e.pred_cycle.map(_ := pred_cycle)
    e
  }
}

// class UpdateQueueEntry(implicit p: Parameters) extends XSBundle with FTBParams {
//   val pc = UInt(VAddrBits.W)
//   val ftb_entry = new FTBEntry
//   val hit = Bool()
//   val hit_way = UInt(log2Ceil(numWays).W)
// }
//
// object UpdateQueueEntry {
//   def apply(pc: UInt, fe: FTBEntry, hit: Bool, hit_way: UInt)(implicit p: Parameters): UpdateQueueEntry = {
//     val e = Wire(new UpdateQueueEntry)
//     e.pc := pc
//     e.ftb_entry := fe
//     e.hit := hit
//     e.hit_way := hit_way
//     e
//   }
// }

class FTB(implicit p: Parameters) extends BasePredictor with FTBParams with BPUUtils
  with HasCircularQueuePtrHelper with HasPerfEvents {
  override val meta_size = WireInit(0.U.asTypeOf(new FTBMeta)).getWidth

  val ftbAddr = new TableAddr(log2Up(numSets), 1)

  class FTBBank(val numSets: Int, val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val s1_fire = Input(Bool())

      // when ftb hit, read_hits.valid is true, and read_hits.bits is OH of hit way
      // when ftb not hit, read_hits.valid is false, and read_hits is OH of allocWay
      // val read_hits = Valid(Vec(numWays, Bool()))
      val req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val read_resp = Output(new FTBEntry)
      val read_hits = Valid(UInt(log2Ceil(numWays).W))

      val read_multi_entry = Output(new FTBEntry)
      val read_multi_hits = Valid(UInt(log2Ceil(numWays).W))

      val u_req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val update_hits = Valid(UInt(log2Ceil(numWays).W))
      val update_access = Input(Bool())

      val update_pc = Input(UInt(VAddrBits.W))
      val update_write_data = Flipped(Valid(new FTBEntryWithTag))
      val update_write_way = Input(UInt(log2Ceil(numWays).W))
      val update_write_alloc = Input(Bool())
    })

    // Extract holdRead logic to fix bug that update read override predict read result
    val ftb = Module(new SRAMTemplate(new FTBEntryWithTag, set = numSets, way = numWays, shouldReset = true, holdRead = false, singlePort = true))
    val ftb_r_entries = ftb.io.r.resp.data.map(_.entry)

    val pred_rdata   = HoldUnless(ftb.io.r.resp.data, RegNext(io.req_pc.valid && !io.update_access))
    ftb.io.r.req.valid := io.req_pc.valid || io.u_req_pc.valid // io.s0_fire
    ftb.io.r.req.bits.setIdx := Mux(io.u_req_pc.valid, ftbAddr.getIdx(io.u_req_pc.bits), ftbAddr.getIdx(io.req_pc.bits)) // s0_idx

    assert(!(io.req_pc.valid && io.u_req_pc.valid))

    io.req_pc.ready := ftb.io.r.req.ready
    io.u_req_pc.ready := ftb.io.r.req.ready

    val req_tag = RegEnable(ftbAddr.getTag(io.req_pc.bits)(tagSize-1, 0), io.req_pc.valid)
    val req_idx = RegEnable(ftbAddr.getIdx(io.req_pc.bits), io.req_pc.valid)

    val u_req_tag = RegEnable(ftbAddr.getTag(io.u_req_pc.bits)(tagSize-1, 0), io.u_req_pc.valid)

    val read_entries = pred_rdata.map(_.entry)
    val read_tags    = pred_rdata.map(_.tag)

    val total_hits = VecInit((0 until numWays).map(b => read_tags(b) === req_tag && read_entries(b).valid && io.s1_fire))
    val hit = total_hits.reduce(_||_)
    // val hit_way_1h = VecInit(PriorityEncoderOH(total_hits))
    val hit_way = OHToUInt(total_hits)

    //There may be two hits in the four paths of the ftbBank, and the OHToUInt will fail.
    //If there is a redirect in s2 at this time, the wrong FTBEntry will be used to calculate the target,
    //resulting in an address error and affecting performance.
    //The solution is to select a hit entry during multi hit as the entry for s2.
    //Considering timing, use this entry in s3 and trigger s3-redirect.
    val total_hits_reg = RegEnable(total_hits, io.s1_fire)
    val read_entries_reg = read_entries.map(w => RegEnable(w, io.s1_fire))

    val multi_hit = VecInit((0 until numWays).map{
      i => (0 until numWays).map(j => {
        if(i < j) total_hits_reg(i) && total_hits_reg(j)
        else false.B
      }).reduce(_||_)
    }).reduce(_||_)
    val multi_way = PriorityMux(Seq.tabulate(numWays)(i => ((total_hits_reg(i)) -> i.asUInt(log2Ceil(numWays).W))))
    val multi_hit_selectEntry = PriorityMux(Seq.tabulate(numWays)(i => ((total_hits_reg(i)) -> read_entries_reg(i))))

    //Check if the entry read by ftbBank is legal.
    for (n <- 0 to numWays -1 ) {
      val req_pc_reg = RegEnable(io.req_pc.bits, io.req_pc.valid)
      val ftb_entry_fallThrough = read_entries(n).getFallThrough(req_pc_reg)
      when(read_entries(n).valid && total_hits(n) && io.s1_fire){
        assert(req_pc_reg + (2*PredictWidth).U >= ftb_entry_fallThrough, s"FTB sram entry in way${n} fallThrough address error!")
      }
    }

    val u_total_hits = VecInit((0 until numWays).map(b =>
        ftb.io.r.resp.data(b).tag === u_req_tag && ftb.io.r.resp.data(b).entry.valid && RegNext(io.update_access)))
    val u_hit = u_total_hits.reduce(_||_)
    // val hit_way_1h = VecInit(PriorityEncoderOH(total_hits))
    val u_hit_way = OHToUInt(u_total_hits)

    // assert(PopCount(total_hits) === 1.U || PopCount(total_hits) === 0.U)
    // assert(PopCount(u_total_hits) === 1.U || PopCount(u_total_hits) === 0.U)
    for (n <- 1 to numWays) {
      XSPerfAccumulate(f"ftb_pred_${n}_way_hit", PopCount(total_hits) === n.U)
      XSPerfAccumulate(f"ftb_update_${n}_way_hit", PopCount(u_total_hits) === n.U)
    }

    val replacer = ReplacementPolicy.fromString(Some("setplru"), numWays, numSets)
    // val allocWriteWay = replacer.way(req_idx)

    val touch_set = Seq.fill(1)(Wire(UInt(log2Ceil(numSets).W)))
    val touch_way = Seq.fill(1)(Wire(Valid(UInt(log2Ceil(numWays).W))))

    val write_set = Wire(UInt(log2Ceil(numSets).W))
    val write_way = Wire(Valid(UInt(log2Ceil(numWays).W)))

    val read_set = Wire(UInt(log2Ceil(numSets).W))
    val read_way = Wire(Valid(UInt(log2Ceil(numWays).W)))

    read_set := req_idx
    read_way.valid := hit
    read_way.bits  := hit_way

    // Read replacer access is postponed for 1 cycle
    // this helps timing
    touch_set(0) := Mux(write_way.valid, write_set, RegNext(read_set))
    touch_way(0).valid := write_way.valid || RegNext(read_way.valid)
    touch_way(0).bits := Mux(write_way.valid, write_way.bits, RegNext(read_way.bits))

    replacer.access(touch_set, touch_way)

    // Select the update allocate way
    // Selection logic:
    //    1. if any entries within the same index is not valid, select it
    //    2. if all entries is valid, use replacer
    def allocWay(valids: UInt, idx: UInt): UInt = {
      if (numWays > 1) {
        val w = Wire(UInt(log2Up(numWays).W))
        val valid = WireInit(valids.andR)
        w := Mux(valid, replacer.way(idx), PriorityEncoder(~valids))
        w
      } else {
        val w = WireInit(0.U(log2Up(numWays).W))
        w
      }
    }

    io.read_resp := Mux1H(total_hits, read_entries) // Mux1H
    io.read_hits.valid := hit
    io.read_hits.bits := hit_way

    io.read_multi_entry := multi_hit_selectEntry
    io.read_multi_hits.valid := multi_hit
    io.read_multi_hits.bits := multi_way

    io.update_hits.valid := u_hit
    io.update_hits.bits := u_hit_way

    // Update logic
    val u_valid = io.update_write_data.valid
    val u_data = io.update_write_data.bits
    val u_idx = ftbAddr.getIdx(io.update_pc)
    val allocWriteWay = allocWay(RegNext(VecInit(ftb_r_entries.map(_.valid))).asUInt, u_idx)
    val u_way = Mux(io.update_write_alloc, allocWriteWay, io.update_write_way)
    val u_mask = UIntToOH(u_way)

    for (i <- 0 until numWays) {
      XSPerfAccumulate(f"ftb_replace_way$i", u_valid && io.update_write_alloc && u_way === i.U)
      XSPerfAccumulate(f"ftb_replace_way${i}_has_empty", u_valid && io.update_write_alloc && !ftb_r_entries.map(_.valid).reduce(_&&_) && u_way === i.U)
      XSPerfAccumulate(f"ftb_hit_way$i", hit && !io.update_access && hit_way === i.U)
    }

    ftb.io.w.apply(u_valid, u_data, u_idx, u_mask)

    // for replacer
    write_set := u_idx
    write_way.valid := u_valid
    write_way.bits := Mux(io.update_write_alloc, allocWriteWay, io.update_write_way)

    // print hit entry info
    Mux1H(total_hits, ftb.io.r.resp.data).display(true.B)
  } // FTBBank

  //FTB switch register & temporary storage of fauftb prediction results
  val s0_close_ftb_req = RegInit(false.B)
  val s1_close_ftb_req = RegEnable(s0_close_ftb_req, false.B, io.s0_fire(0))
  val s2_close_ftb_req = RegEnable(s1_close_ftb_req, false.B, io.s1_fire(0))
  val s2_fauftb_ftb_entry_dup = io.s1_fire.map(f => RegEnable(io.fauftb_entry_in, f))
  val s2_fauftb_ftb_entry_hit_dup = io.s1_fire.map(f => RegEnable(io.fauftb_entry_hit_in, f))

  val ftbBank = Module(new FTBBank(numSets, numWays))

  //for close ftb read_req
  ftbBank.io.req_pc.valid := io.s0_fire(0) && !s0_close_ftb_req
  ftbBank.io.req_pc.bits := s0_pc_dup(0)

  val s2_multi_hit = ftbBank.io.read_multi_hits.valid && io.s2_fire(0)
  val s2_multi_hit_way = ftbBank.io.read_multi_hits.bits
  val s2_multi_hit_entry = ftbBank.io.read_multi_entry
  val s2_multi_hit_enable = s2_multi_hit && io.s2_redirect(0)
  XSPerfAccumulate("ftb_s2_multi_hit", s2_multi_hit)
  XSPerfAccumulate("ftb_s2_multi_hit_enable", s2_multi_hit_enable)

  //After closing ftb, the entry output from s2 is the entry of FauFTB cached in s1
  val btb_enable_dup = dup(RegNext(io.ctrl.btb_enable))
  val s1_read_resp = Mux(s1_close_ftb_req, io.fauftb_entry_in, ftbBank.io.read_resp)
  val s2_ftbBank_dup = io.s1_fire.map(f => RegEnable(ftbBank.io.read_resp, f))
  val s2_ftb_entry_dup = dup(0.U.asTypeOf(new FTBEntry))
  for(((s2_fauftb_entry, s2_ftbBank_entry), s2_ftb_entry) <-
    s2_fauftb_ftb_entry_dup zip s2_ftbBank_dup zip s2_ftb_entry_dup){
      s2_ftb_entry := Mux(s2_close_ftb_req, s2_fauftb_entry, s2_ftbBank_entry)
  }
  val s3_ftb_entry_dup = io.s2_fire.zip(s2_ftb_entry_dup).map {case (f, e) => RegEnable(Mux(s2_multi_hit_enable, s2_multi_hit_entry, e), f)}

  //After closing ftb, the hit output from s2 is the hit of FauFTB cached in s1.
  //s1_hit is the ftbBank hit.
  val s1_hit = Mux(s1_close_ftb_req, false.B, ftbBank.io.read_hits.valid && io.ctrl.btb_enable)
  val s2_ftb_hit_dup = io.s1_fire.map(f => RegEnable(s1_hit, 0.B, f))
  val s2_hit_dup = dup(0.U.asTypeOf(Bool()))
  for(((s2_fauftb_hit, s2_ftb_hit), s2_hit) <-
    s2_fauftb_ftb_entry_hit_dup zip s2_ftb_hit_dup zip s2_hit_dup){
      s2_hit := Mux(s2_close_ftb_req, s2_fauftb_hit, s2_ftb_hit)
  }
  val s3_hit_dup = io.s2_fire.zip(s2_hit_dup).map {case (f, h) => RegEnable(Mux(s2_multi_hit_enable, s2_multi_hit, h), 0.B, f)}
  val s3_mult_hit_dup = io.s2_fire.map(f => RegEnable(s2_multi_hit_enable,f))
  val writeWay = Mux(s1_close_ftb_req, 0.U, ftbBank.io.read_hits.bits)
  val s2_ftb_meta = RegEnable(FTBMeta(writeWay.asUInt, s1_hit, GTimer()).asUInt, io.s1_fire(0))
  val s2_multi_hit_meta = FTBMeta(s2_multi_hit_way.asUInt, s2_multi_hit, GTimer()).asUInt

  //Consistent count of entries for fauftb and ftb
  val fauftb_ftb_entry_consistent_counter = RegInit(0.U(FTBCLOSE_THRESHOLD_SZ.W))
  val fauftb_ftb_entry_consistent = s2_fauftb_ftb_entry_dup(0).entryConsistent(s2_ftbBank_dup(0))

  //if close ftb_req, the counter need keep
  when(io.s2_fire(0) && s2_fauftb_ftb_entry_hit_dup(0) && s2_ftb_hit_dup(0) ){
    fauftb_ftb_entry_consistent_counter := Mux(fauftb_ftb_entry_consistent, fauftb_ftb_entry_consistent_counter + 1.U, 0.U)
  } .elsewhen(io.s2_fire(0) && !s2_fauftb_ftb_entry_hit_dup(0) && s2_ftb_hit_dup(0) ){
    fauftb_ftb_entry_consistent_counter := 0.U
  }

  when((fauftb_ftb_entry_consistent_counter >= FTBCLOSE_THRESHOLD) && io.s0_fire(0)){
    s0_close_ftb_req := true.B
  }

  //Clear counter during false_hit or ifuRedirect
  val ftb_false_hit = WireInit(false.B)
  val needReopen = s0_close_ftb_req && (ftb_false_hit || io.redirectFromIFU)
  ftb_false_hit := io.update.valid && io.update.bits.false_hit
  when(needReopen){
    fauftb_ftb_entry_consistent_counter := 0.U
    s0_close_ftb_req := false.B
  }

  val s2_close_consistent = s2_fauftb_ftb_entry_dup(0).entryConsistent(s2_ftb_entry_dup(0))
  val s2_not_close_consistent = s2_ftbBank_dup(0).entryConsistent(s2_ftb_entry_dup(0))

  when(s2_close_ftb_req && io.s2_fire(0)){
    assert(s2_close_consistent, s"Entry inconsistency after ftb req is closed!")
  }.elsewhen(!s2_close_ftb_req &&  io.s2_fire(0)){
    assert(s2_not_close_consistent, s"Entry inconsistency after ftb req is not closed!")
  }

  val  reopenCounter = !s1_close_ftb_req && s2_close_ftb_req &&  io.s2_fire(0)
  val  falseHitReopenCounter = ftb_false_hit && s1_close_ftb_req
  XSPerfAccumulate("ftb_req_reopen_counter", reopenCounter)
  XSPerfAccumulate("false_hit_reopen_Counter", falseHitReopenCounter)
  XSPerfAccumulate("ifuRedirec_needReopen",s1_close_ftb_req && io.redirectFromIFU)
  XSPerfAccumulate("this_cycle_is_close",s2_close_ftb_req && io.s2_fire(0))
  XSPerfAccumulate("this_cycle_is_open",!s2_close_ftb_req && io.s2_fire(0))

  // io.out.bits.resp := RegEnable(io.in.bits.resp_in(0), 0.U.asTypeOf(new BranchPredictionResp), io.s1_fire)
  io.out := io.in.bits.resp_in(0)

  io.out.s2.full_pred.map {case fp => fp.multiHit := false.B}

  io.out.s2.full_pred.zip(s2_hit_dup).map {case (fp, h) => fp.hit := h}
  for (full_pred & s2_ftb_entry & s2_pc & s1_pc & s1_fire <-
    io.out.s2.full_pred zip s2_ftb_entry_dup zip s2_pc_dup zip s1_pc_dup zip io.s1_fire) {
      full_pred.fromFtbEntry(s2_ftb_entry,
        s2_pc.getAddr(),
        // Previous stage meta for better timing
        Some(s1_pc, s1_fire),
        Some(s1_read_resp, s1_fire)
      )
  }

  io.out.s3.full_pred.zip(s3_hit_dup).map {case (fp, h) => fp.hit := h}
  io.out.s3.full_pred.zip(s3_mult_hit_dup).map {case (fp, m) => fp.multiHit := m}
  for (full_pred & s3_ftb_entry & s3_pc & s2_pc & s2_fire <-
    io.out.s3.full_pred zip s3_ftb_entry_dup zip s3_pc_dup zip s2_pc_dup zip io.s2_fire)
      full_pred.fromFtbEntry(s3_ftb_entry, s3_pc.getAddr(), Some((s2_pc.getAddr(), s2_fire)))

  io.out.last_stage_ftb_entry := s3_ftb_entry_dup(0)
  io.out.last_stage_meta := RegEnable(Mux(s2_multi_hit_enable, s2_multi_hit_meta, s2_ftb_meta), io.s2_fire(0))
  io.out.s1_ftbCloseReq := s1_close_ftb_req
  io.out.s1_uftbHit := io.fauftb_entry_hit_in
  val s1_uftbHasIndirect = io.fauftb_entry_in.jmpValid &&
    io.fauftb_entry_in.isJalr && !io.fauftb_entry_in.isRet // uFTB determines that it's real JALR, RET and JAL are excluded
  io.out.s1_uftbHasIndirect := s1_uftbHasIndirect

  // always taken logic
  for (i <- 0 until numBr) {
    for (out_fp & in_fp & s2_hit & s2_ftb_entry <-
      io.out.s2.full_pred zip io.in.bits.resp_in(0).s2.full_pred zip s2_hit_dup zip s2_ftb_entry_dup)
      out_fp.br_taken_mask(i) := in_fp.br_taken_mask(i) || s2_hit && s2_ftb_entry.always_taken(i)
    for (out_fp & in_fp & s3_hit & s3_ftb_entry <-
      io.out.s3.full_pred zip io.in.bits.resp_in(0).s3.full_pred zip s3_hit_dup zip s3_ftb_entry_dup)
      out_fp.br_taken_mask(i) := in_fp.br_taken_mask(i) || s3_hit && s3_ftb_entry.always_taken(i)
  }

  // Update logic
  val update = io.update.bits

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  val u_valid = io.update.valid && !io.update.bits.old_entry

  val (_, delay2_pc) = DelayNWithValid(update.pc, u_valid, 2)
  val (_, delay2_entry) = DelayNWithValid(update.ftb_entry, u_valid, 2)


  val update_now = u_valid && u_meta.hit
  val update_need_read = u_valid && !u_meta.hit
  // stall one more cycle because we use a whole cycle to do update read tag hit
  io.s1_ready := ftbBank.io.req_pc.ready && !(update_need_read) && !RegNext(update_need_read)

  ftbBank.io.u_req_pc.valid := update_need_read
  ftbBank.io.u_req_pc.bits := update.pc



  val ftb_write = Wire(new FTBEntryWithTag)
  ftb_write.entry := Mux(update_now, update.ftb_entry, delay2_entry)
  ftb_write.tag   := ftbAddr.getTag(Mux(update_now, update.pc, delay2_pc))(tagSize-1, 0)

  val write_valid = update_now || DelayN(u_valid && !u_meta.hit, 2)
  val write_pc    = Mux(update_now, update.pc, delay2_pc)

  ftbBank.io.update_write_data.valid := write_valid
  ftbBank.io.update_write_data.bits := ftb_write
  ftbBank.io.update_pc          := write_pc
  ftbBank.io.update_write_way   := Mux(update_now, u_meta.writeWay, RegNext(ftbBank.io.update_hits.bits)) // use it one cycle later
  ftbBank.io.update_write_alloc := Mux(update_now, false.B, RegNext(!ftbBank.io.update_hits.valid)) // use it one cycle later
  ftbBank.io.update_access := u_valid && !u_meta.hit
  ftbBank.io.s1_fire := io.s1_fire(0)

  val ftb_write_fallThrough = ftb_write.entry.getFallThrough(write_pc)
  when(write_valid){
    assert(write_pc + (FetchWidth * 4).U >= ftb_write_fallThrough, s"FTB write_entry fallThrough address error!")
  }

  XSDebug("req_v=%b, req_pc=%x, ready=%b (resp at next cycle)\n", io.s0_fire(0), s0_pc_dup(0), ftbBank.io.req_pc.ready)
  XSDebug("s2_hit=%b, hit_way=%b\n", s2_hit_dup(0), writeWay.asUInt)
  XSDebug("s2_br_taken_mask=%b, s2_real_taken_mask=%b\n",
    io.in.bits.resp_in(0).s2.full_pred(0).br_taken_mask.asUInt, io.out.s2.full_pred(0).real_slot_taken_mask().asUInt)
  XSDebug("s2_target=%x\n", io.out.s2.getTarget(0))

  s2_ftb_entry_dup(0).display(true.B)

  XSPerfAccumulate("ftb_read_hits", RegNext(io.s0_fire(0)) && s1_hit)
  XSPerfAccumulate("ftb_read_misses", RegNext(io.s0_fire(0)) && !s1_hit)

  XSPerfAccumulate("ftb_commit_hits", io.update.valid && u_meta.hit)
  XSPerfAccumulate("ftb_commit_misses", io.update.valid && !u_meta.hit)

  XSPerfAccumulate("ftb_update_req", io.update.valid)
  XSPerfAccumulate("ftb_update_ignored", io.update.valid && io.update.bits.old_entry)
  XSPerfAccumulate("ftb_updated", u_valid)

  override val perfEvents = Seq(
    ("ftb_commit_hits            ", io.update.valid  &&  u_meta.hit),
    ("ftb_commit_misses          ", io.update.valid  && !u_meta.hit),
  )
  generatePerfEvent()
}
