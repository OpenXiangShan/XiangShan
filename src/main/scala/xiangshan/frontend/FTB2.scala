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

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName
import freechips.rocketchip.util

import scala.math.min
import chipsalliance.rocketchip.config.Parameters


trait FTB2Params extends HasXSParameter with HasBPUConst {
  val numEntries = 512*8
  val numWays    = 8
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20
  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20
}

class FTB2EntryWithTag(implicit p: Parameters) extends XSBundle with FTB2Params with BPUUtils {
  val entry = new FTBEntry
  val tag = UInt(tagSize.W)
  def display(cond: Bool): Unit = {
    entry.display(cond)
    XSDebug(cond, p"ftb2 tag is ${Hexadecimal(tag)}\n------------------------------- \n")
  }
}

class FTB2Meta(implicit p: Parameters) extends XSBundle with FTB2Params {
  val writeWay = UInt(log2Ceil(numWays).W)
  val hit = Bool()
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

object FTB2Meta {
  def apply(writeWay: UInt, hit: Bool, pred_cycle: UInt)(implicit p: Parameters): FTB2Meta = {
    val e = Wire(new FTB2Meta)
    e.writeWay := writeWay
    e.hit := hit
    e.pred_cycle.map(_ := pred_cycle)
    e
  }
}


class FTB2(implicit p: Parameters) extends BasePredictor with FTB2Params with BPUUtils
  with HasCircularQueuePtrHelper with HasPerfEvents {
  override val meta_size = WireInit(0.U.asTypeOf(new FTB2Meta)).getWidth
  //override val numEntries = 512*8
  val ftb2Addr = new TableAddr(log2Up(numSets), 1)
  val ftb2_entrywithtag = new FTB2EntryWithTag
  val ftb2io = IO(new Bundle {
    val ftb2_hit = Valid(new FTBEntry)
    val ftb2_hit_pc = Output(UInt(VAddrBits.W))
    val ftb1_victim = Flipped(Valid(new FTBEntry))
    val ftb1_victim_pc = Input(UInt(VAddrBits.W))
    val ftb1_s2_hit = Input(Bool())
  })
  class FTB2Bank(val numSets: Int, val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val s1_fire = Input(Bool())
      val s2_fire = Input(Bool())
      val req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val read_resp = Output(new FTBEntry)
      val read_hits = Valid(UInt(log2Ceil(numWays).W))

      val u_req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val update_hits = Valid(UInt(log2Ceil(numWays).W))
      val update_access = Input(Bool())

      val update_pc = Input(UInt(VAddrBits.W))
      val update_write_data = Flipped(Valid(new FTB2EntryWithTag))
      val update_write_way = Input(UInt(log2Ceil(numWays).W))
      val update_write_alloc = Input(Bool())

      val ftb2_hit = Valid(new FTBEntry)
      val ftb2_hit_pc = Output(UInt(VAddrBits.W))
      val ftb1_s2_hit = Input(Bool())
      val ftb1_victim = Flipped(Valid(new FTBEntry))
      val ftb1_victim_pc = Input(UInt(VAddrBits.W))

      //val victim_from_ftb1 = Flipped(Valid(new FTBEntryWithTag))
    })

    val ftb2 = Module(new SRAMTemplate(new FTB2EntryWithTag, set = numSets, way = numWays, shouldReset = true, holdRead = false, singlePort = true))
    val ftb2_r_entries = ftb2.io.r.resp.data.map(_.entry)

    //val pred_rdata  = RegEnable(HoldUnless(ftb2.io.r.resp.data, RegNext(io.req_pc.valid && !io.update_access)),io.s1_fire) //比FTB1多一拍
    //val pred_rdata   = HoldUnless(ftb2.io.r.resp.data, RegNext(io.req_pc.valid && !io.update_access))
    //val pred_all_delay_rdata = HoldUnless(ftb2.io.r.resp.data, RegNext(io.s1_fire))
   // val real_read = RegEnable(io.req_pc.valid && !io.update_access, io.s1_fire)
    //val pred_delay_rdata = Mux(real_read, pred_all_delay_rdata, pred_delay_rdata)
   // val pred_rdata = HoldUnless(ftb2.io.r.resp.data, real_read)
    val pred0_rdata   = HoldUnless(ftb2.io.r.resp.data, RegNext(io.req_pc.valid && !io.update_access))
    val pred_rdata = RegEnable(pred0_rdata, io.s1_fire)
  /*  val pred_delay_rdata = HoldUnless(ftb2.io.r.resp.data, real_read)
    val delay_read_tags = pred_delay_rdata.map(_.tag)
    val delay_read_entries = pred_delay_rdata.map(_.entry)
    val delay_total_hits = WireInit(VecInit((0 until numWays).map(b => delay_read_tags(b) === req_tag && delay_read_entries(b).valid && io.s2_fire)))*/

    //val pred_rdata = RegInit(0.U.asTypeOf(Vec(numWays, ftb2_entrywithtag)))
    //pred_rdata := RegEnable(pred0_rdata, io.s1_fire)
    ftb2.io.r.req.valid := io.req_pc.valid || io.u_req_pc.valid // io.s0_fire
    ftb2.io.r.req.bits.setIdx := Mux(io.u_req_pc.valid, ftb2Addr.getIdx(io.u_req_pc.bits), ftb2Addr.getIdx(io.req_pc.bits)) // s0_idx

    assert(!(io.req_pc.valid && io.u_req_pc.valid))

    io.req_pc.ready := ftb2.io.r.req.ready 
    io.u_req_pc.ready := ftb2.io.r.req.ready


    val req0_tag = RegEnable(ftb2Addr.getTag(io.req_pc.bits)(tagSize - 1, 0), io.req_pc.valid)
    val req0_idx = RegEnable(ftb2Addr.getIdx(io.req_pc.bits), io.req_pc.valid)
    val req_tag = RegEnable(req0_tag, io.s1_fire)
    val req_idx = RegEnable(req0_idx, io.s1_fire)

    val u_req0_tag = RegEnable(ftb2Addr.getTag(io.u_req_pc.bits)(tagSize-1, 0),io.u_req_pc.valid)
    val u_req_tag = RegEnable(u_req0_tag, RegNext(io.u_req_pc.valid))

    val read_entries = pred_rdata.map(_.entry)
    val read_tags = pred_rdata.map(_.tag)
    //val s2_fire = RegNext(io.s1_fire) //相比于s1延迟一拍
    val total_hits = VecInit((0 until numWays).map(b => read_tags(b) === req_tag && read_entries(b).valid && io.s2_fire)) //s1->s2延迟一拍
    val already_ext = WireInit(VecInit((0 until numWays).map(b => read_tags(b) === req_tag && read_entries(b).valid)))
    //alreay_ext := Vec((0 until numWays).map(b => read_tags(b) === req_tag && read_entries(b).valid)) 
    val al_hit = already_ext.reduce(_ || _)
    val al_hit_way = OHToUInt(already_ext)
    val hit = total_hits.reduce(_ || _)
    val hit_way = OHToUInt(total_hits)

    val update0_data = HoldUnless(ftb2.io.r.resp.data, RegNext(!io.req_pc.valid && io.update_access))
    //val update0_data = RegEnable(ftb2.io.r.resp.data, !io.req_pc.valid && io.update_access)
    val update_data = RegEnable(update0_data, RegNext(!io.req_pc.valid && io.update_access))
    //val update_data = HoldUnless(ftb2.io.r.resp.data, RegNext(RegNext(!io.req_pc.valid && io.update_access)))
    val update0_entries = update0_data.map(_.entry)
    val update_entries = update_data.map(_.entry)
    val u_total_hits = VecInit((0 until numWays).map(b =>
      update_data(b).tag === u_req_tag && update_data(b).entry.valid && RegNext(RegNext(io.update_access))))
    assert(PopCount(u_total_hits) <= 1.U)
    val u_hit = u_total_hits.reduce(_||_)
    val u_hit_way = OHToUInt(u_total_hits)
    for (n <- 1 to numWays) {
      XSPerfAccumulate(f"ftb2_pred_${n}_way_hit",PopCount(total_hits) === n.U)
      XSPerfAccumulate(f"ftb2_updata_${n}_way_hit", PopCount(u_total_hits) === n.U)
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
    read_way.bits := hit_way

    touch_set(0) := Mux(write_way.valid, write_set, read_set)

    touch_way(0).valid := write_way.valid || read_way.valid
    touch_way(0).bits := Mux(write_way.valid, write_way.bits, read_way.bits)

    replacer.access(touch_set, touch_way)

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
    assert(PopCount(total_hits) <= 1.U)
    XSDebug(PopCount(total_hits) > 1.U, "ftb2_req_idx = %x\n", req_idx)
    io.read_hits.valid := hit //s2
    io.read_hits.bits := hit_way
    io.ftb2_hit.bits := Mux1H(total_hits, read_entries) //s2
    io.ftb2_hit.valid := hit && !io.ftb1_s2_hit //s2
    val req0_pc = RegEnable(io.req_pc.bits, io.req_pc.valid)
    io.ftb2_hit_pc := RegEnable(req0_pc, io.s1_fire)

    io.update_hits.valid := u_hit
    io.update_hits.bits := u_hit_way


    // Update logic
    val u_w_valid = io.update_write_data.valid
    val u_data = io.update_write_data.bits
    val u_idx = ftb2Addr.getIdx(io.update_pc)
    val allocWriteWay = allocWay(VecInit(update_entries.map(_.valid)).asUInt, u_idx)
    val u_way = Mux(io.update_write_alloc, allocWriteWay, io.update_write_way)
    val u_mask = UIntToOH(u_way)

    val ftb1_vic_data = Wire(new FTB2EntryWithTag)
    ftb1_vic_data.entry := io.ftb1_victim.bits
    ftb1_vic_data.tag := ftb2Addr.getTag(io.ftb1_victim_pc)(tagSize-1, 0)
    val delay_ftb1_victim_pc = RegEnable(io.ftb1_victim_pc, io.ftb1_victim.valid)
    val rewrite = delay_ftb1_victim_pc === io.ftb1_victim_pc
    val ftb1_vic_valid = io.ftb1_victim.valid
    val ftb1_vic_idx = ftb2Addr.getIdx(io.ftb1_victim_pc)
    val ftb1_alloc_way = allocWay(VecInit(update0_entries.map(_.valid)).asUInt, ftb1_vic_idx)
    val ftb1_vic_way = Mux(al_hit, al_hit_way, ftb1_alloc_way)
    val ftb1_vic_mask = UIntToOH(ftb1_vic_way)
    val w_way = Mux(u_w_valid, u_way, ftb1_vic_way)
    //write 如果hazard放弃写入ftbp_hit
    val hazard = u_w_valid && ftb1_vic_valid
    //val w_valid = u_w_valid || (ftb1_vic_valid && !rewrite && !io.u_req_pc.valid && !RegNext(io.u_req_pc.valid) && !RegNext(RegNext(io.u_req_pc.valid)))
    val w_valid = u_w_valid
    assert(!RegNext(w_valid && ftb2.io.r.req.valid))
    val w_data = Mux(u_w_valid, u_data, ftb1_vic_data)
    val w_idx = Mux(u_w_valid, u_idx, ftb1_vic_idx)
    val w_mask = Mux(u_w_valid, u_mask, ftb1_vic_mask)

    XSDebug(w_valid, "ftb2_write_idx = %x\n", w_idx)
    //XSPerfAccumulate("ftb2_write_up", u_w_valid)
    //XSPerfAccumulate("ftb2_write_ftb1_vic", ftb1_vic_valid && !rewrite && !io.u_req_pc.valid && !RegNext(io.u_req_pc.valid) && !RegNext(RegNext(io.u_req_pc.valid)))
    XSDebug(w_valid, "ftb2_write_way = %x\n", w_way)
    for (i <- 0 until numWays) {
     // XSPerfAccumulate(f"ftb2_replace_way$i", u_w_valid && io.update_write_alloc && u_way === i.U)
      XSPerfAccumulate(f"ftb2_replace_way${i}_has_empty", u_w_valid && io.update_write_alloc && !ftb2_r_entries.map(_.valid).reduce(_ && _) && u_way === i.U)
      XSPerfAccumulate(f"ftb2_hit_way$i", hit && !io.update_access && hit_way === i.U)
      XSPerfAccumulate(f"ftb2_write_way$i", w_way === i.U && w_valid)
      XSPerfAccumulate(f"ftb2_write_ftb1_vic_way$i", w_way === i.U && w_valid && !u_w_valid)
      XSPerfAccumulate(f"ftb2_write_up_way$i", w_way === i.U && w_valid && u_w_valid)

    }
    XSPerfAccumulate("ftb2_recv_ftb1_vic", ftb1_vic_valid && !hit && !u_w_valid)

    ftb2.io.w.apply(w_valid, w_data, w_idx, w_mask)

    write_set := w_idx
    write_way.valid := w_valid
    write_way.bits :=  Mux(u_w_valid, Mux(io.update_write_alloc, allocWriteWay, io.update_write_way), ftb1_vic_way)

    Mux1H(total_hits, ftb2.io.r.resp.data).display(true.B)


  }//FTB2Bank
  val ftb2Bank = Module(new FTB2Bank(numSets, numWays))
  ftb2Bank.io.req_pc.valid := io.s0_fire
  ftb2Bank.io.req_pc.bits := s0_pc

  val ftb2_entry = RegEnable(ftb2Bank.io.read_resp,io.s2_fire)  //与FTB不同，这里为s2_fire,在s3出结果

  val ftb2_s2_hit = ftb2Bank.io.read_hits.valid && io.ctrl.btb_enable
  val ftb2_s3_hit = RegEnable(ftb2_s2_hit,io.s2_fire)
  val ftb2_writeWay = ftb2Bank.io.read_hits.bits

  //ftb2 hit entry to ftbp
  //io.ftb2_up.valid := s2_hit   //此处改为s2_hit
  //io.ftb2_up.bits := ftb2Bank.io.read_hit_entrywithtag

  val ftb2_fallThruAddr = getFallThroughAddr(s3_pc, ftb2_entry.carry, ftb2_entry.pftAddr)  //s3

  io.out := io.in.bits.resp_in(0)
when(ftb2_s3_hit){
  /*  io.out.s2.full_pred.hit := s2_hit  //怎么区分ftb1和ftb2的hit
    io.out.s2.pc := s2_pc
    io.out.s2.full_pred.fromFtbEntry(ftb2_entry, s2_pc, Some((s1_pc,io.s1_fire)))*/

  io.out.s3.full_pred.hit := ftb2_s3_hit
  io.out.s3.pc := s3_pc
  io.out.s3.full_pred.fromFtbEntry(ftb2_entry,s3_pc,Some((s2_pc,io.s2_fire)))

  io.out.last_stage_ftb_entry := ftb2_entry   //s3

  for (i <- 0 until numBr) {
    //io.out.s2.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s2.full_pred.br_taken_mask(i) || ftb2_s2_hit && ftb2_entry.always_taken(i)
    io.out.s3.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s3.full_pred.br_taken_mask(i) || ftb2_s3_hit && ftb2_entry.always_taken(i)
  }
}
  io.out.last_stage_meta := RegEnable(FTB2Meta(ftb2_writeWay.asUInt(), ftb2_s2_hit, GTimer()).asUInt(), io.s2_fire)
  // Update logic
  val update = io.update.bits

  val u_meta = update.meta.asTypeOf(new FTB2Meta)
  val u_valid = io.update.valid && !io.update.bits.old_entry

  val delay2_pc = DelayN(update.pc, 2)
  val delay3_pc = DelayN(update.pc, 3)
  val delay2_entry = DelayN(update.ftb_entry, 2)
  val delay3_entry = DelayN(update.ftb_entry, 3)

  val update_now = u_valid && u_meta.hit
  val update_need_read = u_valid && !u_meta.hit

  io.s1_ready := ftb2Bank.io.req_pc.ready && !(update_need_read) && !RegNext(update_need_read) && !RegNext(RegNext(update_need_read))

  ftb2Bank.io.u_req_pc.valid := update_need_read
  ftb2Bank.io.u_req_pc.bits := update.pc

  val ftb2_write = Wire(new FTB2EntryWithTag)
  ftb2_write.entry := Mux(update_now, update.ftb_entry, delay3_entry)
  ftb2_write.tag := ftb2Addr.getTag(Mux(update_now, update.pc, delay3_pc))(tagSize - 1, 0)

  val write_valid = update_now || DelayN(u_valid && !u_meta.hit, 3)

  ftb2Bank.io.update_write_data.valid := write_valid
  ftb2Bank.io.update_write_data.bits := ftb2_write
  ftb2Bank.io.update_pc := Mux(update_now, update.pc, delay3_pc)
  ftb2Bank.io.update_write_way := Mux(update_now, u_meta.writeWay, RegNext(ftb2Bank.io.update_hits.bits))
  ftb2Bank.io.update_write_alloc := Mux(update_now, false.B, RegNext(!ftb2Bank.io.update_hits.valid))
  ftb2Bank.io.update_access := u_valid && !u_meta.hit
  ftb2Bank.io.s1_fire := io.s1_fire
  ftb2Bank.io.s2_fire := io.s2_fire

  ftb2Bank.io.ftb1_victim <> ftb2io.ftb1_victim
  ftb2Bank.io.ftb1_victim_pc := ftb2io.ftb1_victim_pc
  ftb2Bank.io.ftb1_s2_hit := ftb2io.ftb1_s2_hit
  ftb2io.ftb2_hit <> ftb2Bank.io.ftb2_hit
  ftb2io.ftb2_hit_pc := ftb2Bank.io.ftb2_hit_pc


  ftb2_entry.display(true.B)
  //assert(!RegNext(!io.in.bits.resp_in(0).s3.full_pred.hit && ftb2_s3_hit && io.out.s3.full_pred.fallThroughErr && !io.out.s3.full_pred.br_taken_mask.reduce(_||_)))
  //ftb2 read hit to ftbp
  //io.ftb2_up.valid := s3_hit
  //io.ftb2_up.bits := ftb2Bank.io.read_hit_entrywithtag

  XSPerfAccumulate("ftb2_read_hits", RegNext(io.s2_fire) && ftb2_s2_hit)  //s3
  XSPerfAccumulate("ftb2_read_misses", RegNext(io.s2_fire) && !ftb2_s2_hit)

  XSPerfAccumulate("ftb2_commit_hits", io.update.valid && u_meta.hit)
  XSPerfAccumulate("ftb2_commit_misses", io.update.valid && !u_meta.hit)

  XSPerfAccumulate("ftb2_s3_miss_commit_hits", io.update.valid && u_meta.hit && !io.in.bits.resp_in(0).s3.full_pred.hit)
  XSPerfAccumulate("ftb2_s3_miss_commit_misses", io.update.valid && !u_meta.hit && !io.in.bits.resp_in(0).s3.full_pred.hit)

  XSDebug("if_ftb2_commit_miss=%d, req_pc=%x\n", io.update.valid && !u_meta.hit && !io.in.bits.resp_in(0).s3.full_pred.hit, io.update.bits.pc)

  XSPerfAccumulate("ftbandftbp_miss_ftb2_readhit", !io.in.bits.resp_in(0).s3.full_pred.hit && ftb2_s3_hit)
  XSPerfAccumulate("ftb2_fallthrouerror", !io.in.bits.resp_in(0).s3.full_pred.hit && ftb2_s3_hit && io.out.s3.full_pred.fallThroughErr)

  XSPerfAccumulate("ftb2_update_req", io.update.valid)
  XSPerfAccumulate("ftb2_update_ignored", io.update.valid && io.update.bits.old_entry)
  XSPerfAccumulate("ftb2_updated", u_valid)
  XSDebug(u_valid, "ftb2_update_pc = %x, update_pred_hit=%b, update_false_hit=%b\n", update.pc, update.pred_hit, update.false_hit)

  generatePerfEvent()





}
