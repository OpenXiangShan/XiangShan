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


trait FTBPParams extends HasXSParameter with HasBPUConst {
  val numEntries = 128
  val numWays = 128
  val numSets    = numEntries/numWays // 128
  val tagSize    = 16

  val TAR_STAT_SZ = 2
  def TAR_FIT = 0.U(TAR_STAT_SZ.W)
  def TAR_OVF = 1.U(TAR_STAT_SZ.W)
  def TAR_UDF = 2.U(TAR_STAT_SZ.W)  //3个状态 2位

  def BR_OFFSET_LEN = 12
  def JMP_OFFSET_LEN = 20
  def getTag(pc: UInt) = pc(tagSize+instOffsetBits- 1, instOffsetBits)
}//20+2+32

class FTBPEntryWithTag(implicit p: Parameters) extends XSBundle with FTBPParams with BPUUtils {
  val ftbp_entry = new FTBEntry
  val tag = UInt(tagSize.W)
  //val ftb1_vic = UInt(1.W)
  def display(cond: Bool): Unit = {
    ftbp_entry.display(cond)
    XSDebug(cond, p"tag is ${Hexadecimal(tag)}\n------------------------------- \n")
  }
}

class FTBPMeta(implicit p: Parameters) extends XSBundle with FTBPParams {
  val writeWay = UInt(log2Ceil(numWays).W)
  val hit = Bool()
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

object FTBPMeta {
  def apply(writeWay: UInt, hit: Bool, pred_cycle: UInt)(implicit p: Parameters): FTBPMeta = {
    val e = Wire(new FTBPMeta)
    e.writeWay := writeWay
    e.hit := hit
    e.pred_cycle.map(_ := pred_cycle)
    e
  }
}


class FTBP (implicit p: Parameters) extends BasePredictor with FTBPParams with BPUUtils
  with HasCircularQueuePtrHelper with HasPerfEvents {
  override val meta_size = WireInit(0.U.asTypeOf(new FTBPMeta)).getWidth
  val ftbp_entrywithtag = new FTBPEntryWithTag
  val ftbpAddr = new TableAddr(log2Up(numSets), 1)
  val ftbpio = IO(new Bundle {
    val ftbp_from_ftb1 = Flipped(Valid(new FTBEntry))
    val ftb1_pc = Input(UInt(VAddrBits.W))
    val ftbp_from_ftb2 = Flipped(Valid(new FTBEntry))
    val ftb2_pc = Input(UInt(VAddrBits.W))
    val ftbp_hit = Valid(new FTBEntry)
    val ftbp_hit_pc = Output(UInt(VAddrBits.W))
    val ftb_s1_hit = Input(Bool())
    //val s0_pc = Input(UInt(VAddrBits.W))
  })

  class FTBPBank(val numSets: Int, val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val s1_fire = Input(Bool())
      //val s2_fire = Input(Bool())
      val req_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      //val read_resp = Output(new FTBEntry)
      //val read_hits = Valid(UInt(log2Ceil(numWays).W))

      val ftbp_from_ftb1 = Flipped(Valid(new FTBEntry))
      val ftb1_pc = Input(UInt(VAddrBits.W))
      val ftbp_from_ftb2 = Flipped(Valid(new FTBEntry))
      val ftb2_pc = Input(UInt(VAddrBits.W))

      val ftbp_hit = Valid(new FTBEntry)
      val ftbp_hit_pc = Output(UInt(VAddrBits.W))
      val writeWay = Valid(UInt(log2Ceil(numWays).W))
    })
    val ftbp = RegInit(0.U.asTypeOf(Vec(numWays,ftbp_entrywithtag)))
    val req_tag = RegEnable(getTag(io.req_pc.bits),io.req_pc.valid)
    val total_hits = VecInit((0 until numWays).map(b => ftbp(b).tag === req_tag && ftbp(b).ftbp_entry.valid && io.s1_fire))//s1
    //val total_hits = VecInit((0 until numWays).map(b => ftbp(b).tag === req_tag && ftbp(b).ftbp_entry.valid && io.req_pc.valid))//s0
    val hit = total_hits.reduce(_||_)  //s1
    val hit_way = OHToUInt(total_hits)
    //io.ftbp_hit.bits := RegNext(ftbp(hit_way).ftbp_entry)
    
    
    //io.ftbp_hit_pc := RegEnable(io.req_pc.bits, io.req_pc.valid)
    io.ftbp_hit_pc := RegEnable(io.req_pc.bits, io.req_pc.valid)
    io.req_pc.ready := !io.ftbp_from_ftb1.valid   //delay 1 s2

    val replacer = ReplacementPolicy.fromString("plru", numWays)
    val replacer_touch_ways = Wire(Vec(2, Valid(UInt(log2Ceil(numWays).W))))


    val w1_tag = getTag(io.ftb1_pc)
    val w1_valid = io.ftbp_from_ftb1.valid
    val w2_tag = getTag(io.ftb2_pc)
    val w2_valid = io.ftbp_from_ftb2.valid 
    val hazard = w1_valid && w2_valid
    val w_valid = w1_valid || w2_valid
    val w_tag = Mux(w1_valid, w1_tag, w2_tag)
    val w_entry = Mux(w1_valid,io.ftbp_from_ftb1.bits,io.ftbp_from_ftb2.bits )

    val up_total_hits = VecInit((0 until numWays).map(b => ftbp(b).tag === w_tag && ftbp(b).ftbp_entry.valid && w_valid))
    val up_hit = up_total_hits.reduce(_||_)
    val up_hit_way = OHToUInt(up_total_hits)
    val w_way = Mux(up_hit,up_hit_way,replacer.way)
    io.writeWay.valid := hit
    io.writeWay.bits := hit_way
    val work_by_pass = WireInit(false.B)
    work_by_pass := w_valid && (w_tag === req_tag)
    io.ftbp_hit.valid := hit || work_by_pass
    io.ftbp_hit.bits := Mux(work_by_pass, w_entry, Mux1H(total_hits,ftbp.map(_.ftbp_entry)))
    assert(PopCount(total_hits) <= 1.U)
    assert(PopCount(up_total_hits) <= 1.U)
    
    ftbp(w_way).ftbp_entry := Mux(w_valid, w_entry, ftbp(w_way).ftbp_entry)
    ftbp(w_way).tag := Mux(w_valid, w_tag, ftbp(w_way).tag)


    replacer_touch_ways(0).valid := RegNext(io.s1_fire && hit)
    replacer_touch_ways(0).bits := RegEnable(hit_way, io.s1_fire && hit)

    replacer_touch_ways(1).valid := RegNext(w_valid)
    replacer_touch_ways(1).bits := RegEnable(w_way, w_valid)

    replacer.access(replacer_touch_ways)
    XSDebug(w_valid, "ftbp_write_way = %x\n", w_way)
    XSPerfAccumulate("ftbp_recv_ftb1_vic", io.ftbp_from_ftb1.valid)
    XSPerfAccumulate("ftbp_recv_ftb2_hit", io.ftbp_from_ftb2.valid && !io.ftbp_from_ftb1.valid)

  }//FTBPBank
  val ftbpBank = Module(new FTBPBank(numSets, numWays))
  ftbpBank.io.req_pc.valid := io.s0_fire
  ftbpBank.io.req_pc.bits := s0_pc

  val ftbp_entry = RegEnable(ftbpBank.io.ftbp_hit.bits,io.s1_fire)
  val s3_ftbp_entry = RegEnable(ftbp_entry, io.s2_fire)
  //val s1_hit = ftbpBank.io.ftbp_hit.valid && io.ctrl.btb_enable && !ftbpio.ftb_s1_hit
  val s1_hit = ftbpBank.io.ftbp_hit.valid && io.ctrl.btb_enable 
  val s2_hit = RegEnable(s1_hit,io.s1_fire)
  val s3_hit = RegEnable(s2_hit,io.s2_fire)
  val writeWay = ftbpBank.io.writeWay.bits

  val fallThruAddr = getFallThroughAddr(s2_pc, ftbp_entry.carry, ftbp_entry.pftAddr)
  
  io.s1_ready := ftbpBank.io.req_pc.ready
  io.out := io.in.bits.resp_in(0)
/*when(s2_hit){
  io.out.s2.full_pred.hit := s2_hit
  io.out.s2.pc := s2_pc
  io.out.s2.full_pred.fromFtbEntry(ftbp_entry, s2_pc, Some((s1_pc, io.s1_fire)))

  for (i <- 0 until numBr) {
    io.out.s2.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s2.full_pred.br_taken_mask(i) || s2_hit && ftbp_entry.always_taken(i)
  }
}
when(s3_hit){
  io.out.s3.full_pred.hit := s3_hit
  io.out.s3.pc := s3_pc
  io.out.s3.full_pred.fromFtbEntry(s3_ftbp_entry, s3_pc, Some((s2_pc, io.s2_fire)))

  io.out.last_stage_ftb_entry := s3_ftbp_entry

  for (i <- 0 until numBr) {
    io.out.s3.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s3.full_pred.br_taken_mask(i) || s3_hit && s3_ftbp_entry.always_taken(i)
  }

}*/

  io.out.s2.full_pred.hit := s2_hit
  io.out.s2.pc := s2_pc
  io.out.s2.full_pred.fromFtbEntry(ftbp_entry, s2_pc, Some((s1_pc, io.s1_fire)))

  for (i <- 0 until numBr) {
    io.out.s2.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s2.full_pred.br_taken_mask(i) || s2_hit && ftbp_entry.always_taken(i)
  }
  io.out.s3.full_pred.hit := s3_hit
  io.out.s3.pc := s3_pc
  io.out.s3.full_pred.fromFtbEntry(s3_ftbp_entry, s3_pc, Some((s2_pc, io.s2_fire)))

  io.out.last_stage_ftb_entry := s3_ftbp_entry

  for (i <- 0 until numBr) {
    io.out.s3.full_pred.br_taken_mask(i) := io.in.bits.resp_in(0).s3.full_pred.br_taken_mask(i) || s3_hit && s3_ftbp_entry.always_taken(i)
  }



  io.out.last_stage_meta := RegEnable(RegEnable(FTBMeta(writeWay.asUInt(), s1_hit, GTimer()).asUInt(), io.s1_fire), io.s2_fire)

  ftbpBank.io.s1_fire := io.s1_fire
  ftbpBank.io.ftbp_from_ftb1 <> ftbpio.ftbp_from_ftb1
  ftbpBank.io.ftb1_pc := ftbpio.ftb1_pc
  ftbpBank.io.ftbp_from_ftb2 <> ftbpio.ftbp_from_ftb2
  ftbpBank.io.ftb2_pc := ftbpio.ftb2_pc

  ftbpio.ftbp_hit <> ftbpBank.io.ftbp_hit
  ftbpio.ftbp_hit_pc := ftbpBank.io.ftbp_hit_pc
  val ftbp_fallthrough_error = s2_hit && io.out.s2.full_pred.fallThroughErr && !io.out.s2.full_pred.br_taken_mask.reduce(_||_)
  
  assert(!RegNext(ftbp_fallthrough_error))

  XSPerfAccumulate("ftbp_read_hits", RegNext(io.s0_fire) && s1_hit)
  XSPerfAccumulate("ftbp_read_misses", RegNext(io.s0_fire) && !s1_hit)
  XSPerfAccumulate("ftbp_fallthrouerror", s2_hit && io.out.s2.full_pred.fallThroughErr && !io.out.s2.full_pred.br_taken_mask.reduce(_||_))

  generatePerfEvent()

}