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


trait FTBParams extends HasXSParameter with HasBPUConst {
  val numEntries = 2048
  val numWays    = 4
  val numSets    = numEntries/numWays // 512
  val tagSize    = 20
}

class FTBEntry (implicit p: Parameters) extends XSBundle with FTBParams with BPUUtils {
  val valid       = Bool()
  val tag         = UInt(tagSize.W)

  val brOffset    = Vec(numBr, UInt(log2Up(FetchWidth*2).W))
  val brTargets    = Vec(numBr, UInt(VAddrBits.W))
  val brValids    = Vec(numBr, Bool())

  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jmpTarget   = UInt(VAddrBits.W)
  val jmpValid    = Bool()

  // Partial Fall-Through Address
  val pftAddr     = UInt((log2Up(PredictWidth)+1).W)
  val carry       = Bool()

  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  val oversize    = Bool()

  val last_is_rvc = Bool()

  def getOffsetVec = VecInit(brOffset :+ jmpOffset)
  def isJal = !isJalr
  def getFallThrough(pc: UInt) = getFallThroughAddr(pc, carry, pftAddr)
  def hasBr(offset: UInt) = (brValids zip brOffset).map{
    case (v, off) => v && off <= offset
  }.reduce(_||_)

  def getBrMaskByOffset(offset: UInt) = (brValids zip brOffset).map{
    case (v, off) => v && off <= offset
  }
  
  def brIsSaved(offset: UInt) = (brValids zip brOffset).map{
    case (v, off) => v && off === offset
  }.reduce(_||_)

  def display(cond: Bool): Unit = {
    XSDebug(cond, p"-----------FTB entry----------- \n")
    XSDebug(cond, p"v=${valid}, tag=${Hexadecimal(tag)}\n")
    for(i <- 0 until numBr) {
      XSDebug(cond, p"[br$i]: v=${brValids(i)}, offset=${brOffset(i)}, target=${Hexadecimal(brTargets(i))}\n")
    }
    XSDebug(cond, p"[jmp]: v=${jmpValid}, offset=${jmpOffset}, target=${Hexadecimal(jmpTarget)}\n")
    XSDebug(cond, p"pftAddr=${Hexadecimal(pftAddr)}, carry=$carry\n")
    XSDebug(cond, p"isCall=$isCall, isRet=$isRet, isjalr=$isJalr\n")
    XSDebug(cond, p"oversize=$oversize, last_is_rvc=$last_is_rvc\n")
    XSDebug(cond, p"------------------------------- \n")
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
      val read_pc = Flipped(DecoupledIO(UInt(VAddrBits.W)))
      val read_resp = Output(new FTBEntry)

      // when ftb hit, read_hits.valid is true, and read_hits.bits is OH of hit way
      // when ftb not hit, read_hits.valid is false, and read_hits is OH of allocWay
      val read_hits = Valid(Vec(numWays, Bool()))

      val update_pc = Input(UInt(VAddrBits.W))
      val update_write_data = Flipped(Valid(new FTBEntry))
      val update_write_mask = Input(UInt(numWays.W))
    })

    val ftb = Module(new SRAMTemplate(new FTBEntry, set = numSets, way = numWays, shouldReset = true, holdRead = true, singlePort = true))

    ftb.io.r.req.valid := io.read_pc.valid // io.s0_fire
    ftb.io.r.req.bits.setIdx := ftbAddr.getIdx(io.read_pc.bits) // s0_idx

    io.read_pc.ready := ftb.io.r.req.ready

    val read_tag = RegEnable(ftbAddr.getTag(io.read_pc.bits)(tagSize-1, 0), io.read_pc.valid)

    val read_datas = ftb.io.r.resp.data

    val total_hits = VecInit((0 until numWays).map(b => read_datas(b).tag === read_tag && read_datas(b).valid))
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

    val allocWriteWay = allocWay(VecInit(read_datas.map(w => w.valid)).asUInt,
        VecInit(read_datas.map(w => w.tag)).asUInt,
        read_tag)

    io.read_resp := PriorityMux(total_hits, read_datas) // Mux1H
    io.read_hits.valid := hit
    io.read_hits.bits := Mux(hit, hit_way_1h, VecInit(UIntToOH(allocWriteWay).asBools()))

    // Update logic
    val u_valid = io.update_write_data.valid
    val u_data = io.update_write_data.bits
    val u_idx = ftbAddr.getIdx(io.update_pc)
    val u_mask = io.update_write_mask

    ftb.io.w.apply(u_valid, u_data, u_idx, u_mask)
  } // FTBBank

  val ftbBank = Module(new FTBBank(numSets, numWays))

  ftbBank.io.read_pc.valid := io.s0_fire
  ftbBank.io.read_pc.bits := s0_pc

  io.s1_ready := ftbBank.io.read_pc.ready //  && !io.redirect.valid

  val ftb_entry = RegEnable(ftbBank.io.read_resp, io.s1_fire)
  val s1_hit = ftbBank.io.read_hits.valid
  val s2_hit = RegEnable(s1_hit, io.s1_fire)
  val writeWay = ftbBank.io.read_hits.bits

  val fallThruAddr = getFallThroughAddr(s2_pc, ftb_entry.carry, ftb_entry.pftAddr)

  // io.out.bits.resp := RegEnable(io.in.bits.resp_in(0), 0.U.asTypeOf(new BranchPredictionResp), io.s1_fire)
  io.out.resp := io.in.bits.resp_in(0)

  // val s2_target = Wire(UInt(VAddrBits.W))
  // s2_target := s2_pc + (FetchWidth*4).U
  // when(s2_hit) {
  //   s2_target := Mux((io.in.bits.resp_in(0).s2.preds.taken_mask.asUInt & ftb_entry.brValids.asUInt) =/= 0.U,
  //     PriorityMux(io.in.bits.resp_in(0).s2.preds.taken_mask.asUInt & ftb_entry.brValids.asUInt, ftb_entry.brTargets),
  //     Mux(ftb_entry.jmpValid, ftb_entry.jmpTarget, fallThruAddr))
  // }

  val s1_latch_call_is_rvc   = DontCare // TODO: modify when add RAS

  io.out.resp.s2.preds.taken_mask    := io.in.bits.resp_in(0).s2.preds.taken_mask

  io.out.resp.s2.preds.hit           := s2_hit
  // io.out.resp.s2.preds.target        := s2_target
  io.out.resp.s2.pc                  := s2_pc
  io.out.resp.s2.ftb_entry           := ftb_entry

  io.out.s3_meta                     := RegEnable(RegEnable(FTBMeta(writeWay.asUInt(), s1_hit, GTimer()).asUInt(), io.s1_fire), io.s2_fire)

  when(s2_hit) {
    io.out.resp.s2.ftb_entry.pftAddr := ftb_entry.pftAddr
    io.out.resp.s2.ftb_entry.carry := ftb_entry.carry
  }.otherwise {
    io.out.resp.s2.ftb_entry.pftAddr := s2_pc(instOffsetBits + log2Ceil(PredictWidth), instOffsetBits) ^ (1 << log2Ceil(PredictWidth)).U
    io.out.resp.s2.ftb_entry.carry := s2_pc(instOffsetBits + log2Ceil(PredictWidth)).asBool
  }

  // io.out.resp.s3 := RegEnable(io.out.resp.s2, io.s2_fire)

  // Update logic
  val has_update = RegInit(VecInit(Seq.fill(64)(0.U(VAddrBits.W))))
  val has_update_ptr = RegInit(0.U(log2Up(64)))

  val update = RegNext(io.update.bits)

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  val u_valid = RegNext(io.update.valid && !io.update.bits.old_entry)
  val u_way_mask = u_meta.writeWay

  val ftb_write = WireInit(update.ftb_entry)

  ftb_write.valid := true.B
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

  if (debug && !env.FPGAPlatform && env.EnablePerfDebug) {
    XSDebug("req_v=%b, req_pc=%x, ready=%b (resp at next cycle)\n", io.s0_fire, s0_pc, ftbBank.io.read_pc.ready)
    XSDebug("s2_hit=%b, hit_way=%b\n", s2_hit, writeWay.asUInt)
    XSDebug("s2_taken_mask=%b, s2_real_taken_mask=%b\n",
      io.in.bits.resp_in(0).s2.preds.taken_mask.asUInt, io.out.resp.s2.real_taken_mask().asUInt)
    XSDebug("s2_target=%x\n", io.out.resp.s2.target)

    ftb_entry.display(true.B)

    XSDebug(u_valid, "Update from ftq\n")
    XSDebug(u_valid, "update_pc=%x, tag=%x, update_write_way=%b, pred_cycle=%d\n",
      update.pc, ftbAddr.getTag(update.pc), u_way_mask, u_meta.pred_cycle)





    XSPerfAccumulate("ftb_first_miss", u_valid && !u_updated && !update.preds.hit)
    XSPerfAccumulate("ftb_updated_miss", u_valid && u_updated && !update.preds.hit)

    XSPerfAccumulate("ftb_read_first_miss", RegNext(io.s0_fire) && !s1_hit && !r_updated)
    XSPerfAccumulate("ftb_read_updated_miss", RegNext(io.s0_fire) && !s1_hit && r_updated)

    XSPerfAccumulate("ftb_read_hits", RegNext(io.s0_fire) && s1_hit)
    XSPerfAccumulate("ftb_read_misses", RegNext(io.s0_fire) && !s1_hit)

    XSPerfAccumulate("ftb_commit_hits", u_valid && update.preds.hit)
    XSPerfAccumulate("ftb_commit_misses", u_valid && !update.preds.hit)

    XSPerfAccumulate("ftb_update_req", io.update.valid)
    XSPerfAccumulate("ftb_update_ignored", io.update.valid && io.update.bits.old_entry)
    XSPerfAccumulate("ftb_updated", u_valid)
  }
}
