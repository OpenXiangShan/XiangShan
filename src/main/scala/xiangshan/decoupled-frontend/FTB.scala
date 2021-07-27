/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

class FTBEntry (implicit p: Parameters) extends XSBundle with FTBParams {
  val valid       = Bool()
  val tag         = UInt(tagSize.W)

  val brOffset    = Vec(numBr, UInt(log2Up(FetchWidth*2).W))
  val brTargets    = Vec(numBr, UInt(VAddrBits.W))
  val brValids    = Vec(numBr, Bool())

  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jmpTarget   = UInt(VAddrBits.W)
  val jmpValid    = Bool()

  // Partial Fall-Through Address
  val pftAddr     = UInt(VAddrBits.W) // TODO: Modify only use lowerbits
  val carry       = Bool()

  val isCall      = Bool()
  val isRet       = Bool()
  val isJalr      = Bool()

  val oversize    = Bool()

  val last_is_rvc = Bool()

  // def getTarget(pred: Vec[UInt], pc: UInt): (UInt, UInt) = {
  //   val taken_mask = Cat(jmpValid, pred(1)(1), pred(0)(1))
  //   val target = pc + (FetchWidth*4).U

  //   when(taken_mask =/= 0.U) {
  //     target := PriorityMux(taken_mask, Seq(brTargets(0), brTargets(1), jmpTarget))
  //   }

  //   (taken_mask, target)
  // }

  def getOffsetVec = VecInit(brOffset :+ jmpOffset)
  def isJal = !isJalr
}

class FTBMeta(implicit p: Parameters) extends XSBundle with FTBParams {
  val writeWay = UInt(log2Up(numWays).W)
  val hit = Bool()
}

object FTBMeta {
  def apply(writeWay: UInt, hit: Bool)(implicit p: Parameters): FTBMeta = {
    val e = Wire(new FTBMeta)
    e.writeWay := writeWay
    e.hit := hit
    e
  }
}

class FTB(implicit p: Parameters) extends BasePredictor with FTBParams {
  val ftbAddr = new TableAddr(log2Up(numSets), numBr)

  val ftb = Module(new SRAMTemplate(new FTBEntry, set = numSets, way = numWays, shouldReset = true, holdRead = true, singlePort = true))

  val s0_idx = ftbAddr.getBankIdx(s0_pc)
  val s1_tag = ftbAddr.getTag(s1_pc)(tagSize-1, 0)

  ftb.io.r.req.valid := io.s0_fire
  ftb.io.r.req.bits.setIdx := s0_idx

  io.in.ready := ftb.io.r.req.ready && !io.redirect.valid // TODO: remove
  io.s1_ready := ftb.io.r.req.ready && !io.redirect.valid
  // io.out.valid := RegEnable(RegNext(io.s0_fire), io.s1_fire) && !io.flush.valid
  io.out.valid := io.s2_fire && !io.redirect.valid

  io.out.bits.resp.valids(1) := io.out.valid

  val s1_read = VecInit((0 until numWays).map(w =>
    ftb.io.r.resp.data(w)
  ))

  val s1_totalHits = VecInit((0 until numWays).map(b => s1_read(b).tag === s1_tag && s1_read(b).valid))
  val s1_hit = s1_totalHits.reduce(_||_)
  val s2_hit = RegEnable(s1_hit, io.s1_fire)
  val s1_hit_way = PriorityEncoder(s1_totalHits) // TODO: Replace by Mux1H, and when not hit, clac tag and save it in ftb_entry

  def allocWay(valids: UInt, meta_tags: UInt, req_tag: UInt) = {
    val randomAlloc = true
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
  val allocWays = VecInit((0 until numWays).map(b =>
    allocWay(VecInit(s1_read.map(w => w.valid)).asUInt,
      VecInit(s1_read.map(w => w.tag)).asUInt,
      s1_tag)))

  val writeWay = Mux(s1_hit, s1_hit_way, allocWays(0)) // TODO: allocWays is Vec

  val ftb_entry = Mux1H(s1_totalHits, s1_read)

  val brTargets = ftb_entry.brTargets
  val jmpTarget = ftb_entry.jmpTarget

  // io.out.bits.resp := RegEnable(io.in.bits.resp_in(0), 0.U.asTypeOf(new BranchPredictionResp), io.s1_fire)
  io.out.bits.resp := io.in.bits.resp_in(0)

  val s1_latch_target = Wire(UInt(VAddrBits.W))
  // s1_latch_target := io.in.bits.resp_in(0).s1.preds.target
  s1_latch_target := s1_pc + (FetchWidth*4).U
  when(s1_hit) {
    s1_latch_target := Mux((io.in.bits.resp_in(0).s1.preds.taken_mask.asUInt & ftb_entry.brValids.asUInt) =/= 0.U,
      PriorityMux(io.in.bits.resp_in(0).s1.preds.taken_mask.asUInt & ftb_entry.brValids.asUInt, ftb_entry.brTargets),
      Mux(ftb_entry.jmpValid, ftb_entry.jmpTarget, ftb_entry.pftAddr))
  }

  val s1_latch_taken_mask = Wire(Vec(numBr+1, Bool()))

  // TODO: mask must is zero when ftb not hit
  when(s1_hit) {
    s1_latch_taken_mask     := io.in.bits.resp_in(0).s1.preds.taken_mask
    s1_latch_taken_mask(numBr)  := ftb_entry.jmpValid
  }.otherwise {
    s1_latch_taken_mask     := 0.U.asTypeOf(Vec(numBr+1, Bool()))
  }

  val s1_latch_call_is_rvc   = DontCare // TODO: modify when add RAS

  io.out.bits.resp.s2.preds.taken_mask    := RegEnable(s1_latch_taken_mask, io.s1_fire)
  io.out.bits.resp.s2.preds.is_br         := RegEnable(ftb_entry.brValids, io.s1_fire)
  io.out.bits.resp.s2.preds.is_jal        := RegEnable(ftb_entry.jmpValid && !ftb_entry.isJalr, io.s1_fire)
  io.out.bits.resp.s2.preds.is_jalr       := RegEnable(ftb_entry.isJalr, io.s1_fire)
  io.out.bits.resp.s2.preds.is_call       := RegEnable(ftb_entry.isCall, io.s1_fire)
  io.out.bits.resp.s2.preds.is_ret        := RegEnable(ftb_entry.isRet, io.s1_fire)

  io.out.bits.resp.s2.preds.target        := RegEnable(s1_latch_target, io.s1_fire)
  io.out.bits.resp.s2.pc                  := RegEnable(s1_pc, io.s1_fire) //s2_pc
  io.out.bits.resp.s2.hit                 := RegEnable(s1_hit, io.s1_fire)
  io.out.bits.resp.s2.ftb_entry           := RegEnable(ftb_entry, io.s1_fire)

  io.out.bits.s3_meta                     := RegEnable(RegEnable(FTBMeta(writeWay.asUInt(), s1_hit).asUInt(), io.s1_fire), io.s2_fire)

  io.out.bits.resp.s3 := RegEnable(io.out.bits.resp.s2, io.s2_fire)

  when(!s2_hit) {
    io.out.bits.resp.s2.ftb_entry.pftAddr := RegEnable(s1_pc + (FetchWidth*4).U, io.s1_fire)
  }.otherwise {
    io.out.bits.resp.s2.ftb_entry.pftAddr := RegEnable(ftb_entry.pftAddr, io.s1_fire)
  }

  // Update logic
  val has_update = RegInit(VecInit(Seq.fill(64)(0.U(VAddrBits.W))))
  val has_update_ptr = RegInit(0.U(log2Up(64)))

  val update = RegNext(io.update.bits)

  val u_pc = update.pc

  val u_meta = update.meta.asTypeOf(new FTBMeta)
  val u_way = u_meta.writeWay
  val u_idx = ftbAddr.getIdx(u_pc)
  val u_valid = RegNext(io.update.valid)
  val u_way_mask = UIntToOH(u_way)

  val ftb_write = WireInit(update.ftb_entry)

  ftb_write.valid := true.B
  ftb_write.tag   := ftbAddr.getTag(u_pc)(tagSize-1, 0)

  ftb.io.w.apply(u_valid, ftb_write, u_idx, u_way_mask)

  val r_updated = (0 until 64).map(i => has_update(i) === s1_pc).reduce(_||_)
  val u_updated = (0 until 64).map(i => has_update(i) === update.pc).reduce(_||_)

  when(u_valid) {
    when(!u_updated) { has_update(has_update_ptr) := update.pc }

    has_update_ptr := has_update_ptr + !u_updated
  }

  XSPerfAccumulate("ftb_first_miss", u_valid && !u_updated && !update.hit)
  XSPerfAccumulate("ftb_updated_miss", u_valid && u_updated && !update.hit)

  XSPerfAccumulate("ftb_read_first_miss", RegNext(io.s0_fire) && !s1_hit && !r_updated)
  XSPerfAccumulate("ftb_read_updated_miss", RegNext(io.s0_fire) && !s1_hit && r_updated)

  XSPerfAccumulate("ftb_read_hits", RegNext(io.s0_fire) && s1_hit)
  XSPerfAccumulate("ftb_read_misses", RegNext(io.s0_fire) && !s1_hit)

  XSPerfAccumulate("ftb_commit_hits", u_valid && update.hit)
  XSPerfAccumulate("ftb_commit_misses", u_valid && !update.hit)
}
