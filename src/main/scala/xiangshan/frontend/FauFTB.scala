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
import chisel3.util._
import utils._
import xiangshan._
import chisel3.experimental.chiselName
import scala.{Tuple2 => &}

trait FauFTBParams extends HasXSParameter with HasBPUConst {
  val numWays = 32
  val tagSize = 16

  val numDup_local = 2

  def special_idx_for_dup = dupForTageSC

  def getTag(pc: UInt) = pc(tagSize+instOffsetBits-1, instOffsetBits)
}

class FauFTBEntry(implicit p: Parameters) extends FTBEntry()(p) {}

class FauFTBWay(implicit p: Parameters) extends XSModule with FauFTBParams {
  val io = IO(new Bundle{
    val req_tag = Input(UInt(tagSize.W))
    val resp = Output(new FauFTBEntry)
    val resp_hit = Output(Bool())
    val update_req_tag = Input(UInt(tagSize.W))
    val update_hit = Output(Bool())
    val write_valid = Input(Bool())
    val write_entry = Input(new FauFTBEntry)
    val write_tag = Input(UInt(tagSize.W))
    val tag_read = Output(UInt(tagSize.W))
  })

  val data = Reg(new FauFTBEntry)
  val tag = Reg(UInt(tagSize.W))
  val valid = RegInit(false.B)

  io.resp := data
  io.resp_hit := tag === io.req_tag && valid
  // write bypass to avoid multiple hit
  io.update_hit := ((tag === io.update_req_tag) && valid) ||
                   ((io.write_tag === io.update_req_tag) && io.write_valid)
  io.tag_read := tag

  when (io.write_valid) {
    when (!valid) {
      valid := true.B
    }
    tag   := io.write_tag
    data  := io.write_entry
  }
}


class FauFTB(implicit p: Parameters) extends BasePredictor with FauFTBParams {
  
  class FauFTBMeta(implicit p: Parameters) extends XSBundle with FauFTBParams {
    val pred_way = UInt(log2Ceil(numWays).W)
    val hit = Bool()
  }
  val resp_meta = Wire(new FauFTBMeta)
  override val meta_size = resp_meta.getWidth
  override val is_fast_pred = true

  class FauFTBBank(implicit p: Parameters) extends XSModule with FauFTBParams {
    val io = IO(new Bundle {
      val req_tag = Input(UInt(tagSize.W))
      val resp_hit_oh = Output(Vec(numWays, Bool()))
      val resp_entries = Output(Vec(numWays, new FTBEntry))
      val resp_ctrs = Output(Vec(numWays, Vec(numBr, UInt(2.W))))
      val update_req_tag = Input(UInt(tagSize.W))
      val update_hit_oh = Output(Vec(numWays, Bool()))

      val write_valid_oh = Input(Vec(numWays, Bool()))
      val write_tag = Input(UInt(tagSize.W))
      val write_entry = Input(new FTBEntry)

      val write_ctrs_valid = Input(Vec(numWays, Vec(numBr, Bool())))
      val write_ctrs = Input(Vec(numWays, Vec(numBr, UInt(2.W))))
    })

    val ways = Seq.tabulate(numWays)(w => Module(new FauFTBWay))
    // numWays * numBr
    val ctrs = Seq.tabulate(numWays)(w => Seq.tabulate(numBr)(b => RegInit(2.U(2.W))))

    // pred req
    ways.foreach(_.io.req_tag := io.req_tag)

    // pred resp
    io.resp_hit_oh  := VecInit(ways.map(_.io.resp_hit))
    io.resp_entries := VecInit(ways.map(_.io.resp))
    io.resp_ctrs    := VecInit(ctrs.map(VecInit(_)))

    // update req
    ways.foreach(_.io.update_req_tag := io.update_req_tag)
    io.update_hit_oh := VecInit(ways.map(_.io.update_hit))

    // write req
    ways.zip(io.write_valid_oh).foreach{ case (w, v) => w.io.write_valid := v }
    ways.foreach(_.io.write_tag   := io.write_tag)
    ways.foreach(_.io.write_entry := io.write_entry)

    // write ctrs
    for (ctr & valid & w_ctr <- ctrs zip io.write_ctrs_valid zip io.write_ctrs) {
      for (c & v & w_c <- ctr zip valid zip w_ctr) {
        when (v) {
          c := w_c
        }
      }
    }
  }

  // bank 1 for tage, bank 0 for others
  val banks = Seq.fill(numDup_local)(Module(new FauFTBBank))
  banks.foreach(b => dontTouch(b.io))
  
  val replacer = Seq.fill(numDup_local)(ReplacementPolicy.fromString("plru", numWays))
  val replacer_touch_ways = Wire(Vec(numDup_local, Vec(2, Valid(UInt(log2Ceil(numWays).W)))))

  val s1_fire_dup = Wire(Vec(numDup_local, Bool()))
  s1_fire_dup(0) := io.s1_fire(dupForUbtb)
  s1_fire_dup(1) := io.s1_fire(special_idx_for_dup)
  
  // pred req
  banks(0).io.req_tag := getTag(s1_pc_dup(dupForUbtb))
  banks(1).io.req_tag := getTag(s1_pc_dup(special_idx_for_dup))

  // pred resp
  val s1_hit_oh_dup = VecInit(banks.map(_.io.resp_hit_oh.asUInt))
  val s1_hit_dup = s1_hit_oh_dup.map(_.orR)
  val s1_hit_way_dup = s1_hit_oh_dup.map(OHToUInt(_))
  val s1_possible_full_preds_dup = Wire(Vec(numDup_local, Vec(numWays, new FullBranchPrediction)))
  
  val s1_all_entries_dup = VecInit(banks.map(_.io.resp_entries))
  for (b <- 0 until numDup_local) {
    for (w <- 0 until numWays) {
      val fp = s1_possible_full_preds_dup(b)(w)
      val entry = s1_all_entries_dup(b)(w)
      val s1_pc = if (b == 0) s1_pc_dup(dupForUbtb) else s1_pc_dup(special_idx_for_dup)
      fp.fromFtbEntry(entry, s1_pc)
      fp.hit := DontCare
      for (i <- 0 until numBr) {
        val ctr = banks(b).io.resp_ctrs(w)(i)
        fp.br_taken_mask(i) := ctr(1) || entry.always_taken(i)
      }
    }

    // pred update replacer state
    replacer_touch_ways(b)(0).valid := RegNext(s1_fire_dup(b) && s1_hit_dup(b))
    replacer_touch_ways(b)(0).bits  := RegEnable(s1_hit_way_dup(b), s1_fire_dup(b) && s1_hit_dup(b))

  }

  val s1_hit_full_pred_dup = s1_hit_oh_dup.zip(s1_possible_full_preds_dup).map(t => Mux1H(t._1, t._2))
  XSError(PopCount(s1_hit_oh_dup(0)) > 1.U, "fauftb has multiple hits!\n")
  val fauftb_enable_dup = RegNext(dup(io.ctrl.ubtb_enable))

  io.out.s1.full_pred.map(_ := s1_hit_full_pred_dup(0))
  io.out.s1.full_pred.zip(fauftb_enable_dup).map {case (fp, en) => fp.hit := s1_hit_dup(0) && en}
  io.out.s1.full_pred(special_idx_for_dup) := s1_hit_full_pred_dup(1)
  io.out.s1.full_pred(special_idx_for_dup).hit := s1_hit_dup(1) && fauftb_enable_dup(special_idx_for_dup)

  for (i <- 1 until numDup) {
    XSError(io.out.s1.full_pred(i).asUInt =/= io.out.s1.full_pred(0).asUInt,
      p"fauftb s1 pred $i differs from pred 0\n")
  }

  // assign metas
  io.out.last_stage_meta := resp_meta.asUInt
  resp_meta.hit := RegEnable(RegEnable(s1_hit_dup(0), io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))
  resp_meta.pred_way := RegEnable(RegEnable(s1_hit_way_dup(0), io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))


  val s1_ftb_entry = Mux1H(s1_hit_oh_dup(0), s1_all_entries_dup(0))
  io.out.last_stage_ftb_entry := RegEnable(RegEnable(s1_ftb_entry, io.s1_fire(dupForUbtb)), io.s2_fire(dupForUbtb))




  /********************** update ***********************/
  // s0: update_valid, read and tag comparison
  // s1: alloc_way and write

  // s0
  val us = Wire(Vec(numDup_local, io.update(0).cloneType))
  val u_valids = Wire(Vec(numDup_local, Bool()))
  u_valids(0) := io.update(dupForUbtb).valid
  u_valids(1) := io.update(dupForTageSC).valid
  us(0) := io.update(dupForUbtb)
  us(1) := io.update(dupForTageSC)
  val u_meta_dup = us.map(_.bits.meta.asTypeOf(new FauFTBMeta))
  val u_s0_tag_dup = us.map(u => getTag(u.bits.pc))
  for (b <- 0 until numDup_local) {
    banks(b).io.update_req_tag := u_s0_tag_dup(b)
  }
  val u_s0_hit_oh_dup = VecInit(banks.map(_.io.update_hit_oh.asUInt))
  val u_s0_hit_dup = u_s0_hit_oh_dup.map(_.orR)
  val u_s0_br_update_valids_dup = VecInit(us.map(u =>
    VecInit((0 until numBr).map(w =>
      u.bits.ftb_entry.brValids(w) && u.valid && !u.bits.ftb_entry.always_taken(w) &&
      !(PriorityEncoder(u.bits.br_taken_mask) < w.U)))
  ))
  // s1
  val u_s1_valid_dup = us.map(u => RegNext(dup(u.valid, numWays+1))) // reduce fanouts
  val u_s1_tag_dup       = u_valids.zip(u_s0_tag_dup).map {case (v, tag) => RegEnable(tag, v)}
  val u_s1_hit_oh_dup    = u_valids.zip(u_s0_hit_oh_dup).map {case (v, oh) => RegEnable(oh, v)}
  val u_s1_hit_dup       = u_valids.zip(u_s0_hit_dup).map {case (v, h) => RegEnable(h, v)}
  val u_s1_alloc_way_dup = replacer.map(_.way)
  val u_s1_write_way_oh_dup =
    for (u_s1_hit & u_s1_hit_oh & u_s1_alloc_way <- u_s1_hit_dup zip u_s1_hit_oh_dup zip u_s1_alloc_way_dup)
      yield Mux(u_s1_hit, u_s1_hit_oh, UIntToOH(u_s1_alloc_way))
  val u_s1_ftb_entry_dup = us.map(u => RegEnable(u.bits.ftb_entry, u.valid))
  val u_s1_ways_write_valid_dup = Wire(Vec(numDup_local, Vec(numWays, Bool())))
  for (b <- 0 until numDup_local) {
    u_s1_ways_write_valid_dup(b) := VecInit((0 until numWays).map(w => u_s1_write_way_oh_dup(b)(w).asBool && u_s1_valid_dup(b)(w)))
    for (w <- 0 until numWays) {
      banks(b).io.write_valid_oh(w) := u_s1_ways_write_valid_dup(b)(w)
      banks(b).io.write_tag         := u_s1_tag_dup(b)
      banks(b).io.write_entry       := u_s1_ftb_entry_dup(b)
    }
  }

  // update saturating counters
  val u_s1_br_update_valids_dup = us.zip(u_s0_br_update_valids_dup).map {case (u, bruv) => RegEnable(bruv, u.valid)}
  val u_s1_br_takens_dup        = us.map(u => RegEnable(u.bits.br_taken_mask,  u.valid))


  for (b <- 0 until numDup_local) {
    for (w <- 0 until numWays) {
      for (br <- 0 until numBr) {
        banks(b).io.write_ctrs(w)(br) := satUpdate(banks(b).io.resp_ctrs(w)(br), 2, u_s1_br_takens_dup(b)(br))
        banks(b).io.write_ctrs_valid(w)(br) := u_s1_br_update_valids_dup(b)(br) && u_s1_ways_write_valid_dup(b)(w)
      }
    }
    // commit update replacer state
    replacer_touch_ways(b)(1).valid := u_s1_valid_dup(b).last
    replacer_touch_ways(b)(1).bits  := OHToUInt(u_s1_write_way_oh_dup(b))
    /******** update replacer *********/
    replacer(b).access(replacer_touch_ways(b))
  }



  /********************** perf counters **********************/
  val s0_fire_next_cycle = RegNext(io.s0_fire(dupForUbtb))
  val u_pred_hit_way_map   = (0 until numWays).map(w => s0_fire_next_cycle && s1_hit_dup(0) && s1_hit_way_dup(0) === w.U)
  val u_commit_hit_way_map = (0 until numWays).map(w => us(0).valid && u_meta_dup(0).hit && u_meta_dup(0).pred_way === w.U)
  XSPerfAccumulate("uftb_read_hits",   s0_fire_next_cycle &&  s1_hit_dup(0))
  XSPerfAccumulate("uftb_read_misses", s0_fire_next_cycle && !s1_hit_dup(0))
  XSPerfAccumulate("uftb_commit_hits",   us(0).valid &&  u_meta_dup(0).hit)
  XSPerfAccumulate("uftb_commit_misses", us(0).valid && !u_meta_dup(0).hit)
  XSPerfAccumulate("uftb_commit_read_hit_pred_miss", us(0).valid && !u_meta_dup(0).hit && u_s0_hit_oh_dup(0).orR)
  for (w <- 0 until numWays) {
    XSPerfAccumulate(f"uftb_pred_hit_way_${w}",   u_pred_hit_way_map(w))
    XSPerfAccumulate(f"uftb_commit_hit_way_${w}", u_commit_hit_way_map(w))
    XSPerfAccumulate(f"uftb_replace_way_${w}", !u_s1_hit_dup(0) && u_s1_alloc_way_dup(0) === w.U)
  }

  override val perfEvents = Seq(
    ("fauftb_commit_hit       ", us(0).valid &&  u_meta_dup(0).hit),
    ("fauftb_commit_miss      ", us(0).valid && !u_meta_dup(0).hit),
  )
  generatePerfEvent()
  
}