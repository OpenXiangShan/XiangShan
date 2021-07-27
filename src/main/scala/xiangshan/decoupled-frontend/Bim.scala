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
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

trait BimParams extends HasXSParameter {
  val bimSize = 2048
  val bypassEntries = 4
}

@chiselName
class BIM(implicit p: Parameters) extends BasePredictor with BimParams with BPUUtils {
  val bimAddr = new TableAddr(log2Up(bimSize), numBr)

  val bim = Module(new SRAMTemplate(UInt(2.W), set = bimSize, way=numBr, shouldReset = false, holdRead = true))

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(bimSize).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (bimSize-1).U) { doing_reset := false.B }

  val s1_idx = bimAddr.getIdx(s1_pc)

  bim.io.r.req.valid := io.s0_fire
  bim.io.r.req.bits.setIdx := s1_idx

  io.in.ready := bim.io.r.req.ready && !io.redirect.valid
  io.s1_ready := bim.io.r.req.ready && !io.redirect.valid
  io.out.valid := io.s2_fire && !io.redirect.valid

  // val s1_pc = RegEnable(s0_pc, s0_valid)

  val s1_read = bim.io.r.resp.data

  io.out.bits.resp := io.in.bits.resp_in(0)
  // io.out.bits.resp.s1.preds.taken_mask := Cat(0.U(1.W), s1_read(1)(1), s1_read(0)(1))
  // io.out.bits.resp.s1.preds.taken_mask := VecInit(Cat(0.U(1.W), s1_read(0)(1)).asBools())
  // io.out.bits.resp.s1.meta := s1_read.asUInt()

  val s1_latch_taken_mask = VecInit(Cat(0.U(1.W), Cat((0 until numBr reverse).map(i => s1_read(i)(1)))).asBools())
  val s1_latch_meta       = s1_read.asUInt()

  io.out.bits.resp.s2.preds.taken_mask := RegEnable(s1_latch_taken_mask, 0.U.asTypeOf(Vec(numBr+1, Bool())), io.s1_fire)

  io.out.bits.resp.s3.preds.taken_mask := RegEnable(RegEnable(s1_latch_taken_mask, io.s1_fire), io.s2_fire)
  io.out.bits.s3_meta := RegEnable(RegEnable(s1_latch_meta, io.s1_fire), io.s2_fire)

  // Update logic
  val u_valid = RegNext(io.update.valid)
  val update = RegNext(io.update.bits)

  val u_idx = bimAddr.getIdx(update.pc)

  // Bypass logic
  val wrbypass_ctrs       = RegInit(0.U.asTypeOf(Vec(bypassEntries, Vec(numBr, UInt(2.W)))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(bypassEntries, Vec(numBr, Bool()))))
  val wrbypass_idx       = RegInit(0.U.asTypeOf(Vec(bypassEntries, UInt(log2Up(bimSize).W))))
  val wrbypass_enq_ptr    = RegInit(0.U(log2Up(bypassEntries).W))

  val wrbypass_hits = VecInit((0 until bypassEntries).map(i =>
    !doing_reset && wrbypass_idx(i) === u_idx))
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  val oldCtrs = VecInit((0 until numBr).map(i =>
    Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(i),
    wrbypass_ctrs(wrbypass_hit_idx)(i), update.meta(2*i+1, 2*i))))

  val newTakens = update.preds.taken_mask
  val newCtrs = VecInit((0 until numBr).map(i =>
    satUpdate(oldCtrs(i), 2, newTakens(i))
  ))

  val update_mask = LowerMask(PriorityEncoderOH(update.preds.taken_mask.asUInt))
  val need_to_update = VecInit((0 until numBr).map(i => u_valid && update.preds.is_br(i) && update_mask(i)))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_ := VecInit(Seq.fill(numBr)(false.B)))}

  for (i <- 0 until numBr) {
    when(need_to_update.reduce(_||_)) {
      when(wrbypass_hit) {
        when(need_to_update(i)) {
          wrbypass_ctrs(wrbypass_hit_idx)(i) := newCtrs(i)
          wrbypass_ctr_valids(wrbypass_hit_idx)(i) := true.B
        }
      }.otherwise {
        wrbypass_ctr_valids(wrbypass_enq_ptr)(i) := false.B
        when(need_to_update(i)) {
          wrbypass_ctrs(wrbypass_enq_ptr)(i) := newCtrs(i)
          wrbypass_ctr_valids(wrbypass_enq_ptr)(i) := true.B
        }
      }
    }
  }

  when (need_to_update.reduce(_||_) && !wrbypass_hit) {
    wrbypass_idx(wrbypass_enq_ptr) := u_idx
    wrbypass_enq_ptr := (wrbypass_enq_ptr + 1.U)(log2Up(bypassEntries)-1, 0)
  }

  bim.io.w.apply(
    valid = need_to_update.asUInt.orR || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, Fill(numBr, 1.U(1.W)).asUInt(), need_to_update.asUInt())
  )
}
