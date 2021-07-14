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
  val numBr = 1
}

@chiselName
class BIM(implicit p: Parameters) extends BasePredictor with BimParams with BPUUtils {
  val bimAddr = new TableAddr(log2Up(bimSize), numBr)

  val bim = Module(new SRAMTemplate(UInt(2.W), set = bimSize, way=numBr, shouldReset = false, holdRead = true))

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(bimSize).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (bimSize-1).U) { doing_reset := false.B }

  val s0_idx = bimAddr.getIdx(s0_pc)

  bim.io.r.req.valid := io.s0_fire
  bim.io.r.req.bits.setIdx := s0_idx

  io.in.ready := bim.io.r.req.ready && !io.flush.valid
  io.out.valid := RegNext(io.s0_fire) && !io.flush.valid

  // val s1_pc = RegEnable(s0_pc, s0_valid)

  val s1_read = bim.io.r.resp.data

  io.out.bits.resp.s1.preds.taken_mask := Cat(s1_read(0)(1), s1_read(1)(1), 0.U(1.W))
  io.out.bits.resp.s1.meta := s1_read

  // TODO: Replace RegNext by RegEnable
  io.out.bits.resp.s2.preds.taken := RegEnable(io.out.bits.resp.s1.preds.taken, io.s1_fire)
  io.out.bits.resp.s2.meta := RegEnable(io.out.bits.resp.s1.meta, io.s1_fire)

  io.out.bits.resp.s3.preds.taken := RegEnable(io.out.bits.resp.s2.preds.taken, io.s2_fire)
  io.out.bits.resp.s3.meta := RegEnable(io.out.bits.resp.s2.meta, io.s2_fire)

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

  val need_to_update = u_valid && update.preds.is_br.reduce(_||_)

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_ := false.B)}

  when (need_to_update) {
    when (wrbypass_hit) {
      wrbypass_ctrs(wrbypass_hit_idx) := newCtrs
      wrbypass_ctr_valids(wrbypass_hit_idx) := true.B
    }.otherwise {
      wrbypass_ctr_valids(wrbypass_enq_ptr) := false.B
      when (need_to_update) {
        wrbypass_ctrs(wrbypass_enq_ptr) := newCtrs
        wrbypass_ctr_valids(wrbypass_enq_ptr) := true.B
      }
    }
  }

  when (need_to_update && !wrbypass_hit) {
    wrbypass_idx(wrbypass_enq_ptr) := u_idx
    wrbypass_enq_ptr := (wrbypass_enq_ptr + 1.U)(log2Up(bypassEntries)-1, 0)
  }

  bim.io.w.apply(
    valid = need_to_update.asUInt.orR || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, 1.U(1.W), need_to_update)
  )
}
