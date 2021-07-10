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
  val bim_size = 2048
  val bypass_entries = 4
}

@chiselName
class BIM(implicit p: Parameters) extends BasePredictor with BimParams with BPUUtils {
  val bimAddr = new TableAddr(log2Up(bim_size), 1)

  val bim = Module(new SRAMTemplate(UInt(2.W), set = bim_size, shouldReset = false, holdRead = true))

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(bim_size).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (bim_size-1).U) { doing_reset := false.B }

  val f0_pc = io.f0_pc
  val f0_pc.valid = io.f0_pc.valid
  val f0_idx = bimAddr.getIdx(f0_pc.bits)

  bim.io.r.req.valid := f0_pc.valid
  bim.io.r.req.bits.setIdx := f0_idx

  io.resp.valid := io.f0_pc.valid && bim.io.r.req.ready && io.flush

  val f1_pc = RegEnable(f0_pc, f0_pc.valid)

  val f1_read = bim.io.r.resp.data

  io.resp.bits.f1.preds.taken := f1_read(1)
  io.resp.bits.f1.meta := f1_read

  io.resp.bits.f2.preds.taken := RegNext(f1_read(1))
  io.resp.bits.f2.meta := RegNext(f1_read)

  io.resp.bits.f3.preds.taken := RegNext(RegNext(f1_read(1)))
  io.resp.bits.f3.meta := RegNext(RegNext(f1_read))

  // Update logic
  val u_valid = RegNext(io.update.valid)
  val update = RegNext(io.update.bits)

  val u_idx = bimAddr.getIdx(update.pc)

  // Bypass logic
  val wrbypass_ctrs       = RegInit(0.U.asTypeOf(Vec(bypass_entries, UInt(2.W))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(bypass_entries, Bool())))
  val wrbypass_idx       = RegInit(0.U.asTypeOf(Vec(bypass_entries, UInt(log2Up(bim_size).W))))
  val wrbypass_enq_ptr    = RegInit(0.U(log2Up(bypass_entries).W))

  val wrbypass_hits = VecInit((0 until bypass_entries).map( i =>
    !doing_reset && wrbypass_idx(i) === u_idx))
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  val oldCtrs = Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx), wrbypass_ctrs(wrbypass_hit_idx), update.meta)

  val newTaken = update.preds.taken
  val newCtrs = satUpdate(oldCtrs, 2, newTaken)

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
    wrbypass_enq_ptr := (wrbypass_enq_ptr + 1.U)(log2Up(bypass_entries)-1, 0)
  }

  bim.io.w.apply(
    valid = need_to_update.asUInt.orR || doing_reset,
    data = Mux(doing_reset, 2.U(2.W), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, 1.U(1.W), need_to_update)
  )
}
