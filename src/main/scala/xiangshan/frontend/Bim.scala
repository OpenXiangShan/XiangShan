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
import chisel3.experimental.chiselName
import chisel3.util._
import huancun.utils.SRAMTemplate
import utils._
import xiangshan._

trait BimParams extends HasXSParameter {
  val bimSize = 2048
  val bypassEntries = 4
}

@chiselName
class BIM(implicit p: Parameters) extends BasePredictor with BimParams with BPUUtils {
  val bimAddr = new TableAddr(log2Up(bimSize), 1)

  val bim = Module(new SRAMTemplate(UInt(2.W), set = bimSize, way=numBr, shouldReset = false))

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(bimSize).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (bimSize-1).U) { doing_reset := false.B }

  val s0_idx = bimAddr.getIdx(s0_pc)

  // bim.io.r.req.valid := io.s0_fire
  bim.io.r.req.valid := false.B
  bim.io.r.req.bits.setIdx := s0_idx

  io.in.ready := bim.io.r.req.ready
  io.s1_ready := bim.io.r.req.ready

  val s1_read = bim.io.r.resp.data

  io.out.resp := io.in.bits.resp_in(0)

  val s1_latch_taken_mask = VecInit(Cat((0 until numBr reverse).map(i => s1_read(i)(1))).asBools())
  val s1_latch_meta       = s1_read.asUInt()
  override val meta_size = s1_latch_meta.getWidth

  // io.out.resp.s1.full_pred.br_taken_mask := s1_latch_taken_mask
  // io.out.resp.s2.full_pred.br_taken_mask := RegEnable(s1_latch_taken_mask, 0.U.asTypeOf(Vec(numBr, Bool())), io.s1_fire)

  io.out.last_stage_meta := RegEnable(RegEnable(s1_latch_meta, io.s1_fire), io.s2_fire) // TODO: configurable with total-stages

  // Update logic
  val u_valid = RegNext(io.update.valid)
  val update = RegNext(io.update.bits)
  val u_idx = bimAddr.getIdx(update.pc)
  
  val update_mask = LowerMask(PriorityEncoderOH(update.full_pred.br_taken_mask.asUInt))
  val newCtrs = Wire(Vec(numBr, UInt(2.W)))
  val need_to_update = VecInit((0 until numBr).map(i => u_valid && update.ftb_entry.brValids(i) && update_mask(i)))


  // Bypass logic
  val wrbypass = Module(new WrBypass(UInt(2.W), bypassEntries, log2Up(bimSize), numWays = numBr))
  wrbypass.io.wen := need_to_update.reduce(_||_)
  wrbypass.io.write_idx := u_idx
  wrbypass.io.write_data := newCtrs
  wrbypass.io.write_way_mask.map(_ := need_to_update)

  val oldCtrs = 
    VecInit((0 until numBr).map(i =>
      Mux(wrbypass.io.hit && wrbypass.io.hit_data(i).valid,
        wrbypass.io.hit_data(i).bits,
        update.meta(2*i+1, 2*i))
    ))

  val newTakens = update.full_pred.br_taken_mask
  newCtrs := VecInit((0 until numBr).map(i =>
    satUpdate(oldCtrs(i), 2, newTakens(i))
  ))


  bim.io.w.apply(
    valid = false.B,
    // valid = need_to_update.asUInt.orR || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, Fill(numBr, 1.U(1.W)).asUInt(), need_to_update.asUInt())
  )

  val latch_s0_fire = RegNext(io.s0_fire)

  XSDebug(doing_reset, "Doing reset...\n")

  XSDebug(io.s0_fire, "req_pc=%x, req_idx=%d\n", s0_pc, s0_idx)

  for(i <- 0 until numBr) {
    XSDebug(latch_s0_fire, "last_cycle req %d: ctr=%b\n", i.U, s1_read(i))
  }

  XSDebug(u_valid, "update_pc=%x, update_idx=%d, is_br=%b\n", update.pc, u_idx, update.ftb_entry.brValids.asUInt)

  XSDebug(u_valid, "newTakens=%b\n", newTakens.asUInt)

  for(i <- 0 until numBr) {
    XSDebug(u_valid, "oldCtrs%d=%b\n", i.U, oldCtrs(i))
  }

  for(i <- 0 until numBr) {
    XSDebug(u_valid, "newCtrs%d=%b\n", i.U, newCtrs(i))
  }

}
