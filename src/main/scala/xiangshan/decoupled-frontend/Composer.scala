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

class Composer(implicit p: Parameters) extends BasePredictor with HasBPUConst {
  val (components, resp) = getBPDComponents(io.in.bits.resp_in(0), p)
  io.out.bits.resp := resp

  val s0_pc_next = RegNext(io.in.bits.s0_pc)

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.in.valid           := io.in.valid
    c.io.in.bits.s0_pc      := io.in.bits.s0_pc
    c.io.in.bits.ghist      := io.in.bits.ghist
    c.io.in.bits.toFtq_fire := io.in.bits.toFtq_fire
    if (c.meta_size > 0) {
      metas = (metas << c.meta_size) | c.io.out.bits.meta(c.meta_size-1,0)
    }
    meta_sz = meta_sz + c.meta_size
  }

  val overrideFlush = RegInit(0.U.asTypeOf(Valid(UInt(VAddrBits.W))))
  when(overrideFlush.valid) { overrideFlush := 0.U.asTypeOf(Valid(UInt(VAddrBits.W))) }

  val s0_all_ready = components.map(_.io.s0_ready).reduce(_ && _) && io.out.ready
  val s0_fire = io.in.valid && s0_all_ready
  io.in.ready := s0_all_ready
  components.foreach(_.io.s0_fire := s0_fire)

  val s1_valid = RegInit(false.B)
  val s1_all_ready = components.map(_.io.s1_ready).reduce(_ && _) && io.out.ready
  val s1_fire = s1_valid && s1_all_ready

  when(s0_fire)             { s1_valid := true.B }
  .elsewhen(io.flush.valid || overrideFlush.valid) { s1_valid := false.B }
  .elsewhen(s1_fire)        { s1_valid := false.B }

  components.foreach(_.io.s1_fire := s1_fire)

  val s2_valid = RegInit(false.B)
  val s2_all_ready = components.map(_.io.s2_ready).reduce(_ && _) && io.out.ready
  val s2_fire = s2_valid && s2_all_ready

  when(io.flush.valid || overrideFlush.valid)  { s2_valid := false.B }
  .elsewhen(s1_fire)    { s2_valid := true.B }
  .elsewhen(s2_fire)    { s2_valid := false.B }

  components.foreach(_.io.s2_fire := s2_fire)

  val s3_valid = RegInit(false.B)
  val s3_all_ready = components.map(_.io.s3_ready).reduce(_ && _)
  val s3_fire = s3_valid && s3_all_ready

  when(io.flush.valid || overrideFlush.valid)  { s3_valid := false.B }
  .elsewhen(s2_fire)    { s3_valid := true.B }
  .elsewhen(s3_fire)    { s3_valid := false.B }

  components.foreach(_.io.s3_fire := s3_fire)

  io.out.valid := components(2).io.out.valid

  components.map(_.io.flush := Mux(io.flush.valid, io.flush, overrideFlush))

  // predictor override redirect
  val finalPredValid = components(2).io.out.valid
  val finalPredResp = components(2).io.out.bits.resp
  when(finalPredValid) {
    when(finalPredResp.s2.preds.target =/= s0_pc_next) {
      overrideFlush.valid := true.B
      overrideFlush.bits := finalPredResp.s2.preds.target
    }
  }

  when(io.redirect.valid) {
    s0_pc := io.redirect.bits.cfiUpdate.target
  }


  require(meta_sz < MaxMetaLength)
  io.out.bits.meta := metas


  var update_meta = io.update.bits.meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    update_meta = update_meta >> c.meta_size
  }
}