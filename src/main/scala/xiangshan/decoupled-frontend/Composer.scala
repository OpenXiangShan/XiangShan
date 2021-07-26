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

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.in.valid           := io.in.valid
    c.io.in.bits.s0_pc      := io.in.bits.s0_pc
    c.io.in.bits.ghist      := io.in.bits.ghist
    c.io.in.bits.toFtq_fire := io.in.bits.toFtq_fire
    if (c.meta_size > 0) {
      metas = (metas << c.meta_size) | c.io.out.bits.s3_meta(c.meta_size-1,0)
    }
    meta_sz = meta_sz + c.meta_size
  }

  val s1_flush, s2_flush, s3_flush = Wire(Bool())
  val s2_redirect, s3_redirect = Wire(Bool())

  s2_redirect := false.B
  s3_redirect := false.B

  s3_flush := false.B
  s2_flush := io.redirect.valid
  s1_flush := s2_flush || s2_redirect

  val s1_fire = Wire(Bool())
  val s1_valid = RegInit(false.B)
  val s1_components_ready = components.map(_.io.s1_ready).reduce(_ && _)
  val s1_ready = s1_fire || !s1_valid
  val s0_fire = io.in.valid && s1_components_ready && s1_ready
  io.in.ready := s1_components_ready && s1_ready
  components.foreach(_.io.s0_fire := s0_fire)

  val s2_fire = Wire(Bool())
  val s2_valid = RegInit(false.B)
  val s2_components_ready = components.map(_.io.s2_ready).reduce(_ && _)
  val s2_ready = s2_fire || !s2_valid
  s1_fire := s1_valid && s2_components_ready && s2_ready

  when(s0_fire)       { s1_valid := true.B  }
  .elsewhen(s1_flush) { s1_valid := false.B }
  .elsewhen(s1_fire)  { s1_valid := false.B }

  components.foreach(_.io.s1_fire := s1_fire)

  val s3_fire = Wire(Bool())
  val s3_valid = RegInit(false.B)
  val s3_components_ready = components.map(_.io.s3_ready).reduce(_ && _)
  val s3_ready = s3_fire || !s3_valid
  s2_fire := s2_valid && s3_components_ready && s3_ready

  when(s2_flush)                  { s2_valid := false.B }
  .elsewhen(s1_fire && !s1_flush) { s2_valid := true.B  }
  .elsewhen(s2_fire)              { s2_valid := false.B }

  components.foreach(_.io.s2_fire := s2_fire)

  s3_fire := s3_valid && io.out.ready

  when(s3_flush)                  { s3_valid := false.B }
  .elsewhen(s2_fire && !s2_flush) { s3_valid := true.B  }
  .elsewhen(s3_fire)              { s3_valid := false.B }

  components.foreach(_.io.s3_fire := s3_fire)

  io.out.valid := s3_fire && !io.redirect.valid // TODO: Delete it

  // predictor override redirect
  val finalPredValid = components(2).io.out.valid
  val finalPredResp = components(2).io.out.bits.resp
  when(finalPredValid) {
    when(finalPredResp.s2.preds.target =/= RegEnable(s0_pc, s0_fire)) {
      s2_redirect := true.B
      io.flush_out.valid := true.B
      io.flush_out.bits  := finalPredResp.s2.preds.target
    }

    // when(finalPredResp.s3.preds.target =/= s2_pc) {
    //   s3_redirect := true.B
    //   io.out.valid := true.B
    //   io.out.bits  := finalPredResp.s3.preds.target
    // }
  }

  when(io.redirect.valid) {
    s0_pc := io.redirect.bits.cfiUpdate.target
  }


  require(meta_sz < MaxMetaLength)
  io.out.bits.s3_meta := metas


  var update_meta = io.update.bits.meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    update_meta = update_meta >> c.meta_size
  }
}