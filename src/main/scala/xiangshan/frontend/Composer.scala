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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

class Composer(implicit p: Parameters) extends BasePredictor with HasBPUConst with HasPerfEvents {
  val (components, resp) = getBPDComponents(io.in.bits.resp_in(0), p)
  io.out := resp
  // shorter path for s1 pred
  val all_fast_pred = components.filter(_.is_fast_pred)
  require(all_fast_pred.length <= 1)
  if (all_fast_pred.length == 1) {
    val fast_pred = all_fast_pred(0)
    println("[composer] bypassing output of fast pred: " + fast_pred.name)
    io.out.s1 := fast_pred.io.out.s1
  }

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.reset_vector        := io.reset_vector
    c.io.in.valid            := io.in.valid
    c.io.in.bits.s0_pc       := io.in.bits.s0_pc
    c.io.in.bits.folded_hist := io.in.bits.folded_hist
    c.io.in.bits.ghist       := io.in.bits.ghist

    c.io.s0_fire := io.s0_fire
    c.io.s1_fire := io.s1_fire
    c.io.s2_fire := io.s2_fire
    c.io.s3_fire := io.s3_fire

    c.io.s2_redirect := io.s2_redirect
    c.io.s3_redirect := io.s3_redirect

    c.io.redirect := io.redirect
    c.io.ctrl := DelayN(io.ctrl, 1)

    if (c.meta_size > 0) {
      metas = (metas << c.meta_size) | c.io.out.last_stage_meta(c.meta_size-1,0)
    }
    meta_sz = meta_sz + c.meta_size
  }
  println(s"total meta size: $meta_sz\n\n")


  io.in.ready := components.map(_.io.s1_ready).reduce(_ && _)

  io.s1_ready := components.map(_.io.s1_ready).reduce(_ && _)
  io.s2_ready := components.map(_.io.s2_ready).reduce(_ && _)

  require(meta_sz < MaxMetaLength)
  io.out.last_stage_meta := metas

  var update_meta = io.update.bits.meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    update_meta = update_meta >> c.meta_size
  }

  def extractMeta(meta: UInt, idx: Int): UInt = {
    var update_meta = meta
    var metas: Seq[UInt] = Nil
    for (c <- components.reverse) {
      metas = metas :+ update_meta
      update_meta = update_meta >> c.meta_size
    }
    metas(idx)
  }

  override def getFoldedHistoryInfo = Some(components.map(_.getFoldedHistoryInfo.getOrElse(Set())).reduce(_++_))

  override val perfEvents = components.map(_.getPerfEvents).reduce(_++_)
  generatePerfEvent()
}
