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
import xiangshan._
import utils._

class Composer(implicit p: Parameters) extends BasePredictor with HasBPUConst {
  val (components, resp) = getBPDComponents(io.in.bits.resp_in(0), p, EnableSC)
  io.out.resp := resp

  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.in.valid           := io.in.valid
    c.io.in.bits.s0_pc      := io.in.bits.s0_pc
    c.io.in.bits.ghist      := io.in.bits.ghist
    c.io.in.bits.phist      := io.in.bits.phist

    c.io.s0_fire := io.s0_fire
    c.io.s1_fire := io.s1_fire
    c.io.s2_fire := io.s2_fire
    c.io.s3_fire := io.s3_fire

    c.io.redirect := io.redirect

    if (c.meta_size > 0) {
      metas = (metas << c.meta_size) | c.io.out.s3_meta(c.meta_size-1,0)
    }
    meta_sz = meta_sz + c.meta_size
  }

  io.in.ready := components.map(_.io.s1_ready).reduce(_ && _)

  io.s1_ready := components.map(_.io.s1_ready).reduce(_ && _)
  io.s2_ready := components.map(_.io.s2_ready).reduce(_ && _)
  io.s3_ready := components.map(_.io.s3_ready).reduce(_ && _)

  require(meta_sz < MaxMetaLength)
  io.out.s3_meta := metas

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
}