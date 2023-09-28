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
package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._


class VectorLoadWrapperIOBundle(implicit p: Parameters) extends VLSUBundle {
  // redirect
  val redirect    = Flipped(ValidIO(new Redirect))
  // input from load rs along with regfile src data
  val loadRegIn = Flipped(Decoupled(new ExuInput(isVpu = true)))

  // issue load to ldu
  val pipeIssue = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipeBundle()))
  // loads that fail and need to be replayed
  val pipeReplay = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new LsPipelineBundle())))
  // loads that succeed
  val pipeResult = Vec(VecLoadPipelineWidth, Flipped(DecoupledIO(new VecExuOutput())))

  // writeback uop results
  val uopWriteback = DecoupledIO(new ExuOutput(isVpu = true))
}

class VectorLoadWrapper(implicit p: Parameters) extends VLSUModule {

  val io = IO(new VectorLoadWrapperIOBundle())

  val uopQueue = Module(new VlUopQueue())
  val flowQueue = Module(new VlFlowQueue())

  uopQueue.io.redirect := io.redirect
  uopQueue.io.loadRegIn <> io.loadRegIn
  uopQueue.io.flowWriteback <> flowQueue.io.flowWriteback

  flowQueue.io.redirect := io.redirect
  flowQueue.io.flowIn <> uopQueue.io.flowIssue
  flowQueue.io.pipeReplay <> io.pipeReplay
  flowQueue.io.pipeResult <> io.pipeResult

  io.uopWriteback <> uopQueue.io.uopWriteback
  io.pipeIssue <> flowQueue.io.pipeIssue

}