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

package utils

import chisel3._
import chisel3.util._

// Async reset requires carefully synchronization of the reset deassertion.
class RST_SYNC_NO_DFT(SYNC_NUM: Int = 2) extends Module {
  val o_reset = IO(Output(AsyncReset()))

  val pipe_reset = RegInit(((1L << SYNC_NUM) - 1).U(SYNC_NUM.W))
  pipe_reset := Cat(pipe_reset(SYNC_NUM - 2, 0), 0.U(1.W))

  // deassertion of the reset needs to be synchronized.
  o_reset := pipe_reset(SYNC_NUM - 1).asAsyncReset
}

object RST_SYNC_NO_DFT {
  def apply(SYNC_NUM: Int = 2): AsyncReset = {
    val resetSync = Module(new RST_SYNC_NO_DFT(SYNC_NUM))
    resetSync.o_reset
  }
}

trait ResetNode

case class ModuleNode(mod: Module) extends ResetNode

case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {

  def apply(resetTree: ResetNode, reset: Reset, sim: Boolean): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Reset())
          withReset(reset){
            next_rst := RST_SYNC_NO_DFT()
          }
          children.foreach(child => apply(child, next_rst, sim))
      }
    }
  }

  def apply(resetChain: Seq[Seq[Reset]], reset: Reset, sim: Boolean): Seq[Reset] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Reset()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          resetReg(i + 1) := RST_SYNC_NO_DFT()
        }
      }
      resetLevel.foreach(_ := resetReg(i + 1))
    }
    resetReg.tail
  }
}
