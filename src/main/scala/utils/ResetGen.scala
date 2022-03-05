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

class ResetGen extends Module {
  val io = IO(new Bundle() {
    val out = Output(Bool())
  })

  io.out := RegNext(RegNext(reset.asBool))
}

trait ResetNode

case class ModuleNode(mod: MultiIOModule) extends ResetNode

case class ResetGenNode(children: Seq[ResetNode]) extends ResetNode

object ResetGen {

  def apply(resetTree: ResetNode, reset: Bool, sim: Boolean): Unit = {
    if(!sim) {
      resetTree match {
        case ModuleNode(mod) =>
          mod.reset := reset
        case ResetGenNode(children) =>
          val next_rst = Wire(Bool())
          withReset(reset){
            val resetGen = Module(new ResetGen)
            next_rst := resetGen.io.out
          }
          children.foreach(child => apply(child, next_rst, sim))
      }
    }
  }

  def apply(resetChain: Seq[Seq[MultiIOModule]], reset: Bool, sim: Boolean): Seq[Bool] = {
    val resetReg = Wire(Vec(resetChain.length + 1, Bool()))
    resetReg.foreach(_ := reset)
    for ((resetLevel, i) <- resetChain.zipWithIndex) {
      if (!sim) {
        withReset(resetReg(i)) {
          val resetGen = Module(new ResetGen)
          resetReg(i + 1) := resetGen.io.out
        }
      }
      resetLevel.foreach(_.reset := resetReg(i + 1))
    }
    resetReg.tail
  }
}
