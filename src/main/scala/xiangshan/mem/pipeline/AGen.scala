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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles._

object AGen {
  class AGenModule(bits: Int) extends Module {
    val io = IO(new Bundle() {
      val src0 = Input(UInt(bits.W))
      val src1 = Input(UInt(bits.W))
      val out  = Output(UInt(bits.W))
    })
    io.out := io.src0 + SignExt(io.src1(11, 0), bits)
  }

  def apply(src0: UInt, src1: UInt, bits: Int): UInt = {
    val mod = Module(new AGenModule(bits))
    mod.io.src0 := src0
    mod.io.src1 := src1
    mod.io.out
  }
}