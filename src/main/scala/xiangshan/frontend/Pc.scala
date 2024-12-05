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

import chisel3._
import chisel3.util._

class Pc(val length: Int) extends Bundle {
  val pc: UInt = UInt((length - 1).W)

  def apply(): UInt = Cat(pc, 0.U(1.W))

  def apply(x: Int): Bool = apply()(x)

  def apply(x: Int, y: Int): UInt = apply()(x, y)

  def :=(x: UInt): Unit = {
    assert(length == x.getWidth)
    pc := x(length - 1, 1)
  }

  def +(offset: UInt): UInt = apply() + offset

  def ===(that: Pc): Bool = {
    assert(length == that.length)
    pc === that.pc
  }

  def =/=(that: Pc): Bool = { // scalastyle:ignore method.name
    assert(length == that.length)
    pc =/= that.pc
  }
}

object Pc {
  def apply(length: Int): Pc = new Pc(length)
}

object PcInit {
  def apply(fullPc: UInt): Pc = {
    val pc = Wire(new Pc(fullPc.getWidth))
    pc := fullPc
    pc
  }
}
