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
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle

class PrunedAddr(val length: Int)(implicit p: Parameters) extends XSBundle {
  val addr: UInt = UInt((length - instOffsetBits).W)

  def toUInt: UInt = Cat(addr, 0.U(instOffsetBits.W))

  def apply(x: Int): Bool = toUInt(x)

  def apply(x: Int, y: Int): UInt = toUInt(x, y)

  def :=(UIntAddr: UInt): Unit = {
    assert(length == UIntAddr.getWidth)
    addr := UIntAddr(length - 1, instOffsetBits)
  }

  def +(offset: UInt): PrunedAddr = PrunedAddrInit(toUInt + offset)

  def -(offset: UInt): PrunedAddr = PrunedAddrInit(toUInt - offset)

  def -(that: PrunedAddr): UInt = toUInt - that.toUInt

  def >>(offset: Int): UInt = (toUInt >> offset).asUInt

  def ===(that: PrunedAddr): Bool = {
    assert(length == that.length)
    addr === that.addr
  }

  def =/=(that: PrunedAddr): Bool = { // scalastyle:ignore method.name
    assert(length == that.length)
    addr =/= that.addr
  }

  def >=(that: PrunedAddr): Bool = {
    assert(length == that.length)
    addr >= that.addr
  }
}

object PrunedAddr {
  def apply(length: Int)(implicit p: Parameters): PrunedAddr = new PrunedAddr(length)
}

object PrunedAddrInit {
  def apply(fullAddr: UInt)(implicit p: Parameters): PrunedAddr = {
    val address = Wire(new PrunedAddr(fullAddr.getWidth))
    address := fullAddr
    address
  }
}
