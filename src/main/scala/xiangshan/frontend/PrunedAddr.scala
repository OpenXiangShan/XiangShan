// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.SignExt
import utility.ZeroExt
import xiangshan.XSBundle

class PrunedAddr(
    val length:       Int,
    val strictAssign: Boolean = true
)(implicit p: Parameters) extends XSBundle {
  val addr: UInt = UInt((length - instOffsetBits).W)

  def toUInt: UInt = Cat(addr, 0.U(instOffsetBits.W))

  def name: String =
    try this.pathName
    catch { case _: java.util.NoSuchElementException => "<unbound PrunedAddr>" }

  def signExt(targetWidth: Int): PrunedAddr = {
    require(targetWidth >= length)
    if (targetWidth == length) {
      println(s"PrunedAddr: unnecessary ${this.name}.signExt call")
    }
    PrunedAddrInit(SignExt(toUInt, targetWidth))
  }

  def zeroExt(targetWidth: Int): PrunedAddr = {
    require(targetWidth >= length)
    if (targetWidth == length) {
      println(s"PrunedAddr: unnecessary ${this.name}.zeroExt call")
    }
    PrunedAddrInit(ZeroExt(toUInt, targetWidth))
  }

  def truncate(targetWidth: Int): PrunedAddr = {
    require(targetWidth <= length)
    if (targetWidth == length) {
      println(s"PrunedAddr: unnecessary ${this.name}.truncate call")
    }
    PrunedAddrInit(toUInt(targetWidth - 1, 0))
  }

  def apply(x: Int): Bool = toUInt(x)

  def apply(x: Int, y: Int): UInt = toUInt(x, y)

  def :=(that: UInt): Unit = {
    assert(
      !strictAssign || length == that.getWidth,
      s"PrunedAddr: width mismatch when assigning ${this.name}: ${length} != ${that.getWidth}"
    )
    addr := that(length - 1, instOffsetBits)
  }

  def :=(that: PrunedAddr): Unit = {
    assert(
      !strictAssign || length == that.length,
      s"PrunedAddr: width mismatch when assigning ${this.name}: ${length} != ${that.length}"
    )
    addr := that.addr
  }

  // This method should only be used when offset is an immediate value
  def +(offset: UInt): PrunedAddr = PrunedAddrInit(toUInt + offset)

  def +(that: PrunedAddr): PrunedAddr = PrunedAddrInit(toUInt + that.toUInt)

  def -(that: PrunedAddr): PrunedAddr = PrunedAddrInit(toUInt - that.toUInt)

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

class Pc(_l: Int)(implicit p: Parameters) extends PrunedAddr(_l, strictAssign = true) {
  require(_l == VAddrBits)
  // Hack: we cannot do this(VAddrBits):
  //       methods are not accessible before parent class construction (thus in auxiliary constructor),
  //       here we use an empty XSBundle object to access VAddrBits
  def this()(implicit p: Parameters) = this(new XSBundle {}.VAddrBits)(p)

  override def +(offset: UInt):       Pc = PcInit(toUInt + offset)
  override def +(that:   PrunedAddr): Pc = PcInit(toUInt + that.toUInt)

  def signGuard: GuardedPc = this.signExt(GuardedVAddrBits).asTypeOf(new GuardedPc)
  def zeroGuard: GuardedPc = this.zeroExt(GuardedVAddrBits).asTypeOf(new GuardedPc)
  def guard(bit: Bool):      GuardedPc = Cat(bit, this.addr).asTypeOf(new GuardedPc)
  def guard(ref: GuardedPc): GuardedPc = guard(ref.getGuard)
}

object Pc {
  def apply()(implicit p: Parameters): Pc = new Pc
}

object PcInit {
  def apply(fullPc: UInt)(implicit p: Parameters): Pc = {
    val pc = Wire(new Pc)
    pc := fullPc
    pc
  }
}

class GuardedPc(_l: Int)(implicit p: Parameters) extends PrunedAddr(_l, strictAssign = true) {
  require(_l == GuardedVAddrBits)
  // Hack: we cannot do this(VAddrBits):
  //       methods are not accessible before parent class construction (thus in auxiliary constructor),
  //       here we use an empty XSBundle object to access VAddrBits
  def this()(implicit p: Parameters) = this(new XSBundle {}.GuardedVAddrBits)(p)

  override def +(offset: UInt):       GuardedPc = GuardedPcInit(toUInt + offset)
  override def +(that:   PrunedAddr): GuardedPc = GuardedPcInit(toUInt + that.toUInt)

  def getGuard: Bool = addr(_l - instOffsetBits - 1) // highest bit

  def unGuard: Pc = this.truncate(VAddrBits).asTypeOf(new Pc)
}

object GuardedPc {
  def apply()(implicit p: Parameters): GuardedPc = new GuardedPc
}

object GuardedPcInit {
  def apply(fullGuardedPc: UInt)(implicit p: Parameters): GuardedPc = {
    val pc = Wire(new GuardedPc)
    pc := fullGuardedPc
    pc
  }
}
