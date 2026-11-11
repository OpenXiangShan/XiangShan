/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import xiangshan.{Redirect, XSCoreParamsKey}

class RobPtr(entries: Int) extends CircularQueuePtr[RobPtr](
  entries
) with HasCircularQueuePtrHelper {

  val isFormer = Bool()

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).RobSize)

  def isSameEntry(that: RobPtr): Bool = this.flag === that.flag && this.value === that.value
  def isSameSlot(that: RobPtr): Bool = isSameEntry(that) && this.isFormer === that.isFormer

  def isAfterSlot(that: RobPtr): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value > that.value
    val sameEntry = this.flag === that.flag && this.value === that.value
    (differentFlag ^ compare) || (sameEntry && !this.isFormer && that.isFormer)
  }

  def isBeforeSlot(that: RobPtr): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value < that.value
    val sameEntry = this.flag === that.flag && this.value === that.value
    (differentFlag ^ compare) || (sameEntry && this.isFormer && !that.isFormer)
  }

  def isNotAfterSlot(that: RobPtr): Bool = isBeforeSlot(that) || isSameSlot(that)
  def isNotBeforeSlot(that: RobPtr): Bool = isAfterSlot(that) || isSameSlot(that)

  def addEntries(v: UInt): RobPtr = {
    val newPtr = Wire(new RobPtr(entries))
    if (isPow2(entries)) {
      val raw = Cat(this.flag, this.value) + v
      newPtr.flag := raw(PTR_WIDTH)
      newPtr.value := raw(PTR_WIDTH - 1, 0)
    } else {
      val newValue = this.value +& v
      val diff = Cat(0.U(1.W), newValue).asSInt - Cat(0.U(1.W), entries.U.asTypeOf(newValue)).asSInt
      val reverseFlag = diff >= 0.S
      newPtr.flag := Mux(reverseFlag, !this.flag, this.flag)
      newPtr.value := Mux(reverseFlag, diff.asUInt, newValue)
    }
    newPtr.isFormer := this.isFormer
    newPtr
  }

  def subEntries(v: UInt): RobPtr = {
    val flippedPtr = addEntries(entries.U - v)
    val newPtr = Wire(new RobPtr(entries))
    newPtr.flag := !flippedPtr.flag
    newPtr.value := flippedPtr.value
    newPtr.isFormer := this.isFormer
    newPtr
  }

  def asFormer: RobPtr = {
    val ptr = Wire(new RobPtr(entries))
    ptr := this
    ptr.isFormer := true.B
    ptr
  }

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && isSameSlot(redirect.bits.robIdx)
    redirect.valid && (flushItself || isAfterSlot(redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR

  def lineHeadPtr(implicit p: Parameters): RobPtr = {
    val CommitWidth = p(XSCoreParamsKey).CommitWidth
    val out = Wire(new RobPtr)
    out.flag := this.flag
    out.value := Cat(this.value(this.PTR_WIDTH-1, log2Up(CommitWidth)), 0.U(log2Up(CommitWidth).W))
    out.isFormer := true.B
    out
  }

  def isBeforeWithChannel(that: RobPtr, thisChannelIdx: UInt, thatChannelIdx: UInt): Bool = {
    isBeforeSlot(that) || isSameSlot(that) && (thisChannelIdx < thatChannelIdx)
  }

}

object RobPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RobPtr = {
    val ptr = Wire(new RobPtr)
    ptr.flag := f
    ptr.value := v
    ptr.isFormer := true.B
    ptr
  }
}
