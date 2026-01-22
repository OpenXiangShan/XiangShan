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

package xiangshan.frontend.bpu.counter

import chisel3._

abstract class SaturateCounter[T <: Bits with Num[T]](width: Int) // scalastyle:ignore number.of.methods
  extends Bundle {
  val value: T

  def ===(that: SaturateCounter[T]): Bool = this.value === that.value
  def =/=(that: SaturateCounter[T]): Bool = !(this.value === that.value) // scalastyle:ignore method.name
  def <(that:   SaturateCounter[T]): Bool = this.value < that.value
  def <=(that:  SaturateCounter[T]): Bool = this.value <= that.value
  def >(that:   SaturateCounter[T]): Bool = this.value > that.value
  def >=(that:  SaturateCounter[T]): Bool = this.value >= that.value

  // direction
  def isPositive: Bool // implement in subclass
  def isNegative: Bool // implement in subclass
  // saturated
  def isSaturatePositive: Bool // implement in subclass
  def isSaturateNegative: Bool // implement in subclass
  def isSaturate:         Bool = isSaturatePositive || isSaturateNegative
  def shouldHold(increase: Bool): Bool =
    isSaturatePositive && increase || isSaturateNegative && !increase
  // weak
  def isWeakPositive: Bool // implement in subclass
  def isWeakNegative: Bool // implement in subclass
  def isWeak:         Bool = isWeakPositive || isWeakNegative
  // medium
  def isMid: Bool = {
    require(width >= 3, "SaturateCounter width must be at least 3 to have mid states")
    !isSaturate && !isWeak
  }

  /* *** private update methods *** */
  protected def getUpdatedValue(increase: Bool, en: Bool): T // implement in subclass

  protected def getIncreasedValue(step: UInt, en: Bool): T // implement in subclass

  protected def getDecreasedValue(step: UInt, en: Bool): T // implement in subclass

  /* *** update method for wires *** */
  def getUpdate(increase: Bool, en: Bool = true.B): SaturateCounter[T] // implement in subclass

  def getIncrease(step: UInt = 1.U, en: Bool = true.B): SaturateCounter[T] // implement in subclass

  def getDecrease(step: UInt = 1.U, en: Bool = true.B): SaturateCounter[T] // implement in subclass

  /* *** update method for regs *** */
  def selfUpdate(increase: Bool, en: Bool = true.B): Unit =
    value := getUpdatedValue(increase, en)

  def selfIncrease(step: UInt = 1.U, en: Bool = true.B): Unit =
    value := getIncreasedValue(step, en)

  def selfDecrease(step: UInt = 1.U, en: Bool = true.B): Unit =
    value := getDecreasedValue(step, en)

  def resetZero(): Unit // implement in subclass

  def resetSaturatePositive(): Unit // implement in subclass

  def resetSaturateNegative(): Unit // implement in subclass

  def resetWeakPositive(): Unit // implement in subclass

  def resetWeakNegative(): Unit // implement in subclass
}
