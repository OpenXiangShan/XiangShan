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
import org.chipsalliance.cde.config.Parameters

class UnsignedSaturateCounter(width: Int) extends SaturateCounter[UInt](width) {
  val value: UInt = UInt(width.W)

  /* *** state methods *** */
  /* example: 3bit
   *  0  1  2  3  4  5  6  7
   * SN MN MN WN WP MP MP SP
   * S for Saturate, M for Mid, W for Weak, N for Negative, P for Positive
   */
  // direction
  def isPositive: Bool = value(width - 1)  // value >= (1 << (width - 1)).U
  def isNegative: Bool = !value(width - 1) // value < (1 << (width - 1)).U
  // saturated
  def isSaturatePositive: Bool = value.andR // value === ((1 << width) - 1).U
  def isSaturateNegative: Bool = !value.orR // value === 0.U
  // weak
  def isWeakPositive: Bool = { // value === (1 << (width - 1))
    require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
    isPositive && !value(width - 2, 0).orR
  }
  def isWeakNegative: Bool = { // value === (1 << (width - 1)) - 1
    require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
    isNegative && value(width - 2, 0).andR
  }

  /* *** private update methods *** */
  protected def getUpdatedValue(increase: Bool, en: Bool): UInt =
    // hold if not enabled or already saturated on the direction
    Mux(!en || shouldHold(increase), value, Mux(increase, value + 1.U, value - 1.U))

  protected def getIncreasedValue(step: UInt, en: Bool): UInt =
    if (step.isLit && step.litValue == 1) {
      Mux(!en || isSaturatePositive, value, value + 1.U)
    } else {
      Mux(
        !en,
        value,
        Mux(
          value +& step >= UnsignedSaturateCounter.Value.SaturatePositive(width).U,
          UnsignedSaturateCounter.Value.SaturatePositive(width).U,
          value + step
        )
      )
    }

  protected def getDecreasedValue(step: UInt, en: Bool): UInt =
    if (step.isLit && step.litValue == 1) {
      Mux(!en || isSaturateNegative, value, value - 1.U)
    } else {
      Mux(
        !en,
        value,
        Mux(
          value <= step,
          0.U,
          value - step
        )
      )
    }

  /* *** update method for wires *** */
  def getUpdate(increase: Bool, en: Bool = true.B): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(this.width, getUpdatedValue(increase, en))

  def getIncrease(step: UInt = 1.U, en: Bool = true.B): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(this.width, getIncreasedValue(step, en))

  def getDecrease(step: UInt = 1.U, en: Bool = true.B): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(this.width, getDecreasedValue(step, en))

  /* *** reset method for regs *** */
  def resetZero(): Unit =
    value := 0.U

  def resetSaturatePositive(): Unit =
    value := UnsignedSaturateCounter.Value.SaturatePositive(this.width).U

  def resetSaturateNegative(): Unit =
    value := UnsignedSaturateCounter.Value.SaturateNegative(this.width).U

  def resetWeakPositive(): Unit =
    value := UnsignedSaturateCounter.Value.WeakPositive(this.width).U

  def resetWeakNegative(): Unit =
    value := UnsignedSaturateCounter.Value.WeakNegative(this.width).U
}

object UnsignedSaturateCounter {
  object Value {
    def SaturatePositive(width: Int): Int = (1 << width) - 1
    def SaturateNegative(width: Int): Int = 0
    def WeakPositive(width: Int): Int = {
      require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
      1 << (width - 1)
    }
    def WeakNegative(width: Int): Int = {
      require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
      (1 << (width - 1)) - 1
    }
  }

  def SaturatePositive(width: Int): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, Value.SaturatePositive(width))

  def SaturateNegative(width: Int): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, Value.SaturateNegative(width))

  def WeakPositive(width: Int): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, Value.WeakPositive(width))

  def WeakNegative(width: Int): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, Value.WeakNegative(width))

  // replace `0.U.asTypeOf(SaturateCounter(width))` with `SaturateCounter.Zero(width)`
  def Zero(width: Int): UnsignedSaturateCounter =
    UnsignedSaturateCounterInit(width, 0)
}

object UnsignedSaturateCounterInit {
  def apply(width: Int, init: Int): UnsignedSaturateCounter = {
    val counter = Wire(new UnsignedSaturateCounter(width))
    counter.value := init.U
    counter
  }

  def apply(width: Int, init: UInt): UnsignedSaturateCounter = {
    val counter = Wire(new UnsignedSaturateCounter(width))
    counter.value := init
    counter
  }
}

/**
 * trait to generate UnsignedSaturateCounter objects with width from Parameters
 *
 * @example {{{
 *   object TageTakenCounter extends SaturateCounterFactory {
 *     def width(implicit p: Parameters): Int =
 *       p(XSCoreParamsKey).frontendParameters.bpuParameters.tageParameters.TakenCntWidth
 *   }
 *
 *   class Tage(implicit p: Parameters) {
 *     val cnt: SaturateCounter = Reg(TageTakenCounter())
 *     val cntWithReset: SaturateCounter = RegInit(TageTakenCounter.WeakPositive)
 *
 *     // ...
 *   }
 * }}}
 */
trait UnsignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int

  def SaturatePositive(implicit p: Parameters): UnsignedSaturateCounter = UnsignedSaturateCounter.SaturatePositive(width)
  def SaturateNegative(implicit p: Parameters): UnsignedSaturateCounter = UnsignedSaturateCounter.SaturateNegative(width)

  def WeakPositive(implicit p: Parameters): UnsignedSaturateCounter = UnsignedSaturateCounter.WeakPositive(width)
  def WeakNegative(implicit p: Parameters): UnsignedSaturateCounter = UnsignedSaturateCounter.WeakNegative(width)

  def Zero(implicit p: Parameters): UnsignedSaturateCounter = UnsignedSaturateCounter.Zero(width)

  def apply()(implicit p: Parameters): UnsignedSaturateCounter = new UnsignedSaturateCounter(width)
}
