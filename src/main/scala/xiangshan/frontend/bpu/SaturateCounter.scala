// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.bpu

import chisel3._
import org.chipsalliance.cde.config.Parameters

class SaturateCounter(width: Int) extends Bundle {
  val value: UInt = UInt(width.W)

  // === is defined in Bundle, but =/=, <, >, etc. are not
  def =/=(that: SaturateCounter): Bool = this.value =/= that.value // scalastyle:ignore method.name
  def <(that:   SaturateCounter): Bool = this.value < that.value
  def <=(that:  SaturateCounter): Bool = this.value <= that.value
  def >(that:   SaturateCounter): Bool = this.value > that.value
  def >=(that:  SaturateCounter): Bool = this.value >= that.value

  /* *** state methods *** */
  // direction
  def isPositive: Bool = value(width - 1)  // value >= (1 << (width - 1)).U
  def isNegative: Bool = !value(width - 1) // value < (1 << (width - 1)).U
  // saturated
  def isSaturatePositive: Bool = value.andR // value === ((1 << width) - 1).U
  def isSaturateNegative: Bool = !value.orR // value === 0.U
  def isSaturate:         Bool = isSaturatePositive || isSaturateNegative
  def shouldHold(increase: Bool): Bool =
    isSaturatePositive && increase || isSaturateNegative && !increase
  // weak
  def isWeakPositive: Bool = { // value === (1 << (width - 1))
    require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
    isPositive && !value(width - 2, 0).orR
  }
  def isWeakNegative: Bool = { // value === (1 << (width - 1)) - 1
    require(width >= 2, "SaturateCounter width must be at least 2 to have weak states")
    isNegative && value(width - 2, 0).andR
  }
  def isWeak: Bool = isWeakPositive || isWeakNegative
  // medium
  def isMid: Bool = {
    require(width >= 3, "SaturateCounter width must be at least 3 to have mid states")
    !isSaturate && !isWeak
  }

  /* *** private update methods *** */
  private def getUpdatedValue(increase: Bool, en: Bool): UInt =
    // hold if not enabled or already saturated on the direction
    Mux(!en || shouldHold(increase), value, Mux(increase, value + 1.U, value - 1.U))

  private def getIncreasedValue(en: Bool): UInt =
    Mux(!en || isSaturatePositive, value, value + 1.U)

  private def getDecreasedValue(en: Bool): UInt =
    Mux(!en || isSaturateNegative, value, value - 1.U)

  /* *** update method for wires *** */
  def getUpdate(increase: Bool, en: Bool = true.B): SaturateCounter =
    SaturateCounterInit(this.width, getUpdatedValue(increase, en))

  def getIncrease(en: Bool = true.B): SaturateCounter =
    SaturateCounterInit(this.width, getIncreasedValue(en))

  def getDecrease(en: Bool = true.B): SaturateCounter =
    SaturateCounterInit(this.width, getDecreasedValue(en))

  /* *** update method for regs *** */
  def selfUpdate(increase: Bool, en: Bool = true.B): Unit =
    value := getUpdatedValue(increase, en)

  def selfIncrease(en: Bool = true.B): Unit =
    value := getIncreasedValue(en)

  def selfDecrease(en: Bool = true.B): Unit =
    value := getDecreasedValue(en)

  /* *** reset method for regs *** */
  def resetZero(): Unit =
    value := 0.U

  def resetSaturatePositive(): Unit =
    value := SaturateCounter.Value.SaturatePositive(this.width).U

  def resetSaturateNegative(): Unit =
    value := SaturateCounter.Value.SaturateNegative(this.width).U

  def resetWeakPositive(): Unit =
    value := SaturateCounter.Value.WeakPositive(this.width).U

  def resetWeakNegative(): Unit =
    value := SaturateCounter.Value.WeakNegative(this.width).U
}

object SaturateCounter {
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

  def SaturatePositive(width: Int): SaturateCounter =
    SaturateCounterInit(width, Value.SaturatePositive(width))

  def SaturateNegative(width: Int): SaturateCounter =
    SaturateCounterInit(width, Value.SaturateNegative(width))

  def WeakPositive(width: Int): SaturateCounter =
    SaturateCounterInit(width, Value.WeakPositive(width))

  def WeakNegative(width: Int): SaturateCounter =
    SaturateCounterInit(width, Value.WeakNegative(width))

  // replace `0.U.asTypeOf(SaturateCounter(width))` with `SaturateCounter.Zero(width)`
  def Zero(width: Int): SaturateCounter =
    SaturateCounterInit(width, 0)
}

object SaturateCounterInit {
  def apply(width: Int, init: Int): SaturateCounter = {
    val counter = Wire(new SaturateCounter(width))
    counter.value := init.U
    counter
  }

  def apply(width: Int, init: UInt): SaturateCounter = {
    val counter = Wire(new SaturateCounter(width))
    counter.value := init
    counter
  }
}

/**
 * trait to generate SaturateCounter objects with width from Parameters
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
trait SaturateCounterFactory {
  def width(implicit p: Parameters): Int

  def SaturatePositive(implicit p: Parameters): SaturateCounter = SaturateCounter.SaturatePositive(width)
  def SaturateNegative(implicit p: Parameters): SaturateCounter = SaturateCounter.SaturateNegative(width)

  def WeakPositive(implicit p: Parameters): SaturateCounter = SaturateCounter.WeakPositive(width)
  def WeakNegative(implicit p: Parameters): SaturateCounter = SaturateCounter.WeakNegative(width)

  def Zero(implicit p: Parameters): SaturateCounter = SaturateCounter.Zero(width)

  def apply()(implicit p: Parameters): SaturateCounter = new SaturateCounter(width)
}
