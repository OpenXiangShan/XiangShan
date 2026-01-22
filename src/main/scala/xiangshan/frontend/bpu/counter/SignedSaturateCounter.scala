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

class SignedSaturateCounter(width: Int) extends SaturateCounter[SInt](width) {
  val value: SInt = SInt(width.W)

  /* *** state methods *** */
  /* example: 3bit
   * -4 -3 -2 -1  0  1  2  3
   * SN MN MN WN WP MP MP SP
   * S for Saturate, M for Mid, W for Weak, N for Negative, P for Positive
   */
  // direction
  def isPositive: Bool = value >= 0.S
  def isNegative: Bool = value < 0.S
  // saturated
  def isSaturatePositive: Bool = value === ((1 << (width - 1)) - 1).S
  def isSaturateNegative: Bool = value === (-(1 << (width - 1))).S
  // weak
  def isWeakPositive: Bool = { // value === 0.S
    require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
    value === 0.S
  }
  def isWeakNegative: Bool = { // value === -1.S
    require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
    value === -1.S
  }

  /* *** private update methods *** */
  protected def getUpdatedValue(positive: Bool, en: Bool): SInt =
    // hold if already saturated on the direction
    Mux(!en || shouldHold(positive), value, Mux(positive, value + 1.S, value - 1.S))

  protected def getIncreasedValue(step: UInt, en: Bool): SInt =
    if (step.isLit && step.litValue == 1) {
      Mux(!en || isSaturatePositive, value, value + 1.S)
    } else {
      Mux(
        !en,
        value,
        Mux(
          value +& step.zext >= SignedSaturateCounter.Value.SaturatePositive(width).S,
          SignedSaturateCounter.Value.SaturatePositive(width).S,
          value + step.zext
        )
      )
    }

  protected def getDecreasedValue(step: UInt, en: Bool): SInt =
    if (step.isLit && step.litValue == 1) {
      Mux(!en || isSaturateNegative, value, value - 1.S)
    } else {
      Mux(
        !en,
        value,
        Mux(
          value -& step.zext <= SignedSaturateCounter.Value.SaturateNegative(width).S,
          SignedSaturateCounter.Value.SaturateNegative(width).S,
          value - step.zext
        )
      )
    }

  /* *** update method for wires *** */
  def getUpdate(increase: Bool, en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getUpdatedValue(increase, en))

  def getIncrease(step: UInt = 1.U, en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getIncreasedValue(step, en))

  def getDecrease(step: UInt = 1.U, en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getDecreasedValue(step, en))

  /* *** reset method for regs *** */
  def resetZero(): Unit =
    value := 0.S

  def resetSaturatePositive(): Unit =
    value := SignedSaturateCounter.Value.SaturatePositive(this.width).S

  def resetSaturateNegative(): Unit =
    value := SignedSaturateCounter.Value.SaturateNegative(this.width).S

  def resetWeakPositive(): Unit =
    value := SignedSaturateCounter.Value.WeakPositive(this.width).S

  def resetWeakNegative(): Unit =
    value := SignedSaturateCounter.Value.WeakNegative(this.width).S
}

object SignedSaturateCounter {
  object Value {
    def SaturatePositive(width: Int): Int = (1 << (width - 1)) - 1
    def SaturateNegative(width: Int): Int = -(1 << (width - 1))
    def WeakPositive(width: Int): Int = {
      require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
      1
    }
    def WeakNegative(width: Int): Int = {
      require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
      -1
    }
  }

  def SaturatePositive(width: Int): SignedSaturateCounter =
    SignedSaturateCounterInit(width, Value.SaturatePositive(width))

  def SaturateNegative(width: Int): SignedSaturateCounter =
    SignedSaturateCounterInit(width, Value.SaturateNegative(width))

  def WeakPositive(width: Int): SignedSaturateCounter =
    SignedSaturateCounterInit(width, Value.WeakPositive(width))

  def WeakNegative(width: Int): SignedSaturateCounter =
    SignedSaturateCounterInit(width, Value.WeakNegative(width))

  // replace `0.U.asTypeOf(SignedSaturateCounter(width))` with `SignedSaturateCounter.Zero(width)`
  def Zero(width: Int): SignedSaturateCounter =
    SignedSaturateCounterInit(width, 0)
}

object SignedSaturateCounterInit {
  def apply(width: Int, init: Int): SignedSaturateCounter = {
    val counter = Wire(new SignedSaturateCounter(width))
    counter.value := init.S
    counter
  }

  def apply(width: Int, init: SInt): SignedSaturateCounter = {
    val counter = Wire(new SignedSaturateCounter(width))
    counter.value := init
    counter
  }
}

/**
 * trait to generate SignedSaturateCounter objects with width from Parameters
 *
 * @example {{{
 *   object ScThreshold extends SignedSaturateCounterFactory {
 *     def width(implicit p: Parameters): Int =
 *       p(XSCoreParamsKey).frontendParameters.bpuParameters.scParameters.ThresholdWidth
 *   }
 *
 *   class Sc(implicit p: Parameters) {
 *     val cnt: SignedSaturateCounter = Reg(ScThreshold())
 *     val cntWithReset: SignedSaturateCounter = RegInit(ScThreshold.SaturatePositive)
 *
 *     // ...
 *   }
 * }}}
 */
trait SignedSaturateCounterFactory {
  def width(implicit p: Parameters): Int

  def SaturatePositive(implicit p: Parameters): SignedSaturateCounter = SignedSaturateCounter.SaturatePositive(width)
  def SaturateNegative(implicit p: Parameters): SignedSaturateCounter = SignedSaturateCounter.SaturateNegative(width)

  def WeakPositive(implicit p: Parameters): SignedSaturateCounter = SignedSaturateCounter.WeakPositive(width)
  def WeakNegative(implicit p: Parameters): SignedSaturateCounter = SignedSaturateCounter.WeakNegative(width)

  def Zero(implicit p: Parameters): SignedSaturateCounter = SignedSaturateCounter.Zero(width)

  def apply()(implicit p: Parameters): SignedSaturateCounter = new SignedSaturateCounter(width)
}
