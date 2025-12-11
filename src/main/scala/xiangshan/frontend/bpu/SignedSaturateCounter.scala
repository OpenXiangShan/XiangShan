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

class SignedSaturateCounter(width: Int) extends Bundle {
  val value: SInt = SInt(width.W)

  // === is defined in Bundle, but =/= is not
  def =/=(that: SignedSaturateCounter): Bool = this.value =/= that.value // scalastyle:ignore method.name
  def <(that:   SignedSaturateCounter): Bool = this.value < that.value
  def <=(that:  SignedSaturateCounter): Bool = this.value <= that.value
  def >(that:   SignedSaturateCounter): Bool = this.value > that.value
  def >=(that:  SignedSaturateCounter): Bool = this.value >= that.value

  /* *** state methods *** */
  // direction
  def isPositive: Bool = value >= 0.S
  def isNegative: Bool = value < 0.S
  // saturated
  def isSaturatePositive: Bool = value === ((1 << (width - 1)) - 1).S
  def isSaturateNegative: Bool = value === (-(1 << (width - 1))).S
  def isSaturate:         Bool = isSaturatePositive || isSaturateNegative
  def shouldHold(positive: Bool): Bool =
    isSaturatePositive && positive || isSaturateNegative && !positive

  // weak
  def isWeakPositive: Bool = { // value === 1.S
    require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
    value === 1.S
  }
  def isWeakNegative: Bool = { // value === -1.S
    require(width >= 2, "SignedSaturateCounter width must be at least 2 to have weak states")
    value === -1.S
  }
  def isWeak: Bool = isWeakPositive || isWeakNegative
  // medium
  def isMid: Bool = {
    require(width >= 3, "SaturateCounter width must be at least 3 to have mid states")
    !isSaturate && !isWeak
  }

  /* *** private update methods *** */
  private def getUpdatedValue(positive: Bool, en: Bool): SInt =
    // hold if already saturated on the direction
    Mux(!en || shouldHold(positive), value, Mux(positive, value + 1.S, value - 1.S))

  private def getIncreasedValue(en: Bool): SInt =
    Mux(!en || isSaturatePositive, value, value + 1.S)

  private def getDecreasedValue(en: Bool): SInt =
    Mux(!en || isSaturateNegative, value, value - 1.S)

  /* *** update method for wires *** */
  def getUpdate(increase: Bool, en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getUpdatedValue(increase, en))

  def getIncrease(en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getIncreasedValue(en))

  def getDecrease(en: Bool = true.B): SignedSaturateCounter =
    SignedSaturateCounterInit(this.width, getDecreasedValue(en))

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
