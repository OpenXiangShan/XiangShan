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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.scalatest.flatspec.AnyFlatSpec
import utils.EnumUInt
import scala.util.Random
import scala.math.pow

class SignedSaturateCounterTest extends AnyFlatSpec {
  object OP extends EnumUInt(3) {
    def Inc: UInt = 0.U(width.W)
    def Dec: UInt = 1.U(width.W)
    def Upd: UInt = 2.U(width.W)
  }

  class TestModule(width: Int) extends Module {
    class ControlIO extends Bundle {
      val en: Bool = Input(Bool())
      val op: UInt = Input(OP())
      val step: UInt = Input(UInt(width.W)) // for inc/dec
      val inc: Bool = Input(Bool()) // for upd
    }
    val ctrl: ControlIO = IO(new ControlIO)

    class QueryIO extends Bundle {
      class ShouldHold extends Bundle {
        val increase: Bool = Bool()
      }
      val shouldHold: ShouldHold = Input(new ShouldHold)
    }
    val query: QueryIO = IO(new QueryIO)

    class ResultIO extends Bundle {
      val value: SInt = Output(SInt(width.W))
      // direction
      val isPositive: Bool = Output(Bool())
      val isNegative: Bool = Output(Bool())
      // saturated
      val isSaturatePositive: Bool = Output(Bool())
      val isSaturateNegative: Bool = Output(Bool())
      val isSaturate:         Bool = Output(Bool())
      val shouldHold: Bool = Output(Bool())
      // weak
      val isWeakPositive: Option[Bool] = Option.when(width >= 2)(Output(Bool()))
      val isWeakNegative: Option[Bool] = Option.when(width >= 2)(Output(Bool()))
      val isWeak: Option[Bool] = Option.when(width >= 2)(Output(Bool()))
      // medium
      val isMid: Option[Bool] = Option.when(width >= 3)(Output(Bool()))
    }
    val res: ResultIO = IO(new ResultIO)

    private val cnt = RegInit(SignedSaturateCounter.Zero(width))

    when(ctrl.op === OP.Inc) {
      cnt.selfIncrease(step = ctrl.step, en = ctrl.en)
    }.elsewhen(ctrl.op === OP.Dec) {
      cnt.selfDecrease(step = ctrl.step, en = ctrl.en)
    }.elsewhen(ctrl.op === OP.Upd) {
      cnt.selfUpdate(ctrl.inc, en = ctrl.en)
    }

    res.value := cnt.value
    res.isPositive := cnt.isPositive
    res.isNegative := cnt.isNegative
    res.isSaturatePositive := cnt.isSaturatePositive
    res.isSaturateNegative := cnt.isSaturateNegative
    res.isSaturate := cnt.isSaturate
    res.shouldHold := cnt.shouldHold(query.shouldHold.increase)
    if (width >= 2) {
      res.isWeakPositive.get := cnt.isWeakPositive
      res.isWeakNegative.get := cnt.isWeakNegative
      res.isWeak.get := cnt.isWeak
    }
    if (width >= 3) {
      res.isMid.get := cnt.isMid
    }
  }

  def maxValue(implicit width: Int): Int = pow(2, width - 1).toInt - 1
  def minValue(implicit width: Int): Int = -pow(2, width - 1).toInt
  def thres(implicit width: Int): Int = 0

  def isPositive(value: Int)(implicit width: Int): Boolean = value >= thres
  def isNegative(value: Int)(implicit width: Int): Boolean = value < thres
  def isSaturatePositive(value: Int)(implicit width: Int): Boolean = value == maxValue
  def isSaturateNegative(value: Int)(implicit width: Int): Boolean = value == minValue
  def isSaturate(value: Int)(implicit width: Int): Boolean = isSaturatePositive(value) || isSaturateNegative(value)

  def forceInRange(value: Int)(implicit width: Int): Int = value.max(minValue).min(maxValue)

  private val opString = Seq("Inc", "Dec", "Upd")
  private def flagString(value: Int)(implicit width: Int): String = {
    val ret = new StringBuilder()
    if (isSaturate(value)) ret.append("S")
    else ret.append("-")
    if (isPositive(value)) ret.append("P")
    else ret.append("N")
    ret.toString()
  }

  behavior of "SignedSaturateCounter"
  it should "work" in {
    def test(implicit width: Int): Unit = {
      print(f"===== test SignedSaturateCounter width=${width} =====\n")
      var value = 0
      simulate(new TestModule(width)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(10)
        dut.reset.poke(false.B)
        dut.clock.step(10)
        for (_ <- 0 until 1000) {
          val op = Random.nextInt(3)
          val step = Random.nextInt(pow(2, width - 1).toInt - 1)
          val inc = Random.nextInt(2) == 1

          dut.ctrl.en.poke(true.B)
          dut.ctrl.op.poke(op.U)
          dut.ctrl.step.poke(step.U)
          dut.ctrl.inc.poke(inc.B)
          dut.clock.step(1)

          val old = value
          if (op == 0) {
            value = value + step
          } else if (op == 1) {
            value = value - step
          } else {
            if (inc) value += 1
            else value -= 1
          }
          value = forceInRange(value)

          print(
            f"op=${opString(op)} " +(
              if (op == 0 || op == 1) f"step=${step}%03d           "
              else f"         inc=${inc}%5s ") +
              f"value=${old}%04d(${flagString(old)})->${value}%04d(${flagString(value)}) ... "
          )
          dut.res.value.expect(value.S)
          dut.res.isPositive.expect(isPositive(value).B)
          dut.res.isNegative.expect(isNegative(value).B)
          dut.res.isSaturatePositive.expect(isSaturatePositive(value).B)
          dut.res.isSaturateNegative.expect(isSaturateNegative(value).B)
          dut.res.isSaturate.expect((isSaturatePositive(value) || isSaturateNegative(value)).B)
          // TODO: weak/mid test?
          print("pass\n")
        }
      }
    }

    for (width <- 2 until 10) {
      test(width)
    }
  }
}
