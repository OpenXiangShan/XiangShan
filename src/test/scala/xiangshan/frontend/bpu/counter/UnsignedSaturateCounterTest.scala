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
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.scalatest.flatspec.AnyFlatSpec
import utils.EnumUInt

import scala.math.pow
import scala.util.Random

class SaturateCounterTest extends AnyFlatSpec {
  object OP extends EnumUInt(3) {
    def Inc: UInt = 0.U(width.W)
    def Dec: UInt = 1.U(width.W)
    def Upd: UInt = 2.U(width.W)
  }

  abstract class TestModule[T <: Bits with Num[T]](width: Int) extends Module {
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

    def newValueCls(width: Int): T
    def newValueInit(value: Int): T
    class ResultIO extends Bundle {
      val value: T = Output(newValueCls(width))
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

    protected val cnt: SaturateCounter[T]

    // define as function and call in subclass to avoid nullptr
    def connect(): Unit = {
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

    def maxValue(implicit width: Int): Int
    def minValue(implicit width: Int): Int
    def thres(implicit width: Int): Int

    def isPositive(value: Int)(implicit width: Int): Boolean = value >= thres
    def isNegative(value: Int)(implicit width: Int): Boolean = value < thres
    def isSaturatePositive(value: Int)(implicit width: Int): Boolean = value == maxValue
    def isSaturateNegative(value: Int)(implicit width: Int): Boolean = value == minValue
    def isSaturate(value: Int)(implicit width: Int): Boolean = isSaturatePositive(value) || isSaturateNegative(value)

    def isWeakPositive(value: Int)(implicit width: Int): Boolean = value == thres
    def isWeakNegative(value: Int)(implicit width: Int): Boolean = value == thres - 1
    def isWeak(value: Int)(implicit width: Int): Boolean = isWeakPositive(value) || isWeakNegative(value)

    def isMid(value: Int)(implicit width: Int): Boolean = !isSaturate(value) && !isWeak(value)

    def forceInRange(value: Int)(implicit width: Int): Int = value.max(minValue).min(maxValue)

    def opString(op: Int): String = Seq("Inc", "Dec", "Upd")(op)
    def flagString(value: Int)(implicit width: Int): String = {
      val ret = new StringBuilder()
      if (isSaturate(value)) ret.append("S")
      else if (isWeak(value)) ret.append("W")
      else ret.append("M")
      if (isPositive(value)) ret.append("P")
      else ret.append("N")
      ret.toString()
    }
  }

  class UnsignedTest(width: Int) extends TestModule[UInt](width) {
    protected val cnt: UnsignedSaturateCounter = RegInit(UnsignedSaturateCounter.Zero(width))
    def maxValue(implicit width: Int): Int = pow(2, width).toInt - 1
    def minValue(implicit width: Int): Int = 0
    def thres(implicit width: Int): Int = pow(2, width - 1).toInt
    def newValueCls(width: Int): UInt = UInt(width.W)
    def newValueInit(value: Int): UInt = value.U
    connect()
  }

  class SignedTest(width: Int) extends TestModule[SInt](width) {
    protected val cnt: SignedSaturateCounter = RegInit(SignedSaturateCounter.Zero(width))
    def maxValue(implicit width: Int): Int = pow(2, width - 1).toInt - 1
    def minValue(implicit width: Int): Int = -pow(2, width - 1).toInt
    def thres(implicit width: Int): Int = 0
    def newValueCls(width: Int): SInt = SInt(width.W)
    def newValueInit(value: Int): SInt = value.S
    connect()
  }

  behavior of "SaturateCounter"
  it should "work" in {
    def test[T <: Bits with Num[T], TT <: TestModule[T]](cls: (Int) => TT)(implicit width: Int): Unit = {
      print(f"===== test SaturateCounter width=${width} =====\n")
      var value = 0
      simulate(cls(width)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(10)
        dut.reset.poke(false.B)
        dut.clock.step(10)
        for (_ <- 0 until 1000) {
          val op = Random.nextInt(3)
          val step = Random.nextInt(pow(2, width).toInt - 1)
          val inc = Random.nextInt(2) == 1

          dut.ctrl.en.poke(true.B)
          dut.ctrl.op.poke(op.U)
          dut.ctrl.step.poke(step.U)
          dut.ctrl.inc.poke(inc.B)
          dut.clock.step(1)

          val old = value
          value = dut.forceInRange(
            if (op == 0) value + step
            else if (op == 1) value - step
            else if (inc) value + 1 // op == 2
            else value - 1 // op == 2 && !inc
          )

          print(
            f"op=${dut.opString(op)} " +(
              if (op == 0 || op == 1) f"step=${step}%03d           "
              else f"         inc=${inc}%5s ") +
              f"value=${old}%04d(${dut.flagString(old)})->${value}%04d(${dut.flagString(value)}) ... "
          )
          dut.res.value.expect(dut.newValueInit(value))
          dut.res.isPositive.expect(dut.isPositive(value).B)
          dut.res.isNegative.expect(dut.isNegative(value).B)
          dut.res.isSaturatePositive.expect(dut.isSaturatePositive(value).B)
          dut.res.isSaturateNegative.expect(dut.isSaturateNegative(value).B)
          dut.res.isSaturate.expect((dut.isSaturatePositive(value) || dut.isSaturateNegative(value)).B)
          dut.res.isWeakPositive.foreach(_.expect(dut.isWeakPositive(value).B))
          dut.res.isWeakNegative.foreach(_.expect(dut.isWeakNegative(value).B))
          dut.res.isWeak.foreach(_.expect(dut.isWeak(value).B))
          dut.res.isMid.foreach(_.expect(dut.isMid(value).B))
          print("pass\n")
        }
      }
    }

    for (width <- 1 until 10) {
      test[UInt, UnsignedTest]((width) => new UnsignedTest(width))(width)
    }

    for (width <- 2 until 10) {
      test[SInt, SignedTest]((width) => new SignedTest(width))(width)
    }
  }
}
