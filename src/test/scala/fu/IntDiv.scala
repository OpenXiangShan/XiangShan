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

package futest

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.test.types._
import xiangshan.types.PrintModuleName

import xiangshan.backend.fu._

import scala.util.Random


class SRT4DividerWrapper extends Module {
  val io = IO(new Bundle{
    val dividend = Input(UInt(64.W))
    val divisor = Input(UInt(64.W))
    val sign = Input(Bool())
    val isHi = Input(Bool())
    val isW = Input(Bool())
    val in_valid = Input(Bool())
    val out_ready = Input(Bool())
    val in_ready = Output(Bool())
    val out_valid = Output(Bool())
    val result = Output(UInt(64.W))
  })
  val divider = Module(new SRT16DividerDataModule(len = 64))
  divider.io.src(0) := io.dividend
  divider.io.src(1) := io.divisor
  divider.io.kill_r := false.B
  divider.io.kill_w := false.B
  divider.io.sign := io.sign
  divider.io.isHi := io.isHi
  divider.io.isW := io.isW
  divider.io.out_ready := io.out_ready
  divider.io.valid := io.in_valid

  io.in_ready := divider.io.in_ready
  io.out_valid := divider.io.out_valid

  io.result := divider.io.out_data

}

class IntDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "srt16 divider"
  it should "run" in {
    val rand = new Random(0x14226)
    val testNum = 1000

    val printModuleNameAnno = chisel3.BuildInfo.version match {
      case "3.6.0" => Seq(RunFirrtlTransformAnnotation(new PrintModuleName))
      case _ => Seq()
    }

    test(new SRT4DividerWrapper).withAnnotations(Seq(VerilatorBackendAnnotation,
      // LineCoverageAnnotation,
      // ToggleCoverageAnnotation,
      VerilatorFlags(Seq(
        // "--output-split 20", "--output-split-cfuncs 20",
        "+define+RANDOMIZE_REG_INIT", "+define+RANDOMIZE_MEM_INIT", "--trace")),
      ) ++ printModuleNameAnno){ m =>
      println("Test started!")
      m.clock.step(20)

      for (i <- 1 to testNum) {
        m.clock.step(3)
        m.io.in_ready.expect(true.B)
        val divisor = rand.nextLong()
        val dividend = rand.nextLong()
        // val sign = rand.nextBoolean()

        // val isSigned = if (sign) s"Signed division" else s"Unsigned division"
        println(s"$i th iteration\n" + s"divisor is ${divisor.toHexString}, dividend is ${dividend.toHexString}")
        m.io.in_valid.poke(true.B)
        m.io.dividend.poke((s"b" + dividend.toBinaryString).asUInt(64.W))
        m.io.divisor.poke((s"b" + divisor.toBinaryString).asUInt(64.W))
        m.io.sign.poke(true.B)
        val (quotient, remainder) = (dividend / divisor, dividend % divisor)
        println(s"quotient is ${quotient.toHexString}, remainder is ${remainder.toHexString}")
        var timeTaken = 0
        while (m.io.out_valid.peek().litToBoolean != true) {
          m.clock.step()
          timeTaken += 1
          if (timeTaken >= 62) assert(false, s"Timeout for single execution!!!")
        }

        m.io.in_valid.poke(false.B)
        m.io.out_ready.poke(true.B)
        m.io.isHi.poke(false.B)
        m.clock.step()

        m.io.result.expect((s"b" + quotient.toBinaryString).asUInt(64.W))
        m.io.isHi.poke(true.B)
        m.clock.step()

        m.io.result.expect((s"b" + remainder.toBinaryString).asUInt(64.W))
      }
    }
  }
}