/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan

import chisel3._
import chisel3.simulator.Settings
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import scala.sys.process._
import scala.util.Random

class DipSamplerDifferential(implicit p: Parameters) extends DipSampler {
  private val reference = Module(new DipSamplerReference)
  reference.io.enable := io.enable
  reference.io.period := io.period
  reference.io.dispatch := io.dispatch
  reference.io.rob := io.rob
  assert(io.trace.valid === reference.io.trace.valid)
  when(io.trace.valid) {
    assert(io.trace.bits.asUInt === reference.io.trace.bits.asUInt, "DIP native sample mismatch")
  }
  io.finals.zip(reference.io.finals).foreach { case (actual, expected) =>
    assert(actual.valid === expected.valid, "DIP native lifecycle validity mismatch")
    when(actual.valid) {
      assert(actual.bits.asUInt === expected.bits.asUInt, "DIP native lifecycle payload mismatch")
    }
  }
}

class DipSamplerDifferentialTest extends XSTester {
  private val dipConfig = config.alterPartial {
    case XSCoreParamsKey => config(XSCoreParamsKey).copy(RobSize = 8, RenameWidth = 4, CommitWidth = 2)
  }
  private val nativeSettings = {
    val directory = Files.createDirectories(Paths.get("build/chiselsim/dip-native-differential")).toAbsolutePath
    val source = directory.resolve("sampler.cpp")
    val obj = directory.resolve("sampler.o")
    Files.writeString(source, DipNative.cpp("DipNative_8_4_2_0_eval", 8, 4, 2))
    require(Seq("g++", "-std=c++17", "-O2", "-c", source.toString, "-o", obj.toString).! == 0)
    Settings.default[DipSamplerDifferential].withLibraryPaths(Seq(obj.toString))
  }
  it should "preserve sampled history across idle windows, enable changes, flushes and ROB reuse" in {
    simulate(new DipSamplerDifferential()(dipConfig), settings = nativeSettings) { dut =>
      val random = new Random(0xd19)
      for (cycle <- 0 until 2000) {
        dut.io.enable.poke((cycle % 101 >= 70).B)
        dut.io.period.poke((if (cycle % 47 == 0) 0 else 1 + cycle % 13).U)
        dut.io.rob.cycle.poke(cycle.U)
        dut.io.rob.retired.poke(random.nextInt(9).U)
        val validMask = random.nextInt(16)
        dut.io.dispatch.validMask.poke(validMask.U)
        dut.io.dispatch.fireMask.poke((random.nextInt(16) & validMask).U)
        dut.io.dispatch.stallCause.poke(random.nextInt(4096).U)
        dut.io.dispatch.uops.foreach { u =>
          u.robIdx.value.poke(random.nextInt(8).U)
          u.robIdx.flag.poke(random.nextBoolean().B)
          u.robIdx.isFormer.poke(random.nextBoolean().B)
          u.pc.poke((0x80000000L + 4L * random.nextInt(10000)).U)
          u.psv.poke(random.nextInt(256).U)
        }
        dut.io.rob.commits.foreach { c =>
          c.valid.poke(random.nextBoolean().B)
          c.bits.value.poke(random.nextInt(8).U)
          c.bits.flag.poke(random.nextBoolean().B)
          c.bits.isFormer.poke(random.nextBoolean().B)
        }
        dut.io.rob.flush.valid.poke((random.nextInt(7) == 0).B)
        dut.io.rob.flush.bits.robIdx.value.poke(random.nextInt(8).U)
        dut.io.rob.flush.bits.robIdx.flag.poke(random.nextBoolean().B)
        dut.io.rob.flush.bits.robIdx.isFormer.poke(random.nextBoolean().B)
        dut.io.rob.flush.bits.self.poke(random.nextBoolean().B)
        dut.io.rob.flush.bits.pc.poke((0x80010000L + 4L * cycle).U)
        dut.io.rob.flush.bits.cause.poke(random.nextInt(4096).U)
        dut.clock.step()
      }
    }
  }
}
