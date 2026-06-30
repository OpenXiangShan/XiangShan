// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package utils

import chisel3._
import chisel3.simulator.EphemeralSimulator._ // scalastyle:ignore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PartialConnectTest extends AnyFlatSpec with should.Matchers {
  import PartialConnect.Behavior

  class CommonBundle extends Bundle {
    val uint: UInt = UInt(8.W)
  }

  private def genInput =
    IO(Input(new Bundle {
      val common:   CommonBundle = new CommonBundle
      val uint:     UInt         = UInt(8.W)
      val optional: Option[UInt] = Option.when(true)(UInt(8.W))
      val extraA:   UInt         = UInt(8.W)
    }))
  private def genOutput =
    IO(Output(new Bundle {
      val common:   CommonBundle = new CommonBundle
      val uint:     UInt         = UInt(16.W)
      val optional: Option[UInt] = Option.when(true)(UInt(8.W))
      val extraB:   UInt         = UInt(8.W)
    }))
  private def genOutputWithWrongType =
    IO(Output(new Bundle {
      val common: UInt = UInt(8.W)
    }))

  behavior of "PartialConnect"

  // default parameter test
  it should "fail due to out.extraB not connected" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      PartialConnect(out, in)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("sink \"out_extraB\" not fully initialized in \"TestModule\"")
  }

  // auto apply DontCare to extra fields in sink
  it should "work when connected out.extraB to DontCare" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      PartialConnect(out, in, onExtraFields = Behavior.DontCare)
    }
    simulate(new TestModule) { dut =>
      dut.in.common.uint.poke(1.U)
      dut.in.uint.poke(2.U)
      dut.in.optional.foreach(_.poke(3.U))
      dut.in.extraA.poke(4.U)
      dut.out.common.uint.expect(1.U)
      dut.out.uint.expect(2.U)
      dut.out.optional.foreach(_.expect(3.U))
      dut.out.extraB.expect(0.U) // extraB is DontCare, in simulate() it should be always 0
    }
  }

  // fail if extra field in sink, this should not be used, that will be the same as `sink := source`, anyway test here
  it should "fail due to extra out.extraB" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      PartialConnect(out, in, onExtraFields = Behavior.Error)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("Extra field in sink: extraB")
  }

  // width mismatch tests
  it should "fail due to width mismatch" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      PartialConnect(out, in, onWidthMismatch = Behavior.Error)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("Width mismatch for field 'uint'")
  }
  it should "use dontCare on width mismatch" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      // also add onExtraFields = Behavior.DontCare to prevent dut.out.extraB uninitialized
      PartialConnect(out, in, onExtraFields = Behavior.DontCare, onWidthMismatch = Behavior.DontCare)
    }
    simulate(new TestModule) { dut =>
      dut.in.uint.poke(2.U)
      dut.out.uint.expect(0.U) // should be connected to DontCare due to width mismatch
    }
  }
  it should "ignore on width mismatch, and fail due to out.uint not initialized" in {
    class TestModule extends Module {
      val in  = genInput
      val out = genOutput
      PartialConnect(out, in, onWidthMismatch = Behavior.Ignore)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("sink \"out_uint\" not fully initialized in \"TestModule\"")
  }

  // type mismatch tests
  it should "fail due to type mismatch" in {
    class TestModule extends Module {
      val in  = genInput
      val out = genOutputWithWrongType
      PartialConnect(out, in)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("Type mismatch for field 'common'")
  }
  it should "force to connect on type mismatch" in {
    class TestModule extends Module {
      val in  = genInput
      val out = genOutputWithWrongType
      // also add onExtraFields = Behavior.DontCare to prevent dut.out.extraB uninitialized
      PartialConnect(out, in, onExtraFields = Behavior.DontCare, onTypeMismatch = Behavior.Force)
    }
    simulate(new TestModule) { dut =>
      dut.in.common.uint.poke(1.U)
      dut.in.uint.poke(2.U)
      dut.in.extraA.poke(3.U)
      dut.out.common.expect(1.U) // forced to connect to in.common
    }
  }
  it should "use dontCare on type mismatch" in {
    class TestModule extends Module {
      val in  = genInput
      val out = genOutputWithWrongType
      // also add onExtraFields = Behavior.DontCare to prevent dut.out.extraB uninitialized
      PartialConnect(out, in, onExtraFields = Behavior.DontCare, onTypeMismatch = Behavior.DontCare)
    }
    simulate(new TestModule) { dut =>
      dut.in.common.uint.poke(1.U)
      dut.in.uint.poke(2.U)
      dut.in.extraA.poke(3.U)
      dut.out.common.expect(0.U) // should be dontCare
    }
  }
  it should "ignore on type mismatch, and fail due to out.common not initialized" in {
    class TestModule extends Module {
      val in  = genInput
      val out = genOutputWithWrongType
      PartialConnect(out, in, onTypeMismatch = Behavior.Ignore)
    }
    val exception = the[Exception] thrownBy { simulate(new TestModule) { _ => } }
    exception.getMessage should include("sink \"out_common\" not fully initialized in \"TestModule\"")
  }

  // fail if unused field in source
  it should "fail due to unused in.extraA" in {
    class TestModule extends Module {
      val (out, in) = (genOutput, genInput)
      PartialConnect(out, in, onUnusedFields = Behavior.Error)
    }
    val exception = the[Exception] thrownBy {
      simulate(new TestModule) { _ => }
    }
    exception.getMessage should include("Unused fields in source: extraA")
  }
}
