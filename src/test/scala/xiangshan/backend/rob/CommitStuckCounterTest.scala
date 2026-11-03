package xiangshan.backend.rob

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

private class CommitStuckCounterHarness(width: Int, forceEnable: Bool) extends Module {
  val io = IO(new Bundle {
    val stuck = Input(Bool())
    val runtimeEnable = Input(Bool())
    val overflowEnabled = Input(Bool())
    val count = Output(UInt(width.W))
    val overflow = Output(Bool())
  })

  private val dut = Module(new CommitStuckCounter(width = width, forceEnable = forceEnable))
  dut.io.stuck := io.stuck
  dut.io.runtimeEnable := io.runtimeEnable
  dut.io.overflowEnabled := io.overflowEnabled

  io.count := dut.io.count
  io.overflow := dut.io.overflow
}

class CommitStuckCounterTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CommitStuckCounter"

  it should "count only when enabled and clear immediately when disabled or unstuck" in {
    test(new CommitStuckCounterHarness(width = 4, forceEnable = false.B)) { dut =>
      dut.io.stuck.poke(true.B)
      dut.io.overflowEnabled.poke(true.B)
      dut.io.runtimeEnable.poke(false.B)
      dut.clock.step()
      dut.io.count.expect(0.U)
      dut.io.overflow.expect(false.B)

      dut.io.runtimeEnable.poke(true.B)
      dut.clock.step(3)
      dut.io.count.expect(3.U)

      dut.io.runtimeEnable.poke(false.B)
      dut.clock.step()
      dut.io.count.expect(0.U)
      dut.io.overflow.expect(false.B)

      dut.io.runtimeEnable.poke(true.B)
      dut.clock.step(2)
      dut.io.count.expect(2.U)

      dut.io.stuck.poke(false.B)
      dut.clock.step()
      dut.io.count.expect(0.U)
      dut.io.overflow.expect(false.B)
    }
  }

  it should "keep counting when force enable is active" in {
    test(new CommitStuckCounterHarness(width = 3, forceEnable = true.B)) { dut =>
      dut.io.stuck.poke(true.B)
      dut.io.runtimeEnable.poke(false.B)
      dut.io.overflowEnabled.poke(true.B)
      dut.clock.step(3)
      dut.io.count.expect(3.U)
      dut.io.overflow.expect(false.B)

      dut.clock.step(4)
      dut.io.overflow.expect(true.B)
    }
  }

  it should "suppress overflow when overflow detection is disabled dynamically" in {
    test(new CommitStuckCounterHarness(width = 3, forceEnable = true.B)) { dut =>
      dut.io.stuck.poke(true.B)
      dut.io.runtimeEnable.poke(false.B)
      dut.io.overflowEnabled.poke(false.B)

      dut.clock.step(7)
      dut.io.count.expect(7.U)
      dut.io.overflow.expect(false.B)

      dut.io.overflowEnabled.poke(true.B)
      dut.io.overflow.expect(true.B)
    }
  }
}
