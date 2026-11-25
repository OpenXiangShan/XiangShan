package xiangshan.mem.prefetch

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LldpReplayTest extends AnyFlatSpec with ChiselScalatestTester {
  private case class Step(op: LldpReplayOp.Type, immediate: Int, word: Boolean = false)

  private def send(
    dut: LldpReplay,
    bytes: BigInt,
    size: Int = 3,
    sext: Boolean = false,
    displacement: Int = 0,
    steps: Seq[Step] = Nil,
    floating: Boolean = false,
    dependencyOnly: Boolean = false
  ): Unit = {
    val input = dut.io.input
    input.ready.expect(true.B)
    input.bits.bytes.poke(bytes.U)
    input.bits.size.poke(size.U)
    input.bits.sext.poke(sext.B)
    input.bits.floating.poke(floating.B)
    input.bits.source.poke(3.U)
    input.bits.descriptor.length.poke((if (dependencyOnly) 0 else steps.length + 1).U)
    input.bits.descriptor.displacement.poke(displacement.S)
    for ((step, index) <- steps.zipWithIndex) {
      val descriptor = input.bits.descriptor.steps(index)
      descriptor.op.poke(step.op)
      descriptor.immediate.poke(step.immediate.S)
      descriptor.word.poke(step.word.B)
    }
    input.valid.poke(true.B)
    dut.clock.step()
    input.valid.poke(false.B)
    dut.clock.step(steps.length + 1)
  }

  private def check(dut: LldpReplay, address: BigInt): Unit = {
    dut.io.output.valid.expect(true.B)
    dut.io.output.bits.vaddr.expect((address & ((BigInt(1) << 64) - 1)).U)
    dut.io.output.bits.source.expect(3.U)
  }

  private def retire(dut: LldpReplay): Unit = {
    dut.io.output.ready.poke(true.B)
    dut.clock.step()
    dut.io.output.ready.poke(false.B)
  }

  it should "extend the selected producer bytes and apply the consumer displacement" in {
    test(new LldpReplay(sourceBits = 3)) { dut =>
      dut.io.output.ready.poke(false.B)
      send(dut, bytes = 0x123480, size = 0, sext = true, displacement = -16)
      check(dut, -144)
      retire(dut)
      send(dut, bytes = 0x123480, size = 0, displacement = -16)
      check(dut, 112)
      retire(dut)
    }
  }

  it should "execute both steps with RV64 word semantics and retain the address until accepted" in {
    test(new LldpReplay(sourceBits = 3)) { dut =>
      dut.io.output.ready.poke(false.B)
      send(
        dut,
        bytes = BigInt("1234567880000000", 16),
        displacement = 12,
        steps = Seq(Step(LldpReplayOp.Sar, 31, word = true), Step(LldpReplayOp.And, -16))
      )
      check(dut, -4)
      dut.clock.step(4)
      check(dut, -4)
      retire(dut)
      send(dut, bytes = 0x1080, steps = Seq(Step(LldpReplayOp.Add, -16)), displacement = -16)
      check(dut, 0x1060)
    }
  }

  it should "consume FP and dependency-only descriptors without emitting a value address" in {
    test(new LldpReplay(sourceBits = 3)) { dut =>
      dut.io.output.ready.poke(false.B)
      send(dut, bytes = 0x1000, floating = true)
      dut.io.output.valid.expect(false.B)
      send(dut, bytes = 0x1000, dependencyOnly = true)
      dut.io.output.valid.expect(false.B)
    }
  }
}
