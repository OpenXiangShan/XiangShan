package xiangshan.mem.prefetch

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LldpAddressPredictorTest extends AnyFlatSpec with ChiselScalatestTester {
  private def initialize(dut: LldpAddressPredictor): Unit = {
    dut.io.train.valid.poke(false.B)
    dut.io.lookup.valid.poke(false.B)
    dut.io.feedback.valid.poke(false.B)
    dut.io.admit.poke(false.B)
    dut.io.observe.poke(0.U)
    dut.clock.step()
  }

  private def key(port: LldpAddressKey, address: Int, generation: Int): Unit = {
    port.producer.poke(0x210.U)
    port.consumer.poke(0x220.U)
    port.context.poke(1.U)
    port.producerAddress.poke(address.U)
    port.ruleGeneration.poke(generation.U)
  }

  private def train(dut: LldpAddressPredictor, address: Int, target: Int, generation: Int = 1): Unit = {
    val port = dut.io.train
    key(port.bits.key, address, generation)
    port.bits.target.paddr.poke(target.U)
    port.bits.target.alias.poke(2.U)
    port.valid.poke(true.B)
    port.ready.expect(true.B)
    dut.clock.step()
    port.valid.poke(false.B)
    dut.clock.step(4)
  }

  private def lookup(dut: LldpAddressPredictor, address: Int, generation: Int = 1): Unit = {
    key(dut.io.lookup.bits, address, generation)
    dut.io.lookup.valid.poke(true.B)
    dut.io.lookup.ready.expect(true.B)
    dut.clock.step()
    dut.io.lookup.valid.poke(false.B)
  }

  private def feedback(
    dut: LldpAddressPredictor,
    index: BigInt,
    generation: BigInt,
    outcome: LldpAddressOutcome.Type,
    stale: Boolean = false
  ): Unit = {
    val port = dut.io.feedback
    port.bits.ref.index.poke(index.U)
    port.bits.ref.generation.poke(generation.U)
    port.bits.outcome.poke(outcome)
    port.valid.poke(true.B)
    port.ready.expect(true.B)
    dut.clock.step()
    dut.io.staleFeedback.expect(stale.B)
    port.valid.poke(false.B)
    dut.clock.step()
  }

  it should "use byte addresses and only charge an admitted prediction" in {
    test(new LldpAddressPredictor(16, 24, 4, 2, sets = 16)) { dut =>
      initialize(dut)
      train(dut, 0x8048, 0xc158)
      lookup(dut, 0x8048)
      dut.io.candidate.valid.expect(false.B)
      dut.clock.step()
      train(dut, 0x8048, 0xc158)
      lookup(dut, 0x804c)
      dut.io.candidate.valid.expect(false.B)
      dut.clock.step()
      lookup(dut, 0x8048)
      dut.io.candidate.valid.expect(true.B)
      dut.io.candidate.bits.target.paddr.expect(0xc158.U)
      dut.io.candidate.bits.target.alias.expect(2.U)
      dut.clock.step()
      lookup(dut, 0x8048)
      dut.io.candidate.valid.expect(true.B)
      dut.clock.step()
      lookup(dut, 0x8048, generation = 2)
      dut.io.candidate.valid.expect(false.B)
    }
  }

  it should "hold one busy candidate and reject feedback for a previous admission" in {
    test(new LldpAddressPredictor(16, 24, 4, 2, sets = 16)) { dut =>
      initialize(dut)
      for (_ <- 0 until 2) { train(dut, 0x8048, 0xc158) }
      lookup(dut, 0x8048)
      val first = dut.io.candidate.bits.ref
      val index = first.index.peek().litValue
      val firstGeneration = first.generation.peek().litValue
      dut.io.admit.poke(true.B)
      dut.clock.step()
      dut.io.admit.poke(false.B)
      lookup(dut, 0x8048)
      dut.io.blocked.expect(true.B)
      dut.clock.step()
      feedback(dut, index, firstGeneration, LldpAddressOutcome.Dropped)
      lookup(dut, 0x8048)
      dut.io.candidate.valid.expect(true.B)
      val secondGeneration = dut.io.candidate.bits.ref.generation.peek().litValue
      dut.io.admit.poke(true.B)
      dut.clock.step()
      dut.io.admit.poke(false.B)
      feedback(dut, index, firstGeneration, LldpAddressOutcome.Useful, stale = true)
      lookup(dut, 0x8048)
      dut.io.blocked.expect(true.B)
      dut.clock.step()
      feedback(dut, index, secondGeneration, LldpAddressOutcome.Late)
      lookup(dut, 0x8048)
      dut.io.blocked.expect(true.B)
      dut.io.candidate.valid.expect(false.B)
    }
  }
}
