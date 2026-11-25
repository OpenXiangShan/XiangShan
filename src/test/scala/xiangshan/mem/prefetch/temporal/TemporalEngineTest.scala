package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TemporalEngineTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "replay the same trigger, PC hash and ordered targets as the gem5 engine" in {
    test(new TemporalEngine(24, 2, 39, 4, 32, 8, 16, 4, 8, 2, 8, 2)) { dut =>
      val load = dut.io.load
      val candidate = dut.io.candidate
      val role = dut.io.role
      load.valid.poke(false.B)
      role.valid.poke(false.B)
      dut.io.feedback.valid.poke(false.B)
      dut.io.generationWrap.ready.poke(true.B)
      candidate.ready.poke(true.B)

      val observed = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
      var sample = -1
      def step(): Unit = {
        if (candidate.valid.peek().litToBoolean) {
          candidate.bits.owner.pc.expect(10304.U)
          candidate.bits.owner.context.expect(3.U)
          observed += ((sample, candidate.bits.target.line.peek().litValue.toInt))
        }
        dut.clock.step()
      }

      for (mask <- Seq(1, 2)) {
        role.ready.expect(true.B)
        role.bits.pc.poke(0x1002.U)
        role.bits.context.poke(3.U)
        role.bits.roles.poke(mask.U)
        role.valid.poke(true.B)
        step()
        role.valid.poke(false.B)
        for (_ <- 0 until 6) { step() }
      }

      def observe(i: Int, offset: Int): Unit = {
        var waited = 0
        while (!load.ready.peek().litToBoolean && waited < 96) {
          step()
          waited += 1
        }
        load.ready.expect(true.B)
        sample = i
        load.bits.pc.poke(0x1002.U)
        load.bits.context.poke(3.U)
        load.bits.kind.poke(2.U)
        load.bits.target.line.poke((512 + offset).U)
        load.bits.target.alias.poke((offset % 4).U)
        load.valid.poke(true.B)
        step()
        load.valid.poke(false.B)
      }

      // The standalone gem5 vector sees its first replay on zero-based sample 32.
      for (i <- 0 to 32) { observe(i, i % 16) }
      for (_ <- 0 until 96) { step() }
      assert(observed.toSeq == (513 to 524).map(line => (32, line)))

      // Issuing the first record puts this PC below the issue threshold.
      observed.clear()
      for (i <- 33 to 96) { observe(i, 0) }
      for (_ <- 0 until 96) { step() }
      assert(observed.toSeq == (513 to 524).map(line => (96, line)))
    }
  }
}
