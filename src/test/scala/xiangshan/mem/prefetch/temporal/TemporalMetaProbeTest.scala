package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TemporalMetaProbeTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "install the 64th low-Meta attempt and distinguish halfword PCs" in {
    test(new TemporalEngine(24, 2, 39, 4, 32, 8, 16, 4, 8, 2, 8, 2)) { dut =>
      val load = dut.io.load
      val role = dut.io.role
      load.valid.poke(false.B)
      role.valid.poke(false.B)
      dut.io.feedback.valid.poke(false.B)
      dut.io.generationWrap.ready.poke(true.B)
      dut.io.candidate.ready.poke(true.B)

      for (mask <- Seq(1, 2)) {
        role.ready.expect(true.B)
        role.bits.pc.poke(0x1002.U)
        role.bits.context.poke(3.U)
        role.bits.roles.poke(mask.U)
        role.valid.poke(true.B)
        dut.clock.step()
        role.valid.poke(false.B)
        dut.clock.step(6)
      }

      val installs = scala.collection.mutable.ArrayBuffer.empty[Int]
      val metaHits = scala.collection.mutable.ArrayBuffer.empty[Int]
      val candidates = scala.collection.mutable.ArrayBuffer.empty[Int]
      def observe(sample: Int, line: Int, pc: Int): Unit = {
        load.ready.expect(true.B)
        load.bits.pc.poke(pc.U)
        load.bits.context.poke(3.U)
        load.bits.kind.poke(2.U)
        load.bits.target.line.poke(line.U)
        load.bits.target.alias.poke(0.U)
        load.valid.poke(true.B)
        dut.clock.step()
        load.valid.poke(false.B)

        // Allow all engine and Recorder work for this sample to complete before the next load.
        for (_ <- 0 until 48) {
          if (dut.io.install.peek().litToBoolean) installs += sample
          if (dut.io.metaHit.peek().litToBoolean) metaHits += sample
          if (dut.io.candidate.valid.peek().litToBoolean) candidates += sample
          dut.clock.step()
        }
      }

      // Both gem5 revisions first replay at 32 and admit a low-Meta install at 796.
      for (sample <- 0 to 796) {
        val base = if (sample <= 32) 512 else 1024
        observe(sample, base + sample % 16, 0x1002)
      }
      assert(installs.toSeq == Seq(28, 796))
      assert(candidates.toSeq == Seq.fill(12)(32))

      metaHits.clear()
      observe(797, 512, 0x1000)
      observe(798, 512, 0x1002)
      assert(metaHits.toSeq == Seq(798))
    }
  }
}
