package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TemporalRecorderTest extends AnyFlatSpec with ChiselScalatestTester {
  private def pair(
    dut: TemporalRecorder,
    pc: Int,
    base: Int,
    target: Int,
    roles: Int = 1,
    context: Int = 0
  ): Unit = {
    dut.io.pair.ready.expect(true.B)
    dut.io.pair.bits.pc.poke(pc.U)
    dut.io.pair.bits.context.poke(context.U)
    dut.io.pair.bits.base.line.poke(base.U)
    dut.io.pair.bits.base.alias.poke((base % 4).U)
    dut.io.pair.bits.target.line.poke(target.U)
    dut.io.pair.bits.target.alias.poke((target % 4).U)
    dut.io.pair.bits.roles.poke(roles.U)
    dut.io.pair.bits.kind.poke(2.U)
    dut.io.pair.valid.poke(true.B)
    dut.clock.step()
    dut.io.pair.valid.poke(false.B)
    dut.clock.step()
  }

  private def check(
    dut: TemporalRecorder,
    pc: Int,
    trigger: Int,
    targets: Seq[Int],
    roles: Int,
    context: Int = 0
  ): Unit = {
    val record = dut.io.record
    record.valid.expect(true.B)
    record.bits.pc.expect(pc.U)
    record.bits.context.expect(context.U)
    record.bits.trigger.expect(trigger.U)
    record.bits.length.expect(targets.size.U)
    record.bits.roles.expect(roles.U)
    targets.zipWithIndex.foreach { case (addr, index) =>
      record.bits.targets(index).line.expect(addr.U)
      record.bits.targets(index).alias.expect((addr % 4).U)
    }
  }

  private def drain(dut: TemporalRecorder): Unit = {
    dut.io.record.ready.poke(true.B)
    dut.clock.step()
    dut.io.record.ready.poke(false.B)
    dut.io.record.valid.expect(false.B)
  }

  it should "drop an eight-target victim and submit nine- and eleven-target victims under their original keys" in {
    test(new TemporalRecorder(sets = 4, lineBits = 12, aliasBits = 2, contextBits = 2)) { dut =>
      dut.io.record.ready.poke(false.B)
      val pc = 13
      for (length <- Seq(8, 9, 11)) {
        for (i <- 0 until length) {
          pair(dut, pc, 100 + i, 101 + i, context = 1)
        }
        pair(dut, pc, 70, 71, context = 2)
        pair(dut, 21, 90, 91)
        if (length == 8) {
          dut.io.record.valid.expect(false.B)
        } else {
          check(dut, pc, 100, 101 to (100 + length), roles = 1, context = 1)
          drain(dut)
        }
      }
    }
  }

  it should "carry the second target into a new record and hold the completed record under backpressure" in {
    test(new TemporalRecorder(sets = 4, lineBits = 12, aliasBits = 2, contextBits = 2)) { dut =>
      dut.io.record.ready.poke(false.B)
      for (i <- 0 until 11) {
        pair(dut, 6, 100 + i, 101 + i)
      }
      pair(dut, 6, 112, 400, roles = 2)
      check(dut, 6, 100, 101 to 112, roles = 3)
      dut.io.pair.ready.expect(false.B)
      dut.clock.step(4)
      check(dut, 6, 100, 101 to 112, roles = 3)
      drain(dut)

      for (i <- 0 until 11) {
        pair(dut, 6, 400 + i, 401 + i)
      }
      check(dut, 6, 112, 400 to 411, roles = 3)
      drain(dut)
    }
  }

  it should "use the pre-update membership of both endpoints when they are equal" in {
    test(new TemporalRecorder(sets = 4, lineBits = 12, aliasBits = 2, contextBits = 2)) { dut =>
      dut.io.record.ready.poke(false.B)
      for (i <- 0 until 10) {
        pair(dut, 2, 100 + i, 101 + i)
      }
      pair(dut, 2, 400, 400)
      check(dut, 2, 100, (101 to 110) ++ Seq(400, 400), roles = 1)
    }
  }
}
