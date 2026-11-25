package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TemporalMetaTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private def transaction(
    dut: TemporalMeta,
    op: TemporalMetaOp.Type,
    pc: Int,
    trigger: Int,
    targets: Seq[Int] = Nil,
    ref: (Int, Int) = (0, 0),
    priority: Int = 0
  ): Unit = {
    val request = dut.io.request
    request.ready.expect(true.B)
    request.bits.op.poke(op)
    request.bits.allowReplay.poke(true.B)
    request.bits.record.pc.poke(pc.U)
    request.bits.record.context.poke(0.U)
    request.bits.record.roles.poke(1.U)
    request.bits.record.kind.poke(0.U)
    request.bits.record.trigger.poke(trigger.U)
    request.bits.record.length.poke(targets.length.U)
    request.bits.ref.slot.poke(ref._1.U)
    request.bits.ref.generation.poke(ref._2.U)
    request.bits.priority.poke(priority.U)
    for ((target, i) <- targets.zipWithIndex) {
      request.bits.record.targets(i).line.poke(target.U)
      request.bits.record.targets(i).alias.poke((i % 4).U)
    }
    request.valid.poke(true.B)
    dut.clock.step()
    request.valid.poke(false.B)
    var waitCycles = 0
    while (!dut.io.response.valid.peek().litToBoolean && waitCycles < 25) {
      dut.clock.step()
      waitCycles += 1
    }
    dut.io.response.valid.expect(true.B)
  }

  private def drain(dut: TemporalMeta): Unit = {
    dut.io.response.ready.poke(true.B)
    dut.clock.step()
    dut.io.response.ready.poke(false.B)
  }

  private def identity(dut: TemporalMeta): (Int, Int) =
    (dut.io.response.bits.ref.slot.peek().litValue.toInt, dut.io.response.bits.ref.generation.peek().litValue.toInt)

  it should "replay both formats and preserve only the generation of a same-content refresh" in {
    test(new TemporalMeta(20, 2, 2, sets = 6, ways = 2, compactSlots = 2, fullSlots = 2)) { dut =>
      dut.io.response.ready.poke(false.B)
      dut.io.generationWrap.ready.poke(true.B)
      val base = 64000
      val compact = Seq(base - 32768, base + 32767) ++ (1 to 7).map(_ + base)
      val full = Seq(base - 32769, base + 32768) ++ (1 to 7).map(_ + base)
      transaction(dut, TemporalMetaOp.Install, 7, base, compact)
      dut.io.response.bits.success.expect(true.B)
      val original = identity(dut)
      drain(dut)
      for (addresses <- Seq(compact, full)) {
        transaction(dut, TemporalMetaOp.Install, 7, base, addresses)
        dut.io.response.bits.hit.expect(true.B)
        dut.io.response.bits.success.expect(true.B)
        identity(dut) shouldBe (original._1, original._2 + (if (addresses == compact) 0 else 1))
        drain(dut)
        transaction(dut, TemporalMetaOp.Query, 7, base)
        dut.io.response.bits.success.expect(true.B)
        val record = dut.io.response.bits.record
        record.length.expect(addresses.length.U)
        for ((address, i) <- addresses.zipWithIndex) {
          record.targets(i).line.expect(address.U)
          record.targets(i).alias.expect((i % 4).U)
        }
        drain(dut)
      }
      transaction(dut, TemporalMetaOp.Feedback, 0, 0, ref = original)
      dut.io.response.bits.success.expect(false.B)
      drain(dut)
      transaction(dut, TemporalMetaOp.Feedback, 0, 0, ref = (original._1, original._2 + 1))
      dut.io.response.bits.success.expect(true.B)
    }
  }

  it should "retain a compact victim if every full slot is occupied" in {
    test(new TemporalMeta(20, 2, 2, sets = 6, ways = 4, compactSlots = 2, fullSlots = 2)) { dut =>
      dut.io.response.ready.poke(false.B)
      dut.io.generationWrap.ready.poke(true.B)
      val base = 64000
      val compact = (1 to 9).map(_ + base)
      val full = (1 to 9).map(_ + base + 32768)
      for ((pc, addresses) <- Seq((1, full), (2, full), (3, compact))) {
        transaction(dut, TemporalMetaOp.Install, pc, base, addresses)
        dut.io.response.bits.success.expect(true.B)
        drain(dut)
      }
      transaction(dut, TemporalMetaOp.Install, 3, base, full)
      dut.io.response.bits.hit.expect(true.B)
      dut.io.response.bits.success.expect(false.B)
      drain(dut)
      transaction(dut, TemporalMetaOp.Query, 3, base)
      dut.io.response.bits.success.expect(true.B)
      for ((address, i) <- compact.zipWithIndex) {
        dut.io.response.bits.record.targets(i).line.expect(address.U)
      }
    }
  }

  it should "replace the same-set RRIP victim and reject feedback for its old generation" in {
    test(new TemporalMeta(20, 2, 2, sets = 6, ways = 2, compactSlots = 2, fullSlots = 2)) { dut =>
      dut.io.response.ready.poke(false.B)
      dut.io.generationWrap.ready.poke(true.B)
      val pc = 1
      // These triggers map to set 4 under the six-set Meta hash.
      val first = 128
      val second = 134
      val third = 140

      def install(trigger: Int): (Int, Int) = {
        transaction(dut, TemporalMetaOp.Install, pc, trigger, (1 to 9).map(trigger + _))
        dut.io.response.bits.success.expect(true.B)
        val ref = identity(dut)
        drain(dut)
        ref
      }

      val keep = install(first)
      val victim = install(second)
      keep._1 / 2 shouldBe victim._1 / 2
      transaction(dut, TemporalMetaOp.Feedback, 0, 0, ref = keep, priority = 0)
      dut.io.response.bits.success.expect(true.B)
      drain(dut)

      val replacement = install(third)
      replacement shouldBe (victim._1, victim._2 + 1)
      transaction(dut, TemporalMetaOp.Feedback, 0, 0, ref = victim, priority = 3)
      dut.io.response.bits.success.expect(false.B)
      drain(dut)

      for ((trigger, hit) <- Seq((first, true), (second, false), (third, true))) {
        transaction(dut, TemporalMetaOp.Query, pc, trigger)
        dut.io.response.bits.hit.expect(hit.B)
        drain(dut)
      }
    }
  }
}
