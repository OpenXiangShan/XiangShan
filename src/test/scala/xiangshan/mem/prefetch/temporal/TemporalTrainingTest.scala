package xiangshan.mem.prefetch.temporal

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TemporalTrainingTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "preserve context, order, role bits and aliases on a stable sampled pair" in {
    test(new TemporalTraining(24, 2, 4, 4, 2)) { dut =>
      val load = dut.io.load
      val pair = dut.io.pair
      load.valid.poke(false.B)
      pair.ready.poke(false.B)

      def observe(pc: Int, context: Int, line: Int, alias: Int, roles: Int, kind: Int): Unit = {
        load.ready.expect(true.B)
        load.bits.pc.poke(pc.U)
        load.bits.context.poke(context.U)
        load.bits.target.line.poke(line.U)
        load.bits.target.alias.poke(alias.U)
        load.bits.roles.poke(roles.U)
        load.bits.kind.poke(kind.U)
        load.bits.bypassSampler.poke(false.B)
        load.valid.poke(true.B)
        dut.clock.step()
        load.valid.poke(false.B)
        dut.clock.step(2)
      }

      observe(0x123, 3, 0x103, 1, 1, 0)
      observe(0x123, 3, 0x241, 2, 2, 1)
      observe(0x123, 3, 0x103, 1, 0, 0)
      observe(0x123, 4, 0x103, 0, 2, 0)
      observe(0x123, 4, 0x341, 3, 2, 1)
      pair.valid.expect(false.B)
      observe(0x123, 3, 0x241, 2, 0, 1)

      def checkPair(): Unit = {
        pair.valid.expect(true.B)
        pair.bits.pc.expect(0x123.U)
        pair.bits.context.expect(3.U)
        pair.bits.roles.expect(3.U)
        pair.bits.kind.expect(1.U)
        pair.bits.base.line.expect(0x103.U)
        pair.bits.base.alias.expect(1.U)
        pair.bits.target.line.expect(0x241.U)
        pair.bits.target.alias.expect(2.U)
      }

      checkPair()
      dut.clock.step(5)
      checkPair()
      pair.ready.poke(true.B)
      dut.clock.step()
      pair.valid.expect(false.B)
    }
  }

  it should "merge dependency roles and qualify the high-quality bypass" in {
    test(new TemporalConfidence(4, 4)) { dut =>
      val request = dut.io.request
      val response = dut.io.response
      request.valid.poke(false.B)
      response.ready.poke(true.B)

      def requestConf(op: TemporalConfOp.Type, roles: Int = 0, sent: Boolean = false,
        timely: Boolean = false, checkBypass: Boolean = false, expectedRoles: Int = 3): Unit = {
        request.ready.expect(true.B)
        request.bits.op.poke(op)
        request.bits.pc.poke(0x8123.U)
        request.bits.context.poke(4.U)
        request.bits.roles.poke(roles.U)
        request.bits.issued.poke(0.U)
        request.bits.legacyUseful.poke(false.B)
        request.bits.legacyTimely.poke(false.B)
        request.bits.sent.poke(sent.B)
        request.bits.useful.poke(timely.B)
        request.bits.timely.poke(timely.B)
        request.valid.poke(true.B)
        dut.clock.step()
        request.valid.poke(false.B)
        dut.clock.step()
        response.valid.expect(true.B)
        response.bits.roles.expect(expectedRoles.U)
        response.bits.canBypassSampler.expect(checkBypass.B)
        dut.clock.step()
      }

      requestConf(TemporalConfOp.Lookup, expectedRoles = 0)
      requestConf(TemporalConfOp.Mark, roles = 1, expectedRoles = 1)
      requestConf(TemporalConfOp.Mark, roles = 2)
      for (_ <- 0 until 24) {
        requestConf(TemporalConfOp.Feedback, sent = true, timely = true)
      }
      requestConf(TemporalConfOp.Lookup, checkBypass = true)
    }
  }
}
