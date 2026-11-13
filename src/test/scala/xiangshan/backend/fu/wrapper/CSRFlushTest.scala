package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan.{XSCoreParameters, XSCoreParamsKey}
import xiangshan.backend.fu.FuConfig

/**
 * A CSR read that is not serialised (CSROoORead) is written back three cycles after the CSR module produced it. A
 * redirect that flushes the read while it sits in those cycles must stop the write-back.
 */
class CSRFlushTest extends AnyFlatSpec with Matchers with ChiselSim {
  implicit val p: Parameters = (new DefaultConfig).alterPartial {
    case XSCoreParamsKey => XSCoreParameters()
  }

  behavior of "CSR output delay"

  it should "drop a read flushed while it is in the output delay" in {
    simulate(new CSR(FuConfig.CsrCfg)) { dut =>
      val window = 12
      def bits(seen: Seq[Boolean]): String = seen.map(if (_) "1" else "0").mkString
      // the delay registers have no reset value: let them settle with an idle input before the first read is issued
      dut.io.in.valid.poke(false.B)
      dut.io.flush.valid.poke(false.B)
      dut.io.out.ready.poke(true.B)
      dut.clock.step(8)
      // Issue `csrrs x1, mscratch, x0` (robIdx = entry 3) and return, for each of the following cycles, whether the
      // write-back valid was high. A redirect flushing that robIdx is raised `flushAt` cycles after the issue.
      def issue(flushAt: Option[Int], flushRobIdx: Int = 3): Seq[Boolean] = {
        dut.io.out.ready.poke(true.B)
        dut.io.flush.valid.poke(false.B)
        dut.io.in.bits.ctrl.fuOpType.poke("b001010".U) // csrrs
        dut.io.in.bits.data.imm.poke(((1L << 17) | 0x340L).U) // rd = x1, rs1 = x0, csr = mscratch
        dut.io.in.bits.ctrl.robIdx.value.poke(3.U)
        dut.io.in.bits.ctrl.robIdx.flag.poke(false.B)
        dut.io.in.bits.ctrl.robIdx.isFormer.poke(false.B)
        dut.io.in.valid.poke(true.B)
        dut.io.in.ready.expect(true.B)
        dut.clock.step()
        dut.io.in.valid.poke(false.B)
        val seen = (1 to window).map { cycle =>
          if (flushAt.contains(cycle)) {
            dut.io.flush.valid.poke(true.B)
            dut.io.flush.bits.robIdx.value.poke(flushRobIdx.U)
            dut.io.flush.bits.robIdx.flag.poke(false.B)
            dut.io.flush.bits.robIdx.isFormer.poke(false.B)
            dut.io.flush.bits.level.poke(true.B) // flush the instruction itself
          } else {
            dut.io.flush.valid.poke(false.B)
          }
          val v = dut.io.out.valid.peek().litToBoolean
          dut.clock.step()
          v
        }
        dut.io.flush.valid.poke(false.B)
        dut.clock.step(window)
        seen
      }

      val noFlush = issue(None)
      info(s"no flush: out.valid per cycle = ${bits(noFlush)}")
      // positive control: without a flush the read is written back exactly once
      noFlush.count(identity) shouldBe 1
      val t = noFlush.indexWhere(identity) + 1 // cycle (after the issue) in which out.valid is high

      // control: a redirect on a younger entry (robIdx 5, flushes entries after 5) does not touch the older read
      val youngerRedirect = issue(Some(t - 1), flushRobIdx = 5)
      info(s"flush of a younger entry at cycle ${t - 1}: out.valid per cycle = ${bits(youngerRedirect)}")
      youngerRedirect.count(identity) shouldBe 1

      // a flush that arrives while the read is in the three delay cycles before out.valid must stop it
      val flushed = ((t - 3) until t).map { flushAt =>
        val seen = issue(Some(flushAt))
        info(s"flush at cycle $flushAt: out.valid per cycle = ${bits(seen)}")
        flushAt -> seen.count(identity)
      }
      flushed.foreach { case (flushAt, writebacks) =>
        withClue(s"flush at cycle $flushAt (write-back would be at cycle $t): ") { writebacks shouldBe 0 }
      }
    }
  }
}
