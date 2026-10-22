package xiangshan.backend.fu.NewCSR

import chisel3._
import xiangshan.XSTester

class HPMCounterRegressionTest extends XSTester {
  it should "suppress overflow on direct and indirect counter writes and honor inhibit" in {
    simulate(new MhpmcounterModule(3)) { dut =>
      val max = (BigInt(1) << 64) - 1
      dut.w.wen.poke(false.B)
      dut.w.wdata.poke(0.U)
      dut.fromSireg.valid.poke(false.B)
      dut.fromSireg.bits.poke(0.U)
      dut.mcountinhibit.CY.poke(BigInt(0))
      dut.mcountinhibit.IR.poke(BigInt(0))
      dut.mcountinhibit.HPM3.poke(BigInt(0))
      dut.countingEn.poke(false.B)
      dut.perf.value.poke(0.U)
      def seed(value: BigInt): Unit = {
        dut.w.wen.poke(true.B)
        dut.w.wdata.poke(value.U)
        dut.clock.step()
        dut.w.wen.poke(false.B)
      }
      for (step <- Seq(1, 63); source <- Seq("direct", "indirect", "both", "increment", "inhibit", "disabled")) {
        dut.countingEn.poke(false.B)
        dut.mcountinhibit.HPM3.poke(BigInt(0))
        dut.fromSireg.valid.poke(false.B)
        seed(max)
        dut.countingEn.poke((source != "disabled").B)
        dut.perf.value.poke(step.U)
        dut.w.wen.poke((source == "direct" || source == "both").B)
        dut.w.wdata.poke(0.U)
        dut.fromSireg.valid.poke((source == "indirect" || source == "both").B)
        dut.fromSireg.bits.poke(7.U)
        dut.mcountinhibit.HPM3.poke(BigInt(if (source == "inhibit") 1 else 0))
        dut.toMhpmeventOF.expect((source == "increment").B)
        dut.clock.step()
        val expected = source match {
          case "direct" | "both" => BigInt(0)
          case "indirect" => BigInt(7)
          case "increment" => BigInt(step - 1)
          case _ => max
        }
        dut.rdata.expect(expected.U)
      }
      dut.fromSireg.valid.poke(false.B)
      dut.mcountinhibit.HPM3.poke(BigInt(0))
      dut.countingEn.poke(false.B)
      seed(max - 1)
      dut.countingEn.poke(true.B)
      dut.perf.value.poke(1.U)
      dut.toMhpmeventOF.expect(false.B)
      dut.clock.step()
      dut.rdata.expect(max.U)
      dut.perf.value.poke(0.U)
      dut.toMhpmeventOF.expect(false.B)
    }
  }

}
