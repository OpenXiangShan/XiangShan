package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util.ValidIO
import org.chipsalliance.cde.config.Parameters
import utility.PerfEvent
import xiangshan.{DebugOptionsKey, XSTester}

class HPMCounterTestIO extends Bundle {
  val w = Input(new CSRAddrWriteBundle(new MhpmcounterBundle))
  val fromSireg = Flipped(ValidIO(UInt(64.W)))
  val mcountinhibit = Input(new McountinhibitBundle)
  val countingEn = Input(Bool())
  val perf = Input(new PerfEvent)
  val rdata = Output(UInt(64.W))
  val toMhpmeventOF = Output(Bool())
}

/** Drive the actual inline HPM3 instance; do not duplicate its implementation. */
class HPMCounterHarness(implicit p: Parameters) extends Module {
  val io = IO(new HPMCounterTestIO)
  val csr = Module(new NewCSR {
    val counterTest = IO(new HPMCounterTestIO)
    private val counter = mhpmcounters.head.asInstanceOf[
      CSRModule[MhpmcounterBundle] with HasMachineCounterControlBundle
        with HasPerfCounterBundle with HasSiregCounterBundle]
    counter.w := counterTest.w
    counter.fromSireg := counterTest.fromSireg
    counter.mcountinhibit := counterTest.mcountinhibit
    counter.countingEn := counterTest.countingEn
    counter.perf := counterTest.perf
    counterTest.rdata := counter.rdata
    counterTest.toMhpmeventOF := counter.toMhpmeventOF
  })
  // Keep unrelated CSR inputs idle. Clock/reset retain their normal connections.
  DataMirror.modulePorts(csr).filterNot { case (name, _) => Set("clock", "reset", "counterTest").contains(name) }
    .flatMap { case (_, port) => DataMirror.collectLeafMembers(port) }
    .filter(DataMirror.directionOf(_) == ActualDirection.Input)
    .foreach(input => input := 0.U.asTypeOf(input))
  io <> csr.counterTest
}

class HPMCounterRegressionTest extends XSTester {
  it should "suppress overflow on direct and indirect counter writes and honor inhibit" in {
    val params = config.alterPartial {
      case DebugOptionsKey => config(DebugOptionsKey).copy(EnableDifftest = false, AlwaysBasicDiff = false)
    }
    simulate(new HPMCounterHarness()(params)) { dut =>
      val max = (BigInt(1) << 64) - 1
      dut.io.w.wen.poke(false.B)
      dut.io.w.wdata.poke(0.U)
      dut.io.fromSireg.valid.poke(false.B)
      dut.io.fromSireg.bits.poke(0.U)
      dut.io.mcountinhibit.CY.poke(BigInt(0))
      dut.io.mcountinhibit.IR.poke(BigInt(0))
      dut.io.mcountinhibit.HPM3.poke(BigInt(0))
      dut.io.countingEn.poke(false.B)
      dut.io.perf.value.poke(0.U)
      def seed(value: BigInt): Unit = {
        dut.io.w.wen.poke(true.B)
        dut.io.w.wdata.poke(value.U)
        dut.clock.step()
        dut.io.w.wen.poke(false.B)
      }
      for (step <- Seq(1, 63); source <- Seq("direct", "indirect", "both", "increment", "inhibit", "disabled")) {
        dut.io.countingEn.poke(false.B)
        dut.io.mcountinhibit.HPM3.poke(BigInt(0))
        dut.io.fromSireg.valid.poke(false.B)
        seed(max)
        dut.io.countingEn.poke((source != "disabled").B)
        dut.io.perf.value.poke(step.U)
        dut.io.w.wen.poke((source == "direct" || source == "both").B)
        dut.io.w.wdata.poke(0.U)
        dut.io.fromSireg.valid.poke((source == "indirect" || source == "both").B)
        dut.io.fromSireg.bits.poke(7.U)
        dut.io.mcountinhibit.HPM3.poke(BigInt(if (source == "inhibit") 1 else 0))
        dut.io.toMhpmeventOF.expect((source == "increment").B)
        dut.clock.step()
        val expected = source match {
          case "direct" | "both" => BigInt(0)
          case "indirect" => BigInt(7)
          case "increment" => BigInt(step - 1)
          case _ => max
        }
        dut.io.rdata.expect(expected.U)
      }
      dut.io.fromSireg.valid.poke(false.B)
      dut.io.mcountinhibit.HPM3.poke(BigInt(0))
      dut.io.countingEn.poke(false.B)
      seed(max - 1)
      dut.io.countingEn.poke(true.B)
      dut.io.perf.value.poke(1.U)
      dut.io.toMhpmeventOF.expect(false.B)
      dut.clock.step()
      dut.io.rdata.expect(max.U)
      dut.io.perf.value.poke(0.U)
      dut.io.toMhpmeventOF.expect(false.B)
      dut.clock.step()
      dut.io.rdata.expect(max.U)
    }
  }

}
