package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan.{XSCoreParameters, XSCoreParamsKey}
import xiangshan.backend.decode.isa.CSRs

/**
 * A read-only CSR instruction (csrrs rd, csr, x0) of a counter must wait for the older instructions (noSpec) and a
 * read of an ordinary CSR must not. Reading the counters out of order returns a value that misses older instructions.
 */
class PseudoDecodeChannelCsrTest extends AnyFlatSpec with Matchers with ChiselSim {
  implicit val p: Parameters = (new DefaultConfig).alterPartial {
    case XSCoreParamsKey => XSCoreParameters()
  }

  // csrrs x1, csr, x0
  private def csrrs(csr: Int): UInt = ((csr.toLong << 20) | (2L << 12) | (1L << 7) | 0x73L).U(32.W)

  behavior of "PseudoDecodeChannel read-only CSR instructions"

  it should "wait for older instructions on counter reads and not on other CSRs" in {
    simulate(new PseudoDecodeChannel()(p)) { dut =>
      dut.in.fromCSR.illegalInst.vsIsOff.poke(false.B)
      def noSpec(csr: Int): Boolean = {
        dut.in.rawInst.poke(csrrs(csr))
        dut.out.valid.expect(true.B)
        dut.out.bits.noSpec.peek().litToBoolean
      }
      val counters = Seq(
        "minstret" -> CSRs.minstret,
        "instret" -> CSRs.instret,
        "mcycle" -> CSRs.mcycle,
        "cycle" -> CSRs.cycle,
      )
      val controls = Seq("mstatus (waits)" -> CSRs.mstatus, "mscratch (does not wait)" -> CSRs.mscratch)
      val waits = (counters ++ controls).map { case (name, addr) => name -> noSpec(addr) }.toMap
      (counters ++ controls).foreach { case (name, _) => info(s"csrrs x1, $name, x0: noSpec = ${waits(name)}") }
      // controls first: they hold on the unchanged tree, so a failing counter line below is about the counters only
      waits("mstatus (waits)") shouldBe true
      waits("mscratch (does not wait)") shouldBe false
      // counters change on their own; a read must see every older instruction, so it waits for them
      counters.foreach { case (name, _) => withClue(s"csrrs x1, $name, x0: ") { waits(name) shouldBe true } }
    }
  }
}
