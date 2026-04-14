package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import xiangshan.{CSROpType, XSCoreParamsKey, XSTileKey}
import xiangshan.backend.regfile.IntPregParams

class NewCSRHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new NewCSRInput))
    val out = Decoupled(new NewCSROutput)
    val commitStuckCheckEnable = Output(Bool())
    val powerDownEnable = Output(Bool())
  })

  val dut = Module(new NewCSR)

  dut.io.fromTop.hartId := 0.U
  dut.io.fromTop.clintTime.valid := false.B
  dut.io.fromTop.clintTime.bits := 0.U
  dut.io.fromTop.l2FlushDone := false.B
  dut.io.fromTop.criticalErrorState := false.B
  dut.platformIRP := 0.U.asTypeOf(dut.platformIRP)
  dut.nonMaskableIRP := 0.U.asTypeOf(dut.nonMaskableIRP)
  dut.fromAIA := 0.U.asTypeOf(dut.fromAIA)
  dut.io.trapInst.valid := false.B
  dut.io.trapInst.bits := 0.U
  dut.io.fromMem := 0.U.asTypeOf(dut.io.fromMem)
  dut.io.fromRob := 0.U.asTypeOf(dut.io.fromRob)
  dut.io.fromVecExcpMod.busy := false.B
  dut.io.perf := 0.U.asTypeOf(dut.io.perf)
  dut.io.fetchMalTval := 0.U

  dut.io.in <> io.in
  io.out <> dut.io.out

  io.commitStuckCheckEnable := dut.io.status.custom.commit_stuck_check_enable
  io.powerDownEnable := dut.io.status.custom.power_down_enable
}

class McorepwrCommitStuckCheckTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  private val defaultConfig = new DefaultConfig
  implicit val config: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
      intPreg = IntPregParams(
        numEntries = 64,
        numRead = Some(14),
        numWrite = Some(8),
      ),
    )
  })

  behavior of "mcorepwr commit-stuck control"

  private def stepUntil(dut: NewCSRHarness, hint: String, maxCycles: Int = 20)(cond: => Boolean): Unit = {
    var cycles = 0
    while (!cond && cycles < maxCycles) {
      dut.clock.step()
      cycles += 1
    }
    withClue(hint) {
      cond shouldBe true
    }
  }

  private def driveNoTrapDefaults(dut: NewCSRHarness): Unit = {
    dut.io.out.ready.poke(true.B)
    dut.io.in.valid.poke(false.B)
    dut.io.in.bits.wen.poke(false.B)
    dut.io.in.bits.ren.poke(false.B)
    dut.io.in.bits.op.poke(CSROpType.getCSROp(CSROpType.wrt))
    dut.io.in.bits.addr.poke(0.U)
    dut.io.in.bits.src.poke(0.U)
    dut.io.in.bits.wdata.poke(0.U)
    dut.io.in.bits.mnret.poke(false.B)
    dut.io.in.bits.mret.poke(false.B)
    dut.io.in.bits.sret.poke(false.B)
    dut.io.in.bits.dret.poke(false.B)
    dut.io.in.bits.redirectFlush.poke(false.B)
  }

  it should "write mcorepwr bit1 and expose it through custom status and CSR readback" in {
    test(new NewCSRHarness()(config)).withAnnotations(Seq(VerilatorFlags(Seq()))) { dut =>
      driveNoTrapDefaults(dut)
      dut.clock.step(2)

      dut.io.commitStuckCheckEnable.expect(false.B)
      dut.io.powerDownEnable.expect(false.B)

      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.wen.poke(true.B)
      dut.io.in.bits.addr.poke("hBC0".U)
      dut.io.in.bits.wdata.poke("h2".U)
      stepUntil(dut, "write request was not accepted") {
        dut.io.in.ready.peek().litToBoolean
      }
      dut.clock.step()
      // NewCSR consumes the write data on the delayed legal-write cycle.
      // Keep the payload stable for one more cycle to match the real CSR wrapper usage.
      dut.clock.step()

      dut.io.in.valid.poke(false.B)
      dut.io.in.bits.wen.poke(false.B)

      dut.io.commitStuckCheckEnable.expect(true.B)
      dut.io.powerDownEnable.expect(false.B)

      dut.io.out.ready.poke(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.in.bits.ren.poke(true.B)
      dut.io.in.bits.addr.poke("hBC0".U)
      stepUntil(dut, "read request was not accepted") {
        dut.io.in.ready.peek().litToBoolean
      }
      dut.clock.step()
      dut.io.in.valid.poke(false.B)
      dut.io.in.bits.ren.poke(false.B)

      stepUntil(dut, "read response did not become valid") {
        dut.io.out.valid.peek().litToBoolean
      }
      dut.io.out.bits.rData.expect("h2".U)
      dut.io.out.ready.poke(true.B)
      dut.clock.step()
    }
  }
}
