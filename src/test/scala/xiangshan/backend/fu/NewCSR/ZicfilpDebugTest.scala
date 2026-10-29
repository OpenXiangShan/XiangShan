package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.ArchZicfilp
import xiangshan.backend.decode.SpecZicfilp

// Exercise real CSR events and the consumers of enableZicfilp. The probe supplies
// ROB entry snapshots/commit events, without instantiating the rest of the core.
private class ZicfilpDebugProbe(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new NewCSRInput))
    val outReady = Input(Bool())
    val trap = Input(Bool())
    val debugIP = Input(Bool())
    val snapshot = Input(Bool())
    val jalr = Input(Bool())
    val lpad = Input(Bool())
    val enable = Output(Bool())
    val debugMode = Output(Bool())
    val entryClear = Output(Bool())
    val archELP = Output(Bool())
    val lpadValid = Output(Bool())
    val exception = Output(Bool())
    val rawCSR = Output(UInt(64.W))
    val retELP = Output(Bool())
    val outValid = Output(Bool())
  })

  val csr = Module(new NewCSR)
  def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input =>
      leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  zeroInputs(csr.io)
  zeroInputs(csr.fromAIA)
  zeroInputs(csr.platformIRP)
  zeroInputs(csr.nonMaskableIRP)

  val arch = Option.when(p(XSCoreParamsKey).HasZicfilp)(Module(new ArchZicfilp))
  val spec = Option.when(p(XSCoreParamsKey).HasZicfilp)(Module(new SpecZicfilp(p(XSCoreParamsKey).DecodeWidth)))
  // Match CSR wrapper's register before the signal reaches Decode/CtrlBlock.
  val effectiveEnable = RegNext(csr.io.toDecode.enableZicfilp.getOrElse(false.B), false.B)
  arch.foreach { state =>
    state.io.commitValid.foreach(_ := false.B)
    state.io.commitJalr.foreach(_ := false.B)
    state.io.commitLPAD.foreach(_ := false.B)
    state.io.commitValid(0) := io.jalr || io.lpad
    state.io.commitJalr(0) := io.jalr
    state.io.commitLPAD(0) := io.lpad
    state.io.enable := effectiveEnable
    state.io.trap := io.trap
    state.io.entryClear := csr.io.status.zicfilpEntryClear.get
    state.io.xret.valid := false.B
    state.io.xret.bits := false.B
  }
  spec.foreach { state =>
    state.io.enable := effectiveEnable
    state.io.redirect.valid := io.trap || csr.io.status.zicfilpEntryClear.get
    state.io.redirect.bits := false.B
    state.io.valid.foreach(_ := false.B)
    state.io.fire.foreach(_ := false.B)
    state.io.isJalr.foreach(_ := false.B)
    state.io.isLPAD.foreach(_ := false.B)
    state.io.valid(0) := io.jalr || io.lpad
    state.io.fire(0) := io.jalr || io.lpad
    state.io.isJalr(0) := io.jalr
    state.io.isLPAD(0) := io.lpad
  }
  csr.io.ZicfilpELP.foreach(_ := arch.get.io.archELP)
  csr.io.in <> io.request
  csr.io.out.ready := io.outReady
  csr.io.fromRob.trap.valid := io.trap
  csr.io.fromRob.trap.bits.ZicfilpELP.foreach(_ := io.snapshot)
  csr.io.fromRob.trap.bits.isInterrupt := true.B
  csr.platformIRP.debugIP := io.debugIP

  io.enable := csr.io.toDecode.enableZicfilp.getOrElse(false.B)
  io.debugMode := csr.io.status.debugMode
  io.entryClear := csr.io.status.zicfilpEntryClear.getOrElse(false.B)
  io.archELP := arch.map(_.io.archELP).getOrElse(false.B)
  io.lpadValid := spec.map(_.io.lpadValid(0)).getOrElse(false.B)
  io.exception := spec.map(_.io.exception(0)).getOrElse(false.B)
  io.rawCSR := csr.io.out.bits.regOut
  io.retELP := csr.io.out.bits.retELP.getOrElse(false.B)
  io.outValid := csr.io.out.valid
}

class ZicfilpDebugTest extends AnyFlatSpec with ChiselSim {
  import chisel3.simulator.HasSimulator.simulators.verilator

  private def config(enabled: Boolean): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasZicfilp = enabled)
    }
  }

  behavior of "Zicfilp Debug state"

  it should "disable tracking throughout Debug and consume PELP without losing normal DRET results" in {
    simulate(new ZicfilpDebugProbe()(config(true))) { dut =>
      val mseccfg = 0x747
      val menvcfg = 0x30a
      val dcsr = 0x7b0
      val pelpMask = BigInt(1) << 18
      def bool(value: Bool): Boolean = value.peek().litToBoolean
      def raw: BigInt = dut.io.rawCSR.peek().litValue
      def address(value: Int): Unit = dut.io.request.bits.addr.poke(value.U)
      def initialize(): Unit = {
        dut.io.request.valid.poke(false.B)
        dut.io.request.bits.wen.poke(false.B)
        dut.io.request.bits.ren.poke(false.B)
        dut.io.request.bits.op.poke(0.U)
        dut.io.request.bits.addr.poke(0.U)
        dut.io.request.bits.src.poke(0.U)
        dut.io.request.bits.wdata.poke(0.U)
        dut.io.request.bits.mnret.poke(false.B)
        dut.io.request.bits.mret.poke(false.B)
        dut.io.request.bits.sret.poke(false.B)
        dut.io.request.bits.dret.poke(false.B)
        dut.io.request.bits.redirectFlush.poke(false.B)
        dut.io.outReady.poke(true.B)
        dut.io.trap.poke(false.B)
        dut.io.debugIP.poke(false.B)
        dut.io.snapshot.poke(false.B)
        dut.io.jalr.poke(false.B)
        dut.io.lpad.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(4)
        dut.reset.poke(false.B)
        dut.clock.step(4)
      }
      def write(csr: Int, value: BigInt): Unit = {
        address(csr)
        dut.io.request.bits.src.poke(value.U)
        dut.io.request.bits.wdata.poke(value.U)
        dut.io.request.bits.wen.poke(true.B)
        dut.io.request.valid.poke(true.B)
        dut.io.request.ready.expect(true.B)
        dut.clock.step()
        dut.io.request.valid.poke(false.B)
        dut.io.request.bits.wen.poke(false.B)
        dut.clock.step(3)
      }
      def enterDebug(): Unit = {
        dut.io.debugIP.poke(true.B)
        dut.clock.step(5)
        dut.io.snapshot.poke(true.B)
        dut.io.trap.poke(true.B)
        dut.io.entryClear.expect(true.B)
        dut.clock.step()
        dut.io.trap.poke(false.B)
        dut.io.debugIP.poke(false.B)
        dut.clock.step(4)
        dut.io.debugMode.expect(true.B)
        address(dcsr)
        assert((raw & pelpMask) != 0, "Debug entry must preserve the ROB's ELP snapshot")
      }

      // Vary both the saved state and the return mode's LPE. The HS cases
      // ensure DRET does not accidentally use Debug's M-mode MLPE setting.
      val cases = Seq(
        (true, true, false, 3, true),
        (true, false, false, 3, false),
        (false, true, false, 3, false),
        (true, false, true, 1, true),
        (true, true, false, 1, false),
      )
      for ((saved, mlpe, slpe, returnPrivilege, expected) <- cases) {
        initialize()
        write(mseccfg, if (mlpe) BigInt(1) << 10 else BigInt(0))
        write(menvcfg, if (slpe) BigInt(1) << 2 else BigInt(0))
        enterDebug()
        dut.io.enable.expect(false.B)
        dut.io.archELP.expect(false.B)
        address(mseccfg)
        assert(((raw >> 10) & 1) == (if (mlpe) 1 else 0), "Debug must not clear MLPE")
        address(menvcfg)
        assert(((raw >> 2) & 1) == (if (slpe) 1 else 0), "Debug must not clear LPE")

        dut.io.jalr.poke(true.B)
        dut.clock.step()
        dut.io.jalr.poke(false.B)
        dut.io.archELP.expect(false.B)
        dut.io.lpad.poke(true.B)
        dut.io.lpadValid.expect(false.B)
        dut.io.exception.expect(false.B)
        dut.clock.step()
        dut.io.lpad.poke(false.B)
        dut.io.archELP.expect(false.B)

        write(dcsr, (if (saved) pelpMask else BigInt(0)) | BigInt(returnPrivilege))
        address(dcsr)
        assert(((raw >> 18) & 1) == (if (saved) 1 else 0))
        dut.io.outReady.poke(false.B)
        dut.io.request.bits.dret.poke(true.B)
        dut.io.request.valid.poke(true.B)
        dut.io.request.ready.expect(true.B)
        dut.clock.step()
        dut.io.request.valid.poke(false.B)
        dut.io.request.bits.dret.poke(false.B)

        // The return event has consumed PELP, but the pending transaction
        // must keep the result calculated from its pre-event value.
        for (_ <- 0 until 3) {
          dut.io.debugMode.expect(false.B)
          dut.io.outValid.expect(true.B)
          dut.io.retELP.expect(expected.B)
          assert((raw & pelpMask) == 0, "Normal DRET must clear PELP")
          dut.clock.step()
        }
        dut.io.outReady.poke(true.B)
        dut.clock.step(3)
        dut.io.enable.expect((if (returnPrivilege == 3) mlpe else slpe).B)
        assert(!bool(dut.io.debugMode))
      }
    }
  }

  it should "elaborate and run without optional Zicfilp ports" in {
    simulate(new ZicfilpDebugProbe()(config(false))) { dut =>
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.wen.poke(false.B)
      dut.io.request.bits.ren.poke(false.B)
      dut.io.request.bits.op.poke(0.U)
      dut.io.request.bits.addr.poke(0.U)
      dut.io.request.bits.src.poke(0.U)
      dut.io.request.bits.wdata.poke(0.U)
      dut.io.request.bits.mnret.poke(false.B)
      dut.io.request.bits.mret.poke(false.B)
      dut.io.request.bits.sret.poke(false.B)
      dut.io.request.bits.dret.poke(false.B)
      dut.io.request.bits.redirectFlush.poke(false.B)
      dut.io.outReady.poke(true.B)
      dut.io.trap.poke(false.B)
      dut.io.debugIP.poke(false.B)
      dut.io.snapshot.poke(false.B)
      dut.io.jalr.poke(false.B)
      dut.io.lpad.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)
      dut.clock.step(4)
      dut.io.enable.expect(false.B)
      dut.io.archELP.expect(false.B)
      dut.io.lpadValid.expect(false.B)
      dut.io.retELP.expect(false.B)
    }
  }
}
