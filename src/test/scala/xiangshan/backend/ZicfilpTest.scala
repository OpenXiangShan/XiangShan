package xiangshan.backend

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.ArgParser
import xiangshan._
import xiangshan.backend.Bundles.DecodeOutUop
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.wrapper.JumpUnit
import xiangshan.backend.vector.Decoder.DecodeStage

class ZicfilpDecodeHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inst = Input(Vec(DecodeWidth, UInt(32.W)))
    val count = Input(UInt(log2Ceil(DecodeWidth + 1).W))
    val aligned = Input(Bool())
    val ready = Input(Bool())
    val redirect = Input(Bool())
    val elp = Input(Bool())
    val enable = Input(Bool())
    val out = Output(Vec(DecodeWidth, Valid(new DecodeOutUop)))
  })
  val decoder = Module(LazyModule(new DecodeStage).module)
  decoder.in.redirect := 0.U.asTypeOf(decoder.in.redirect)
  decoder.in.fusion := 0.U.asTypeOf(decoder.in.fusion)
  decoder.in.fromCSR := 0.U.asTypeOf(decoder.in.fromCSR)
  decoder.in.vstart := 0.U
  decoder.stallReason.in := 0.U.asTypeOf(decoder.stallReason.in)
  decoder.in.redirect.valid := io.redirect
  decoder.in.ZicfilpRedirect.foreach { r => r.valid := io.redirect; r.bits := io.elp }
  decoder.in.fromCSR.enableZicfilp.foreach(_ := io.enable)
  for (i <- 0 until DecodeWidth) {
    decoder.in.mop(i).valid := i.U < io.count
    decoder.in.mop(i).bits := 0.U.asTypeOf(decoder.in.mop(i).bits)
    decoder.in.mop(i).bits.instr := io.inst(i)
    decoder.in.mop(i).bits.ZicfilpPCAligned.foreach(_ := io.aligned)
    decoder.out.uop(i).ready := io.ready
    io.out(i).valid := decoder.out.uop(i).valid
    io.out(i).bits := decoder.out.uop(i).bits
    decoder.out.intRat(i).foreach(_.data := 0.U)
    decoder.out.fpRat(i).foreach(_.data := 0.U)
    decoder.out.vecRat(i).foreach(_.data := 0.U)
    decoder.out.vlRat(i).data := 0.U
  }
}

class ZicfilpJumpHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val label = Input(UInt(20.W))
    val x7 = Input(UInt(64.W))
    val lpad = Input(Bool())
    val exception = Output(Bool())
    val redirect = Output(Bool())
    val resolve = Output(Bool())
  })
  val jump = Module(new JumpUnit(FuConfig.NJmpCfg))
  jump.io.in.valid := true.B
  jump.io.in.bits := 0.U.asTypeOf(jump.io.in.bits)
  jump.io.flush := 0.U.asTypeOf(jump.io.flush)
  jump.io.out.ready := true.B
  jump.io.instrAddrTransType.get := 0.U.asTypeOf(new AddrTransType)
  jump.io.instrAddrTransType.get.bare := true.B
  jump.io.in.bits.ctrl.ZicfilpInfos.get.ZicfilpLPADValid := io.lpad
  jump.io.in.bits.data.imm := Cat(io.label, 0.U(12.W))
  jump.io.in.bits.data.src(0) := io.x7
  io.exception := jump.io.out.bits.ctrl.exceptionVec(ExceptionNO.softwareCheck)
  io.redirect := jump.io.out.bits.res.redirect.get.valid
  io.resolve := jump.io.toFrontendBJUResolve.get.valid
}

class ZicfilpTest extends AnyFlatSpec {
  private def config(enabled: Boolean): Parameters = {
    val (base, _, _) = ArgParser.parse(Array("--fpga-platform", "--disable-always-basic-diff", "--disable-perf"))
    val core = base(XSTileKey).head.copy(HasZicfilp = enabled)
    base.alterPartial {
      case XSCoreParamsKey => core
      case DebugOptionsKey => DebugOptions(FPGAPlatform = true, AlwaysBasicDiff = false, EnablePerfDebug = false)
    }
  }

  behavior of "Zicfilp on the split-uop backend"

  it should "check landing pads once per instruction across stalls and buffered JALR uops" in {
    implicit val p: Parameters = config(true)
    simulate(new ZicfilpDecodeHarness) { dut =>
      val nop = BigInt("00000013", 16)
      val jalr = BigInt("000100e7", 16) // jalr x1, x2, 0: link followed by jr
      val lpad = BigInt("12345017", 16)
      def inputs(insts: Seq[BigInt]): Unit = {
        dut.io.count.poke(insts.size.U)
        dut.io.inst.zipWithIndex.foreach { case (x, i) => x.poke(insts.lift(i).getOrElse(nop).U) }
      }
      dut.io.ready.poke(true.B)
      dut.io.enable.poke(true.B)
      dut.io.aligned.poke(true.B)
      dut.io.redirect.poke(false.B)
      dut.io.elp.poke(false.B)
      inputs(Seq.empty)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)

      inputs(Seq(jalr, lpad, nop))
      dut.io.out(0).bits.firstUop.expect(true.B)
      dut.io.out(1).bits.firstUop.expect(false.B)
      dut.io.out(1).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
      dut.io.out(2).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(true.B)
      dut.io.out(2).bits.fuType.expect(FuType.jmp.U)
      dut.io.out(2).bits.lsrc(0).expect(7.U)
      dut.io.out(2).bits.rfWen.expect(false.B)
      dut.clock.step()

      // Buffer the jr uop at the end of an output group.
      inputs(Seq.fill(dut.io.inst.size - 1)(nop) :+ jalr)
      dut.clock.step()
      inputs(Seq(lpad))
      dut.io.ready.poke(false.B)
      dut.io.out(0).bits.firstUop.expect(false.B)
      dut.io.out(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
      dut.io.out(1).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(true.B)
      dut.clock.step(2)
      dut.io.out(1).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(true.B)
      dut.io.ready.poke(true.B)
      dut.clock.step()

      // Restore ELP on redirect, then reject a misaligned LPAD.
      inputs(Seq.empty)
      dut.io.redirect.poke(true.B)
      dut.io.elp.poke(true.B)
      dut.clock.step()
      dut.io.redirect.poke(false.B)
      dut.io.aligned.poke(false.B)
      inputs(Seq(lpad))
      dut.io.out(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(true.B)
      dut.io.out(0).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(false.B)
      dut.io.enable.poke(false.B)
      dut.io.out(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
      dut.io.out(0).bits.fuType.expect(FuType.link.U)
    }
  }

  it should "check LPAD labels without redirecting or training the branch predictor" in {
    implicit val p: Parameters = config(true)
    simulate(new ZicfilpJumpHarness) { dut =>
      dut.io.lpad.poke(true.B)
      dut.io.label.poke("h12345".U)
      dut.io.x7.poke("h12345000".U)
      dut.io.exception.expect(false.B)
      dut.io.redirect.expect(false.B)
      dut.io.resolve.expect(false.B)
      dut.io.x7.poke("h54321000".U)
      dut.io.exception.expect(true.B)
      dut.io.label.poke(0.U)
      dut.io.exception.expect(false.B)
      dut.io.lpad.poke(false.B)
      dut.io.redirect.expect(true.B)
      dut.io.resolve.expect(true.B)
    }
  }

  it should "keep AUIPC behavior when Zicfilp is compiled out" in {
    implicit val p: Parameters = config(false)
    simulate(new ZicfilpDecodeHarness) { dut =>
      dut.io.count.poke(1.U)
      dut.io.inst.foreach(_.poke("h12345017".U))
      dut.io.ready.poke(true.B)
      dut.io.enable.poke(true.B)
      dut.io.aligned.poke(true.B)
      dut.io.redirect.poke(false.B)
      dut.io.elp.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.io.out(0).valid.expect(true.B)
      dut.io.out(0).bits.fuType.expect(FuType.link.U)
    }
  }

  it should "elaborate the backend with the extended exception writeback ports" in {
    val base = config(true)
    // The backend currently obtains architectural vl through its basic diff port.
    implicit val p: Parameters = base.alterPartial {
      case DebugOptionsKey => base(DebugOptionsKey).copy(AlwaysBasicDiff = true)
    }
    val backend = LazyModule(new Backend(p(XSCoreParamsKey).backendParams))
    assert(_root_.circt.stage.ChiselStage.emitCHIRRTL(backend.module).nonEmpty)
  }
}
