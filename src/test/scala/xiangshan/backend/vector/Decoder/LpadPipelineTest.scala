package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.ArchZicfilp
import xiangshan.backend.Bundles.DecodeOutUop
import xiangshan.backend.decode.SpecZicfilp
import xiangshan.backend.decode.isa.Instructions
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.wrapper.{JumpUnit, LinkUnit}
import xiangshan.backend.issue.ImmExtractor

// Connect the actual DecodeStage to the actual immediate extractor and execution units.
// The x7 input supplies register data; rename/issue scheduling is outside this probe.
private class LpadPipelineProbe(implicit p: Parameters) extends Module {
  private val decode = Module(LazyModule(new DecodeStage()).module)
  private val jump = Module(new JumpUnit(FuConfig.NJmpCfg))
  private val link = Module(new LinkUnit(FuConfig.LinkCfg))
  val io = IO(new Bundle {
    val insts = Input(Vec(8, UInt(32.W)))
    val valid = Input(Vec(8, Bool()))
    val aligned = Input(Vec(8, Bool()))
    val ready = Output(Vec(8, Bool()))
    val canAccept = Input(Bool())
    val enable = Input(Bool())
    val flush = Input(Bool())
    val redirect = Flipped(ValidIO(Bool()))
    val lane = Input(UInt(3.W))
    val x7 = Input(UInt(64.W))
    val pc = Input(UInt(64.W))
    val decoded = Output(Vec(8, ValidIO(new DecodeOutUop)))
    val intRatAddr = Output(Vec(8, UInt(6.W)))
    val jumpValid = Output(Bool())
    val jumpException = Output(Bool())
    val jumpRedirect = Output(Bool())
    val jumpResolve = Output(Bool())
    val expandedImm = Output(UInt(64.W))
    val linkValid = Output(Bool())
    val linkValue = Output(UInt(64.W))
  })
  private def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  zeroInputs(decode.in)
  zeroInputs(decode.out)
  zeroInputs(decode.stallReason)
  zeroInputs(jump.io)
  zeroInputs(link.io)
  decode.in.redirect.valid := io.flush
  decode.in.ZicfilpRedirect.get := io.redirect
  decode.in.fromCSR.enableZicfilp.get := io.enable
  decode.in.fromCSR.enableZicfiss.foreach(_ := true.B)
  for (i <- 0 until 8) {
    decode.in.mop(i).valid := io.valid(i)
    decode.in.mop(i).bits.instr := io.insts(i)
    decode.in.mop(i).bits.ZicfilpPCAligned.get := io.aligned(i)
    decode.out.uop(i).ready := io.canAccept
    io.ready(i) := decode.in.mop(i).ready
    io.decoded(i).valid := decode.out.uop(i).valid
    io.decoded(i).bits := decode.out.uop(i).bits
    io.intRatAddr(i) := decode.out.intRat(i)(0).addr
  }
  private val selected = decode.out.uop(io.lane)
  private val jumpImm = ImmExtractor(selected.bits.imm, selected.bits.selImm, 64, FuConfig.NJmpCfg.immType)
  private val linkImm = ImmExtractor(selected.bits.imm, selected.bits.selImm, 64, FuConfig.LinkCfg.immType)
  jump.io.in.valid := selected.valid && selected.bits.fuType === FuType.jmp.U
  jump.io.in.bits.ctrl.fuOpType := selected.bits.fuOpType
  jump.io.in.bits.ctrl.ZicfilpInfos.get := selected.bits.ZicfilpInfos.get
  jump.io.in.bits.data.src(0) := io.x7
  jump.io.in.bits.data.imm := jumpImm
  jump.io.in.bits.data.pc.get := io.pc
  jump.io.instrAddrTransType.get.bare := true.B
  jump.io.out.ready := io.canAccept
  io.jumpValid := jump.io.out.valid
  io.jumpException := jump.io.out.bits.ctrl.exceptionVec(ExceptionNO.softwareCheck)
  io.jumpRedirect := jump.io.out.bits.res.redirect.get.valid
  io.jumpResolve := jump.io.toFrontendBJUResolve.get.valid
  io.expandedImm := jumpImm
  link.io.in.valid := selected.valid && selected.bits.fuType === FuType.link.U
  link.io.in.bits.ctrl.fuOpType := selected.bits.fuOpType
  link.io.in.bits.ctrl.rfWen.get := selected.bits.rfWen
  link.io.in.bits.data.imm := linkImm
  link.io.in.bits.data.pc.get := io.pc
  link.io.in.bits.data.nextPcOffset.get := 4.U
  link.io.instrAddrTransType.get.bare := true.B
  link.io.out.ready := io.canAccept
  io.linkValid := link.io.out.valid
  io.linkValue := link.io.out.bits.res.data
}

class LpadPipelineTest extends AnyFlatSpec with ChiselSim {
  import chisel3.simulator.HasSimulator.simulators.verilator

  private def config: Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasZicfilp = true, HasShadowStack = true)
    }
  }
  private def lpad(label: Int): BigInt = (BigInt(label) << 12) | 0x17
  private def jalr(rs1: Int, rd: Int): BigInt = (BigInt(rs1) << 15) | (rd << 7) | 0x67
  private val mask64 = (BigInt(1) << 64) - 1

  behavior of "LPAD pipeline"

  it should "check real DecodeStage and JumpUnit behavior for labels, JALRs, buffering and redirects" in {
    simulate(new LpadPipelineProbe()(config)) { dut =>
      dut.io.insts.foreach(_.poke(0x00108093.U))
      dut.io.valid.foreach(_.poke(false.B))
      dut.io.aligned.foreach(_.poke(true.B))
      dut.io.canAccept.poke(true.B)
      dut.io.enable.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.io.redirect.bits.poke(false.B)
      dut.io.lane.poke(0.U)
      dut.io.x7.poke(0.U)
      dut.io.pc.poke(0x80000000L.U)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      def restore(elp: Boolean): Unit = {
        dut.io.valid.foreach(_.poke(false.B))
        dut.io.flush.poke(true.B)
        dut.io.redirect.valid.poke(true.B)
        dut.io.redirect.bits.poke(elp.B)
        dut.clock.step()
        dut.io.flush.poke(false.B)
        dut.io.redirect.valid.poke(false.B)
        dut.io.decoded.foreach(_.valid.expect(false.B))
      }
      def target(label: Int, x7: BigInt, fault: Boolean): Unit = {
        dut.io.insts(0).poke(lpad(label).U)
        dut.io.x7.poke(x7.U)
        dut.io.decoded(0).bits.fuType.expect(FuType.jmp.U)
        dut.io.decoded(0).bits.lsrc(0).expect(7.U)
        dut.io.decoded(0).bits.srcType(0).expect(SrcType.reg)
        dut.io.decoded(0).bits.rfWen.expect(false.B)
        dut.io.decoded(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
        dut.io.intRatAddr(0).expect(7.U)
        val rawImm = BigInt(label) << 12
        val expanded = (if (rawImm.testBit(31)) rawImm | (mask64 ^ ((BigInt(1) << 32) - 1)) else rawImm) & mask64
        dut.io.expandedImm.expect(expanded.U)
        dut.io.jumpValid.expect(true.B)
        dut.io.jumpException.expect(fault.B)
        dut.io.jumpRedirect.expect(false.B)
        dut.io.jumpResolve.expect(false.B)
        dut.io.linkValid.expect(false.B)
      }
      restore(elp = true)
      dut.io.valid(0).poke(true.B)
      dut.io.canAccept.poke(false.B)
      for (label <- Seq(0, 1, 0x12345, 0x7ffff, 0x80000, 0xfffff)) {
        val matching = (BigInt("fedcba98", 16) << 32) | (BigInt(label) << 12) | 0xabc
        target(label, matching, fault = false)
        target(label, matching ^ (BigInt(1) << 12), fault = label != 0)
      }
      target(0x12345, BigInt("12344000", 16), fault = true)
      dut.clock.step(3)
      dut.io.jumpException.expect(true.B)
      target(0x12345, BigInt("12345000", 16), fault = false)
      dut.io.canAccept.poke(true.B)
      dut.clock.step()
      dut.io.decoded(0).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(false.B)
      dut.io.decoded(0).bits.fuType.expect(FuType.link.U)
      dut.io.decoded(0).bits.srcType(0).expect(SrcType.no)
      dut.io.jumpValid.expect(false.B)
      dut.io.linkValid.expect(true.B)
      dut.io.linkValue.expect(BigInt("92345000", 16).U)

      // All rs1 values, with and without a link destination, must select the same ELP policy.
      for (rd <- Seq(0, 1, 5, 7, 31); rs1 <- 0 until 32) {
        restore(elp = false)
        dut.io.valid(0).poke(true.B)
        dut.io.valid(1).poke(true.B)
        dut.io.insts(0).poke(jalr(rs1, rd).U)
        dut.io.insts(1).poke(lpad(0).U)
        val lane = if (rd == 0) 1 else 2
        val protectedJalr = !Seq(1, 5, 7).contains(rs1)
        dut.io.decoded(lane).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(protectedJalr.B)
        dut.io.decoded(lane).bits.fuType.expect((if (protectedJalr) FuType.jmp else FuType.link).U)
      }

      restore(elp = true)
      dut.io.valid(0).poke(true.B)
      dut.io.insts(0).poke(lpad(0).U)
      dut.io.aligned(0).poke(false.B)
      dut.io.decoded(0).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(false.B)
      dut.io.decoded(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(true.B)
      dut.io.decoded(0).bits.fuType.expect(FuType.link.U)
      dut.io.aligned(0).poke(true.B)
      dut.io.insts(0).poke((lpad(0) | 0x80).U)
      dut.io.decoded(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(true.B)
      dut.io.insts(0).poke(0x00108093.U)
      dut.io.decoded(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(true.B)

      // The missing-LPAD exception belongs only to the first SSPUSH uop, including its buffered tail.
      restore(elp = false)
      dut.io.valid.foreach(_.poke(true.B))
      dut.io.insts.foreach(_.poke(0x00108093.U))
      dut.io.insts(6).poke(jalr(10, 0).U)
      dut.io.insts(7).poke(Instructions.SSPUSH_X1.value.U)
      dut.io.decoded(7).bits.firstUop.expect(true.B)
      dut.io.decoded(7).bits.exceptionVec(ExceptionNO.softwareCheck).expect(true.B)
      dut.clock.step()
      dut.io.valid.foreach(_.poke(false.B))
      for (i <- 0 until 4) {
        dut.io.decoded(i).valid.expect(true.B)
        dut.io.decoded(i).bits.firstUop.expect(false.B)
        dut.io.decoded(i).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
      }
      restore(elp = true)
      dut.io.valid(0).poke(true.B)
      dut.io.insts(0).poke(lpad(0).U)
      dut.io.decoded(0).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(true.B)
      dut.io.enable.poke(false.B)
      dut.io.decoded(0).bits.fuType.expect(FuType.link.U)
      dut.io.decoded(0).bits.ZicfilpInfos.get.ZicfilpLPADValid.expect(false.B)
      dut.io.decoded(0).bits.exceptionVec(ExceptionNO.softwareCheck).expect(false.B)
    }
  }

  it should "retain speculative ELP for bubbles and unaccepted suffixes with redirect priority" in {
    simulate(new SpecZicfilp(4)(config)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.redirect.valid.poke(false.B)
      dut.io.redirect.bits.poke(false.B)
      dut.io.valid.foreach(_.poke(false.B))
      dut.io.fire.foreach(_.poke(false.B))
      dut.io.isJalr.foreach(_.poke(false.B))
      dut.io.isLPAD.foreach(_.poke(false.B))
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      val random = new scala.util.Random(0x51ec)
      var state = false
      for (_ <- 0 until 500) {
        val enable = random.nextInt(8) != 0
        val redirect = random.nextInt(10) == 0
        val restored = random.nextBoolean()
        val valid = Seq.fill(4)(random.nextBoolean())
        val accepted = random.nextInt(valid.count(identity) + 1)
        var seen = 0
        val fire = valid.map { v =>
          val f = v && seen < accepted
          if (v) seen += 1
          f
        }
        val kinds = Seq.fill(4)(random.nextInt(3))
        dut.io.enable.poke(enable.B)
        dut.io.redirect.valid.poke(redirect.B)
        dut.io.redirect.bits.poke(restored.B)
        var preview = state
        var next = state
        for (i <- 0 until 4) {
          dut.io.valid(i).poke(valid(i).B)
          dut.io.fire(i).poke(fire(i).B)
          dut.io.isJalr(i).poke((kinds(i) == 1).B)
          dut.io.isLPAD(i).poke((kinds(i) == 2).B)
        }
        for (i <- 0 until 4) {
          dut.io.elp(i).expect(preview.B)
          dut.io.exception(i).expect((enable && valid(i) && preview && kinds(i) != 2).B)
          dut.io.lpadValid(i).expect((enable && valid(i) && preview && kinds(i) == 2).B)
          if (valid(i)) preview = kinds(i) == 1
          if (fire(i)) next = kinds(i) == 1
        }
        state = if (redirect) restored else if (!enable) false else next
        dut.clock.step()
      }
    }
  }

  it should "preserve architectural ELP across ordered commit, traps and xRET restoration" in {
    simulate(new ArchZicfilp()(config)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.trap.poke(false.B)
      dut.io.entryClear.poke(false.B)
      dut.io.xret.valid.poke(false.B)
      dut.io.xret.bits.poke(false.B)
      dut.io.commitValid.foreach(_.poke(false.B))
      dut.io.commitJalr.foreach(_.poke(false.B))
      dut.io.commitLPAD.foreach(_.poke(false.B))
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      val random = new scala.util.Random(0xa2c)
      var state = false
      for (_ <- 0 until 500) {
        val enable = random.nextInt(8) != 0
        val trap = random.nextInt(10) == 0
        val entry = random.nextInt(12) == 0
        val xret = random.nextInt(10) == 0
        val restored = random.nextBoolean()
        dut.io.enable.poke(enable.B)
        dut.io.trap.poke(trap.B)
        dut.io.entryClear.poke(entry.B)
        dut.io.xret.valid.poke(xret.B)
        dut.io.xret.bits.poke(restored.B)
        var committed = state
        for (i <- dut.io.commitValid.indices) {
          val valid = random.nextBoolean()
          val kind = random.nextInt(3)
          dut.io.commitValid(i).poke(valid.B)
          dut.io.commitJalr(i).poke((kind == 1).B)
          dut.io.commitLPAD(i).poke((kind == 2).B)
          if (enable && valid && kind != 0) committed = kind == 1
        }
        dut.io.archELP.expect((state && enable).B)
        state = if (trap || entry) false else if (xret) restored else if (!enable) false else committed
        dut.clock.step()
      }
    }
  }
}
