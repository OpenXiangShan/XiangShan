package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._
import xiangshan.backend.decode.isa.Instructions
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeChannel.PseudoDecodeChannel
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, DecodeSrcType}

private class PseudoLpadProbe(implicit p: Parameters) extends Module {
  private val channel = Module(new PseudoDecodeChannel())
  val in = IO(chiselTypeOf(channel.in))
  val out = IO(chiselTypeOf(channel.out))
  channel.in <> in
  out <> channel.out
}

private class LpadChannelsProbe(implicit p: Parameters) extends Module {
  private val channels = Module(new DecodeChannels(8, 8, (8, 8, 8)))
  val io = IO(new Bundle {
    val insts = Input(Vec(8, UInt(32.W)))
    val valid = Input(Vec(8, Bool()))
    val aligned = Input(Vec(8, Bool()))
    val ready = Output(Vec(8, Bool()))
    val canAccept = Input(Bool())
    val enable = Input(Bool())
    val flush = Input(Bool())
    val redirect = Flipped(ValidIO(Bool()))
  })
  val out = IO(chiselTypeOf(channels.out))
  private def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  zeroInputs(channels.in)
  channels.in.redirect := io.flush
  channels.in.renameCanAccept := io.canAccept
  channels.in.ZicfilpRedirect.foreach(_ := io.redirect)
  for (i <- 0 until 8) {
    val mop = channels.in.mops(i)
    mop.valid := io.valid(i)
    mop.bits.info.rawInst := io.insts(i)
    mop.bits.info.fromCSR.enableZicfilp.foreach(_ := io.enable)
    mop.bits.info.fromCSR.enableZicfiss.foreach(_ := true.B)
    mop.bits.ctrl.rawInst := io.insts(i)
    mop.bits.ctrl.ZicfilpPCAligned.foreach(_ := io.aligned(i))
    io.ready(i) := mop.ready
  }
  out := channels.out
}

class PseudoLpadTest extends AnyFlatSpec with ChiselSim {
  import chisel3.simulator.HasSimulator.simulators.verilator

  private def config(enabled: Boolean, shadowStack: Boolean = true): Parameters = {
    val base = new top.DefaultConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasZicfilp = enabled, HasShadowStack = shadowStack)
    }
  }
  private def lpad(label: Int = 0x12345): BigInt = (BigInt(label) << 12) | 0x17
  private def jalr(rs1: Int, rd: Int = 1): BigInt = (BigInt(rs1) << 15) | (rd << 7) | 0x67
  private val addi: BigInt = 0x00108093

  private def zero(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zero)
    case vector: Vec[_] => vector.foreach(zero)
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case _ =>
  }
  private def initialize(dut: LpadChannelsProbe): Unit = {
    zero(dut.io.insts)
    zero(dut.io.valid)
    dut.io.aligned.foreach(_.poke(true.B))
    dut.io.canAccept.poke(true.B)
    dut.io.enable.poke(true.B)
    dut.io.flush.poke(false.B)
    dut.io.redirect.valid.poke(false.B)
    dut.io.redirect.bits.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
  }
  private def restore(dut: LpadChannelsProbe, elp: Boolean): Unit = {
    dut.io.valid.foreach(_.poke(false.B))
    dut.io.flush.poke(true.B)
    dut.io.redirect.valid.poke(true.B)
    dut.io.redirect.bits.poke(elp.B)
    dut.clock.step()
    dut.io.flush.poke(false.B)
    dut.io.redirect.valid.poke(false.B)
  }

  behavior of "Pseudo LPAD decode"

  it should "claim LPAD only with LPE, ELP and alignment while preserving the SS fallback gate" in {
    simulate(new PseudoLpadProbe()(config(enabled = true))) { dut =>
      zero(dut.in)
      dut.in.rawInst.poke(lpad().U)
      for (lpe <- Seq(false, true); elp <- Seq(false, true); aligned <- Seq(false, true)) {
        dut.in.fromCSR.enableZicfilp.get.poke(lpe.B)
        dut.in.ZicfilpELP.get.poke(elp.B)
        dut.in.ZicfilpPCAligned.get.poke(aligned.B)
        dut.out.valid.expect((lpe && elp && aligned).B)
      }
      val bits = dut.out.bits
      bits.fuType.expect(FuType.jmp.U)
      bits.opcode.expect(Opcode.JmpOpcodes.j.encode.value)
      bits.src1RenType.ren.expect(true.B)
      bits.src1RenType.typ.value.expect(DecodeSrcType.GP)
      bits.lsrc1.expect(7.U)
      bits.selImm.valid.expect(true.B)
      bits.selImm.bits.expect(DecodeSelImm.U)
      bits.imm.expect(0x12345.U)
      bits.gpWen.expect(false.B)
      bits.fpWen.expect(false.B)
      bits.commitType.expect(CommitType.NORMAL)
      bits.canRobCompress.expect(false.B)
      bits.isJ.expect(false.B)
      bits.isJr.expect(false.B)
      bits.exceptionII.expect(false.B)
      dut.in.rawInst.poke((lpad() | 0x80).U)
      dut.out.valid.expect(false.B)
      dut.in.rawInst.poke(Instructions.SSPUSH_X1.value.U)
      dut.in.fromCSR.enableZicfiss.get.poke(false.B)
      dut.out.valid.expect(true.B)
      dut.in.fromCSR.enableZicfiss.get.poke(true.B)
      dut.out.valid.expect(false.B)
      dut.in.rawInst.poke(0x0000006f.U) // J remains a Pseudo instruction regardless of LPE/ELP.
      dut.in.ZicfilpELP.get.poke(false.B)
      dut.out.valid.expect(true.B)
      bits.isJ.expect(true.B)
      bits.src1RenType.ren.expect(false.B)
    }
  }

  it should "preserve instruction-order ELP through split uops, partial acceptance and stalls" in {
    simulate(new LpadChannelsProbe()(config(enabled = true))) { dut =>
      initialize(dut)
      case class Inst(raw: BigInt, count: Int = 1, aligned: Boolean = true)
      val push = Inst(Instructions.SSPUSH_X1.value, 5)
      val pop = Inst(Instructions.SSPOPCHK_X5.value, 3)
      val prefix = Seq.fill(7)(Inst(addi)) ++ Seq(Inst(jalr(10), 2), Inst(lpad()))
      val random = new scala.util.Random(0x1a2d)
      val choices = Seq(
        Inst(addi), Inst(lpad()), Inst(lpad(0)), Inst(lpad(), aligned = false),
        Inst(lpad() | 0x80), Inst(jalr(10), 2), Inst(jalr(10, 0)),
        Inst(jalr(7), 2), Inst(jalr(1, 0)), Inst(jalr(5), 2), push, pop,
      )
      val program = prefix ++ Seq.fill(150)(choices(random.nextInt(choices.size)))
      case class Expected(inst: Inst, idx: Int, jalr: Boolean, lpad: Boolean, active: Boolean, exception: Boolean)
      var elp = false
      val expected = program.flatMap { inst =>
        val isJalr = (inst.raw & 0x707f) == 0x67 && !Seq(1, 5, 7).contains(((inst.raw >> 15) & 31).toInt)
        val isLpad = (inst.raw & 0xfff) == 0x17 && inst.aligned
        val active = elp && isLpad
        val exception = elp && !isLpad
        elp = isJalr
        Seq.tabulate(inst.count)(i => Expected(inst, i, isJalr, isLpad, active, exception))
      }
      var admitted = 0
      var emitted = 0
      var cycle = 0
      var sawBufferedTail = false
      var sawStall = false
      var sawPartial = false
      var sawActive = false
      var sawMissing = false
      while (emitted < expected.size && cycle < 300) {
        val input = program.slice(admitted, admitted + 8)
        for (i <- 0 until 8) {
          dut.io.valid(i).poke((i < input.size).B)
          dut.io.insts(i).poke(input.lift(i).map(_.raw).getOrElse(addi).U)
          dut.io.aligned(i).poke(input.lift(i).forall(_.aligned).B)
        }
        val accept = cycle == 0 || cycle > 2 && random.nextInt(4) != 0
        dut.io.canAccept.poke(accept.B)
        val ready = input.indices.map(i => dut.io.ready(i).peek().litToBoolean)
        val count = ready.count(identity)
        assert(ready == Seq.fill(count)(true) ++ Seq.fill(input.size - count)(false))
        sawPartial ||= accept && count < input.size
        sawStall ||= !accept
        var presented = 0
        for (i <- 0 until 8 if dut.out.uops(i).valid.peek().litToBoolean) {
          val exp = expected(emitted + presented)
          val uop = dut.out.uops(i).bits
          val meta = uop.zicfilp.get
          uop.ctrl.rawInst.expect(exp.inst.raw.U)
          uop.info.uopIdx.expect(exp.idx.U)
          uop.info.isFirstUop.expect((exp.idx == 0).B)
          uop.info.isLastUop.expect((exp.idx == exp.inst.count - 1).B)
          meta.info.ZicfilpJalr.expect(exp.jalr.B)
          meta.isLPAD.expect(exp.lpad.B)
          meta.info.ZicfilpLPADValid.expect(exp.active.B)
          meta.exception.expect(exp.exception.B)
          if (i == 0 && exp.idx != 0) sawBufferedTail = true
          if ((exp.inst.raw & 0xfff) == 0x17) {
            uop.info.fuType.expect((if (exp.active) FuType.jmp else FuType.link).U)
            uop.info.src1Ren.expect(exp.active.B)
            uop.info.imm.expect((exp.inst.raw >> 12).U)
            if (exp.active) {
              uop.info.lsrc1.expect(7.U)
              uop.info.isJ.expect(false.B)
              uop.info.isJr.expect(false.B)
              sawActive = true
            }
          }
          sawMissing ||= exp.exception
          presented += 1
        }
        if (accept) {
          admitted += count
          emitted += presented
        }
        dut.clock.step()
        cycle += 1
      }
      assert(admitted == program.size && emitted == expected.size)
      assert(sawBufferedTail && sawStall && sawPartial && sawActive && sawMissing)

      // Redirects discard queued split tails and restore the ELP used by the next Pseudo decode.
      restore(dut, elp = true)
      dut.io.valid(0).poke(true.B)
      dut.io.insts(0).poke(lpad().U)
      dut.io.aligned(0).poke(true.B)
      dut.io.canAccept.poke(false.B)
      dut.out.uops(0).bits.info.fuType.expect(FuType.jmp.U)
      dut.clock.step(2)
      dut.out.uops(0).bits.zicfilp.get.info.ZicfilpLPADValid.expect(true.B)
      dut.io.canAccept.poke(true.B)
      dut.clock.step()
      restore(dut, elp = false)
      dut.io.valid(0).poke(true.B)
      dut.out.uops(0).bits.info.fuType.expect(FuType.link.U)
      dut.out.uops(0).bits.info.src1Ren.expect(false.B)

      restore(dut, elp = true)
      dut.io.enable.poke(false.B)
      dut.io.valid(0).poke(true.B)
      dut.out.uops(0).bits.info.fuType.expect(FuType.link.U)
      dut.out.uops(0).bits.zicfilp.get.info.ZicfilpLPADValid.expect(false.B)
      dut.clock.step()
      dut.io.enable.poke(true.B)
      dut.io.insts(0).poke(addi.U)
      dut.out.uops(0).bits.zicfilp.get.exception.expect(false.B)
    }
  }

  it should "elaborate with Zicfilp disabled and leave LPAD on the Simple AUIPC path" in {
    simulate(new LpadChannelsProbe()(config(enabled = false, shadowStack = false))) { dut =>
      initialize(dut)
      dut.io.valid(0).poke(true.B)
      dut.io.insts(0).poke(lpad().U)
      dut.out.uops(0).valid.expect(true.B)
      dut.out.uops(0).bits.info.fuType.expect(FuType.link.U)
      dut.out.uops(0).bits.info.opcode.expect(Opcode.LinkOpcodes.auipc.encode.value)
      dut.out.uops(0).bits.info.src1Ren.expect(false.B)
      assert(dut.out.uops(0).bits.zicfilp.isEmpty)
    }
  }
}
