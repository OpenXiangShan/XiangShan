package xiangshan.backend

import chisel3._
import chisel3.util.{Cat, RegEnable}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{DecodeInUop, DecodeOutUop, EnqRobUop, RenameOutUop}
import xiangshan.backend.rob.RobBundles
import xiangshan.backend.rob.RobBundles.RobEntryBundle
import xiangshan.frontend.{BackendRedirectTopdown, FetchToIBuffer, HasFrontendParameters}
import xiangshan.frontend.ibuffer.IBuffer
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan.{TeaEntry, TeaEvent, TeaFlushCause, TeaFrontend, TeaSampleSelector}

class TeaHelperTest extends XSTester {
  behavior of "TEA helper metadata"

  private def teaBitValue(event: Int): BigInt = BigInt(1) << event

  class TeaFieldSmoke(p: Parameters) extends Module {
    val io = IO(new Bundle {
      val out = Output(UInt((TeaEvent.width * 6).W))
    })
    val cf = WireInit(0.U.asTypeOf(new CtrlFlow()(p)))
    val decodeIn = WireInit(0.U.asTypeOf(new DecodeInUop()(p)))
    val decodeOut = WireInit(0.U.asTypeOf(new DecodeOutUop()(p)))
    val renameOut = WireInit(0.U.asTypeOf(new RenameOutUop()(p)))
    val enqRob = WireInit(0.U.asTypeOf(new EnqRobUop()(p)))
    val rob = WireInit(0.U.asTypeOf(new RobEntryBundle()(p)))
    io.out := Cat(cf.teaPsv, decodeIn.teaPsv, decodeOut.teaPsv, renameOut.teaPsv, enqRob.teaPsv, rob.teaPsv)
  }

  class TeaPropagationSmoke(p: Parameters) extends Module {
    private val ctrlTea = TeaPsvOps.setBit(TeaEvent.bit(TeaEvent.DR_L1), TeaEvent.FL_MB)

    val io = IO(new Bundle {
      val decodeInTea = Output(UInt(TeaEvent.width.W))
      val decodeOutTea = Output(UInt(TeaEvent.width.W))
      val renameTea = Output(UInt(TeaEvent.width.W))
      val enqRobTea = Output(UInt(TeaEvent.width.W))
      val robTea = Output(UInt(TeaEvent.width.W))
    })

    val cf = WireInit(0.U.asTypeOf(new CtrlFlow()(p)))
    cf.teaPsv := ctrlTea
    val decodeIn = WireInit(0.U.asTypeOf(new DecodeInUop()(p)))
    decodeIn.connectCtrlFlow(cf)
    val decodeOut = WireInit(0.U.asTypeOf(new DecodeOutUop()(p)))
    decodeOut.connectDecodeInUop(decodeIn)
    val renameOut = WireInit(0.U.asTypeOf(new RenameOutUop()(p)))
    Bundles.connectSamePort(renameOut, decodeOut)
    val enqRob = WireInit(0.U.asTypeOf(new EnqRobUop()(p)))
    enqRob.connectEnqRobUop(renameOut)
    val rob = WireInit(0.U.asTypeOf(new RobEntryBundle()(p)))
    RobBundles.connectEnq(rob, enqRob)

    io.decodeInTea := decodeIn.teaPsv
    io.decodeOutTea := decodeOut.teaPsv
    io.renameTea := renameOut.teaPsv
    io.enqRobTea := enqRob.teaPsv
    io.robTea := rob.teaPsv
  }

  class PacketPsvHarness extends Module {
    val io = IO(new Bundle {
      val valids = Input(Vec(4, Bool()))
      val packetPsv = Input(UInt(TeaEvent.width.W))
      val out = Output(Vec(4, UInt(TeaEvent.width.W)))
    })
    io.out := TeaFrontend.bindPacketPsv(io.valids, io.packetPsv)
  }

  class IBufferBypassRegressionHarness(implicit val p: Parameters) extends Module with HasFrontendParameters {
    require(IBufferEnqueueWidth > DecodeWidth, "IBuffer bypass regression needs enqueue width > decode width")

    val io = IO(new Bundle {
      val inValid = Input(Bool())
      val validMask = Input(UInt(IBufferEnqueueWidth.W))
      val enqEnableMask = Input(UInt(IBufferEnqueueWidth.W))
      val instrs = Input(Vec(IBufferEnqueueWidth, UInt(32.W)))
      val decodeCanAccept = Input(Bool())
      val packetDrL1 = Input(Bool())
      val packetDrTlb = Input(Bool())
      val inReady = Output(Bool())
      val outValid = Output(Vec(DecodeWidth, Bool()))
      val outInstr = Output(Vec(DecodeWidth, UInt(32.W)))
      val outTeaPsv = Output(Vec(DecodeWidth, UInt(TeaEvent.width.W)))
    })

    val ibuffer = Module(new IBuffer()(p))
    val inBits = WireInit(0.U.asTypeOf(new FetchToIBuffer()(p)))

    inBits.instrs := io.instrs
    inBits.valid := io.validMask
    inBits.enqEnable := io.enqEnableMask
    inBits.teaPsv := Mux(
      io.packetDrTlb,
      TeaPsvOps.setBit(
        Mux(io.packetDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty),
        TeaEvent.DR_TLB
      ),
      Mux(io.packetDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty)
    )

    ibuffer.io.flush := false.B
    ibuffer.io.fromBackend := 0.U.asTypeOf(new BackendToIBufBundle()(p))
    ibuffer.io.fromBackend.decodeCanAccept := io.decodeCanAccept
    ibuffer.io.backendRedirectTopdown := 0.U.asTypeOf(new BackendRedirectTopdown()(p))
    ibuffer.io.in.valid := io.inValid
    ibuffer.io.in.bits := inBits
    ibuffer.io.out.foreach(_.ready := true.B)

    io.inReady := ibuffer.io.in.ready
    io.outValid := VecInit(ibuffer.io.out.map(_.valid))
    io.outInstr := VecInit(ibuffer.io.out.map(_.bits.instr))
    io.outTeaPsv := VecInit(ibuffer.io.out.map(_.bits.teaPsv))
  }

  class IfuPacketPsvBackpressureHarness(implicit val p: Parameters) extends Module with HasFrontendParameters {
    val io = IO(new Bundle {
      val inValid = Input(Bool())
      val ready = Input(Bool())
      val validMask = Input(Vec(4, Bool()))
      val liveDrL1 = Input(Bool())
      val liveDrTlb = Input(Bool())
      val outValid = Output(Bool())
      val outPsv = Output(Vec(4, UInt(TeaEvent.width.W)))
    })

    val pending = RegInit(false.B)
    val capture = io.inValid && (!pending || io.ready)
    val packetValidMask = RegEnable(io.validMask, capture)
    val livePsv = Mux(
      io.liveDrTlb,
      TeaPsvOps.setBit(
        Mux(io.liveDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty),
        TeaEvent.DR_TLB
      ),
      Mux(io.liveDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty)
    )
    val packetPsv = RegEnable(livePsv, capture)

    when(capture) {
      pending := true.B
    }.elsewhen(pending && io.ready) {
      pending := false.B
    }

    io.outValid := pending
    io.outPsv := TeaFrontend.bindPacketPsv(packetValidMask, packetPsv)
  }

  class TeaSelectorContractHarness(implicit val p: Parameters) extends Module with HasXSParameter {
    val io = IO(new Bundle {
      val enable = Input(Bool())
      val samplePeriod = Input(UInt(64.W))
      val state = Input(UInt(4.W))
      val hasCommit = Input(Bool())
      val commitMask = Input(UInt(CommitWidth.W))
      val commitPc = Input(Vec(CommitWidth, UInt(VAddrBits.W)))
      val commitPsv = Input(Vec(CommitWidth, UInt(TeaEvent.width.W)))
      val flushCause = Input(new TeaFlushCause()(p))
      val sampleValid = Output(Bool())
      val sample = Output(new TeaEntry()(p))
    })
    val selector = Module(new TeaSampleSelector()(p))
    selector.io.enable := io.enable
    selector.io.samplePeriod := io.samplePeriod
    selector.io.state := io.state
    selector.io.hasCommit := io.hasCommit
    selector.io.commitMask := io.commitMask
    selector.io.commitPc := io.commitPc
    selector.io.commitPsv := io.commitPsv
    selector.io.flushCause := io.flushCause
    io.sampleValid := selector.io.sampleValid
    io.sample := selector.io.sample
  }

  class TeaFlushCauseReplayHarness(implicit val p: Parameters) extends Module with HasXSParameter {
    val io = IO(new Bundle {
      val enable = Input(Bool())
      val triggerFlush = Input(Bool())
      val sampleValid = Output(Bool())
      val sample = Output(new TeaEntry()(p))
    })

    val flushCause = RegInit(0.U.asTypeOf(new TeaFlushCause()(p)))
    when (io.triggerFlush) {
      flushCause.valid := true.B
      flushCause.pc := "h80004000".U
      flushCause.psv := TeaEvent.bit(TeaEvent.FL_EX)
    }

    val selector = Module(new TeaSampleSelector()(p))
    selector.io.enable := io.enable
    selector.io.samplePeriod := 0.U
    selector.io.state := TeaState.flushed
    selector.io.hasCommit := false.B
    selector.io.commitMask := 0.U
    selector.io.commitPc := 0.U.asTypeOf(selector.io.commitPc)
    selector.io.commitPsv := 0.U.asTypeOf(selector.io.commitPsv)
    selector.io.flushCause := flushCause

    when (selector.io.sampleValid && selector.io.sample.oirValid) {
      flushCause.valid := false.B
    }

    io.sampleValid := selector.io.sampleValid
    io.sample := selector.io.sample
  }

  class TeaStateHarness extends Module {
    val io = IO(new Bundle {
      val empty = Input(Bool())
      val walking = Input(Bool())
      val hasCommit = Input(Bool())
      val flushCauseValid = Input(Bool())
      val state = Output(UInt(4.W))
    })
    io.state := TeaState.classify(io.empty, io.walking, io.hasCommit, io.flushCauseValid)
  }

  it should "preserve delayed sample timestamps and require a real commit" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(0.U)
      dut.io.state.poke(1.U)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.clock.step()
      dut.io.sampleValid.expect(false.B)

      dut.io.state.poke(0.U)
      dut.io.hasCommit.poke(true.B)
      dut.io.commitMask.poke(1.U)
      dut.io.commitPc(0).poke("h80001000".U)
      dut.io.commitPsv(0).poke(TeaEvent.bit(TeaEvent.ST_L1))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.state.expect(1.U)
      dut.io.sample.cycle.expect(0.U)
    }
  }

  it should "keep wall-clock sampling cadence while a delayed sample is pending" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(2.U)
      dut.io.state.poke(1.U)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.io.flushCause.valid.poke(false.B)
      dut.clock.step()

      dut.clock.step()
      dut.io.sampleValid.expect(false.B)
      dut.io.sample.overflow.expect(true.B)
      dut.clock.step()

      dut.io.state.poke(0.U)
      dut.io.hasCommit.poke(true.B)
      dut.io.commitMask.poke(1.U)
      dut.io.commitPc(0).poke("h80002000".U)
      dut.io.commitPsv(0).poke(TeaEvent.bit(TeaEvent.ST_L1))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.state.expect(1.U)
      dut.io.sample.cycle.expect(0.U)
      dut.io.sample.overflow.expect(true.B)
    }
  }

  it should "resolve an old delayed sample and retain a new delayed boundary in the same cycle" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(0.U)
      dut.io.state.poke(1.U)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.io.flushCause.valid.poke(false.B)
      dut.clock.step()

      dut.io.hasCommit.poke(true.B)
      dut.io.commitMask.poke(1.U)
      dut.io.commitPc(0).poke("h80002100".U)
      dut.io.commitPsv(0).poke(TeaEvent.bit(TeaEvent.ST_L1))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.state.expect(1.U)
      dut.io.sample.cycle.expect(0.U)
      dut.io.sample.overflow.expect(false.B)
      dut.clock.step()

      dut.io.state.poke(0.U)
      dut.io.commitPc(0).poke("h80002200".U)
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.state.expect(1.U)
      dut.io.sample.cycle.expect(1.U)
      dut.io.sample.overflow.expect(true.B)
    }
  }

  it should "clear delayed samples and overflow when sampling is disabled" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(0.U)
      dut.io.state.poke(1.U)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.io.flushCause.valid.poke(false.B)
      dut.clock.step(2)

      dut.io.enable.poke(false.B)
      dut.clock.step()

      dut.io.enable.poke(true.B)
      dut.io.state.poke(0.U)
      dut.io.hasCommit.poke(true.B)
      dut.io.commitMask.poke(1.U)
      dut.io.commitPc(0).poke("h80002300".U)
      dut.io.commitPsv(0).poke(TeaEvent.bit(TeaEvent.ST_L1))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.state.expect(0.U)
      dut.io.sample.overflow.expect(false.B)
    }
  }

  it should "report a dropped boundary as overflow and sample flushed only in walk" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(0.U)
      dut.io.state.poke(0.U)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.clock.step()
      dut.io.sampleValid.expect(false.B)
      dut.io.sample.overflow.expect(true.B)

      dut.io.state.poke(2.U)
      dut.io.flushCause.valid.poke(true.B)
      dut.io.flushCause.pc.poke("h80002000".U)
      dut.io.flushCause.psv.poke(TeaEvent.bit(TeaEvent.FL_MB))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.oirValid.expect(true.B)
      dut.io.sample.overflow.expect(true.B)
    }
  }

  it should "prioritize a current flush cause over a pending delayed boundary" in {
    simulate(new TeaSelectorContractHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.samplePeriod.poke(0.U)
      dut.io.state.poke(TeaState.stalled)
      dut.io.hasCommit.poke(false.B)
      dut.io.commitMask.poke(0.U)
      dut.io.flushCause.valid.poke(false.B)
      dut.clock.step()

      dut.io.state.poke(TeaState.flushed)
      dut.io.flushCause.valid.poke(true.B)
      dut.io.flushCause.pc.poke("h80003000".U)
      dut.io.flushCause.psv.poke(TeaEvent.bit(TeaEvent.FL_EX))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.oirValid.expect(true.B)
      dut.io.sample.psvVec(0).expect(TeaEvent.bit(TeaEvent.FL_EX))
      dut.io.sample.overflow.expect(true.B)
    }
  }

  it should "consume a remembered flush cause after emitting its OIR sample" in {
    simulate(new TeaFlushCauseReplayHarness) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.triggerFlush.poke(true.B)
      dut.clock.step()

      dut.io.sampleValid.expect(true.B)
      dut.io.sample.oirValid.expect(true.B)
      dut.io.sample.psvVec(0).expect(TeaEvent.bit(TeaEvent.FL_EX))

      dut.io.triggerFlush.poke(false.B)
      dut.clock.step()
      dut.io.sampleValid.expect(false.B)
    }
  }

  it should "classify an empty ROB with a remembered flush cause as flushed" in {
    simulate(new TeaStateHarness) { dut =>
      dut.io.empty.poke(true.B)
      dut.io.walking.poke(false.B)
      dut.io.hasCommit.poke(false.B)
      dut.io.flushCauseValid.poke(true.B)
      dut.io.state.expect(TeaState.flushed)

      dut.io.flushCauseValid.poke(false.B)
      dut.io.state.expect(TeaState.drained)

      dut.io.empty.poke(false.B)
      dut.io.walking.poke(true.B)
      dut.io.state.expect(TeaState.flushed)

      dut.io.walking.poke(false.B)
      dut.io.hasCommit.poke(true.B)
      dut.io.state.expect(TeaState.computing)

      dut.io.hasCommit.poke(false.B)
      dut.io.state.expect(TeaState.stalled)
    }
  }

  it should "define a 9-bit TEA event space and expose teaPsv on the main pipeline bundles" in {
    TeaEvent.width shouldBe 9
    TeaEvent.bit(TeaEvent.ST_LLC).getWidth shouldBe TeaEvent.width
    simulate(new TeaFieldSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.out.getWidth shouldBe TeaEvent.width * 6
    }
  }

  it should "propagate teaPsv through the decode, rename, enqueue, and ROB bundles" in {
    val ctrlTea = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.FL_MB)

    simulate(new TeaPropagationSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.decodeInTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.decodeOutTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.renameTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.enqRobTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.robTea.expect(ctrlTea.U(TeaEvent.width.W))
    }
  }

  it should "bind a packet PSV only to the first valid instruction slot" in {
    simulate(new PacketPsvHarness) { dut =>
      dut.io.valids(0).poke(false.B)
      dut.io.valids(1).poke(true.B)
      dut.io.valids(2).poke(true.B)
      dut.io.valids(3).poke(false.B)
      dut.io.packetPsv.poke(TeaEvent.bit(TeaEvent.DR_L1))
      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.out(2).expect(0.U)
      dut.io.out(3).expect(0.U)
    }
  }

  it should "keep DR_L1 on the earliest valid instruction even when that instruction bypasses IBuffer" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val decodeWidth = dut.io.outInstr.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U
      val queuedInstr = (0x1000 + decodeWidth).U(32.W)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x1000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(true.B)
      dut.io.packetDrTlb.poke(false.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(0x1000.U)
      dut.io.outTeaPsv(0).expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.outInstr(decodeWidth - 1).expect((0x1000 + decodeWidth - 1).U(32.W))
      dut.io.outTeaPsv(decodeWidth - 1).expect(0.U)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(queuedInstr)
      dut.io.outTeaPsv(0).expect(0.U)
    }
  }

  it should "bind DR_TLB to the earliest valid instruction when the fetch packet reports an ITLB miss" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x2000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(true.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(0x2000.U)
      dut.io.outTeaPsv(0).expect(TeaEvent.bit(TeaEvent.DR_TLB))
      dut.io.outTeaPsv(1).expect(0.U)
    }
  }

  it should "bind both front-end drain bits when icache and itlb miss reasons coexist in one packet" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U
      val combinedDrain = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.DR_TLB)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x3000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(true.B)
      dut.io.packetDrTlb.poke(true.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outTeaPsv(0).expect(combinedDrain.U(TeaEvent.width.W))
      dut.io.outTeaPsv(1).expect(0.U)
    }
  }

  it should "hold packet PSV stable under IFU to IBuffer backpressure" in {
    simulate(new IfuPacketPsvBackpressureHarness()(config)) { dut =>
      val drL1 = teaBitValue(TeaEvent.DR_L1)
      dut.io.inValid.poke(true.B)
      dut.io.ready.poke(false.B)
      dut.io.validMask(0).poke(false.B)
      dut.io.validMask(1).poke(true.B)
      dut.io.validMask(2).poke(false.B)
      dut.io.validMask(3).poke(true.B)
      dut.io.liveDrL1.poke(true.B)
      dut.io.liveDrTlb.poke(false.B)
      dut.clock.step()

      dut.io.inValid.poke(false.B)
      dut.io.liveDrL1.poke(false.B)
      dut.io.liveDrTlb.poke(true.B)
      dut.clock.step(3)

      dut.io.outValid.expect(true.B)
      dut.io.outPsv(0).expect(0.U)
      dut.io.outPsv(1).expect(drL1.U(TeaEvent.width.W))
      dut.io.outPsv(2).expect(0.U)
      dut.io.outPsv(3).expect(0.U)

      dut.io.ready.poke(true.B)
      dut.io.outPsv(1).expect(drL1.U(TeaEvent.width.W))
      dut.clock.step()
      dut.io.outValid.expect(false.B)
    }
  }

}
