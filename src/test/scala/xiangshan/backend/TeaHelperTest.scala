package xiangshan.backend

import chisel3._
import chisel3.util.Cat
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.ctrlblock.DebugLsInfo
import xiangshan.backend.Bundles.{DecodedInst, DynInst, StaticInst}
import xiangshan.backend.rob.RobBundles
import xiangshan.backend.rob.RobBundles.RobEntryBundle
import xiangshan.frontend.{FetchToIBuffer, FrontendTopDownBundle, IBuffer}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan.{TeaBinders, TeaEvent, TeaFrontend}

class TeaHelperTest extends XSTester {
  behavior of "TEA helper metadata"

  private def teaBitValue(event: Int): BigInt = BigInt(1) << event

  class TeaFieldSmoke(p: Parameters) extends Module {
    val io = IO(new Bundle {
      val out = Output(UInt((TeaEvent.width * 5).W))
    })
    val cf = WireInit(0.U.asTypeOf(new CtrlFlow()(p)))
    val st = WireInit(0.U.asTypeOf(new StaticInst()(p)))
    val de = WireInit(0.U.asTypeOf(new DecodedInst()(p)))
    val dy = WireInit(0.U.asTypeOf(new DynInst()(p)))
    val rob = WireInit(0.U.asTypeOf(new RobEntryBundle()(p)))
    io.out := Cat(cf.teaPsv, st.teaPsv, de.teaPsv, dy.teaPsv, rob.teaPsv)
  }

  class TeaPropagationSmoke(p: Parameters) extends Module {
    private val ctrlTea = TeaPsvOps.setBit(TeaEvent.bit(TeaEvent.DR_L1), TeaEvent.FL_MB)
    private val robTea = TeaPsvOps.setBit(TeaEvent.bit(TeaEvent.ST_L1), TeaEvent.ST_TLB)

    val io = IO(new Bundle {
      val staticTea = Output(UInt(TeaEvent.width.W))
      val decodedTea = Output(UInt(TeaEvent.width.W))
      val robTea = Output(UInt(TeaEvent.width.W))
    })

    val cf = WireInit(0.U.asTypeOf(new CtrlFlow()(p)))
    cf.teaPsv := ctrlTea
    val st = WireInit(0.U.asTypeOf(new StaticInst()(p)))
    st.connectCtrlFlow(cf)
    val de = WireInit(0.U.asTypeOf(new DecodedInst()(p)))
    de.connectStaticInst(st)

    val dy = WireInit(0.U.asTypeOf(new DynInst()(p)))
    dy.teaPsv := robTea
    val rob = WireInit(0.U.asTypeOf(new RobEntryBundle()(p)))
    RobBundles.connectEnq(rob, dy)

    io.staticTea := st.teaPsv
    io.decodedTea := de.teaPsv
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

  class BypassAwarePacketPsvHarness extends Module {
    val io = IO(new Bundle {
      val valids = Input(Vec(4, Bool()))
      val enqEnable = Input(Vec(4, Bool()))
      val enqOffset = Input(Vec(4, UInt(3.W)))
      val useBypass = Input(Bool())
      val numBypass = Input(UInt(3.W))
      val packetPsv = Input(UInt(TeaEvent.width.W))
      val out = Output(Vec(4, UInt(TeaEvent.width.W)))
    })

    io.out := TeaFrontend.bindPacketPsv(
      TeaFrontend.selectEnqueued(io.valids, io.enqEnable, io.enqOffset, io.useBypass, io.numBypass),
      io.packetPsv
    )
  }

  class IBufferBypassRegressionHarness(implicit val p: Parameters) extends Module with HasXSParameter {
    require(PredictWidth > DecodeWidth, "IBuffer bypass regression needs PredictWidth > DecodeWidth")

    val io = IO(new Bundle {
      val inValid = Input(Bool())
      val validMask = Input(UInt(PredictWidth.W))
      val enqEnableMask = Input(UInt(PredictWidth.W))
      val instrs = Input(Vec(PredictWidth, UInt(32.W)))
      val decodeCanAccept = Input(Bool())
      val packetDrL1 = Input(Bool())
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
    inBits.topdown_info.reasons(TopDownCounters.ICacheMissBubble.id) := io.packetDrL1

    ibuffer.io.flush := false.B
    ibuffer.io.ControlRedirect := false.B
    ibuffer.io.ControlBTBMissBubble := false.B
    ibuffer.io.TAGEMissBubble := false.B
    ibuffer.io.SCMissBubble := false.B
    ibuffer.io.ITTAGEMissBubble := false.B
    ibuffer.io.RASMissBubble := false.B
    ibuffer.io.MemVioRedirect := false.B
    ibuffer.io.decodeCanAccept := io.decodeCanAccept
    ibuffer.io.in.valid := io.inValid
    ibuffer.io.in.bits := inBits
    ibuffer.io.stallReason.backReason.valid := false.B
    ibuffer.io.stallReason.backReason.bits := 0.U
    ibuffer.io.out.foreach(_.ready := true.B)

    io.inReady := ibuffer.io.in.ready
    io.outValid := VecInit(ibuffer.io.out.map(_.valid))
    io.outInstr := VecInit(ibuffer.io.out.map(_.bits.instr))
    io.outTeaPsv := VecInit(ibuffer.io.out.map(_.bits.teaPsv))
  }

  class LoadBinderHarness(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(TeaEvent.width.W))
      val out = Output(UInt(TeaEvent.width.W))
    })
    val ls = DebugLsInfo.init(p)
    ls.s2_isDcacheFirstMiss := true.B
    ls.s1_isTlbFirstMiss := true.B
    io.out := TeaBinders.applyLoadDebug(io.in, ls)
  }

  class RedirectBinderHarness(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(TeaEvent.width.W))
      val isCtrl = Input(Bool())
      val out = Output(UInt(TeaEvent.width.W))
    })
    io.out := TeaBinders.applyControlRedirect(io.in, io.isCtrl)
  }

  it should "define a 9-bit TEA event space and expose teaPsv on the main pipeline bundles" in {
    TeaEvent.width shouldBe 9
    TeaEvent.bit(TeaEvent.ST_LLC).getWidth shouldBe TeaEvent.width
    test(new TeaFieldSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.out.getWidth shouldBe TeaEvent.width * 5
    }
  }

  it should "propagate teaPsv through StaticInst.connectCtrlFlow and RobBundles.connectEnq" in {
    val ctrlTea = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.FL_MB)
    val robTea = teaBitValue(TeaEvent.ST_L1) | teaBitValue(TeaEvent.ST_TLB)

    test(new TeaPropagationSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.staticTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.decodedTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.robTea.expect(robTea.U(TeaEvent.width.W))
    }
  }

  it should "bind a packet PSV only to the first valid instruction slot" in {
    test(new PacketPsvHarness) { dut =>
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

  it should "skip bypassed instructions and bind packet PSV to the first actually enqueued slot" in {
    test(new BypassAwarePacketPsvHarness) { dut =>
      dut.io.valids(0).poke(true.B)
      dut.io.valids(1).poke(true.B)
      dut.io.valids(2).poke(true.B)
      dut.io.valids(3).poke(false.B)
      dut.io.enqEnable(0).poke(true.B)
      dut.io.enqEnable(1).poke(true.B)
      dut.io.enqEnable(2).poke(true.B)
      dut.io.enqEnable(3).poke(false.B)
      dut.io.enqOffset(0).poke(0.U)
      dut.io.enqOffset(1).poke(1.U)
      dut.io.enqOffset(2).poke(2.U)
      dut.io.enqOffset(3).poke(3.U)
      dut.io.useBypass.poke(true.B)
      dut.io.numBypass.poke(2.U)
      dut.io.packetPsv.poke(TeaEvent.bit(TeaEvent.DR_L1))
      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(0.U)
      dut.io.out(2).expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.out(3).expect(0.U)
    }
  }

  it should "bind DR_L1 to the first queued IBuffer entry after bypassed outputs" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfDBHartID = 0)
    }

    test(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val decodeWidth = dut.io.outInstr.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U
      val queuedInstr = (0x1000 + decodeWidth).U(32.W)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x1000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(true.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(0x1000.U)
      dut.io.outTeaPsv(0).expect(0.U)
      dut.io.outInstr(decodeWidth - 1).expect((0x1000 + decodeWidth - 1).U(32.W))
      dut.io.outTeaPsv(decodeWidth - 1).expect(0.U)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.packetDrL1.poke(false.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(queuedInstr)
      dut.io.outTeaPsv(0).expect(TeaEvent.bit(TeaEvent.DR_L1))
    }
  }

  it should "set ST_L1 and ST_TLB without clearing existing bits" in {
    val expected = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.ST_L1) | teaBitValue(TeaEvent.ST_TLB)

    test(new LoadBinderHarness) { dut =>
      dut.io.in.poke(TeaEvent.bit(TeaEvent.DR_L1))
      dut.clock.step()
      dut.io.out.expect(expected.U(TeaEvent.width.W))
    }
  }

  it should "set FL_MB only for control redirects" in {
    test(new RedirectBinderHarness) { dut =>
      dut.io.in.poke(0.U)
      dut.io.isCtrl.poke(true.B)
      dut.clock.step()
      dut.io.out.expect(TeaEvent.bit(TeaEvent.FL_MB))

      dut.io.in.poke(0.U)
      dut.io.isCtrl.poke(false.B)
      dut.clock.step()
      dut.io.out.expect(0.U)
    }
  }
}
