package xiangshan.backend

import chisel3._
import chisel3.util.Cat
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{DecodedInst, DynInst, StaticInst}
import xiangshan.backend.rob.RobBundles
import xiangshan.backend.rob.RobBundles.RobEntryBundle
import xiangshan.{TeaEvent, TeaFrontend}

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
}
