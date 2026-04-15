package xiangshan.backend

import chisel3._
import chisel3.util.Cat
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{DecodedInst, DynInst, StaticInst}
import xiangshan.backend.rob.RobBundles.RobEntryBundle
import xiangshan.TeaEvent

class TeaHelperTest extends XSTester {
  behavior of "TEA helper metadata"

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

  it should "define a 9-bit TEA event space and expose teaPsv on the main pipeline bundles" in {
    TeaEvent.width shouldBe 9
    test(new TeaFieldSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.out.getWidth shouldBe TeaEvent.width * 5
    }
  }
}
