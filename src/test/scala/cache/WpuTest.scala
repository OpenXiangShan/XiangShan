package cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.cache.wpu.DCacheWPU
import xiangshan.{XSCoreParamsKey, XSTileKey}

class WpuBasicTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "DCacheWPU"
  it should ("run") in {
    val defaultConfig = (new DefaultConfig)
    implicit val config = defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy()
    })
    println("========== before test ==========")
    test(new DCacheWPU()) { c =>
      println("========== in test ==========")
      // s0
      // c.io.req.bits.vaddr.poke(0.U)
      // c.io.req.bits.replayCarry.valid.poke(false.B)
      // c.io.req.bits.replayCarry.real_way_en.poke(0.U)
      // c.io.req.valid.poke(true.B)
      // c.clock.step()
      // // s1
      // c.io.lookup_upd.valid.poke(true.B)
      // c.io.lookup_upd.bits.s1_real_way_en.poke("b00010000".U)
      // c.io.resp.bits.s1_pred_fail.expect(false.B)
      // println("Last output value :" + c.io.resp.bits.s1_pred_fail.peek().litValue)
    }
  }

}
