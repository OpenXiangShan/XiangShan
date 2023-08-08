package cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.cache.wpu.DCacheWpuWrapper
import xiangshan.{XSCoreParamsKey, XSTileKey}

class WpuBasicTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "DCacheWPU"
  it should ("run") in {
    val defaultConfig = (new DefaultConfig)
    implicit val config = defaultConfig.alterPartial({
      case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy()
    })
    println("========== Test the correctness of syntactic and datapath ==========")
    test(new DCacheWpuWrapper()) { c =>
      println("========== in test ==========")
      // s0
      c.io.req(0).bits.vaddr.poke(0.U)
      c.io.req(0).bits.replayCarry.valid.poke(false.B)
      c.io.req(0).bits.replayCarry.real_way_en.poke(0.U)
      c.io.req(0).valid.poke(true.B)
      c.clock.step()
      // s1
      c.io.lookup_upd(0).valid.poke(true.B)
      c.io.lookup_upd(0).bits.s1_real_way_en.poke("b00010000".U)
      c.io.req(0).bits.vaddr.poke(0.U)
      c.io.req(0).bits.replayCarry.valid.poke(false.B)
      c.io.req(0).bits.replayCarry.real_way_en.poke(0.U)
      c.io.req(0).valid.poke(true.B)
      println("output value1 : " + c.io.resp(0).bits.s0_pred_way_en.peek().litValue)
      c.clock.step()
      println("output value2 : " + c.io.resp(0).bits.s0_pred_way_en.peek().litValue)
    }
    println("========== end test ==========")
  }

}
