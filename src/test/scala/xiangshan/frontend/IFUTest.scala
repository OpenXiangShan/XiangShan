package xiangshan.frontend

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}

class IFUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "IFU Test"

  it should "test IFU pipeline" in {
    test(new IFU) { c =>
      //-----------------
      //Cycle 0
      //-----------------
      //c.io.icacheReq.ready.poke(true.B)
      c.io.icacheReq.ready.poke(false.B)
      c.io.fetchPacket.ready.poke(true.B)
      c.clock.step()
      //-----------------
      //Cycle 1
      //-----------------
      c.clock.step()
      c.clock.step()
      c.clock.step()
      //-----------------
      // Cycle 2
      //-----------------
      c.io.icacheReq.ready.poke(true.B)
      c.clock.step()
      //-----------------
      // Cycle 3
      //-----------------
      c.clock.step()
      //-----------------
      // Cycle 4
      //-----------------
      c.io.icacheResp.valid.poke(true.B)
      c.clock.step()
      //-----------------
      // Cycle 5
      //-----------------
      c.io.redirectInfo.valid.poke(true.B)
      c.io.redirectInfo.misPred.poke(true.B)
      c.io.redirectInfo.redirect.target.poke("h80002800".U)
      c.clock.step()
      //-----------------
      // Cycle 6
      //-----------------
      c.io.redirectInfo.valid.poke(false.B)
      c.io.redirectInfo.misPred.poke(false.B)
      c.clock.step()
      //-----------------
      // Cycle 7
      //-----------------
      c.clock.step()
    }
  }
}

