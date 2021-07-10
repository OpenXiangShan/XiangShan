package xiangshan.unittest.refcnt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import xiangshan.backend.rename.refcnt.Adder

class AdderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Adder"
  it should "yield correct value" in {
    
    test (new Adder) { dut =>
      
      for (inc <- Seq(true, false)) {
        for (dec <- Seq(true, false)) {
          for (init <- 0 until 4) {
            if (!((inc && !dec && init == 3) || (!inc && dec && init == 0))) {
              dut.io.inc.poke(inc.B)
              dut.io.dec.poke(dec.B)
              dut.io.cnt.poke(init.U)
              val next = (inc, dec) match {
                case (true, false) => init + 1
                case (false, true) => init - 1
                case _ => init
              }
              dut.io.nextCnt.expect(next.U)
            }
          }
        }
      }
    }
  }
}