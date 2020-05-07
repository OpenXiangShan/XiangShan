package fpu.DivSqrt

import chisel3._
import chisel3.testers.BasicTester
import chisel3.util._
import chiseltest._
import chiseltest.ChiselScalatestTester
import fpu.divsqrt.OnTheFlyConv
import org.scalatest.{FlatSpec, Matchers}

class OnTheFlyConvTest extends FlatSpec with ChiselScalatestTester with Matchers{
  it should "" in {
    test(new OnTheFlyConv(11)){ c =>
      c.io.resetDiv.poke(false.B)
      c.io.resetSqrt.poke(true.B)
      c.clock.step(1)

      // test sqrt(0.10110111)
      c.io.resetSqrt.poke(false.B)
      c.io.enable.poke(true.B)
      c.io.qi.poke((-1).S(3.W))
      c.io.F.expect("b001_1100_0000".U(11.W))
      c.clock.step(1)

      c.io.enable.poke(true.B)
      c.io.qi.poke(2.S(3.W))
      c.io.F.expect("b100_1100_0000".U(11.W))
      c.clock.step(1)

      c.io.enable.poke(true.B)
      c.io.qi.poke((-2).S(3.W))
      c.io.F.expect("b011_0111_0000".U(11.W))
      c.clock.step(1)

      c.io.enable.poke(true.B)
      c.io.Q.expect("b11011000".U)
      c.clock.step(1)
    }
  }
}
