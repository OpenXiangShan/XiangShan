package fpu.DivSqrt

import chisel3._
import chisel3.testers.BasicTester
import chiseltest.experimental.TestOptionBuilder._
import chisel3.util._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.internal.VerilatorBackendAnnotation
import fpu.divsqrt.{MantDivSqrt, OnTheFlyConv}
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental.BundleLiterals._

class MantSqrtTest extends FlatSpec with ChiselScalatestTester with Matchers{
  it should " " in {
    test(new MantDivSqrt(8)) { c =>
      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)
      c.io.out.expectInvalid()

      //test sqrt
      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1011_0111".U,
        _.b -> 0.U,
        _.isDiv -> false.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b01101100".U)
      c.io.out.ready.poke(true.B)

      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1001_0000".U,
        _.b -> 0.U,
        _.isDiv -> false.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b0110_0000".U)
      c.io.out.ready.poke(true.B)

      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1111_1111".U,
        _.b -> 0.U,
        _.isDiv -> false.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b01111111".U)
      c.io.out.ready.poke(true.B)

      // test div
      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1010_1111".U,
        _.b -> "b1100_0101".U,
        _.isDiv -> true.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b00111000".U)
      c.io.out.ready.poke(true.B)


      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1000_0000".U,
        _.b -> "b1000_0000".U,
        _.isDiv -> true.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b01000000".U)
      c.io.out.ready.poke(true.B)

      c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
        _.a -> "b1100_0001".U,
        _.b -> "b1000_0000".U,
        _.isDiv -> true.B
      ))
      c.io.out.ready.poke(false.B)
      c.io.out.waitForValid()
      c.clock.step(1)
      c.io.out.bits.quotient.expect("b01100000".U)
      c.io.out.ready.poke(true.B)

    }
  }
}
