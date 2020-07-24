package xiangshan.backend.exu

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import utils.XSLog
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random




class MduTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{

  // set to true when you need log
  XSLog.generateLog = false

  "MUL" should "random enq and deq correctly" in {
    test(new MulExeUnit{
      AddSinks()
    }){ c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 100
      val pcSeq = (0 until TEST_SIZE).map(_ => Random.nextInt(0x7fffffff))

      fork{
        c.io.in.enqueuePartialSeq(pcSeq.map(pc => genMul(c.io.in.bits, pc)))
      }.fork{
        c.io.out.expectDequeuePartialSeq(pcSeq.map(
          pc => chiselTypeOf(c.io.out.bits).Lit(
            _.uop.cf.pc -> pc.U
          )
        ))
      }.join()

    }
  }


  "MUL" should "only flush instrs newer than the redirect instr" in {
    test(new MulExeUnit{
      AddSinks()
    }){ c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      fork{
        // 29
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.cf.pc -> 666.U,
          _.uop.brTag.flag -> true.B,
          _.uop.brTag.value -> 12.U
        ))
        // 30
        c.io.redirect.pokePartial(chiselTypeOf(c.io.redirect).Lit(
          _.valid -> true.B,
          _.bits.isException -> false.B,
          _.bits.brTag.flag -> true.B,
          _.bits.brTag.value -> 11.U
        ))
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.cf.pc -> 777.U,
          _.uop.brTag.flag -> true.B,
          _.uop.brTag.value -> 10.U
        ))
        c.io.redirect.pokePartial(chiselTypeOf(c.io.redirect).Lit(_.valid -> false.B))
      }.fork{
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit(_.uop.cf.pc -> 777.U))
      }.join()
    }
  }



  "MUL" should "dont flush same br tag" in {
    test(new MulExeUnit{
      AddSinks()
    }){ c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 100
      val pcSeq = (0 until TEST_SIZE).map(_ => Random.nextInt(0x7fffffff))

      fork{
        // 53
        c.io.in.enqueuePartial(chiselTypeOf(c.io.in.bits).Lit(
          _.uop.cf.pc -> 666.U,
          _.uop.brTag.flag -> true.B,
          _.uop.brTag.value -> 15.U
        ))
        // 54
        c.clock.step(1)
        // 55
        c.io.redirect.valid.poke(true.B)
        c.io.redirect.bits.pokePartial(chiselTypeOf(c.io.redirect.bits).Lit(
          _.isException -> false.B,
          _.brTag.flag -> true.B,
          _.brTag.value -> 15.U
        ))
        c.clock.step(1)
        // 56
        c.io.redirect.valid.poke(false.B)
      }.fork{
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit(_.uop.cf.pc -> 666.U))
      }.join()

    }
  }


  "MDU" should "random enq and deq correctly" in {
    test(new MulDivExeUnit{
      AddSinks()
    }){ c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 50
      val pcSeq = (0 until TEST_SIZE).map(_ => Random.nextInt(0x7fffffff))

      fork{
        c.io.in.enqueuePartialSeq(pcSeq.map(pc => {
          genDiv(c.io.in.bits, pc)
        }))
      }.fork{
        c.io.out.expectDequeuePartialSeq(pcSeq.map(
          pc => chiselTypeOf(c.io.out.bits).Lit(
            _.uop.cf.pc -> pc.U
          )
        ))
      }.join()

    }
  }


}
