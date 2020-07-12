package xiangshan.backend.exu

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.BoringUtils
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import noop.MDUOpType
import xiangshan._
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random




class MduTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  "MUL" should "random enq and deq correctly" in {
    test(new MulExeUnit{
      val disp_begin = WireInit(0.S(64.W).asUInt())
      val disp_end = WireInit((-1).S(64.W).asUInt())
      BoringUtils.addSource(disp_begin, "DISPLAY_LOG_START")
      BoringUtils.addSource(disp_end, "DISPLAY_LOG_END")
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


  "MUL" should "dont flush same br tag" in {
    test(new MulExeUnit{
      val disp_begin = WireInit(0.S(64.W).asUInt())
      val disp_end = WireInit((-1).S(64.W).asUInt())
      BoringUtils.addSource(disp_begin, "DISPLAY_LOG_START")
      BoringUtils.addSource(disp_end, "DISPLAY_LOG_END")
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
      val disp_begin = WireInit(0.S(64.W).asUInt())
      val disp_end = WireInit((-1).S(64.W).asUInt())
      BoringUtils.addSource(disp_begin, "DISPLAY_LOG_START")
      BoringUtils.addSource(disp_end, "DISPLAY_LOG_END")
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
