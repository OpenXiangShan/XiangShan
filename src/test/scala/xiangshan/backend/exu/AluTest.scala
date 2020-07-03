package xiangshan.backend.exu

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import xiangshan._
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random

class AluTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  it should "do simple test corrcetly" in {
    test(new Alu){c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      parallel(
        c.io.in.enqueuePartial(genAluAdd(c.io.in.bits, 0, 0)),
        c.io.out.expectDequeuePartial(chiselTypeOf(c.io.out.bits).Lit(_.data -> 0.U))
      )
    }
  }

  it should "do random add correctly" in {
    test(new Alu){c =>

      c.io.in.initSource().setSourceClock(c.clock)
      c.io.out.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 10

      val src1, src2, res = Array.fill(TEST_SIZE)(0)
      for(i <- 0 until TEST_SIZE){
        // avoid neg add res
        src1(i) = Random.nextInt(0x3fffffff)
        src2(i) = Random.nextInt(0x3fffffff)
        res(i) = src1(i) + src2(i)
      }

      val inputSeq = (0 until TEST_SIZE).map(i =>
        genAluAdd(c.io.in.bits, src1(i), src2(i))
      )

      val outputSeq = (0 until TEST_SIZE).map(i =>
        chiselTypeOf(c.io.out.bits).Lit(
          _.data -> res(i).U
        )
      )

      parallel(
        c.io.in.enqueuePartialSeq(inputSeq),
        c.io.out.expectDequeuePartialSeq(outputSeq)
      )
    }
  }


}
