package xiangshan.backend.brq

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import xiangshan._
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random

class BrqTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver {
  it should "" in {

    test(new Brq {
      AddSinks()
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>

      def genEnqReq(x: => DecoupledIO[CfCtrl], pc: Long) = {
        chiselTypeOf(x.bits).Lit(
          _.cf.pc -> pc.U
        )
      }

      def genExuWb(exuRedirect: => Valid[ExuOutput], tagIdx: Int, tagFlag: Boolean, target: Long) = {
        chiselTypeOf(exuRedirect.bits).Lit(
          _.redirect.brTag.value -> tagIdx.U,
          _.redirect.brTag.flag -> tagFlag.B,
          _.redirect.target -> target.U
        )
      }

      c.io.enqReqs.head.initSource().setSourceClock(c.clock)

      var brqPtrSeq = Seq[(BigInt, Boolean)]()

      for (i <- 0 until 10) {
        val enqPort = c.io.enqReqs.head
        enqPort.enqueuePartial(genEnqReq(enqPort, i * 0x1000))
      }

      var enqTags = List.tabulate(10)(i => i)
      val misPred = Random.nextInt(10)
      println(s"enqTags:$enqTags misPredTag:$misPred")
      enqTags = enqTags.take(misPred + 1)
      var deqTags = List[Int]()

      def checkDeq = {
        if (c.io.out.valid.peek().litToBoolean) {
          deqTags = deqTags :+ c.io.redirect.bits.brTag.value.peek().litValue().toInt
          println(s"====deq tags:$deqTags====")
        }
      }


      println("====Start random write back====")
      val wbPort = c.io.exuRedirect.head
      //-----------------write back-----------------//
      while (enqTags.nonEmpty) {
        val idx = Random.nextInt(enqTags.size)
        val tag = enqTags(idx)
        println(s"====write tag:$tag back to Brq====")
        enqTags = enqTags.filter(x => x != tag)
        wbPort.valid.poke(true.B)
        wbPort.bits.pokePartial(
          genExuWb(wbPort, tag, tagFlag = false, if (tag == misPred) 0xffff else tag * 0x1000 + 4)
        )
        checkDeq
        c.clock.step(1)
        wbPort.valid.poke(false.B)
        for (i <- 0 until Random.nextInt(3)) {
          checkDeq
          c.clock.step(1)
        }
      }
      while (deqTags.size != misPred+1) {
        checkDeq
        c.clock.step(1)
      }

      c.clock.step(10)

      val left = deqTags.takeWhile(x => x!=misPred)
      val right = deqTags.dropWhile(x => x!=misPred).drop(1)

      println(s"deq before mispred: $left")
      println(s"deq after mispred: $right")

      def isValidDeqSeq(in: Seq[Int]): Boolean = {
        for(i <- 1 until in.size){
          if(in(i) == in(i-1)) return false
        }
        true
      }

      assert(isValidDeqSeq(left) && isValidDeqSeq(right))
    }
  }
}
