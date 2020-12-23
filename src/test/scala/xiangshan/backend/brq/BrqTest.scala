package xiangshan.backend.brq

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.Parameters
import utils.XSLog
import xiangshan._
import xiangshan.testutils._
import xiangshan.testutils.TestCaseGenerator._

import scala.util.Random

class BrqTest extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver {
  it should "redirect out-of-order, dequeue in-order" in {
    Parameters.set(Parameters.debugParameters)

    test(new Brq {
      AddSinks()
    }).withAnnotations(Seq()) { c =>

      def genEnqReq(x: => DecoupledIO[CfCtrl], pc: Long) = {
        chiselTypeOf(x.bits).Lit(
          _.cf.pc -> pc.U,
          _.cf.brUpdate.pnpc -> (pc+4).U
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
      val misPred = 6
      println(s"enqTags:$enqTags misPredTag:$misPred")
      enqTags = enqTags.take(misPred + 1)
      var commitTags, deqTags = List[Int]()

      def checkCommit = {
        if (c.io.out.valid.peek().litToBoolean) {
          commitTags = commitTags :+ c.io.redirect.bits.brTag.value.peek().litValue().toInt
          println(s"====commited tags:$commitTags====")
        }
      }
      def checkDeq =  {
        if(c.io.out.valid.peek().litToBoolean){
          deqTags = deqTags :+ c.io.out.bits.uop.brTag.value.peek().litValue().toInt
          println(s"====deq tags:$deqTags====")
        }
      }


      println("====Start random write back====")
      val wbPort = c.io.exuRedirectWb.head
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
        checkCommit
        c.clock.step(1)
        wbPort.valid.poke(false.B)
        for (i <- 0 until Random.nextInt(3)) {
          checkCommit
          c.clock.step(1)
        }
      }
      c.io.bcommit.poke((misPred+1).U)
      c.clock.step(1)
      c.io.bcommit.poke(0.U)
      while (deqTags.size != misPred+1) {
        checkCommit
        checkDeq
        c.clock.step(1)
      }

      c.clock.step(10)

      val left = commitTags.takeWhile(x => x!=misPred)
      val right = commitTags.dropWhile(x => x!=misPred).drop(1)

      println(s"commited before mispred: $left")
      println(s"commited after mispred: $right")

      def isValidCommitSeq(in: Seq[Int]): Boolean = {
        for(i <- 1 until in.size){
          if(in(i) == in(i-1)) return false
        }
        true
      }
      assert(isValidCommitSeq(left) && isValidCommitSeq(right))

      println(s"deq tags: $deqTags")

      def isValidDeqSeq(in: Seq[Int]): Boolean = {
         in.zipWithIndex.map(x => x._1==x._2).reduce(_&&_)
      }
      assert(isValidDeqSeq(deqTags))
    }
  }
}
