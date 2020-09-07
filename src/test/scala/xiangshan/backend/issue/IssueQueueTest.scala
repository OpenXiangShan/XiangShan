package xiangshan.backend.issue

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import top.Parameters
import utils.XSLog
import xiangshan._
import xiangshan.backend.exu.Exu
import xiangshan.testutils._

import scala.util.Random

class IssueQueueTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{

  it should "enq and deq correctly" in {
    test(new IssueQueue(Exu.ldExeUnitCfg, 1, 1){
      AddSinks()
    }){ c =>

      def genEnqRdyReq(x: => DecoupledIO[MicroOp], roqIdx: Long) = {
        chiselTypeOf(x.bits).Lit(
          _.src1State -> SrcState.rdy,
          _.src2State -> SrcState.rdy,
          _.src3State -> SrcState.rdy,
          _.roqIdx -> roqIdx.U,
          _.cf.pc -> roqIdx.U
        )
      }

      c.io.enq.initSource().setSourceClock(c.clock)
      c.io.deq.initSink().setSinkClock(c.clock)

      fork {
        c.io.enq.enqueuePartialSeq((0 until c.qsize).map(i => genEnqRdyReq(c.io.enq, i)))
      }.fork {
//        c.clock.step(10)
        c.io.deq.expectDequeuePartialSeq((0 until c.qsize).map(
          i => chiselTypeOf(c.io.deq.bits).Lit(
            _.uop.roqIdx -> i.U,
            _.uop.cf.pc -> i.U
          )
        ))
      }.join()
    }
  }


  it should "only deq ready inst" in {
    test(new IssueQueue(Exu.ldExeUnitCfg, 1, 1){
      AddSinks()
    }){ c =>

      def genEnqRdyReq(x: => DecoupledIO[MicroOp], pc: Long, ready: Boolean) = {
        chiselTypeOf(x.bits).Lit(
          _.src1State -> (if(ready) SrcState.rdy else SrcState.busy),
          _.src2State -> SrcState.rdy,
          _.src3State -> SrcState.rdy,
          _.cf.pc -> pc.U
        )
      }

      c.io.enq.initSource().setSourceClock(c.clock)
      c.io.deq.initSink().setSinkClock(c.clock)

      fork {
        c.io.enq.enqueuePartialSeq((0 until c.qsize).map(i => genEnqRdyReq(c.io.enq, i, i%2==0)))
      }.fork {
        //        c.clock.step(10)
        c.io.deq.expectDequeuePartialSeq((0 until c.qsize).filter(i => i%2==0).map(
          i => chiselTypeOf(c.io.deq.bits).Lit(
            _.uop.cf.pc -> i.U
          )
        ))
      }.join()
    }
  }

  it should "enq and deq bubble correctly" in {
    test(new IssueQueue(Exu.ldExeUnitCfg, 1, 1){
      AddSinks()
    }){ c =>

      def genEnqRdyReq(x: => DecoupledIO[MicroOp], pc: Long) = {
        chiselTypeOf(x.bits).Lit(
          _.src1State -> SrcState.rdy,
          _.src2State -> SrcState.rdy,
          _.src3State -> SrcState.rdy,
          _.cf.pc -> pc.U
        )
      }

      c.io.enq.initSource().setSourceClock(c.clock)
      c.io.deq.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 100

      fork {
        c.io.enq.enqueuePartialSeq((0 until TEST_SIZE).map(i => genEnqRdyReq(c.io.enq, i)))
      }.fork {
        c.io.deq.expectDequeuePartialSeq((0 until TEST_SIZE).map(
          i => chiselTypeOf(c.io.deq.bits).Lit(
            _.uop.cf.pc -> i.U
          )
        ))
      }.fork{
        c.clock.step(10)
        var cnt = 0
        while (cnt != TEST_SIZE){
          c.io.tlbFeedback.valid.poke(true.B)
          c.io.tlbFeedback.bits.hit.poke(true.B)
          c.clock.step(1)
          cnt += 1
          c.io.tlbFeedback.valid.poke(false.B)
          c.clock.step(1 + Random.nextInt(10))
        }
      }.join()
    }
  }


}