package xiangshan.backend.issue

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import utils.XSLog
import xiangshan._
import xiangshan.backend.exu.Exu
import xiangshan.testutils._

class IssueQueueTest extends FlatSpec
  with ChiselScalatestTester
  with Matchers
  with ParallelTestExecution
  with HasPartialDecoupledDriver
{
  XSLog.generateLog = false
  it should "do enq issue with no delay correctly" in {
    test(new IssueQueue(Exu.aluExeUnitCfg, wakeupCnt = 1, bypassCnt = 1, fifo = false) {
      AddSinks()
    }) { c =>

      def genEnqRdyReq(x: => DecoupledIO[MicroOp], roq: Long) = {
        chiselTypeOf(x.bits).Lit(
          _.src1State -> SrcState.rdy,
          _.src2State -> SrcState.rdy,
          _.src3State -> SrcState.rdy,
          _.roqIdx -> roq.U
        )
      }

      c.io.enqCtrl.initSource().setSourceClock(c.clock)
      c.io.deq.initSink().setSinkClock(c.clock)

      def TEST_SIZE = 2
      val roqSeq = 0 until TEST_SIZE
      val enqPort = c.io.enqCtrl
      fork {
        c.io.enqCtrl.enqueuePartialSeq(roqSeq.map(roq => genEnqRdyReq(enqPort, roq)))
      }.fork {
        c.io.deq.expectDequeuePartialSeq(roqSeq.map(
          roq => chiselTypeOf(c.io.deq.bits).Lit(
            _.uop.roqIdx -> roq.U
          )
        ))
      }.join()
    }
  }
}