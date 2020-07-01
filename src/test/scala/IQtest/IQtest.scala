package chiseltest.tests

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import xiangshan._
import xiangshan.backend.issue.IssueQueue
import xiangshan.utils._
import xiangshan.CtrlFlow

class IQTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "IssueQueue Test"

  it should "test issuequeue" in {
    test(new IssueQueue(FuType.alu.litValue(),wakeupCnt = 1,bypassCnt = 1)) { c =>

    }
  }
}