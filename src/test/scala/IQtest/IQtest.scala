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
    test(new IssueQueue(FuType.alu.litValue(),wakeupCnt = 0,bypassCnt = 0)) { c =>
      c.io.deq.ready.poke(true.B)
      //-----------------
      //Cycle 1
      //-----------------
      c.io.enqCtrl.valid.poke(true.B)
      c.io.enqCtrl.bits.brMask.poke(0.U)
      c.io.enqCtrl.bits.brTag.poke(0.U)
      c.io.enqCtrl.bits.psrc1.poke(3.U)
      c.io.enqCtrl.bits.psrc2.poke(4.U)
      c.io.enqCtrl.bits.psrc3.poke(5.U)
      c.io.enqCtrl.bits.pdest.poke(6.U)
      c.io.enqCtrl.bits.old_pdest.poke(7.U)
      c.io.enqCtrl.bits.src1State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src2State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src3State.poke(SrcState.busy)
      c.io.enqCtrl.bits.freelistAllocPtr.poke(0.U)
      c.io.enqCtrl.bits.roqIdx.poke(7.U)

      c.io.redirect.valid.poke(false.B)
      c.clock.step()
      //-----------------
      //Cycle 2
      //-----------------
      c.io.enqCtrl.valid.poke(true.B)
      c.io.enqCtrl.bits.brMask.poke(0.U)
      c.io.enqCtrl.bits.brTag.poke(0.U)
      c.io.enqCtrl.bits.psrc1.poke(12.U)
      c.io.enqCtrl.bits.psrc2.poke(10.U)
      c.io.enqCtrl.bits.psrc3.poke(9.U)
      c.io.enqCtrl.bits.pdest.poke(8.U)
      c.io.enqCtrl.bits.old_pdest.poke(5.U)
      c.io.enqCtrl.bits.src1State.poke(SrcState.rdy)
      c.io.enqCtrl.bits.src2State.poke(SrcState.rdy)
      c.io.enqCtrl.bits.src3State.poke(SrcState.rdy)
      c.io.enqCtrl.bits.freelistAllocPtr.poke(0.U)
      c.io.enqCtrl.bits.roqIdx.poke(3.U)

      c.io.redirect.valid.poke(false.B)
      c.clock.step()
      //-----------------
      //Cycle 3
      //-----------------
      c.io.enqCtrl.valid.poke(true.B)
      c.io.enqCtrl.bits.brMask.poke(0.U)
      c.io.enqCtrl.bits.brTag.poke(0.U)
      c.io.enqCtrl.bits.psrc1.poke(21.U)
      c.io.enqCtrl.bits.psrc2.poke(12.U)
      c.io.enqCtrl.bits.psrc3.poke(15.U)
      c.io.enqCtrl.bits.pdest.poke(23.U)
      c.io.enqCtrl.bits.old_pdest.poke(5.U)
      c.io.enqCtrl.bits.src1State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src2State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src3State.poke(SrcState.busy)
      c.io.enqCtrl.bits.freelistAllocPtr.poke(0.U)
      c.io.enqCtrl.bits.roqIdx.poke(8.U)
      c.io.redirect.valid.poke(false.B)
      c.clock.step()
      //-----------------
      //Cycle 4
      //-----------------
      c.io.enqCtrl.valid.poke(true.B)
      c.io.enqCtrl.bits.brMask.poke(0.U)
      c.io.enqCtrl.bits.brTag.poke(0.U)
      c.io.enqCtrl.bits.psrc1.poke(21.U)
      c.io.enqCtrl.bits.psrc2.poke(12.U)
      c.io.enqCtrl.bits.psrc3.poke(15.U)
      c.io.enqCtrl.bits.pdest.poke(23.U)
      c.io.enqCtrl.bits.old_pdest.poke(5.U)
      c.io.enqCtrl.bits.src1State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src2State.poke(SrcState.busy)
      c.io.enqCtrl.bits.src3State.poke(SrcState.busy)
      c.io.enqCtrl.bits.freelistAllocPtr.poke(0.U)
      c.io.enqCtrl.bits.roqIdx.poke(8.U)
      c.io.redirect.valid.poke(false.B)
      c.clock.step()

      c.io.deq.valid.expect(true.B)
      c.io.deq.bits.uop.pdest.expect(8.U)
    }
  }
}