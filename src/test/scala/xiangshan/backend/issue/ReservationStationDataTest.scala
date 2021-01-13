package xiangshan.backend.issue

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, LineCoverageAnnotation, ToggleCoverageAnnotation, UserCoverageAnnotation, StructuralCoverageAnnotation}
import firrtl.stage.RunFirrtlTransformAnnotation
import xstransforms.PrintModuleName
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import xiangshan.testutils._
import xiangshan.backend.exu.Exu
import scala.util.Random
import xiangshan.MicroOp
import xiangshan.testutils.AddSinks
import xiangshan.XSModule
import xiangshan.SrcState
import xiangshan.SrcType
import xiangshan.HasXSParameter
import utils.SyncDataModuleTemplate

class RsDataWrapper extends XSModule {
  val testTop = Module(new ReservationStationData(Exu.stExeUnitCfg, 1, 1, -1, false))
  val io = IO(testTop.io.cloneType)
  AddSinks()
  io <> testTop.io
}

class ReservationStationDataTest extends AnyFlatSpec
with ChiselScalatestTester
with Matchers
with ParallelTestExecution
with HasPartialDecoupledDriver
with HasXSParameter {
  
  top.Parameters.set(top.Parameters.debugParameters)

  val annos = Seq(
    VerilatorBackendAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  def nextSrcRegValue(): UInt = {
    (scala.math.abs(Random.nextLong())).U
  }
  
  it should "read/write uop and data correctly" in {
    test(new RsDataWrapper).withAnnotations(annos) { c =>

      val rounds = IssQueSize

      for (i <- 0 until rounds) {
        fork {
          // disable redirect/broadcastedUop/writeBackedData/extraListenPorts
          c.io.broadcastedUops.foreach(_.valid.poke(false.B))
          c.io.extraListenPorts.foreach(_.valid.poke(false.B))
          c.io.redirect.valid.poke(false.B)

          // send ctrl message to enq data
          c.io.ctrl.deqPtr.valid.poke(false.B)
          c.io.ctrl.enqPtr.poke(i.U)
          c.io.ctrl.enqCtrl.valid.poke(true.B)
          
          // build uop
          val uop = c.io.ctrl.enqCtrl.bits
          uop.cf.pc.poke(2333.U) // pc
          uop.ctrl.imm.poke(4567.U) // imm and srcType
          uop.ctrl.rfWen.poke(true.B)
          uop.ctrl.src1Type.poke(SrcType.reg)
          uop.ctrl.src2Type.poke(SrcType.reg)
          uop.ctrl.src3Type.poke(SrcType.reg)
          uop.psrc1.poke(11.U)
          uop.psrc2.poke(22.U)
          uop.psrc3.poke(33.U)
          uop.pdest.poke(44.U)
          uop.src1State.poke(SrcState.rdy)
          uop.src2State.poke(SrcState.rdy)
          uop.src3State.poke(SrcState.rdy)

          // generate random src op
          c.io.srcRegValue.foreach(_.poke(nextSrcRegValue()))
          
          c.clock.step(1)
        } .fork {
          c.io.deq.ready.poke(false.B)
          
          c.clock.step(1)
        } .join()
      }
    }
  }

  it should "bypass data and dequeue item 0" in {
    test(new RsDataWrapper).withAnnotations(annos) { c =>
      val rounds = 3

      for (i <- 0 until rounds) {
        c.io.extraListenPorts.foreach(_.valid.poke(false.B))
        c.io.redirect.valid.poke(false.B)
        
        c.io.broadcastedUops.foreach(_.valid.poke(false.B))
        // bypass 1 instr
        c.io.broadcastedUops(0).valid.poke(true.B)
        val broadcastedUop = c.io.broadcastedUops(0).bits
        broadcastedUop.pdest.poke(11.U)
        c.io.writeBackedData(0).poke("hdeadbeef".U)

        // send ctrl message to enq data
        c.io.ctrl.deqPtr.valid.poke(true.B)
        c.io.ctrl.deqPtr.bits.poke(0.U)
        c.io.ctrl.enqPtr.poke(i.U)
        c.io.ctrl.enqCtrl.valid.poke(true.B)
        
        // build uop
        val uop = c.io.ctrl.enqCtrl.bits
        uop.cf.pc.poke(2333.U) // pc
        uop.ctrl.imm.poke(4567.U) // imm and srcType
        uop.ctrl.rfWen.poke(true.B)
        uop.ctrl.src1Type.poke(SrcType.reg)
        uop.ctrl.src2Type.poke(SrcType.reg)
        uop.ctrl.src3Type.poke(SrcType.reg)
        uop.psrc1.poke(11.U)
        uop.psrc2.poke(22.U)
        uop.psrc3.poke(33.U)
        uop.pdest.poke(44.U)
        uop.src1State.poke(SrcState.busy)
        uop.src2State.poke(SrcState.rdy)
        uop.src3State.poke(SrcState.rdy)

        // generate random src op
        c.io.srcRegValue.foreach(_.poke(nextSrcRegValue()))
        
        c.io.deq.ready.poke(/* if (i == 1) true.B else  */false.B)
        println(s"src1:${c.io.deq.bits.src1.peek()} src2:${c.io.deq.bits.src2.peek()}")
        
        c.clock.step(1)
      }
    }
  }
}

class SyncDataModuleTemplateTest extends AnyFlatSpec
with ChiselScalatestTester
with Matchers {
  it should "read new data" in {
    test(new SyncDataModuleTemplate(UInt(2.W), 1, 1, 1)) { c =>
      val dut = c.io

      dut.raddr(0).poke(0.U)
      dut.waddr(0).poke(0.U)
      dut.wdata(0).poke(3.U)
      dut.wen(0).poke(true.B)
      c.clock.step(1)
      dut.rdata(0).expect(3.U)
    }
  }
}