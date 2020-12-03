package xiangshan.backend.decode

import xiangshan.CtrlFlow
import xiangshan.CfCtrl
import xiangshan.backend.fu.HasExceptionNO

import chisel3._
import chisel3.util._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import chiseltest.ChiselScalatestTester
import scala.util.Random._
import scala.collection.mutable
import xiangshan.XSModule
import xiangshan.frontend.HasTageParameter

class DualDecodeUnitDut extends XSModule {
  val io = IO(new Bundle {
    val in = Input(new CtrlFlow)
    val out_ref = Output(new CfCtrl)
    val out_dut = Output(new CfCtrl)
  })

  val ref = Module(new Decoder)
  val dut = Module(new DecodeUnit)

  ref.io.in := io.in
  io.out_ref := ref.io.out

  dut.io.enq.ctrl_flow := io.in
  io.out_dut := dut.io.deq.cf_ctrl
}

object CtrlFlowGenerator extends HasExceptionNO {
  var inst: Int = 0

  def genProgramCounter(): UInt = (nextInt() & ((1 << 39) - 1)).U

  def genInstruction(): UInt = {
    inst += 1
    inst.U
  }

  def hasNextInstruction(): Boolean = inst <= 10 // FIXME

  def genCtrlFlow(x: => CtrlFlow): Unit = {
    x.instr.poke(genInstruction())
    x.pc.poke(genProgramCounter())
    x.crossPageIPFFix.poke(nextBoolean().B)
    // input: instrPageFault -> true or false  others: false
    // output: may modify illegalInstr , others : false , instrPageFault: hold
    x.exceptionVec.map(_.poke(nextBoolean().B))
    x.intrVec.map(_.poke(nextBoolean().B))
  }

  def init(): Unit = {
    // read instr from file
    inst = 0
  }
}

class DecodeUnitDiffTest
  extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers
{
  behavior of "DecodeUnit"

  it should "yield same output as old Decoder" in {
    test(new DualDecodeUnitDut) { c =>
      // TODO generate instruction
      CtrlFlowGenerator.init()

      while (CtrlFlowGenerator.hasNextInstruction()) {
        CtrlFlowGenerator.genCtrlFlow(c.io.in)
        c.clock.step(1)

        // ctrl signals
        c.io.out_dut.ctrl.expect(c.io.out_ref.ctrl.peek(), s"Bad Instr: ${CtrlFlowGenerator.inst.toHexString}")
        // branch tag: ignore
        // ctrl flow: ignore brUpdate
        c.io.out_dut.cf.instr.expect(c.io.out_ref.cf.instr.peek(), s"Bad Instr: ${CtrlFlowGenerator.inst.toHexString}")
        c.io.out_dut.cf.pc.expect(c.io.out_ref.cf.pc.peek(), s"Bad Instr: ${CtrlFlowGenerator.inst.toHexString}")
        c.io.out_dut.cf.exceptionVec.indices.foreach(i => {
          c.io.out_dut.cf.exceptionVec(i).expect(c.io.out_ref.cf.exceptionVec(i).peek())
        })
        c.io.out_dut.cf.intrVec.indices.foreach(i => {
          c.io.out_dut.cf.intrVec(i).expect(c.io.out_ref.cf.intrVec(i).peek())
        })
        c.io.out_dut.cf.crossPageIPFFix.expect(c.io.out_ref.cf.crossPageIPFFix.peek(), s"Bad Instr: ${CtrlFlowGenerator.inst.toHexString}")
      }
    }
  }
}
