package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.{FPUCtrlSignals, XSModule}
import xiangshan.backend.fu.{FuConfig, FunctionUnit, HasPipelineReg}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

abstract class FPUDataModule extends XSModule {
  val io = IO(new Bundle() {
    val in = Input(new Bundle() {
      val src = Vec(3, UInt(65.W))
      val fpCtrl = new FPUCtrlSignals
      val rm = UInt(3.W)
    })
    val out = Output(new Bundle() {
      val data = UInt(65.W)
      val fflags = UInt(5.W)
    })
  })

  val rm = io.in.rm
  val fflags = io.out.fflags
}

abstract class FPUSubModule extends FunctionUnit(len = 65)
  with HasUIntToSIntHelper
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(UInt(5.W)))
  val dataModule: FPUDataModule
  def connectDataModule = {
    dataModule.io.in.src <> io.in.bits.src
    dataModule.io.in.fpCtrl <> io.in.bits.uop.ctrl.fpu
    dataModule.io.in.rm <> rm
    io.out.bits.data := dataModule.io.out.data
    fflags := dataModule.io.out.fflags
  }
}

abstract class FPUPipelineModule
  extends FPUSubModule
  with HasPipelineReg