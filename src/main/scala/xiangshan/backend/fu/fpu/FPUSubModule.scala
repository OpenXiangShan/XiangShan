package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.{FuConfig, FunctionUnit, HasPipelineReg}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

abstract class FPUSubModule extends FunctionUnit(len = 65)
  with HasUIntToSIntHelper
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(UInt(5.W)))
}

abstract class FPUPipelineModule
  extends FPUSubModule
  with HasPipelineReg