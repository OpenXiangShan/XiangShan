package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.{FuConfig, FunctionUnit, HasPipelineReg}


class FPUSubModuleInput extends Bundle{
  val op = UInt(3.W)
  val isDouble = Bool()
  val a, b, c = UInt(64.W)
  val rm = UInt(3.W)
}

class FPUSubModuleOutput extends Bundle{
  val fflags = new Fflags
  val result = UInt(64.W)
}

class FPUSubModuleIO extends Bundle{
  val in = Flipped(DecoupledIO(new FPUSubModuleInput))
  val out = DecoupledIO(new FPUSubModuleOutput)
}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

trait HasFPUSigs { this: FPUSubModule =>
  val op = io.in.bits.uop.ctrl.fuOpType(2, 0)
  // 'op' must change with fuOpType
  require(io.in.bits.uop.ctrl.fuOpType.getWidth == 7)
  val isDouble = !io.in.bits.uop.ctrl.isRVF
}

abstract class FPUSubModule extends FunctionUnit
  with HasUIntToSIntHelper
  with HasFPUSigs
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(new Fflags))
}

abstract class FPUPipelineModule
  extends FPUSubModule
  with HasPipelineReg