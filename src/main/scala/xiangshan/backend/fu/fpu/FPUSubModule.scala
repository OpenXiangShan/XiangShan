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

//trait HasPipelineReg { this: FPUSubModule =>
//  def latency: Int
//
//  val ready = Wire(Bool())
//  val cnt = RegInit(0.U((log2Up(latency)+1).W))
//
//  ready := (cnt < latency.U) || (cnt === latency.U && io.out.ready)
//  cnt := cnt + io.in.fire() - io.out.fire()
//
//  val valids = io.in.valid +: Array.fill(latency)(RegInit(false.B))
//  for(i <- 1 to latency){
//    when(ready){ valids(i) := valids(i-1) }
//  }
//
//  def PipelineReg[T<:Data](i: Int)(next: T) = RegEnable(next, enable = valids(i-1) && ready)
//  def S1Reg[T<:Data](next: T):T = PipelineReg[T](1)(next)
//  def S2Reg[T<:Data](next: T):T = PipelineReg[T](2)(next)
//  def S3Reg[T<:Data](next: T):T = PipelineReg[T](3)(next)
//  def S4Reg[T<:Data](next: T):T = PipelineReg[T](4)(next)
//  def S5Reg[T<:Data](next: T):T = PipelineReg[T](5)(next)
//
//  io.in.ready := ready
//  io.out.valid := valids.last
//}

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt){
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt()
  }
}

//abstract class FPUSubModule extends Module with HasUIntToSIntHelper {
//  val io = IO(new FPUSubModuleIO)
//}

class FPUExtraInput extends Bundle {
  val op = UInt(3.W)
  val isDouble = Bool()
  val rm = UInt(3.W)
}

trait HasFPUSigs { this: FPUSubModule =>
  val extraIn = io.in.bits.ext.get
  val op = extraIn.op
  val isDouble = extraIn.isDouble
  val rm = extraIn.rm
  val fflags = io.out.bits.ext.get
}

abstract class FPUSubModule(cfg: FuConfig, latency: Int = 0) extends FunctionUnit(
  cfg,
  latency = latency,
  extIn = new FPUExtraInput,
  extOut = new Fflags
) with HasUIntToSIntHelper
  with HasFPUSigs

abstract class FPUPipelineModule(cfg: FuConfig, latency: Int)
  extends FPUSubModule(cfg, latency)
  with HasPipelineReg[FPUExtraInput, Fflags]