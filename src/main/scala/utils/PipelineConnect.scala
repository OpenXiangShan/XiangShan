package utils

import chisel3._
import chisel3.util._

object PipelineConnect {

  class PipelineConnectModule[T <: Data](gen: T) extends Module {
    val io = IO(new Bundle() {
      val in = Flipped(DecoupledIO(gen.cloneType))
      val out = DecoupledIO(gen.cloneType)
      val rightOutFire = Input(Bool())
      val isFlush = Input(Bool())
    })

    val valid = RegInit(false.B)
    valid.suggestName("pipeline_valid")
    when (io.rightOutFire) { valid := false.B }
    when (io.in.valid && io.out.ready) { valid := true.B }
    when (io.isFlush) { valid := false.B }

    io.in.ready := io.out.ready
    io.out.bits := RegEnable(io.in.bits, io.in.valid && io.out.ready)
    io.out.valid := valid //&& !isFlush
  }

  def apply[T <: Data]
  (left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool,
   moduleName: Option[String] = None
  ){
    val pipelineConnect = Module(new PipelineConnectModule[T](left.bits.cloneType))
    if(moduleName.nonEmpty) pipelineConnect.suggestName(moduleName.get)
    pipelineConnect.io.in <> left
    pipelineConnect.io.rightOutFire := rightOutFire
    pipelineConnect.io.isFlush := isFlush
    right <> pipelineConnect.io.out
  }
}
