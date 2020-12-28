package utils

import chisel3._
import chisel3.util._

object PipelineConnect {
  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool) = {
    val valid = RegInit(false.B)
    valid.suggestName("pipeline_valid")
    when (rightOutFire) { valid := false.B }
    when (left.valid && right.ready) { valid := true.B }
    when (isFlush) { valid := false.B }

    left.ready := right.ready
    right.bits := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush
  }
}
