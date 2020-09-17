package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._

class Classify(expWidth: Int, mantWidth: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(UInt((1 + expWidth + mantWidth).W))
    val isNegInf = Output(Bool())
    val isNegNormal = Output(Bool())
    val isNegSubnormal = Output(Bool())
    val isNegZero = Output(Bool())
    val isPosZero = Output(Bool())
    val isPosSubnormal = Output(Bool())
    val isPosNormal = Output(Bool())
    val isPosInf = Output(Bool())
    val isSNaN = Output(Bool())
    val isQNaN = Output(Bool())

    val isNaN = Output(Bool())
    val isInf = Output(Bool())
    val isInfOrNaN = Output(Bool())

    val isSubnormal = Output(Bool())
    val isZero = Output(Bool())
    val isSubnormalOrZero = Output(Bool())
  })
  val flpt = io.in.asTypeOf(new FloatPoint(expWidth, mantWidth))
  val (sign, exp, mant) = (flpt.sign, flpt.exp, flpt.mant)

  val isSubnormOrZero = exp === 0.U
  val mantIsZero = mant === 0.U
  val isInfOrNaN = (~exp).asUInt() === 0.U

  io.isNegInf := sign && io.isInf
  io.isNegNormal := sign && !isSubnormOrZero && !isInfOrNaN
  io.isNegSubnormal := sign && io.isSubnormal
  io.isNegZero := sign && io.isZero

  io.isPosZero := !sign && io.isZero
  io.isPosSubnormal := !sign && io.isSubnormal
  io.isPosNormal := !sign && !isSubnormOrZero && !isInfOrNaN
  io.isPosInf := !sign && io.isInf

  io.isSNaN := io.isNaN && !mant.head(1)
  io.isQNaN := io.isNaN && mant.head(1).asBool()

  io.isNaN := isInfOrNaN && !mantIsZero
  io.isInf := isInfOrNaN && mantIsZero
  io.isInfOrNaN := isInfOrNaN

  io.isSubnormal := isSubnormOrZero && !mantIsZero
  io.isZero := isSubnormOrZero && mantIsZero
  io.isSubnormalOrZero := isSubnormOrZero
}
