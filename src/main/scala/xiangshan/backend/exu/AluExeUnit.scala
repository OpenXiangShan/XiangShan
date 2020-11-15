package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import utils._
import xiangshan.backend._
import xiangshan.backend.fu.{Alu, FunctionUnit}
import xiangshan.backend.fu.FunctionUnit._


class AluExeUnit extends Exu(
  exuName = "AluExeUnit",
  fuGen = Seq((FunctionUnit.alu _, (_: FunctionUnit) => true.B)),
  wbIntPriority = 0,
  wbFpPriority = Int.MaxValue
)
{
  val alu = supportedFunctionUnits.collectFirst{
    case a: Alu => a
  }.get

  io.toInt.bits.redirectValid := alu.redirectOutValid
  io.toInt.bits.redirect := alu.redirectOut
  io.toInt.bits.brUpdate := alu.brUpdate
}
