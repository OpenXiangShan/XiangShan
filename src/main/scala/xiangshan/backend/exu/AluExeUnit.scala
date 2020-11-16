package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan.backend.exu.Exu.aluExeUnitCfg
import xiangshan.backend.fu.Alu

class AluExeUnit extends Exu(aluExeUnitCfg)
{
  val alu = supportedFunctionUnits.collectFirst{
    case a: Alu => a
  }.get

  io.toInt.bits.redirectValid := alu.redirectOutValid
  io.toInt.bits.redirect := alu.redirectOut
  io.toInt.bits.brUpdate := alu.brUpdate
}
