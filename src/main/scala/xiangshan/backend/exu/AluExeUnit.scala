  
package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import utils._
import xiangshan.backend.exu.Exu.aluExeUnitCfg
import xiangshan.backend.fu.Alu

class AluExeUnit extends Exu(aluExeUnitCfg)
{
  val alu = supportedFunctionUnits.collectFirst{
    case a: Alu => a
  }.get

  io.out.bits.redirectValid := alu.redirectOutValid
  io.out.bits.redirect := alu.redirectOut

  XSDebug(io.fromInt.valid || io.redirect.valid,
    p"fromInt(${io.fromInt.valid} ${io.fromInt.ready}) toInt(${io.out.valid} ${io.out.ready})" +
      p"Redirect:(${io.redirect.valid}) roqIdx:${io.redirect.bits.roqIdx}\n",
  )
  XSDebug(io.fromInt.valid,
    p"src1:${Hexadecimal(io.fromInt.bits.src1)} src2:${Hexadecimal(io.fromInt.bits.src2)} " +
      p"src3:${Hexadecimal(io.fromInt.bits.src3)} func:${Binary(io.fromInt.bits.uop.ctrl.fuOpType)} " +
      p"pc:${Hexadecimal(io.fromInt.bits.uop.cf.pc)} roqIdx:${io.fromInt.bits.uop.roqIdx}\n"
  )
  XSDebug(io.out.valid,
    p"res:${Hexadecimal(io.out.bits.data)}\n"
  )
}