  
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

  io.toInt.bits.redirectValid := alu.redirectOutValid
  io.toInt.bits.redirect := alu.redirectOut
  io.toInt.bits.brUpdate := alu.brUpdate

  XSDebug(io.fromInt.valid || io.redirect.valid,
    p"fromInt(${io.fromInt.valid} ${io.fromInt.ready}) toInt(${io.toInt.valid} ${io.toInt.ready})" +
      p"Redirect:(${io.redirect.valid} ${io.redirect.bits.isException}${io.redirect.bits.isFlushPipe}${io.redirect.bits.isMisPred}${io.redirect.bits.isReplay}) roqIdx:${io.redirect.bits.roqIdx}\n",
  )
  XSDebug(io.fromInt.valid,
    p"src1:${Hexadecimal(io.fromInt.bits.src1)} src2:${Hexadecimal(io.fromInt.bits.src2)} " +
      p"src3:${Hexadecimal(io.fromInt.bits.src3)} func:${Binary(io.fromInt.bits.uop.ctrl.fuOpType)} " +
      p"pc:${Hexadecimal(io.fromInt.bits.uop.cf.pc)} roqIdx:${io.fromInt.bits.uop.roqIdx}\n"
  )
  XSDebug(io.toInt.valid,
    p"res:${Hexadecimal(io.toInt.bits.data)}\n"
  )
}