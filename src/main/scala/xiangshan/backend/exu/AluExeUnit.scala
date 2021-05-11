  
package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.fu.Alu

class AluExeUnit(implicit p: Parameters) extends Exu(AluExeUnitCfg)
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
    p"src1:${Hexadecimal(io.fromInt.bits.src(0))} src2:${Hexadecimal(io.fromInt.bits.src(1))} " +
      p"src3:${Hexadecimal(io.fromInt.bits.src(2))} func:${Binary(io.fromInt.bits.uop.ctrl.fuOpType)} " +
      p"pc:${Hexadecimal(io.fromInt.bits.uop.cf.pc)} roqIdx:${io.fromInt.bits.uop.roqIdx}\n"
  )
  XSDebug(io.out.valid,
    p"res:${Hexadecimal(io.out.bits.data)}\n"
  )
}
