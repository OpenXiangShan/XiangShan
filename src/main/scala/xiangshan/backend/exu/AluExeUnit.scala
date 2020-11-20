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

  io.out.bits.redirectValid := alu.redirectOutValid
  io.out.bits.redirect := alu.redirectOut
  io.out.bits.brUpdate := alu.brUpdate

  XSDebug(io.in.valid || io.redirect.valid,
    p"In(${io.in.valid} ${io.in.ready}) Out(${io.out.valid} ${io.out.ready})" + 
      p"Redirect:(${io.redirect.valid} ${io.redirect.bits.isException}${io.redirect.bits.isFlushPipe}${io.redirect.bits.isMisPred}${io.redirect.bits.isReplay}) roqIdx:${io.redirect.bits.roqIdx}\n",
  )
  XSDebug(io.in.valid,
    p"src1:${Hexadecimal(src1)} src2:${Hexadecimal(src2)} " +
      p"src3:${Hexadecimal(src3)} func:${Binary(func)} " +
      p"pc:${Hexadecimal(io.in.bits.uop.cf.pc)} roqIdx:${io.in.bits.uop.roqIdx}\n"
  )
  XSDebug(io.out.valid,
    p"res:${Hexadecimal(io.out.bits.data)}\n"
  )
}