package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{InstPattern, IntJTypePattern, JalrPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

class IsJField(uopIdx: Int) extends BoolDecodeField[InstPattern] {
  override def name: String = s"isJ$uopIdx"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case IntJTypePattern() => y
      case _                 => n
    }
  }
}

class IsJrField(uopIdx: Int) extends BoolDecodeField[InstPattern] {
  override def name: String = s"isJr$uopIdx"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case _: JalrPattern => y
      case _              => n
    }
  }
}
