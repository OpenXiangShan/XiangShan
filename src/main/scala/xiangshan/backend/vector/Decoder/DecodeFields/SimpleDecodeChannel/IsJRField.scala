package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{InstPattern, IntJTypePattern, JalrPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/**
 * Per-uop field that identifies uops belonging to JALR/JAL instructions.
 * Combined with isFirstUop/isLastUop to distinguish link vs jr uop:
 *   - isJR && isFirstUop  => link uop
 *   - isJR && isLastUop   => jr uop
 */
class IsJRField(uopIdx: Int) extends BoolDecodeField[InstPattern] {
  override def name: String = s"isJR$uopIdx"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case _: JalrPattern    => y
      case IntJTypePattern() => y
      case _                 => n
    }
  }
}
