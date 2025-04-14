package xiangshan.backend.decode.isa

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.InstType
import xiangshan.macros.InstanceNameMacro.withNameSeq

object CustomInstructions {
  outer =>

  def TRAP = BitPat("b000000000000?????000000001101011")

  object XSTrapType extends InstType {
    def TRAP = outer.TRAP

    val allWithNames: Seq[(BitPat, String)] = withNameSeq(
      TRAP,
    )
  }
}
