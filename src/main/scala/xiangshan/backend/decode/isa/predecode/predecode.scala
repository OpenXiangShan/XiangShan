package xiangshan.backend.decode.isa.predecode

import chisel3.util._
import xiangshan.frontend.BrType

object PreDecode {
  def C_JAL     = BitPat("b?01_?_??_???_??_???_01")  //c.jal & c.j  //C_ADDIW?
  def C_JALR    = BitPat("b100_?_??_???_00_000_10")
  def C_BRANCH  = BitPat("b11?_?_??_???_??_???_01")
  def JAL       = BitPat("b???_?????_1101111")
  def JALR      = BitPat("b000_?????_1100111")
  def BRANCH    = BitPat("b???_?????_1100011")


  val brTable = Array(
    C_JAL     -> List(BrType.jal),
    C_JALR    -> List(BrType.jalr),
    C_BRANCH  -> List(BrType.branch),
    JAL       -> List(BrType.jal),
    JALR      -> List(BrType.jalr),
    BRANCH    -> List(BrType.branch)
  )
}
