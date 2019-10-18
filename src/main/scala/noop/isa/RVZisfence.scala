package noop

import chisel3._
import chisel3.util._

object RVZitlbInstr extends HasInstrTYpe {
  def SFENCE.VMA = BitPat("b0001001_?????_?????_000_00000_1110011")

  val table = Array(
    SFENCE.VMA -> List(InstrR, FuType.tlb, TLBOpType.vma)
  )
}