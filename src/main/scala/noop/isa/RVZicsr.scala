package noop

import chisel3._
import chisel3.util._

object RVZicsrInstr extends HasInstrType {
  def CSRRW   = BitPat("b????????????_?????_001_?????_1110011")
  def CSRRS   = BitPat("b????????????_?????_010_?????_1110011")
  def ECALL   = BitPat("b001100000010_00000_000_00000_1110011")
  def MRET    = BitPat("b000000000000_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")

  val table = Array(
    CSRRW          -> List(InstrI, FuType.csr, CSROpType.wrt),
    CSRRS          -> List(InstrI, FuType.csr, CSROpType.set),
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp)
  )
}
