package noop

import chisel3._
import chisel3.util._

object Priviledged extends HasInstrType {
  def ECALL   = BitPat("b000000000000_00000_000_00000_1110011")
  def MRET    = BitPat("b001100000010_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")

  val table = Array(
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp)
  )
}
