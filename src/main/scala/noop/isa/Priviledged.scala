package noop

import chisel3._
import chisel3.util._

object Priviledged extends HasInstrType {
  def ECALL   = BitPat("b000000000000_00000_000_00000_1110011")
  def MRET    = BitPat("b001100000010_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")
  def SFANCE_VMA = BitPat("b0001001_?????_?????_000_00000_1110011")
  def FENCE   = BitPat("b????????????_?????_000_?????_0001111")

  val table = Array(
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SFANCE_VMA     -> List(InstrR, FuType.tlb, TLBOpType.vma),
    FENCE          -> List(InstrS, FuType.alu, ALUOpType.add) // InstrS -> !wen
    // FENCE          -> List(InstrB, FuType.mou, MOUOpType.fencei)

  )
}
