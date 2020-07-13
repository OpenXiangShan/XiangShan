package xiangshan.backend.decode.isa

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend._
import xiangshan.backend.decode.HasInstrType


object Privileged extends HasInstrType {
  def ECALL   = BitPat("b000000000000_00000_000_00000_1110011")
  def MRET    = BitPat("b001100000010_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")
  def SFANCE_VMA = BitPat("b0001001_?????_?????_000_00000_1110011")
  def FENCE   = BitPat("b????????????_?????_000_?????_0001111")
  def WFI     = BitPat("b0001000_00101_00000_000_00000_1110011") 

  // fixme: add privilege inst
  val table = Array(
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp)
//    SFANCE_VMA     -> List(InstrR, FuType.mou, MOUOpType.sfence_vma),
//    FENCE          -> List(InstrS, FuType.alu, ALUOpType.add), // nop    InstrS -> !wen
//    WFI            -> List(InstrI, FuType.alu, ALUOpType.add) // nop
    // FENCE          -> List(InstrB, FuType.mou, MOUOpType.fencei)

  )
}
