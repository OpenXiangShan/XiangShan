package noop

import chisel3._
import chisel3.util._

trait HasInstrType {
  def InstrN  = "b0000".U
  def InstrI  = "b0100".U
  def InstrR  = "b0101".U
  def InstrS  = "b0010".U
  def InstrB  = "b0001".U
  def InstrU  = "b0110".U
  def InstrJ  = "b0111".U
  def InstrA  = "b1110".U

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

// trait CompInstConst {
//   val RVCRegNumTable = Array(
//     BitPat("b000") -> 8.U,
//     BitPat("b001") -> 9.U,
//     BitPat("b010") -> 10.U,
//     BitPat("b011") -> 11.U,
//     BitPat("b100") -> 12.U,
//     BitPat("b101") -> 13.U,
//     BitPat("b110") -> 14.U,
//     BitPat("b111") -> 15.U
//   )
// }

object SrcType {
  def reg = "b0".U
  def pc  = "b1".U
  def imm = "b1".U
  def apply() = UInt(1.W)
}

object FuType {
  def num = 6
  def alu = "b000".U
  def lsu = "b001".U
  def mdu = "b010".U
  def csr = "b011".U
  def mou = "b100".U
  def tlb = "b101".U
  def apply() = UInt(log2Up(num).W)
}

object FuOpType {
  def apply() = UInt(6.W)
}

object Instructions extends HasInstrType with HasNOOPParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def DecodeTable = RVIInstr.table ++ NOOPTrap.table ++
    (if (HasMExtension) RVMInstr.table else Nil) ++
    (if (HasCExtension) RVCInstr.table else Nil) ++
    Priviledged.table ++
    RVAInstr.table ++
    RVZicsrInstr.table ++ RVZifenceiInstr.table
}

object CInstructions extends HasInstrType with HasNOOPParameter{
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}
