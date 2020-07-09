package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan.{FuType, HasXSParameter}
import xiangshan.backend._
import xiangshan.backend.decode.isa._

trait HasInstrType {
  def InstrN  = "b0000".U
  def InstrI  = "b0100".U
  def InstrR  = "b0101".U
  def InstrS  = "b0010".U
  def InstrB  = "b0001".U
  def InstrU  = "b0110".U
  def InstrJ  = "b0111".U
  def InstrA  = "b1110".U
  def InstrSA = "b1111".U // Atom Inst: SC

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

object SrcType {
  def reg = "b00".U
  def pc  = "b01".U
  def imm = "b01".U
  def fp  = "b10".U
  def apply() = UInt(2.W)
}


object FuOpType {
  def apply() = UInt(6.W)
}

object Instructions extends HasInstrType with HasXSParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(InstrN, FuType.alu, ALUOpType.sll)
  def DecodeTable = RVIInstr.table ++ XSTrap.table ++
    (if (HasMExtension) RVMInstr.table else Nil) // ++
//    (if (HasCExtension) RVCInstr.table else Nil) ++
//    (if (HasFPU) RVFInstr.table ++ RVDInstr.table else Nil) ++
//    Privileged.table ++
//    RVAInstr.table ++
//    RVZicsrInstr.table ++ RVZifenceiInstr.table
}

object CInstructions extends HasInstrType with HasXSParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}
