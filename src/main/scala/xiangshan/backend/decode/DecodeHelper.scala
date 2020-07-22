package xiangshan.backend.decode

import chisel3._
import xiangshan.{FuType, HasXSParameter}
import xiangshan.backend._
import xiangshan.backend.decode.isa._

trait HasInstrType {

  // TODO: optimize these encoding
  def InstrN    = "b0000".U
  def InstrI    = "b0001".U
  def InstrR    = "b0010".U
  def InstrS    = "b0011".U
  def InstrB    = "b0100".U
  def InstrU    = "b0101".U
  def InstrJ    = "b0110".U
  def InstrA    = "b0111".U
  def InstrSA   = "b1000".U // Atom Inst: SC
  def InstrFR   = "b1001".U
  def InstrFI   = "b1010".U // flw/fld
  def InstrGtoF = "b1011".U
  def InstrFS   = "b1100".U
  def InstrFtoG = "b1101".U

  def isrfWen(instrType : UInt): Bool = Array(
    InstrI, InstrR, InstrU, InstrJ, InstrA, InstrSA, InstrFtoG
  ).map(_===instrType).reduce(_||_)

  def isfpWen(instrType: UInt): Bool = Array(
    InstrFI, InstrFR, InstrGtoF
  ).map(_===instrType).reduce(_||_)
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
  def DecodeTable =
    RVIInstr.table ++
      XSTrap.table ++
      RVZicsrInstr.table ++
      RVZifenceiInstr.table ++
      Privileged.table ++
      RVFInstr.table ++
      RVDInstr.table ++
      (if (HasMExtension) RVMInstr.table else Nil) // ++
//    (if (HasCExtension) RVCInstr.table else Nil) ++
//    (if (HasFPU) RVFInstr.table ++ RVDInstr.table else Nil) ++
//    Privileged.table ++
//    RVAInstr.table ++
//    RVZicsrInstr.table
}

object CInstructions extends HasInstrType with HasXSParameter {
  def NOP = 0x00000013.U
  val DecodeDefault = List(RVCInstr.ImmNone, RVCInstr.DtCare, RVCInstr.DtCare, RVCInstr.DtCare)
  // val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def CExtraDecodeTable = RVCInstr.cExtraTable
}
