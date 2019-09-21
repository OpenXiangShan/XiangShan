package noop

import chisel3._
import chisel3.util._

trait HasInstrType {
  def InstrN  = "b000".U
  def InstrI  = "b100".U
  def InstrR  = "b101".U
  def InstrS  = "b010".U
  def InstrB  = "b001".U
  def InstrU  = "b110".U
  def InstrJ  = "b111".U

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

trait CompInstConst {
  val RVCRegNumTable = Array(
    BitPat("b000") -> 8.U,
    BitPat("b001") -> 9.U,
    BitPat("b010") -> 10.U,
    BitPat("b011") -> 11.U,
    BitPat("b100") -> 12.U,
    BitPat("b101") -> 13.U,
    BitPat("b110") -> 14.U,
    BitPat("b111") -> 15.U
  )
}

object SrcType {
  def reg = "b0".U
  def pc  = "b1".U
  def imm = "b1".U
  def apply() = UInt(1.W)
}

object FuType {
  def num = 4
  def alu = "b00".U
  def lsu = "b01".U
  def mdu = "b10".U
  def csr = "b11".U
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
    RVZicsrInstr.table
}
