package noop

import chisel3._
import chisel3.util._

trait HasInstrType {
  private val InstrTypeNum = 9
  def InstrN  = "b0000".U
  def InstrI  = "b0100".U
  def InstrR  = "b0101".U
  def InstrS  = "b0010".U
  def InstrB  = "b0001".U
  def InstrU  = "b0110".U
  def InstrJ  = "b0111".U

  def InstrIW = "b1100".U

  def isrfWen(instrType : UInt): Bool = instrType(2)
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
