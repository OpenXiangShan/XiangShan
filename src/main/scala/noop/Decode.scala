package noop

import chisel3._
import chisel3.util._

trait HasInstrType {
  private val InstrTypeNum = 7
  def InstrN = "b000".U
  def InstrI = "b100".U
  def InstrR = "b101".U
  def InstrS = "b010".U
  def InstrB = "b001".U
  def InstrU = "b110".U
  def InstrJ = "b111".U

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
  def apply() = UInt(5.W)
}

object Instructions extends HasInstrType {
  def NOP = 0x00000013.U
  val DecodeDefault = List(InstrN, FuType.csr, CSROpType.jmp)
  def DecodeTable(implicit p: NOOPConfig) = ALUInstr.table ++ BRUInstr.table ++ LSUInstr.table ++
                    (if (p.HasMExtension) MDUInstr.table else Nil) ++
                    CSRInstr.table ++ NOOPTrap.table
}
