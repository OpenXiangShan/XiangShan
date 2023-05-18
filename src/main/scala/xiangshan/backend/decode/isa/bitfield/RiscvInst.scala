package xiangshan.backend.decode.isa.bitfield

import chisel3._

abstract class RiscvInst(bitWidth: Int) extends Bundle {
  val inst: UInt = UInt(bitWidth.W)
}

class Riscv32BitInst extends RiscvInst(32) {
  def ALL     : UInt  = inst
  def OPCODE  : UInt  = inst( 6,  2)
  def RD      : UInt  = inst(11,  7)
  def FUNCT3  : UInt  = inst(14, 12)
  def RS1     : UInt  = inst(19, 15)
  def RS2     : UInt  = inst(24, 20)
  def FUNCT7  : UInt  = inst(31, 25)
}

trait BitFieldsI { this: Riscv32BitInst =>
  def IMM12   : UInt  = inst(31, 20)
}

trait BitFieldsS { this: Riscv32BitInst =>
  def IMM5    : UInt  = inst(11,  7)
  def IMM7    : UInt  = inst(31, 25)
}

trait BitFieldsCSR { this: Riscv32BitInst =>
  def CSRIDX  : UInt  = inst(31, 20)
  def CSRIMM  : UInt  = inst(19, 15)
}

trait BitFieldsFp { this: Riscv32BitInst =>
  def FD      : UInt  = inst(11,  7)
  def FS1     : UInt  = inst(19, 15)
  def FS2     : UInt  = inst(24, 20)
  def FS3     : UInt  = inst(31, 27)
  def RM      : UInt  = inst(14, 12) // round mode
  def CONV_SGN: UInt  = inst(24, 20)
  def FUNCT2  : UInt  = inst(26, 25)
}

trait BitFieldsVec { this: Riscv32BitInst =>
  def VCATEGORY     : UInt  = inst(14, 12)
  def NF            : UInt  = inst(31, 29)
  def MEW           : UInt  = inst(28)
  def MOP           : UInt  = inst(27, 26)
  def VM            : UInt  = inst(25)
  def LUMOP         : UInt  = inst(24, 20)
  def SUMOP         : UInt  = inst(24, 20)
  def WIDTH         : UInt  = inst(14, 12)
  def VD            : UInt  = inst(11,  7)
  def VS1           : UInt  = inst(19, 15)
  def VS2           : UInt  = inst(24, 20)
  def VS3           : UInt  = inst(11,  7)
  def FUNCT6        : UInt  = inst(31 ,26)
  def ZIMM_VSETVLI  : UInt  = inst(30, 20)
  def ZIMM_VSETIVLI : UInt  = inst(29, 20)
  def UIMM_VSETIVLI : UInt  = inst(19, 15)
  def ZIMM_VTYPE    : UInt  = ZIMM_VSETIVLI(7, 0)
  def IMM5_OPIVI    : UInt  = inst(19, 15)

  def getInstVType : InstVType = {
    val res = Wire(new InstVType)
    res.vlmul := ZIMM_VSETVLI(2, 0)
    res.vsew  := ZIMM_VSETVLI(5, 3)
    res.vta   := ZIMM_VSETVLI(6)
    res.vma   := ZIMM_VSETVLI(7)
    res
  }
}

class XSInstBitFields extends Riscv32BitInst
  with BitFieldsI
  with BitFieldsS
  with BitFieldsCSR
  with BitFieldsFp
  with BitFieldsVec

class InstVType extends Bundle {
  val vma = Bool()
  val vta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
}

