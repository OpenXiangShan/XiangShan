package xiangshan.backend.decode.isa.bitfield

import chisel3._

abstract class RiscvInst(bitWidth: Int) extends Bundle {
  val inst: UInt = UInt(bitWidth.W)
}

class Riscv32BitInst extends RiscvInst(32) {
  def ALL       : UInt  = inst
  def OPCODE    : UInt  = inst( 6,  0)
  def RD        : UInt  = inst(11,  7)
  def FUNCT3    : UInt  = inst(14, 12)
  def RS1       : UInt  = inst(19, 15)
  def RS2       : UInt  = inst(24, 20)
  def FUNCT7    : UInt  = inst(31, 25)
  def OPCODE5Bit: UInt  = inst( 6,  2)
}

trait BitFieldsI { this: Riscv32BitInst =>
  def IMM12   : UInt  = inst(31, 20)
  def SHAMT6  : UInt  = inst(25, 20)
  def SHAMT5  : UInt  = inst(24, 20)
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
  def FMT     : UInt  = inst(26, 25)
  def TYP     : UInt  = inst(21, 20)
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
  def IMM5_OPIVI    : UInt  = inst(19, 15)

  def getInstVType : InstVType = {
    val res = Wire(new InstVType)
    res.vlmul := ZIMM_VSETVLI(2, 0)
    res.vsew  := ZIMM_VSETVLI(5, 3)
    res.vta   := ZIMM_VSETVLI(6)
    res.vma   := ZIMM_VSETVLI(7)
    res
  }

  def isVecStore = {
    this.OPCODE === "b0100111".U && (this.WIDTH === 0.U || this.WIDTH(2) === 1.B)
  }

  def isVecLoad = {
    this.OPCODE === "b0000111".U && (this.WIDTH === 0.U || this.WIDTH(2) === 1.B)
  }
}

class XSInstBitFields extends Riscv32BitInst
  with BitFieldsI
  with BitFieldsS
  with BitFieldsCSR
  with BitFieldsFp
  with BitFieldsVec

class InstVType extends Bundle {
  val reserved = UInt(3.W)
  val vma = Bool()
  val vta = Bool()
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
}

object OPCODE5Bit {
  val LOAD      = "b00_000".U
  val LOAD_FP   = "b00_001".U
  val CUSTOM_0  = "b00_010".U
  val MSIC_MEM  = "b00_011".U
  val OP_IMM    = "b00_100".U
  val AUIPC     = "b00_101".U
  val OP_IMM_32 = "b00_110".U
  val INST48b_0 = "b00_111".U

  val STORE     = "b01_000".U
  val STORE_FP  = "b01_001".U
  val CUSTOM_1  = "b01_010".U
  val AMO       = "b01_011".U
  val OP        = "b01_100".U
  val LUI       = "b01_101".U
  val OP_32     = "b01_110".U
  val INST64b   = "b01_111".U

  val MADD      = "b10_000".U
  val MSUB      = "b10_001".U
  val NMSUB     = "b10_010".U
  val NMADD     = "b10_011".U
  val OP_FP     = "b10_100".U
  val OP_V      = "b10_101".U
  val CUSTOM_2  = "b10_110".U
  val INST48b_1 = "b10_111".U

  val BRANCH     = "b11_000".U
  val JALR       = "b11_001".U
  val RESERVED_0 = "b11_010".U
  val JAL        = "b11_011".U
  val SYSTEM     = "b11_100".U
  val RESERVED_1 = "b11_101".U
  val CUSTOM_3   = "b11_110".U
  val INSTge80b  = "b11_111".U
}
