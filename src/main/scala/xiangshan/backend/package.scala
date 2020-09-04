package xiangshan

import chisel3._
import chisel3.util._

package object backend {

  object CSROpType {
    def jmp  = "b000".U
    def wrt  = "b001".U
    def set  = "b010".U
    def clr  = "b011".U
    def wrti = "b101".U
    def seti = "b110".U
    def clri = "b111".U
  }

  // jump
  object JumpOpType {
    def jal  = "b11_000".U
    def jalr = "b11_010".U
    def call = "b11_011".U
    def ret  = "b11_100".U
  }


  object ALUOpType {
    def add  = "b000000".U
    def sll  = "b000001".U
    def slt  = "b000010".U
    def sltu = "b000011".U
    def xor  = "b000100".U
    def srl  = "b000101".U
    def or   = "b000110".U
    def and  = "b000111".U
    def sub  = "b001000".U
    def sra  = "b001101".U

    def addw = "b100000".U
    def subw = "b101000".U
    def sllw = "b100001".U
    def srlw = "b100101".U
    def sraw = "b101101".U

    def isWordOp(func: UInt) = func(5)

    def beq  = "b010000".U
    def bne  = "b010001".U
    def blt  = "b010100".U
    def bge  = "b010101".U
    def bltu = "b010110".U
    def bgeu = "b010111".U

    def isBranch(func: UInt) = func(4)
    def getBranchType(func: UInt) = func(2, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object MDUOpType {
    // mul
    // bit encoding: | type (2bit) | isWord(1bit) | opcode(2bit) |
    def mul    = "b00000".U
    def mulh   = "b00001".U
    def mulhsu = "b00010".U
    def mulhu  = "b00011".U
    def mulw   = "b00100".U

    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b01000".U
    def divu   = "b01010".U
    def rem    = "b01001".U
    def remu   = "b01011".U

    def divw   = "b01100".U
    def divuw  = "b01110".U
    def remw   = "b01101".U
    def remuw  = "b01111".U

    // fence
    // bit encoding: | type (2bit) | padding(1bit)(zero) | opcode(2bit) |
    def fence    = "b10000".U
    def sfence   = "b10001".U
    def fencei   = "b10010".U

    // the highest bits are for instruction types
    def typeMSB = 4
    def typeLSB = 3

    def MulType     = "b00".U
    def DivType     = "b01".U
    def FenceType   = "b10".U

    def isMul(op: UInt)     = op(typeMSB, typeLSB) === MulType
    def isDiv(op: UInt)     = op(typeMSB, typeLSB) === DivType
    def isFence(op: UInt)   = op(typeMSB, typeLSB) === FenceType

    def isDivSign(op: UInt) = isDiv(op) && !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = (isDiv(op) && op(0)) || (isMul(op) && op(1,0)=/=0.U)
    def getMulOp(op: UInt) = op(1,0)
  }

  object LSUOpType {
    def lb   = "b000000".U
    def lh   = "b000001".U
    def lw   = "b000010".U
    def ld   = "b000011".U
    def lbu  = "b000100".U
    def lhu  = "b000101".U
    def lwu  = "b000110".U
    def flw  = "b010110".U
    def sb   = "b001000".U
    def sh   = "b001001".U
    def sw   = "b001010".U
    def sd   = "b001011".U

    def lr      = "b100010".U
    def sc      = "b100011".U
    def amoswap = "b100001".U
    def amoadd  = "b100000".U
    def amoxor  = "b100100".U
    def amoand  = "b101100".U
    def amoor   = "b101000".U
    def amomin  = "b110000".U
    def amomax  = "b110100".U
    def amominu = "b111000".U
    def amomaxu = "b111100".U

    def isStore(func: UInt): Bool = func(3)
    def isAtom(func: UInt): Bool = func(5)

    def atomW = "010".U
    def atomD = "011".U
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }
}
