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
    def jal  = "b00".U
    def jalr = "b01".U
    def auipc = "b10".U
//    def call = "b11_011".U
//    def ret  = "b11_100".U
    def jumpOpisJalr(op: UInt) = op(0)
    def jumpOpisAuipc(op: UInt) = op(1)
  }

  object FenceOpType {
    def fence  = "b10000".U
    def sfence = "b10001".U
    def fencei = "b10010".U
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

    def isAddSub(func: UInt) = {
      func === add || func === sub || func === addw || func === subw
    }

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
    // normal load/store
    // bit(1, 0) are size
    def lb   = "b000000".U
    def lh   = "b000001".U
    def lw   = "b000010".U
    def ld   = "b000011".U
    def lbu  = "b000100".U
    def lhu  = "b000101".U
    def lwu  = "b000110".U
    def sb   = "b001000".U
    def sh   = "b001001".U
    def sw   = "b001010".U
    def sd   = "b001011".U

    def isLoad(op: UInt): Bool = !op(3)
    def isStore(op: UInt): Bool = op(3)

    // atomics
    // bit(1, 0) are size
    // since atomics use a different fu type
    // so we can safely reuse other load/store's encodings
    def lr_w      = "b000010".U
    def sc_w      = "b000110".U
    def amoswap_w = "b001010".U
    def amoadd_w  = "b001110".U
    def amoxor_w  = "b010010".U
    def amoand_w  = "b010110".U
    def amoor_w   = "b011010".U
    def amomin_w  = "b011110".U
    def amomax_w  = "b100010".U
    def amominu_w = "b100110".U
    def amomaxu_w = "b101010".U

    def lr_d      = "b000011".U
    def sc_d      = "b000111".U
    def amoswap_d = "b001011".U
    def amoadd_d  = "b001111".U
    def amoxor_d  = "b010011".U
    def amoand_d  = "b010111".U
    def amoor_d   = "b011011".U
    def amomin_d  = "b011111".U
    def amomax_d  = "b100011".U
    def amominu_d = "b100111".U
    def amomaxu_d = "b101011".U
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object SelImm {
    def IMM_X  = "b111".U
    def IMM_S  = "b000".U
    def IMM_SB = "b001".U
    def IMM_U  = "b010".U
    def IMM_UJ = "b011".U
    def IMM_I  = "b100".U
    def IMM_Z  = "b101".U
    def INVALID_INSTR = "b110".U

    def apply() = UInt(3.W)
  }
}
