package xiangshan

import chisel3._
import chisel3.util._

package object backend {

  // jal csr move(x2f) mou
  object BRUOpType {
    // [2bit:Type]: 00:csr 01:move(x2f) mou(fence.i,etc) jump
    // 0. csr
    def jmp  = "b00_000".U
    def wrt  = "b00_001".U
    def set  = "b00_010".U
    def clr  = "b00_011".U
    def wrti = "b00_101".U
    def seti = "b00_110".U
    def clri = "b00_111".U

    // 1. move(x2f)
    // FIXME: temp decode, should be fixed when use it
    def fmv_w_x = "b01_000".U
    def fmv_d_x = "b01_001".U

    // 2. mou
    def fence  = "b01_000".U
    def fencei = "b01_001".U
    def sfence_vma = "b01_010".U

    // 3. jump
    def jal  = "b11_000".U
    def jalr = "b11_010".U
    def call = "b11_011".U
    def ret  = "b11_100".U

    def isCSR(func: UInt) = func(4,3)===0.U
    def isFMV(func: UInt) = func(4,3)===1.U
    def isMOU(func: UInt) = func(4,3)===2.U // TODO: change its name
    def isJUMP(func: UInt) = func(4,3)===3.U
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

    // TODO: move jal/jalr/call/ret from ALU to BRU&CSR
    def jal  = "b011000".U
    def jalr = "b011010".U
    // def cjalr= "b111010".U // pc + 2 instead of 4
    def beq  = "b010000".U
    def bne  = "b010001".U
    def blt  = "b010100".U
    def bge  = "b010101".U
    def bltu = "b010110".U
    def bgeu = "b010111".U

    // for RAS
    def call = "b011100".U
    def ret  = "b011110".U

    // def pcPlus2(func: UInt) = func(5)//[important]
    def isBranch(func: UInt) = func(4,3)===2.U
    def isBru(func: UInt) = func(4)
    def isJump(func: UInt) = func(4,3)===3.U//isBru(func) && !isBranch(func)
    def getBranchType(func: UInt) = func(2, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object MDUOpType {
    def mul    = "b0000".U
    def mulh   = "b0001".U
    def mulhsu = "b0010".U
    def mulhu  = "b0011".U
    def mulw   = "b1000".U

    def div    = "b0100".U
    def divu   = "b0101".U
    def rem    = "b0110".U
    def remu   = "b0111".U

    def divw   = "b1100".U
    def divuw  = "b1101".U
    def remw   = "b1110".U
    def remuw  = "b1111".U

    def isDiv(op: UInt) = op(2)
    def isDivSign(op: UInt) = isDiv(op) && !op(0)
    def isW(op: UInt) = op(3)
  }

  object LSUOpType {
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
