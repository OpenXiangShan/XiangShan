package xiangshan

import chisel3._
import chisel3.util._

package object backend {

  // jal csr mov
  object BRUOpType {
    // 1. jal
    def jal  = "b011000".U
    def jalr = "b011010".U
    // def cjalr= "b111010".U // pc + 2 instead of 4

    // 2. csr
    def jmp  = "b000".U
    def wrt  = "b001".U
    def set  = "b010".U
    def clr  = "b011".U
    def wrti = "b101".U
    def seti = "b110".U
    def clri = "b111".U

    // todo: 3. mov
//    def pcPlus2(func: UInt) = func(5)//[important]
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

  object MULOpType {
    def mul    = "b0000".U
    def mulh   = "b0001".U
    def mulhsu = "b0010".U
    def mulhu  = "b0011".U
    def mulw   = "b1000".U

  }

  object MDUOpType {
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

  object LDUOpType {
    def lb   = "b000000".U
    def lh   = "b000001".U
    def lw   = "b000010".U
    def ld   = "b000011".U
    def lbu  = "b000100".U
    def lhu  = "b000101".U
    def lwu  = "b000110".U
    def flw  = "b010110".U // box 32-bit data to 64-bit with 1s

//    def lr      = "b100000".U
//    def sc      = "b100001".U
//    def amoswap = "b100010".U
//    def amoadd  = "b100011".U
//    def amoxor  = "b100100".U
//    def amoand  = "b100101".U
//    def amoor   = "b100110".U
//    def amomin  = "b110111".U
//    def amomax  = "b110000".U
//    def amominu = "b110001".U
//    def amomaxu = "b110010".U

    def isStore(func: UInt): Bool = func(3)
    def isAtom(func: UInt): Bool = func(5)
    def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
//    def isLR(func: UInt): Bool = func === lr
//    def isSC(func: UInt): Bool = func === sc
//    def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

//    def atomW = "010".U
//    def atomD = "011".U
  }

  object STUOpType {
    def sb   = "b001000".U
    def sh   = "b001001".U
    def sw   = "b001010".U
    def sd   = "b001011".U

  }



  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

}
