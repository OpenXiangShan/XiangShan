package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object BRUOpType {
  def jal  = "b011000".U
  def jalr = "b011010".U
  def beq  = "b010000".U
  def bne  = "b010001".U
  def blt  = "b010100".U
  def bge  = "b010101".U
  def bltu = "b010110".U
  def bgeu = "b010111".U

  // for RAS
  def call = "b011100".U
  def ret  = "b011110".U

  def isBru(func: UInt) = func(4)//[important]
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

object BRUInstr extends HasInstrType {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")

  def BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  def BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  def BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  def BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  def BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  def BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val table = Array(
    JAL            -> List(InstrJ, FuType.alu, BRUOpType.jal),
    JALR           -> List(InstrI, FuType.alu, BRUOpType.jalr),

    BEQ            -> List(InstrB, FuType.alu, BRUOpType.beq),
    BNE            -> List(InstrB, FuType.alu, BRUOpType.bne),
    BLT            -> List(InstrB, FuType.alu, BRUOpType.blt),
    BGE            -> List(InstrB, FuType.alu, BRUOpType.bge),
    BLTU           -> List(InstrB, FuType.alu, BRUOpType.bltu),
    BGEU           -> List(InstrB, FuType.alu, BRUOpType.bgeu)
  )

  val bruFuncTobtbTypeTable = List(
    BRUOpType.beq  -> BTBtype.B,
    BRUOpType.bne  -> BTBtype.B,
    BRUOpType.blt  -> BTBtype.B,
    BRUOpType.bge  -> BTBtype.B,
    BRUOpType.bltu -> BTBtype.B,
    BRUOpType.bgeu -> BTBtype.B,
    BRUOpType.call -> BTBtype.J,
    BRUOpType.ret  -> BTBtype.R,
    BRUOpType.jal  -> BTBtype.J,
    BRUOpType.jalr -> BTBtype.I
  )
}

// BRU implementation is moved to ALU
