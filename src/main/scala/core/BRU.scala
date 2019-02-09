package core

import chisel3._
import chisel3.util._

trait HasBRUOpType {
  val BruOpTypeNum  = 10

  val BruJal  = "b1000".U
  val BruJalr = "b1001".U
  val BruBeq  = "b0000".U
  val BruBne  = "b0001".U
  val BruBlt  = "b0100".U
  val BruBge  = "b0101".U
  val BruBltu = "b0110".U
  val BruBgeu = "b0111".U
}

trait BRUInstr extends HasDecodeConst {
  val JAL     = BitPat("b????????????????????_?????_1101111")
  val JALR    = BitPat("b????????????_?????_000_?????_1100111")

  val BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  val BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  val BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  val BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  val BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  val BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val BRUInstrTable = Array(
    JAL            -> List(InstrJ, FuBru, BruJal),
    JALR           -> List(InstrI, FuBru, BruJalr),

    BEQ            -> List(InstrB, FuBru, BruBeq),
    BNE            -> List(InstrB, FuBru, BruBne),
    BLT            -> List(InstrB, FuBru, BruBlt),
    BGE            -> List(InstrB, FuBru, BruBge),
    BLTU           -> List(InstrB, FuBru, BruBltu),
    BGEU           -> List(InstrB, FuBru, BruBgeu)
  )
}

class BRU extends HasBRUOpType {
  def access(isBru: Bool, pc: UInt, offset: UInt, src1: UInt, src2: UInt, func: UInt): BranchIO = {
    val branch = Wire(new BranchIO)
    branch.target := Mux(func === BruJalr, src1 + offset, pc + offset)
    branch.isTaken := isBru && LookupTree(func, false.B, List(
      BruBeq  -> (src1 === src2),
      BruBne  -> (src1 =/= src2),
      BruBlt  -> (src1.asSInt  <  src2.asSInt),
      BruBge  -> (src1.asSInt >=  src2.asSInt),
      BruBltu -> (src1  <  src2),
      BruBgeu -> (src1  >= src2),
      BruJal  -> true.B,
      BruJalr -> true.B
    ))
    branch
  }
}
