package noop

import chisel3._
import chisel3.util._

trait HasBRUOpType {
  val BruOpTypeNum  = 10

  def BruJal  = "b1000".U
  def BruJalr = "b1001".U
  def BruBeq  = "b0000".U
  def BruBne  = "b0001".U
  def BruBlt  = "b0100".U
  def BruBge  = "b0101".U
  def BruBltu = "b0110".U
  def BruBgeu = "b0111".U
}

object BRUInstr extends HasDecodeConst {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")

  def BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  def BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  def BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  def BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  def BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  def BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val table = Array(
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

class BRUIO extends FunctionUnitIO {
  val pc = Input(UInt(32.W))
  val offset = Input(UInt(32.W))
  val branch = new BranchIO
}

class BRU extends Module with HasBRUOpType {
  val io = IO(new BRUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  io.branch.target := Mux(func === BruJalr, src1, io.pc) + io.offset
  io.branch.isTaken := valid && LookupTree(func, false.B, List(
    BruBeq  -> (src1 === src2),
    BruBne  -> (src1 =/= src2),
    BruBlt  -> (src1.asSInt  <  src2.asSInt),
    BruBge  -> (src1.asSInt >=  src2.asSInt),
    BruBltu -> (src1  <  src2),
    BruBgeu -> (src1  >= src2),
    BruJal  -> true.B,
    BruJalr -> true.B
  ))
  io.out.bits := io.pc + 4.U

  io.in.ready := true.B
  io.out.valid := valid
}
