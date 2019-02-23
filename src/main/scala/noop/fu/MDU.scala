package noop

import chisel3._
import chisel3.util._

trait HasMDUOpType {
  val MduOpTypeNum  = 8

  def MduMul  = "b000".U
  def MduMulh = "b001".U
  def MduDiv  = "b100".U
  def MduDivu = "b101".U
  def MduRem  = "b110".U
  def MduRemu = "b111".U
}

object MDUInstr extends HasDecodeConst {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")

  val table = Array(
    MUL            -> List(InstrR, FuMdu, MduMul),
    MULH           -> List(InstrR, FuMdu, MduMulh)
    //DIV            -> List(InstrR, FuMdu, MduDiv),
    //DIVU           -> List(InstrR, FuMdu, MduDivu),
    //REM            -> List(InstrR, FuMdu, MduRem),
    //REMU           -> List(InstrR, FuMdu, MduRemu)
  )
}

class MDU extends Module with HasMDUOpType {
  val io = IO(new FunctionUnitIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val mulRes = (src1.asSInt * src2.asSInt).asUInt
  val mulPipeOut = Pipe(io.in.fire(), mulRes, 4)
  io.out.bits := LookupTree(func, 0.U, List(
    MduMul  -> mulPipeOut.bits(31, 0),
    MduMulh -> mulPipeOut.bits(63, 32)
    //MduDiv  -> (src1.asSInt  /  src2.asSInt).asUInt,
    //MduDivu -> (src1  /  src2),
    //MduRem  -> (src1.asSInt  %  src2.asSInt).asUInt,
    //MduRemu -> (src1  %  src2)
  ))

  val busy = RegInit(false.B)
  when (io.in.valid && !busy) { busy := true.B }
  when (mulPipeOut.valid) { busy := false.B }

  io.in.ready := !busy
  io.out.valid := mulPipeOut.valid
}
