package core

import chisel3._
import chisel3.util._

trait HasLSUOpType {
  val LsuOpTypeNum  = 10

  def LsuLb   = "b0000".U
  def LsuLh   = "b0001".U
  def LsuLw   = "b0010".U
  def LsuLbu  = "b0100".U
  def LsuLhu  = "b0101".U
  def LsuSb   = "b1000".U
  def LsuSh   = "b1001".U
  def LsuSw   = "b1010".U
}

trait LSUInstr extends HasDecodeConst {
  val LB      = BitPat("b????????????_?????_000_?????_0000011")
  val LH      = BitPat("b????????????_?????_001_?????_0000011")
  val LW      = BitPat("b????????????_?????_010_?????_0000011")
  val LBU     = BitPat("b????????????_?????_100_?????_0000011")
  val LHU     = BitPat("b????????????_?????_101_?????_0000011")
  val SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  val SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  val SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val LSUInstrTable = Array(
    LB             -> List(InstrI, FuLsu, LsuLb ),
    LH             -> List(InstrI, FuLsu, LsuLh ),
    LW             -> List(InstrI, FuLsu, LsuLw ),
    LBU            -> List(InstrI, FuLsu, LsuLbu),
    LHU            -> List(InstrI, FuLsu, LsuLhu),
    SB             -> List(InstrS, FuLsu, LsuSb ),
    SH             -> List(InstrS, FuLsu, LsuSh ),
    SW             -> List(InstrS, FuLsu, LsuSw)
  )
}

class ABundle extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(2.W))
  val wdata = Output(UInt(32.W))
  val wen = Output(Bool())
}

class RBundle extends Bundle {
  val rdata = Output(UInt(32.W))
}

class MemIO extends Bundle {
  val out = Valid(new ABundle)
  val in = Flipped(new RBundle)
}

class LSU extends HasLSUOpType {
  def access(isLsu: Bool, base: UInt, offset: UInt, func: UInt, wdata: UInt): MemIO = {
    val dmem = Wire(new MemIO)
    dmem.out.bits.addr := base + offset
    dmem.out.valid := isLsu
    dmem.out.bits.wen := isLsu && func(3)
    dmem.out.bits.size := func(1, 0)
    dmem.out.bits.wdata := wdata
    dmem
  }
  def rdataExt(rdata: UInt, func: UInt): UInt = {
    LookupTree(func, rdata, List(
      LsuLb   -> Cat(Fill(24, rdata(7)), rdata(7, 0)),
      LsuLh   -> Cat(Fill(16, rdata(15)), rdata(15, 0)),
      LsuLw   -> rdata,
      LsuLbu  -> Cat(0.U(24.W), rdata(7, 0)),
      LsuLhu  -> Cat(0.U(16.W), rdata(15, 0))
    ))
  }
}
