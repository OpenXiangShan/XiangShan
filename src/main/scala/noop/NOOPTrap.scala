package noop

import chisel3._
import chisel3.util._

object NOOPTrap extends HasInstrType {
  def StateGoodTrap  = 0.U
  def StateBadTrap   = 1.U
  def StateInvOpcode = 2.U
  def StateRunning   = 3.U

  def TRAP    = BitPat("b????????????_?????_000_?????_1101011")
  val table = Array(TRAP -> List(InstrI, FuType.alu, ALUOpType.add))
}

class Monitor extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Bool())
    val isNoopTrap = Input(Bool())
    val trapCode = Input(UInt(32.W))
    val trapPC = Input(UInt(64.W))
    val cycleCnt = Input(UInt(64.W))
    val instrCnt = Input(UInt(64.W))
  })
}
