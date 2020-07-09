package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.ALUOpType


object XSTrap extends HasInstrType {
  def StateGoodTrap  = 0.U
  def StateBadTrap   = 1.U
  def StateInvOpcode = 2.U
  def StateRunning   = 3.U
  /*
        calculate as ADDI => addi zero, a0, 0
        replace rs '?????' with '01010'(a0) in decode stage
   */
  def TRAP    = BitPat("b000000000000_?????_000_00000_1101011")
  val table = Array(TRAP -> List(InstrI, FuType.alu, ALUOpType.add))
}
