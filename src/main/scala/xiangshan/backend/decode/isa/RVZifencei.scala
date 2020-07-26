package xiangshan.backend.decode.isa

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.ALUOpType
import xiangshan.backend.decode._
import xiangshan.backend.decode.isa.RV64IInstr.InstrI


object RVZifenceiInstr extends HasInstrType {
  def FENCEI = BitPat("b000000000000_00000_001_00000_0001111")

  // fixme: add rvzifencei inst
  val table = Array(
    FENCEI -> List(InstrI, FuType.alu, ALUOpType.add)
  )
}
