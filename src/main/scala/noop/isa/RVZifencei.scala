package noop

import chisel3._
import chisel3.util._

object RVZifenceiInstr extends HasInstrType {
  def FENCEI = BitPat("b000000000000_00000_001_00000_0001111")

  val table = Array(
    FENCEI -> List(InstrB, FuType.mou, MOUOpType.fencei)
  )
}
