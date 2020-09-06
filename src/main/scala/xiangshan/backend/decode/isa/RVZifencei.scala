package xiangshan.backend.decode.isa

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.MDUOpType
import xiangshan.backend.decode._
import xiangshan.backend.decode.isa.RV64IInstr.InstrU


object RVZifenceiInstr extends HasInstrType {
  def FENCEI = BitPat("b000000000000_00000_001_00000_0001111")

  // fixme: add rvzifencei inst
  val table = Array(
    FENCEI -> List(InstrU, FuType.fence, FenceOpType.fencei)
  )
}