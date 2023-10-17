package xiangshan.backend.fu.util.CSR

import chisel3._
import chisel3.util.Enum

object CSRNamedConstant {
  object ContextStatus {
    val off :: initial :: clean :: dirty :: Nil = Enum(4)
  }

  object MXL {
    val w = 2
    val XLEN32 = 1.U(w.W)
    val XLEN64 = 2.U(w.W)
    val XLEN128 = 3.U(w.W)
  }
}
