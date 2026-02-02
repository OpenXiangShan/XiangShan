package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.ExceptionNO
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.VecPipedFuncUnit

// Temporary placeholder for disabled vector function units (e.g., vperm/reduction).
class VDisabled(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  io.out.bits.res.data := outOldVd
  io.out.bits.res.vxsat.foreach(_ := false.B)
  io.out.bits.ctrl.exceptionVec.foreach { vec =>
    vec := 0.U.asTypeOf(vec)
    vec(ExceptionNO.illegalInstr) := io.out.valid
  }
}
