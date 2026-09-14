package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.fpu.FloatCompare

class FCMP(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  // io alias
  private val src0 = inData.src(0)
  private val src1 = inData.src(1)

  // modules
  private val fcmp = Module(new FloatCompare)
  fcmp.io.src0            := src0
  fcmp.io.src1            := src1
  fcmp.io.opCode          := fuOpType

  private val resultData = fcmp.io.result
  private val fflagsData = fcmp.io.fflags

  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
}
