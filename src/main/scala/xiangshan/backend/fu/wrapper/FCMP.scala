package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import xiangshan.backend.vector.fu.{FltFixLatFunc, VecFuConfig}
import yunsuan.fpu.FloatCompare
import utility.DelayN

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

class FCMPFlt(cfg: VecFuConfig)(implicit p: Parameters) extends FltFixLatFunc(cfg) {
  // io alias
  private val src0 = ex0src0
  private val src1 = ex0src1

  // modules
  private val fcmp = Module(new FloatCompare)
  fcmp.io.src0            := src0
  fcmp.io.src1            := src1
  fcmp.io.opCode          := fuOpType

  private val resultData = fcmp.io.result
  private val fflagsData = fcmp.io.fflags


  out.ex(3).bits.data.fflags.get := DelayN(fflagsData, 3)
  out.ex(3).bits.data.int.get := DelayN(resultData, 3)
  out.ex(0).bits.data.fflags.get := 0.U
  out.ex(0).bits.data.int.get    := 0.U
  out.ex(1).bits.data.fflags.get := 0.U
  out.ex(1).bits.data.int.get    := 0.U
  out.ex(2).bits.data.fflags.get := 0.U
  out.ex(2).bits.data.int.get    := 0.U
}