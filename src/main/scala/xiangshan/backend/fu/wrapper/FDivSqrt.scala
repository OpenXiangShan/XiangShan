package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.fpu.FpNonPipedFuncUnit
import xiangshan.backend.rob.RobPtr
import yunsuan.VfpuType
import yunsuan.vector.VectorFloatDivider
import yunsuan.fpulite.FloatDivider

class FDivSqrt(cfg: FuConfig)(implicit p: Parameters) extends FpNonPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "fdiv OpType not supported")

  // io alias
  private val opcode = fuOpType(0)
  private val src0 = inData.src(0)
  private val src1 = inData.src(1)

  // modules
  private val fdiv = Module(new FloatDivider)

  val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
                              fp_fmt === VSew.e16 && !src1.head(48).andR
  val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
                              fp_fmt === VSew.e16 && !src0.head(48).andR

  val thisRobIdx = Wire(new RobPtr)
  when(io.in.ready){
    thisRobIdx := io.in.bits.ctrl.robIdx
  }.otherwise{
    thisRobIdx := outCtrl.robIdx
  }

  fdiv.io.start_valid_i  := io.in.valid
  fdiv.io.finish_ready_i := io.out.ready & io.out.valid
  fdiv.io.flush_i        := thisRobIdx.needFlush(io.flush)
  fdiv.io.fp_format_i    := fp_fmt
  fdiv.io.opa_i          := src1
  fdiv.io.opb_i          := src0
  fdiv.io.is_sqrt_i      := opcode
  fdiv.io.rm_i           := rm
  fdiv.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
  fdiv.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN

  private val resultData = fdiv.io.fpdiv_res_o
  private val fflagsData = fdiv.io.fflags_o

  io.in.ready  := fdiv.io.start_ready_o
  io.out.valid := fdiv.io.finish_valid_o

  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
}
