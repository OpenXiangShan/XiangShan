package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.{VfaluType, VfpuType}
import yunsuan.vector.VectorFloatAdder

class FAlu(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "falu OpType not supported")

  // io alias
  private val opcode = fuOpType(4, 0)
  private val src0 = inData.src(0)
  private val src1 = inData.src(1)

  // modules
  private val falu = Module(new VectorFloatAdder)

  val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
                              fp_fmt === VSew.e16 && !src1.head(48).andR
  val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
                              fp_fmt === VSew.e16 && !src0.head(48).andR

  falu.io.fire             := io.in.valid
  falu.io.fp_a             := src1
  falu.io.fp_b             := src0
  falu.io.widen_a          := 0.U
  falu.io.widen_b          := 0.U
  falu.io.frs1             := 0.U 
  falu.io.is_frs1          := false.B
  falu.io.mask             := "b1111".U
  falu.io.maskForReduction := 0.U
  falu.io.uop_idx          := 0.U
  falu.io.is_vec           := false.B
  falu.io.round_mode       := rm
  falu.io.fp_format        := fp_fmt
  falu.io.opb_widening     := false.B
  falu.io.res_widening     := false.B
  falu.io.op_code          := opcode
  falu.io.is_vfwredosum    := false.B
  falu.io.is_fold          := false.B
  falu.io.vs2_fold         := 0.U
  falu.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
  falu.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN

  private val resultData = falu.io.fp_result
  private val fflagsData = falu.io.fflags

  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
}
