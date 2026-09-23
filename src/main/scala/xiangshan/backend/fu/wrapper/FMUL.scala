package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import xiangshan.backend.vector.fu.{VecFuConfig, FltFixLatFunc}
import yunsuan.fpu.fmul._
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode

class FMulFlt(cfg: VecFuConfig)(implicit p: Parameters) extends FltFixLatFunc(cfg) {

  // io alias
  private val fire = in.ex.head.valid
  private val src0 = ex0src0
  private val src1 = ex0src1
  private val src2 = ex0src2

  // module
  private val fmul = Module(new FloatMUL)
  private val isFMUL = FMacOpcode.isFmul(fuOpType)
  private val isNeg   = FMacOpcode.isFnmadd(fuOpType) || FMacOpcode.isFnmsub(fuOpType) || FMacOpcode.isFnmacc(fuOpType) || FMacOpcode.isFnmsac(fuOpType)
  private val isSub   = FMacOpcode.isFnmadd(fuOpType) || FMacOpcode.isFmsub(fuOpType) || FMacOpcode.isFnmacc(fuOpType) || FMacOpcode.isFmacc(fuOpType)
  private val isSubS1 = RegEnable(isSub, fire)
  private val src2S1 = RegEnable(ex0src2, fire)

  // connect input
  fmul.io.fire          := fire
  fmul.io.in.isFMUL     := isFMUL
  fmul.io.in.isNeg      := isNeg
  fmul.io.in.fp_fmt     := FMacOpcode.getDataType(ex0ctrl.opcode)
  fmul.io.in.fp_a       := src0
  fmul.io.in.fp_b       := src1
  fmul.io.in.round_mode := ex0ctrl.frm.get

  // fma results to falu
  val outToFaluFromFmul = out.FmulToFadd.get
  //dirty code fuOpType, in valid next cycle outToFaluFromFmul valid
  outToFaluFromFmul.valid := RegNext(in.ex.head.valid && FMacOpcode.isOP3(ex0ctrl.opcode))
  outToFaluFromFmul.bits.FMULToFALUCtrl := fmul.io.outToFADD.FMULToFADDCtrl
  outToFaluFromFmul.bits.fpAAppend := fmul.io.outToFADD.fpAAppend
  outToFaluFromFmul.bits.fpA := fmul.io.outToFADD.fpA
  outToFaluFromFmul.bits.src2 := src2S1
  outToFaluFromFmul.bits.isSub := isSubS1

  // fmul results to preg
  private val resultData = fmul.io.out.fp_result
  private val fflagsData = fmul.io.out.fflags
  out.ex.last.bits.data.fflags.get := fflagsData
  out.ex.last.bits.data.fp.get := resultData
  out.ex(0).bits.data.fflags.get := 0.U
  out.ex(0).bits.data.fp.get     := 0.U
  out.ex(1).bits.data.fflags.get := 0.U
  out.ex(1).bits.data.fp.get     := 0.U
}