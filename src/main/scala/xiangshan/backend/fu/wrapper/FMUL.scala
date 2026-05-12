package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.fpu.fmul._
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode

class FMul(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {

  // io alias
  private val fire = io.in.valid
  private val src0 = inData.src(0)
  private val src1 = inData.src(1)
  private val src2 = inData.src(2)
  private val imm = io.in.bits.data.imm

  private val src2S1 = RegEnable(src2, fire)
  private val immS1 = RegEnable(imm, fire)

  // module
  private val fmul = Module(new FloatMUL)
  private val isFMUL = FMacOpcode.isFmul(fuOpType)
  private val isNeg   = FMacOpcode.isFnmadd(fuOpType) || FMacOpcode.isFnmsub(fuOpType) || FMacOpcode.isFnmacc(fuOpType) || FMacOpcode.isFnmsac(fuOpType)
  private val isSub   = FMacOpcode.isFnmadd(fuOpType) || FMacOpcode.isFmsub(fuOpType) || FMacOpcode.isFnmacc(fuOpType) || FMacOpcode.isFmacc(fuOpType)
  private val isSubS1 = RegEnable(isSub, fire)

  // connect input
  fmul.io.fire          := fire
  fmul.io.in.isFMUL     := isFMUL
  fmul.io.in.isNeg      := isNeg
  fmul.io.in.fp_fmt     := fp_fmt
  fmul.io.in.fp_a       := src0
  fmul.io.in.fp_b       := src1
  fmul.io.in.round_mode := rm

  // fma results to falu
  val outToFaluFromFmul = io.outToFaluFromFmul.get
  //dirty code fuOpType
  outToFaluFromFmul.bits.ctrl.fuOpType := FMacOpcode.getCtrlOpcode(fuOpType)
  outToFaluFromFmul.bits.data.src(0) := fmul.io.outToFADD.fpA
  outToFaluFromFmul.bits.data.src(1) := src2S1
  outToFaluFromFmul.bits.data.FmulToFaluDataInput.get.FMULToFALUCtrl := fmul.io.outToFADD.FMULToFADDCtrl
  outToFaluFromFmul.bits.data.FmulToFaluDataInput.get.fpAAppend := fmul.io.outToFADD.fpAAppend
  outToFaluFromFmul.bits.data.FmulToFaluDataInput.get.isSub := isSubS1
  outToFaluFromFmul.bits.data.imm := immS1

  // fmul results to preg
  private val resultData = fmul.io.out.fp_result
  private val fflagsData = fmul.io.out.fflags
  io.out.bits.res.data := resultData
  io.out.bits.res.fflags.get := fflagsData
}