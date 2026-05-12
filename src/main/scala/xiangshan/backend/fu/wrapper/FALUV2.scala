package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import yunsuan.fpu.falu.FloatAdderV2
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode

class FAluV2(cfg: FuConfig)(implicit p: Parameters) extends FpPipedFuncUnit(cfg) {

  // io alias
  private val fire   = io.in.valid
  private val src0   = inData.src(0)
  private val src1   = inData.src(1)

  private val inputFromFmul  = inData.FmulToFaluDataInput.get
  private val FMULToFALUCtrl = inputFromFmul.FMULToFALUCtrl
  private val fpAApend       = inputFromFmul.fpAAppend
  private val isFMA          = FMULToFALUCtrl.isFMA
  private val isSubFromFmul  = inputFromFmul.isSub

  // modules
  private val falu = Module(new FloatAdderV2)
  falu.io.fire                := fire
  falu.io.in.fp_fmt           := fp_fmt
  falu.io.in.op_code          := fuOpType
  falu.io.in.fp_a             := src0
  falu.io.in.fp_b             := src1
  falu.io.in.fpAAppend        := fpAApend
  falu.io.in.inCtrlFromFMUL   := FMULToFALUCtrl
  falu.io.in.round_mode       := rm
  falu.io.in.isSubFromFMUL    := isSubFromFmul

  private val resultData = falu.io.out.fp_result
  private val fflagsData = falu.io.out.fflags
  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
}