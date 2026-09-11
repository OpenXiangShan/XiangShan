package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.fpu.FpPipedFuncUnit
import xiangshan.backend.vector.fu.{FltFixLatFunc, VecFuConfig}
import yunsuan.fpu.falu.FloatAdderV2
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode

class FAluFlt(cfg: VecFuConfig)(implicit p: Parameters) extends FltFixLatFunc(cfg) {

  // io alias
  private val fire    = in.ex.head.valid
  private val src0    = ex0src0
  private val src1    = ex0src1

  private val inputFromFmul  = in.FmulToFadd.get
  private val FMULToFALUCtrl = inputFromFmul.bits.FMULToFALUCtrl
  private val fpAApend       = inputFromFmul.bits.fpAAppend
  private val isSubFromFmul  = inputFromFmul.bits.isSub

  // modules
  private val falu = Module(new FloatAdderV2)
  falu.io.fire                := fire
  falu.io.in.fp_fmt           := FMacOpcode.getDataType(ex0ctrl.opcode)
  falu.io.in.op_code          := ex0ctrl.opcode
  falu.io.in.fp_a             := src0
  falu.io.in.fp_b             := src1
  falu.io.in.fpAAppend        := fpAApend
  falu.io.in.inCtrlFromFMUL   := FMULToFALUCtrl
  falu.io.in.round_mode       := ex0ctrl.frm.get
  falu.io.in.isSubFromFMUL    := isSubFromFmul

  private val resultData = falu.io.out.fp_result
  private val fflagsData = falu.io.out.fflags
  out.ex.last.bits.data.fflags.get := fflagsData
  out.ex.last.bits.data.fp.get     := resultData
  out.ex(0).bits.data.fflags.get := 0.U
  out.ex(0).bits.data.fp.get     := 0.U
}