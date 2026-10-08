package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.decode.opcode.Opcode.FDivOpcodes
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.fpu.FpNonPipedFuncUnit
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.vector.fu.{FltFixLatFunc, FltNonFixedLatFunc, Func, VecFuConfig}
import yunsuan.fpu.FloatDivider

class FDivSqrt(cfg: FuConfig)(implicit p: Parameters) extends FpNonPipedFuncUnit(cfg) {

  // io alias
  private val opcode = fuOpType(3)
  private val src0 = inData.src(0)
  private val src1 = inData.src(1)

  // modules
  private val fdiv = Module(new FloatDivider)

  val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
                              fp_fmt === VSew.e16 && !src0.head(48).andR
  val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
                              fp_fmt === VSew.e16 && !src1.head(48).andR

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
  fdiv.io.opa_i          := src0
  fdiv.io.opb_i          := src1
  fdiv.io.is_sqrt_i      := opcode
  fdiv.io.rm_i           := rm
  fdiv.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
  fdiv.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN

  private val outFmt = outCtrl.fuOpType(2, 1)

  private val resultData = Mux1H(
    Seq(
      (outFmt === VSew.e16) -> Cat(Fill(48, 1.U), fdiv.io.fpdiv_res_o(15, 0)),
      (outFmt === VSew.e32) -> Cat(Fill(32, 1.U), fdiv.io.fpdiv_res_o(31, 0)),
      (outFmt === VSew.e64) -> fdiv.io.fpdiv_res_o
    )
  )
  private val fflagsData = fdiv.io.fflags_o

  io.in.ready  := fdiv.io.start_ready_o
  io.out.valid := fdiv.io.finish_valid_o

  io.out.bits.res.fflags.get := fflagsData
  io.out.bits.res.data       := resultData
  io.outValidAhead3Cycle.get := fdiv.io.outValidAhead3Cycle
  fdiv.io.wakeupSuccess := io.wakeupSuccess.get
}


class FDivSqrtFlt(cfg: VecFuConfig)(implicit p: Parameters) extends FltNonFixedLatFunc(cfg) {
  // io alias
  private val opcode = fuOpType(3)
  private val src0 = ex0src0
  private val src1 = ex0src1

  private val dpi = Module(new FDivSqrtDpi)
  private val pipe = Module(new FDivSqrtFltDpiPipe(cfg))
  private val dpiInputValid = in.ex(0).valid && !ex0ctrl.robIdx.needFlush(in.flush)
  private val dpiDelayedValid = RegNext(dpiInputValid, false.B)
  private val dpiDelayedBits = Reg(new Func.OutUop)
  private val dpiDelayedFpFormat = RegInit(0.U(2.W))

  private val fp_fmt = fuOpType(2, 1)
  private val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
    fp_fmt === VSew.e16 && !src0.head(48).andR
  private val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
    fp_fmt === VSew.e16 && !src1.head(48).andR

  dpi.io.valid := dpiInputValid
  dpi.io.fpFormat := fp_fmt
  dpi.io.opa := src0
  dpi.io.opb := src1
  dpi.io.isSqrt := opcode
  dpi.io.rm := ex0ctrl.frm.get
  dpi.io.opaCanonicalNan := fp_aIsFpCanonicalNAN
  dpi.io.opbCanonicalNan := fp_bIsFpCanonicalNAN

  private val resultData = Mux1H(
    Seq(
      (dpiDelayedFpFormat === VSew.e16) -> Cat(Fill(48, 1.U), dpi.io.result(15, 0)),
      (dpiDelayedFpFormat === VSew.e32) -> Cat(Fill(32, 1.U), dpi.io.result(31, 0)),
      (dpiDelayedFpFormat === VSew.e64) -> dpi.io.result
    )
  )

  pipe.io.flush := in.flush
  pipe.io.in.valid := dpiDelayedValid
  pipe.io.fpFormat := dpiDelayedFpFormat
  pipe.io.in.bits := dpiDelayedBits
  pipe.io.in.bits.data.fp.get := resultData
  pipe.io.in.bits.data.fflags.get := dpi.io.fflags

  when (dpiInputValid) {
    FDivSqrtDpiFltCtrl.copyToOutput(dpiDelayedBits.ctrl, ex0ctrl)
    dpiDelayedBits.debug.zip(in.ex(0).bits.debug).foreach { case (sink, source) => sink := source }
    dpiDelayedFpFormat := fp_fmt
  }

  out.ex(0).valid := pipe.io.out.valid
  out.ex(0).bits := pipe.io.out.bits
  outFuBusy := false.B
  outFuWakeUp.wen := pipe.io.wakeUp.valid
  outFuWakeUp.pdest := pipe.io.wakeUp.bits
  outFuWakeUp.loadDependency.foreach(_ := 0.U)
}
