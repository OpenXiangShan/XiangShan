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
import xiangshan.backend.vector.fu.{FltFixLatFunc, FltNonFixedLatFunc, VecFuConfig}
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

  // modules
  private val fdiv = Module(new FloatDivider)

  private val fp_fmt = fuOpType(2, 1)
  private val fp_aIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src0.head(32).andR ||
    fp_fmt === VSew.e16 && !src0.head(48).andR
  private val fp_bIsFpCanonicalNAN  = fp_fmt === VSew.e32 && !src1.head(32).andR ||
    fp_fmt === VSew.e16 && !src1.head(48).andR

  private val thisRobIdx = Wire(new RobPtr)
  val inFire = in.ex(0).valid && fdiv.io.start_ready_o
  when(inFire){
    thisRobIdx := ex0ctrl.robIdx
  }.otherwise{
    thisRobIdx := out.ex(0).bits.ctrl.robIdx
  }
  out.ex(0).bits.ctrl := Mux(inFire, nonFixedLatOutCtrlNext, nonFixedLatOutCtrl)
  out.ex(0).bits.debug.zip(nonFixedLatOutDebug).foreach { case (sink, source) =>
    sink := Mux(inFire, in.ex(0).bits.debug.get, source)
  }
  when(inFire) {
    nonFixedLatOutCtrl := nonFixedLatOutCtrlNext
    nonFixedLatOutDebug.zip(ex(0).bits.debug).foreach { case (sink, source) =>
      sink := source
    }
  }

  outFuWakeUp.pdest := Mux(inFire, ex0ctrl.pdest, nonFixedLatOutCtrl.pdest)
  outFuWakeUp.loadDependency := 0.U.asTypeOf(outFuWakeUp.loadDependency)

  fdiv.io.start_valid_i  := in.ex(0).valid && fdiv.io.start_ready_o
  fdiv.io.finish_ready_i := out.ex(0).valid
  fdiv.io.flush_i        := thisRobIdx.needFlush(in.flush)
  fdiv.io.fp_format_i    := fp_fmt
  fdiv.io.opa_i          := src0
  fdiv.io.opb_i          := src1
  fdiv.io.is_sqrt_i      := opcode
  fdiv.io.rm_i           := ex0ctrl.frm.get
  fdiv.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
  fdiv.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN
  fdiv.io.wakeupSuccess := in.busyTableEmpty.get
  outFuBusy := !fdiv.io.start_ready_o

  private val outFmt = RegEnable(FDivOpcodes.getFormat(ex0ctrl.opcode), in.ex.head.valid)

  private val resultData = Mux1H(
    Seq(
      (outFmt === VSew.e16) -> Cat(Fill(48, 1.U), fdiv.io.fpdiv_res_o(15, 0)),
      (outFmt === VSew.e32) -> Cat(Fill(32, 1.U), fdiv.io.fpdiv_res_o(31, 0)),
      (outFmt === VSew.e64) -> fdiv.io.fpdiv_res_o
    )
  )
  private val fflagsData = fdiv.io.fflags_o
  out.ex(0).valid := fdiv.io.finish_valid_o
  out.ex(0).bits.data.fflags.get := fflagsData
  out.ex(0).bits.data.fp.get := resultData
  fdiv.io.wakeupSuccess := Mux(fdiv.io.outValidAhead3Cycle, in.busyTableEmpty.get, true.B)
  outFuWakeUp.wen := fdiv.io.outValidAhead3Cycle
}