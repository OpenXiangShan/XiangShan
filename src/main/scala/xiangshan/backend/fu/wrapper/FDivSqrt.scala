package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{XSError, StartStopCounter, XSPerfHistogram}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.fpu.FpNonPipedFuncUnit
import xiangshan.backend.rob.RobPtr
import yunsuan.VfpuType
import yunsuan.fpu.FloatDivider
import xiangshan.frontend.tracertl.{TraceDummyFpDivider, TraceRTLChoose}

class FDivSqrt(cfg: FuConfig)(implicit p: Parameters) extends FpNonPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VfpuType.dummy, "fdiv OpType not supported")

  // io alias
  private val is_sqrt_i = fuOpType(0)
  private val src0 = if (env.TraceRTLMode) inData.src(1) else inData.src(0)
  private val src1 = if (env.TraceRTLMode) inData.src(0) else inData.src(1)

  // modules
  private val fdiv = Module(new FloatDivider)
  private val dummyFdiv = Module(new TraceDummyFpDivider)

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

  val flush_i = thisRobIdx.needFlush(io.flush)
  if (env.TraceRTLMode && trtl.TraceDummyFixCycleDivSqrt) {
    fdiv.io <> DontCare

    dummyFdiv.io.start_valid_i  := io.in.valid
    dummyFdiv.io.finish_ready_i := io.out.ready & io.out.valid
    dummyFdiv.io.flush_i        := flush_i
    dummyFdiv.io.format_i       := fp_fmt
    dummyFdiv.io.is_sqrt_i      := is_sqrt_i
  } else {
    dummyFdiv.io <> DontCare

    fdiv.io.start_valid_i  := io.in.valid
    fdiv.io.finish_ready_i := io.out.ready & io.out.valid
    fdiv.io.flush_i        := flush_i
    fdiv.io.fp_format_i    := fp_fmt
    fdiv.io.opa_i          := src1
    fdiv.io.opb_i          := src0
    fdiv.io.is_sqrt_i      := is_sqrt_i
    fdiv.io.rm_i           := rm
    fdiv.io.fp_aIsFpCanonicalNAN := fp_aIsFpCanonicalNAN
    fdiv.io.fp_bIsFpCanonicalNAN := fp_bIsFpCanonicalNAN
  }

  private val resultData = TraceRTLChoose(Mux1H(
    Seq(
      (outCtrl.vpu.get.vsew === VSew.e16) -> Cat(Fill(48, 1.U), fdiv.io.fpdiv_res_o(15, 0)),
      (outCtrl.vpu.get.vsew === VSew.e32) -> Cat(Fill(32, 1.U), fdiv.io.fpdiv_res_o(31, 0)),
      (outCtrl.vpu.get.vsew === VSew.e64) -> fdiv.io.fpdiv_res_o
    )
  ), 0.U)
  if (env.TraceRTLMode && trtl.TraceDummyFixCycleDivSqrt) {
    io.in.ready := dummyFdiv.io.start_ready_o
    io.out.valid := dummyFdiv.io.finish_valid_o
    io.out.bits.res.fflags.get := 0.U
    io.out.bits.res.data := 0.U
  } else {
    val fflagsData = fdiv.io.fflags_o

    io.in.ready  := fdiv.io.start_ready_o
    io.out.valid := fdiv.io.finish_valid_o

    io.out.bits.res.fflags.get := fflagsData
    io.out.bits.res.data       := resultData
  }

  val simFormat0 = (fp_fmt === 0.U)
  val simFormat1 = (fp_fmt === 1.U)
  val simFormat2 = (fp_fmt === 2.U)
  val simFormat3 = (fp_fmt === 3.U)
  val exeFdivCycleCounter = StartStopCounter(io.in.fire && !is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFsqrtCycleCounter = StartStopCounter(io.in.fire && is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFdivFM0CycleCounter = StartStopCounter(io.in.fire && simFormat0 && !is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFdivFM1CycleCounter = StartStopCounter(io.in.fire && simFormat1 && !is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFdivFM2CycleCounter = StartStopCounter(io.in.fire && simFormat2 && !is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFdivFM3CycleCounter = StartStopCounter(io.in.fire && simFormat3 && !is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFsqrtFM0CycleCounter = StartStopCounter(io.in.fire && simFormat0 && is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFsqrtFM1CycleCounter = StartStopCounter(io.in.fire && simFormat1 && is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFsqrtFM2CycleCounter = StartStopCounter(io.in.fire && simFormat2 && is_sqrt_i, io.out.valid, 1, flush_i)
  val exeFsqrtFM3CycleCounter = StartStopCounter(io.in.fire && simFormat3 && is_sqrt_i, io.out.valid, 1, flush_i)

  XSPerfHistogram("fdivCycle", exeFdivCycleCounter, io.out.fire && (exeFdivCycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fsqrtCycle", exeFsqrtCycleCounter, io.out.fire && (exeFsqrtCycleCounter =/= 0.U), 0, 24, 1)

  XSPerfHistogram("fdivFM0Cycle", exeFdivFM0CycleCounter, io.out.fire && (exeFdivFM0CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fdivFM1Cycle", exeFdivFM1CycleCounter, io.out.fire && (exeFdivFM1CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fdivFM2Cycle", exeFdivFM2CycleCounter, io.out.fire && (exeFdivFM2CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fdivFM3Cycle", exeFdivFM3CycleCounter, io.out.fire && (exeFdivFM3CycleCounter =/= 0.U), 0, 24, 1)

  XSPerfHistogram("fsqrtFM0Cycle", exeFsqrtFM0CycleCounter, io.out.fire && (exeFsqrtFM0CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fsqrtFM1Cycle", exeFsqrtFM1CycleCounter, io.out.fire && (exeFsqrtFM1CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fsqrtFM2Cycle", exeFsqrtFM2CycleCounter, io.out.fire && (exeFsqrtFM2CycleCounter =/= 0.U), 0, 24, 1)
  XSPerfHistogram("fsqrtFM3Cycle", exeFsqrtFM3CycleCounter, io.out.fire && (exeFsqrtFM3CycleCounter =/= 0.U), 0, 24, 1)
}
