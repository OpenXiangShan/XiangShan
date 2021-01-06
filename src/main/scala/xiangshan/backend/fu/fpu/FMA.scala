package xiangshan.backend.fu.fpu

import chisel3._
import freechips.rocketchip.tile.FType
import hardfloat.MulAddRecFN_pipeline
import xiangshan.backend.fu.FunctionUnit

class FMA extends FPUPipelineModule {
  override def latency: Int = FunctionUnit.fmacCfg.latency.latencyVal.get

  val sfma = Module(new MulAddRecFN_pipeline(FType.S.exp, FType.S.sig))
  val dfma = Module(new MulAddRecFN_pipeline(FType.D.exp, FType.D.sig))

  val fpCtrl = io.in.bits.uop.ctrl.fpu
  val typeTagIn = fpCtrl.typeTagIn

  val singleIn = typeTagIn === S
  sfma.io.in.valid := io.in.valid && singleIn
  dfma.io.in.valid := io.in.valid && !singleIn
  for((t, fma) <- Seq(FType.S, FType.D).zip(Seq(sfma, dfma))){
    val src1 = unbox(io.in.bits.src(0), typeTagIn, Some(t))
    val src2 = unbox(io.in.bits.src(1), typeTagIn, Some(t))
    val src3 = unbox(io.in.bits.src(2), typeTagIn, Some(t))
    val (in1, in2, in3) = (
      WireInit(src1), WireInit(src2), WireInit(Mux(fpCtrl.isAddSub, src2, src3))
    )
    val one = 1.U << (t.sig + t.exp - 1)
    val zero = (src1 ^ src2) & (1.U << (t.sig + t.exp))
    when(fpCtrl.isAddSub){ in2 := one }
    when(!(fpCtrl.isAddSub || fpCtrl.ren3)){ in3 := zero }
    fma.io.in.bits.a := in1
    fma.io.in.bits.b := in2
    fma.io.in.bits.c := in3
    fma.io.in.bits.op := fpCtrl.fmaCmd
    fma.io.in.bits.roundingMode := rm
    fma.io.in.bits.detectTininess := hardfloat.consts.tininess_afterRounding
    fma.io.out.ready := io.out.ready
  }
  val singleOut = io.out.bits.uop.ctrl.fpu.typeTagOut === S
  io.out.bits.data := Mux(singleOut,
    sanitizeNaN(sfma.io.out.bits.out, FType.S),
    sanitizeNaN(dfma.io.out.bits.out, FType.D)
  )
  fflags := Mux(singleOut,
    sfma.io.out.bits.exceptionFlags,
    dfma.io.out.bits.exceptionFlags
  )
}
