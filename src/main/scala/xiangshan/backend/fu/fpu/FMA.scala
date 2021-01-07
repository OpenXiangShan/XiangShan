package xiangshan.backend.fu.fpu

import chisel3._
import freechips.rocketchip.tile.FType
import hardfloat.{MulAddRecFN_pipeline_stage1, MulAddRecFN_pipeline_stage2, MulAddRecFN_pipeline_stage3, MulAddRecFN_pipeline_stage4, RoundAnyRawFNToRecFN}
import xiangshan.backend.fu.FunctionUnit

class FMA extends FPUPipelineModule {
  override def latency: Int = FunctionUnit.fmacCfg.latency.latencyVal.get

  val fpCtrl = io.in.bits.uop.ctrl.fpu
  val typeTagIn = fpCtrl.typeTagIn

  val src1 = unbox(io.in.bits.src(0), typeTagIn, None)
  val src2 = unbox(io.in.bits.src(1), typeTagIn, None)
  val src3 = unbox(io.in.bits.src(2), typeTagIn, None)
  val (in1, in2, in3) = (
    WireInit(src1), WireInit(src2), WireInit(Mux(fpCtrl.isAddSub, src2, src3))
  )
  val one = 1.U << (FType.D.sig + FType.D.exp - 1)
  val zero = (src1 ^ src2) & (1.U << (FType.D.sig + FType.D.exp))
  when(fpCtrl.isAddSub){ in2 := one }
  when(!(fpCtrl.isAddSub || fpCtrl.ren3)){ in3 := zero }

  val stage1 = Module(new MulAddRecFN_pipeline_stage1(maxExpWidth, maxSigWidth))
  val stage2 = Module(new MulAddRecFN_pipeline_stage2(maxExpWidth, maxSigWidth))
  val stage3 = Module(new MulAddRecFN_pipeline_stage3(maxExpWidth, maxSigWidth))
  val stage4 = Module(new MulAddRecFN_pipeline_stage4(maxExpWidth, maxSigWidth))
  val mul = Module(new hardfloat.ArrayMultiplier(
    maxSigWidth+1,
    regDepth = 0,
    realArraryMult = true,
    hasReg = true
  ))
  mul.io.a := stage1.io.mulAddA
  mul.io.b := stage1.io.mulAddB
  mul.io.reg_en := regEnable(1)
  stage2.io.mulSum := mul.io.sum
  stage2.io.mulCarry := mul.io.carry

  stage1.io.in.valid := DontCare
  stage1.io.toStage2.ready := DontCare
  stage2.io.fromStage1.valid := DontCare
  stage2.io.toStage3.ready := DontCare
  stage3.io.fromStage2.valid := DontCare
  stage3.io.toStage4.ready := DontCare
  stage4.io.fromStage3.valid := DontCare
  stage4.io.toStage5.ready := DontCare

  stage1.io.in.bits.a := in1
  stage1.io.in.bits.b := in2
  stage1.io.in.bits.c := in3
  stage1.io.in.bits.op := fpCtrl.fmaCmd
  stage1.io.in.bits.roundingMode := rm
  stage1.io.in.bits.detectTininess := hardfloat.consts.tininess_afterRounding

  stage2.io.fromStage1.bits <> S1Reg(stage1.io.toStage2.bits)
  stage3.io.fromStage2.bits <> S2Reg(stage2.io.toStage3.bits)
  stage4.io.fromStage3.bits <> S3Reg(stage3.io.toStage4.bits)
  val stage4toStage5 = S4Reg(stage4.io.toStage5.bits)

  val rounders = Seq(FType.S, FType.D).map(t => {
    val rounder = Module(new RoundAnyRawFNToRecFN(FType.D.exp, FType.D.sig+2, t.exp, t.sig, 0))
    rounder.io.invalidExc   := stage4toStage5.invalidExc
    rounder.io.infiniteExc  := false.B
    rounder.io.in           := stage4toStage5.rawOut
    rounder.io.roundingMode := stage4toStage5.roundingMode
    rounder.io.detectTininess := stage4toStage5.detectTininess
    rounder
  })

  val singleOut = io.out.bits.uop.ctrl.fpu.typeTagOut === S
  io.out.bits.data := Mux(singleOut,
    sanitizeNaN(rounders(0).io.out, FType.S),
    sanitizeNaN(rounders(1).io.out, FType.D)
  )
  fflags := Mux(singleOut,
    rounders(0).io.exceptionFlags,
    rounders(1).io.exceptionFlags
  )
}
