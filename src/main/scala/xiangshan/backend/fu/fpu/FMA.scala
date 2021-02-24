package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util.RegEnable
import freechips.rocketchip.tile.FType
import hardfloat.{MulAddRecFN_pipeline_stage1, MulAddRecFN_pipeline_stage2, MulAddRecFN_pipeline_stage3, MulAddRecFN_pipeline_stage4, RoundAnyRawFNToRecFN}
import xiangshan.backend.fu.FunctionUnit

class FMADataModule(latency: Int) extends FPUDataModule {

  val regEnables = IO(Input(Vec(latency, Bool())))
  val typeTagOut = IO(Input(UInt(2.W)))

  val fpCtrl = io.in.fpCtrl
  val typeTagIn = fpCtrl.typeTagIn

  val src1 = unbox(io.in.src(0), typeTagIn, None)
  val src2 = unbox(io.in.src(1), typeTagIn, None)
  val src3 = unbox(io.in.src(2), typeTagIn, None)
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
  mul.io.reg_en := regEnables(0)
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

  stage2.io.fromStage1.bits <> RegEnable(stage1.io.toStage2.bits, regEnables(0))
  stage3.io.fromStage2.bits <> RegEnable(stage2.io.toStage3.bits, regEnables(1))
  stage4.io.fromStage3.bits <> RegEnable(stage3.io.toStage4.bits, regEnables(2))
  val stage4toStage5 = RegEnable(stage4.io.toStage5.bits, regEnables(3))

  val rounders = Seq(FType.S, FType.D).map(t => {
    val rounder = Module(new RoundAnyRawFNToRecFN(FType.D.exp, FType.D.sig+2, t.exp, t.sig, 0))
    rounder.io.invalidExc   := stage4toStage5.invalidExc
    rounder.io.infiniteExc  := false.B
    rounder.io.in           := stage4toStage5.rawOut
    rounder.io.roundingMode := stage4toStage5.roundingMode
    rounder.io.detectTininess := stage4toStage5.detectTininess
    rounder
  })

  val singleOut = typeTagOut === S
  io.out.data := Mux(singleOut,
    sanitizeNaN(rounders(0).io.out, FType.S),
    sanitizeNaN(rounders(1).io.out, FType.D)
  )
  fflags := Mux(singleOut,
    rounders(0).io.exceptionFlags,
    rounders(1).io.exceptionFlags
  )
}

class FMA extends FPUPipelineModule {
  override def latency: Int = FunctionUnit.fmacCfg.latency.latencyVal.get

  override val dataModule = Module(new FMADataModule(latency))
  connectDataModule
  dataModule.regEnables <> VecInit((1 to latency) map (i => regEnable(i)))
  dataModule.typeTagOut := io.out.bits.uop.ctrl.fpu.typeTagOut
}
