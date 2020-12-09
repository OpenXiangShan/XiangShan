package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{CertainLatency, FuConfig, FunctionUnit}
import xiangshan.backend.fu.FunctionUnit._

class F32toF64 extends FPUPipelineModule {

  override def latency: Int = FunctionUnit.s2dCfg.latency.latencyVal.get

  val a = io.in.bits.src(0)
  val f32 = Float32(a)

  val classify = Module(new Classify(Float32.expWidth, Float32.mantWidth))
  classify.io.in := a

  val isNaN = classify.io.isNaN
  val isSNaN = classify.io.isSNaN
  val isSubnormal = classify.io.isSubnormal
  val isSubnormalOrZero = classify.io.isSubnormalOrZero
  val invalid = isSNaN
  val isInfOrNaN =  classify.io.isInfOrNaN
  val isInf = classify.io.isInf


  val f32Mant = f32.mant  // not include hidden bit here
  val f32MantLez = PriorityEncoder(f32Mant.asBools().reverse)

  val exp = Mux(isSubnormalOrZero,
    0.U(Float64.expWidth.W),
    Mux(isInfOrNaN,
      Cat("b111".U(3.W), f32.exp),
      Cat("b0111".U(4.W) + f32.exp.head(1), f32.exp.tail(1))
    )
  )

  val s1_isNaN = S1Reg(isNaN)
  val s1_isSNaN = S1Reg(isSNaN)
  val s1_isSubnormal = S1Reg(isSubnormal)
  val s1_mantLez = S1Reg(f32MantLez)
  val s1_mant = S1Reg(f32Mant)
  val s1_exp = S1Reg(exp)
  val s1_sign = S1Reg(f32.sign)


  // MantNorm: 1.xx...x * 2^(-127 - lez)
  val f32MantFromDenorm = Wire(UInt(Float32.mantWidth.W))
  f32MantFromDenorm := Cat(s1_mant.tail(1) << s1_mantLez, 0.U(1.W))

  val f64ExpFromDenorm = Wire(UInt(Float64.expWidth.W))  // -127 - lez + 1023 = 0x380 - lez
  f64ExpFromDenorm := "h380".U - s1_mantLez

  val commonResult = Cat(
    s1_sign,
    Mux(s1_isSubnormal, f64ExpFromDenorm, s1_exp),
    Mux(s1_isSubnormal, f32MantFromDenorm, s1_mant),
    0.U((Float64.mantWidth-Float32.mantWidth).W)
  )
  val result = Mux(s1_isNaN, Float64.defaultNaN, commonResult)

  io.out.bits.data := S2Reg(result)
  fflags.invalid := S2Reg(s1_isSNaN)
  fflags.overflow := false.B
  fflags.underflow := false.B
  fflags.infinite := false.B
  fflags.inexact := false.B
}
