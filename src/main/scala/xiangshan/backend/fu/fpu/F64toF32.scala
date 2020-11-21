package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{CertainLatency, FuConfig, FunctionUnit}
import xiangshan.backend.fu.fpu.util.ShiftRightJam

class F64toF32 extends FPUPipelineModule {

  override def latency = FunctionUnit.d2sCfg.latency.latencyVal.get

  def SEXP_WIDTH = Float64.expWidth + 2

  val a = io.in.bits.src(0)

  val classify = Module(new Classify(Float64.expWidth, Float64.mantWidth))
  classify.io.in := a

  val isNaN = classify.io.isNaN
  val isSNaN = classify.io.isSNaN
  val isInf = classify.io.isInf

  val f64 = Float64(a)
  val f64sign = f64.sign
  val f64exp = Wire(SInt(SEXP_WIDTH.W))
  f64exp := f64.exp.toSInt
  val f64mant = f64.mantExt

  val f32exp = f64exp - (Float64.expBiasInt - Float32.expBiasInt).S

  val shiftAmt = 1.S - f32exp
  val needDenorm = shiftAmt > 0.S

  val mantShifted = ShiftRightJam(f64mant,
    Mux(needDenorm, shiftAmt.asUInt(), 0.U),
    Float32.mantWidth+4
  )

  val s1_mantShifted = S1Reg(mantShifted)
  val s1_shiftAmt = S1Reg(shiftAmt)
  val s1_sign = S1Reg(f64sign)
  val s1_exp = S1Reg(f32exp)
  val s1_rm = S1Reg(rm)
  val s1_isNaN = S1Reg(isNaN)
  val s1_isInf = S1Reg(isInf)
  val s1_isSNaN = S1Reg(isSNaN)


  val rounding = Module(new RoundF64AndF32WithExceptions(expInHasBias = true))
  rounding.io.isDouble := false.B
  rounding.io.denormShiftAmt := s1_shiftAmt
  rounding.io.sign := s1_sign
  rounding.io.expNorm := s1_exp
  rounding.io.mantWithGRS := s1_mantShifted
  rounding.io.rm := s1_rm
  rounding.io.specialCaseHappen := s1_isNaN || s1_isInf

  val inexact = rounding.io.inexact
  val underflow = rounding.io.underflow
  val overflow = rounding.io.overflow
  val ovSetInf = rounding.io.ovSetInf
  val expRounded = rounding.io.expRounded
  val mantRounded = rounding.io.mantRounded
  val result = Mux(s1_isNaN,
    Float32.defaultNaN,
    Mux(overflow || s1_isInf,
      Cat(s1_sign, Mux(ovSetInf || s1_isInf, Float32.posInf, Float32.maxNorm).tail(1)),
      Cat(s1_sign, expRounded(Float32.expWidth-1, 0), mantRounded(Float32.mantWidth-1, 0))
    )
  )

  io.out.bits.data := S2Reg(result)
  fflags.invalid := S2Reg(s1_isSNaN)
  fflags.overflow := S2Reg(overflow)
  fflags.underflow := S2Reg(underflow)
  fflags.infinite := false.B
  fflags.inexact := S2Reg(inexact)
}

