package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{CertainLatency, FuConfig, FunctionUnit}
import xiangshan.backend.fu.fpu.util.{ORTree, ShiftRightJam}

//def f2w:UInt    = FpuOp("011", "000")
//def f2wu:UInt   = FpuOp("011", "001")
//def f2l:UInt    = FpuOp("011", "010")
//def f2lu:UInt   = FpuOp("011", "011")

class FloatToInt extends FPUPipelineModule {

  override def latency = FunctionUnit.f2iCfg.latency.latencyVal.get

  def SEXP_WIDTH = Float64.expWidth + 2

  /** Stage 1: Shift Operand
    */

  val a = Mux(isDouble, io.in.bits.src(0), extF32ToF64(io.in.bits.src(0)))
  val f64 = Float64(a)

  val cls = Module(new Classify(Float64.expWidth, Float64.mantWidth))
  cls.io.in := a
  val isNaN = cls.io.isNaN

  val sign = f64.sign
  val exp = Wire(SInt(SEXP_WIDTH.W))
  exp := f64.exp.toSInt
  val mant = f64.mantExt

  val leftShiftAmt = exp - (Float64.expBiasInt + Float64.mantWidth).S
  val rightShiftAmt = -leftShiftAmt.asUInt()
  val needRightShift = leftShiftAmt.head(1).asBool() // exp - 52 < 0
  val expOv = leftShiftAmt > Mux(op(1), 11.S, (-21).S) // exp > 63 / exp > 31

  val uintUnrounded = Wire(UInt((64+3).W)) // 64 + g r s
  uintUnrounded := Mux(needRightShift,
    ShiftRightJam(Cat(mant, 0.U(3.W)), rightShiftAmt, Float64.mantWidth+4),
    Cat((mant << leftShiftAmt(3, 0))(63, 0), 0.U(3.W))
  )

  val s1_uint = S1Reg(uintUnrounded)
  val s1_sign = S1Reg(sign)
  val s1_rm = S1Reg(rm)
  val s1_op = S1Reg(op)
  val s1_isNaN = S1Reg(isNaN)
  val s1_expOv = S1Reg(expOv)


  /** Stage 2: Rounding
    */

  val rounding = Module(new RoundingUnit(64))

  rounding.io.in.rm := s1_rm
  rounding.io.in.sign := s1_sign
  rounding.io.in.mant := s1_uint.head(64)
  rounding.io.in.guard := s1_uint.tail(64).head(1)
  rounding.io.in.round := s1_uint.tail(65).head(1)
  rounding.io.in.sticky := s1_uint.tail(66).head(1)

  val uint = rounding.io.out.mantRounded
  val int = Mux(s1_sign, -uint, uint)

  val commonResult = Mux(s1_op(1), int, int(31, 0))

  val orHi = ORTree(uint.head(32))
  val orLo = ORTree(uint.tail(32))

  val diffSign = (orHi | orLo) && Mux(s1_op(0),
    s1_sign,
    Mux(s1_op(1),
      int(63),
      int(31)
    ) ^ s1_sign
  )

  val max64 = Cat(s1_op(0), Fill(63, 1.U(1.W)))
  val min64 = Cat(!s1_op(0), 0.U(63.W))

  val specialResult = Mux(s1_isNaN || !s1_sign,
    Mux(s1_op(1), max64, max64.head(32)),
    Mux(s1_op(1), min64, min64.head(32))
  )

  val invalid = s1_isNaN || s1_expOv || diffSign || (!s1_op(1) && orHi)

  val s2_invalid = S2Reg(invalid)
  val s2_result = S2Reg(Mux(invalid, specialResult, commonResult))
  val s2_inexact =S2Reg(!invalid && rounding.io.out.inexact)

  /** Assign Outputs
    */

  io.out.bits.data := s2_result
  fflags.invalid := s2_invalid
  fflags.overflow := false.B
  fflags.underflow := false.B
  fflags.infinite := false.B
  fflags.inexact := s2_inexact
}