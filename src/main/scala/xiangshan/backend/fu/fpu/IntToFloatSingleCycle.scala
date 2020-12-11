package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{CertainLatency, FuConfig, FunctionUnit}
import xiangshan.backend.fu.fpu.util.ORTree

class IntToFloatSingleCycle extends FPUSubModule {

  val a = io.in.bits.src(0)
  val aNeg = (~a).asUInt()
  val aComp = aNeg + 1.U
  val aSign = Mux(op(0), false.B, Mux(op(1), a(63), a(31)))

  val leadingZerosComp = PriorityEncoder(Mux(op(1), aComp, aComp(31, 0)).asBools().reverse)
  val leadingZerosNeg = PriorityEncoder(Mux(op(1), aNeg, aNeg(31, 0)).asBools().reverse)
  val leadingZerosPos = PriorityEncoder(Mux(op(1), a, a(31,0)).asBools().reverse)

  val aVal = Mux(aSign, Mux(op(1), aComp, aComp(31, 0)), Mux(op(1), a, a(31, 0)))
  val leadingZeros = Mux(aSign, leadingZerosNeg, leadingZerosPos)

  // exp = xlen - 1 - leadingZeros + bias
  val expUnrounded = Mux(isDouble,
    (64 - 1 + Float64.expBiasInt).U - leadingZeros,
    (64 - 1 + Float32.expBiasInt).U - leadingZeros
  )

  val leadingZeroHasError = aSign && (leadingZerosComp=/=leadingZerosNeg)
  val rmReg = rm
  val opReg = op
  val isDoubleReg = isDouble
  val aIsZeroReg = a===0.U
  val aSignReg = aSign
  val aShifted = (aVal << leadingZeros)(63, 0)

  /** Stage 2: Rounding
    */
  val aShiftedFix = Mux(leadingZeroHasError, aShifted(63, 1), aShifted(62, 0))
  val mantD = aShiftedFix(62, 62-51)
  val mantS = aShiftedFix(62, 62-22)

  val g = Mux(isDoubleReg, aShiftedFix(62-52), aShiftedFix(62-23))
  val r = Mux(isDoubleReg, aShiftedFix(62-53), aShiftedFix(62-24))
  val s = Mux(isDoubleReg, ORTree(aShiftedFix(62-54, 0)), ORTree(aShiftedFix(62-25, 0)))

  val roudingUnit = Module(new RoundingUnit(Float64.mantWidth))
  roudingUnit.io.in.rm := rmReg
  roudingUnit.io.in.mant := Mux(isDoubleReg, mantD, mantS)
  roudingUnit.io.in.sign := aSignReg
  roudingUnit.io.in.guard := g
  roudingUnit.io.in.round := r
  roudingUnit.io.in.sticky := s

  val mantRounded = roudingUnit.io.out.mantRounded
  val expRounded = Mux(isDoubleReg,
    expUnrounded + roudingUnit.io.out.mantCout,
    expUnrounded + mantRounded(Float32.mantWidth)
  ) + leadingZeroHasError

  val resS = Cat(
    aSignReg,
    expRounded(Float32.expWidth-1, 0),
    mantRounded(Float32.mantWidth-1, 0)
  )
  val resD = Cat(aSignReg, expRounded, mantRounded)

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.data := Mux(aIsZeroReg, 0.U, Mux(isDoubleReg, resD, resS))
  fflags.inexact := roudingUnit.io.out.inexact
  fflags.underflow := false.B
  fflags.overflow := false.B
  fflags.infinite := false.B
  fflags.invalid := false.B

}
