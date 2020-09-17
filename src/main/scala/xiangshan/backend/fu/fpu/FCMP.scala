package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._

class FCMP extends FPUSubModule with HasPipelineReg{
  def latency = 2

  val isDouble = io.in.bits.isDouble
  val src = Seq(io.in.bits.a, io.in.bits.b).map(x => Mux(isDouble, x, extF32ToF64(x)))
  val sign = src.map(_(63))
  val aSign = sign(0)

  val subRes = src(0).toSInt - src(1).toSInt

  val classify = Array.fill(2)(Module(new Classify(Float64.expWidth, Float64.mantWidth)).io)
  classify.zip(src).foreach({case (c, s) => c.in := s})

  val srcIsNaN = classify.map(_.isNaN)
  val srcIsSNaN = classify.map(_.isSNaN)

  val isDoubleReg = S1Reg(isDouble)
  val opReg = S1Reg(io.in.bits.op)
  val srcReg = Seq(io.in.bits.a, io.in.bits.b).map(S1Reg)
  val (aSignReg, bSignReg) = (S1Reg(sign(0)), S1Reg(sign(1)))

  val hasNaNReg = S1Reg(srcIsNaN(0) || srcIsNaN(1))
  val bothNaNReg = S1Reg(srcIsNaN(0) && srcIsNaN(1))
  val hasSNaNReg = S1Reg(srcIsSNaN(0) || srcIsSNaN(1))
  val aIsNaNReg = S1Reg(srcIsNaN(0))
  val bothZeroReg = S1Reg(src(0).tail(1)===0.U && src(1).tail(1)===0.U)

  val uintEqReg = S1Reg(subRes===0.S)
  val uintLessReg = S1Reg(aSign ^ (subRes < 0.S))


  val invalid = Mux(opReg(2) || !opReg(1), hasSNaNReg, hasNaNReg)

  val le,lt,eq = Wire(Bool())
  eq := uintEqReg || bothZeroReg
  le := Mux(aSignReg =/= bSignReg, aSignReg || bothZeroReg, uintEqReg || uintLessReg)
  lt := Mux(aSignReg =/= bSignReg, aSignReg && !bothZeroReg, !uintEqReg && uintLessReg)
  val fcmpResult = Mux(hasNaNReg,
    false.B,
    Mux(opReg(2), eq, Mux(opReg(0), lt, le))
  )

  val sel_a = lt || (eq && aSignReg)
  val defaultNaN = Mux(isDoubleReg, Float64.defaultNaN, Float32.defaultNaN)
  val min = Mux(bothNaNReg, defaultNaN, Mux(sel_a && !aIsNaNReg, srcReg(0), srcReg(1)))
  val max = Mux(bothNaNReg, defaultNaN, Mux(!sel_a && !aIsNaNReg, srcReg(0), srcReg(1)))

  io.out.bits.fflags.inexact := false.B
  io.out.bits.fflags.underflow := false.B
  io.out.bits.fflags.overflow := false.B
  io.out.bits.fflags.infinite := false.B
  io.out.bits.fflags.invalid := S2Reg(invalid)
  io.out.bits.result := S2Reg(Mux(opReg===0.U, min, Mux(opReg===1.U, max, fcmpResult)))
}