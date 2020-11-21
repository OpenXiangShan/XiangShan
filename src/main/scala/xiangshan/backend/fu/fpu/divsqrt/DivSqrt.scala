package xiangshan.backend.fu.fpu.divsqrt

import xiangshan.backend.fu.fpu._
import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{FuConfig, FunctionUnit, UncertainLatency}
import xiangshan.backend.fu.fpu.util.{FPUDebug, ORTree, ShiftRightJam}

class DivSqrt extends FPUSubModule {

  def SEXP_WIDTH: Int = Float64.expWidth + 2
  def D_MANT_WIDTH: Int = Float64.mantWidth + 1
  def S_MANT_WIDTH: Int = Float32.mantWidth+1


  val s_idle :: s_norm :: s_start :: s_compute :: s_round:: s_finish :: Nil = Enum(6)
  val state = RegInit(s_idle)


  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  val kill = state=/=s_idle && uopReg.roqIdx.needFlush(io.redirectIn)
  val rmReg = RegEnable(rm, io.in.fire())
  val isDiv = !op(0)
  val isDivReg = RegEnable(isDiv, io.in.fire())
  val isDoubleReg = RegEnable(isDouble, io.in.fire())

  val (a, b) = (
    Mux(isDouble, io.in.bits.src(0), extF32ToF64(io.in.bits.src(0))),
    Mux(isDouble, io.in.bits.src(1), extF32ToF64(io.in.bits.src(1)))
  )


  /**  Detect special cases
    */
  val classify_a = Module(new Classify(Float64.expWidth, Float64.mantWidth))
  classify_a.io.in := a
  val aIsSubnormalOrZero = classify_a.io.isSubnormalOrZero

  val classify_b = Module(new Classify(Float64.expWidth, Float64.mantWidth))
  classify_b.io.in := b
  val bIsSubnormalOrZero = classify_b.io.isSubnormalOrZero


  def decode(x: UInt, expIsZero: Bool) = {
    val f64 = Float64(x)
    val exp = Cat(0.U(1.W), f64.exp) - Float64.expBias
    val mantExt = Cat(!expIsZero, f64.mant)
    (f64.sign, exp.asSInt(), mantExt)
  }

  val (aSign, aExp, aMant) = decode(a, aIsSubnormalOrZero)
  val (bSign, bExp, bMant) = decode(b, bIsSubnormalOrZero)

  val resSign = Mux(isDiv, aSign ^ bSign, aSign)
  val resSignReg = RegEnable(resSign, io.in.fire())
  val aExpReg = Reg(SInt(SEXP_WIDTH.W))
  val aMantReg = Reg(UInt(D_MANT_WIDTH.W))
  val aIsOddExp = aExpReg(0)

  val bMantReg = Reg(UInt(D_MANT_WIDTH.W))
  val bExpReg = Reg(SInt(SEXP_WIDTH.W))

  val aIsNaN = classify_a.io.isNaN
  val aIsSNaN = classify_a.io.isSNaN
  val aIsInf = classify_a.io.isInf
  val aIsPosInf = classify_a.io.isPosInf
  val aIsInfOrNaN = classify_a.io.isInfOrNaN
  val aIsSubnormal = classify_a.io.isSubnormal
  val aIsSubnormalReg = RegEnable(aIsSubnormal, io.in.fire())
  val aIsZero = classify_a.io.isZero

  val sel_NaN_OH = UIntToOH(2.U, 3)
  val sel_Zero_OH = UIntToOH(1.U, 3)
  val sel_Inf_OH = UIntToOH(0.U, 3)

  val sqrtSepcialResSel = MuxCase(sel_NaN_OH, Seq(
    aIsZero -> sel_Zero_OH,
    aIsPosInf -> sel_Inf_OH
  ))


  val sqrtInvalid = ((aSign && !aIsNaN && !aIsZero) || aIsSNaN) && !isDiv
  val sqrtSpecial = (aSign || aIsInfOrNaN || aIsZero) && !isDiv
  val sqrtInvalidReg = RegEnable(sqrtInvalid, io.in.fire())


  val bIsZero = classify_b.io.isZero
  val bIsNaN = classify_b.io.isNaN
  val bIsSNaN = classify_b.io.isSNaN
  val bIsSubnormal = classify_b.io.isSubnormal && isDiv
  val bIsSubnormalReg = RegEnable(bIsSubnormal, io.in.fire())
  val bIsInf = classify_b.io.isInf

  val hasNaN = aIsNaN || bIsNaN
  val bothZero = aIsZero && bIsZero
  val bothInf = aIsInf && bIsInf
  val divInvalid = bothZero || aIsSNaN || bIsSNaN || bothInf
  val divInf = !divInvalid && !aIsNaN && bIsZero && !aIsInf
  val divSepcial = (aIsZero || bIsZero || hasNaN || bIsInf || aIsInf) && isDiv
  val divZeroReg = RegEnable(bIsZero, io.in.fire())
  val divInvalidReg = RegEnable(divInvalid, io.in.fire())
  val divInfReg = RegEnable(divInf, io.in.fire())

  val divSepcialResSel = PriorityMux(Seq(
    (divInvalid || hasNaN) -> sel_NaN_OH,
    bIsZero -> sel_Inf_OH,
    (aIsZero || bIsInf) -> sel_Zero_OH,
    aIsInf -> sel_Inf_OH
  ))

  val specialCaseHappen = sqrtSpecial || divSepcial
  val specialCaseHappenReg = RegEnable(specialCaseHappen, io.in.fire())
  val specialResSel = Mux(sqrtSpecial, sqrtSepcialResSel, divSepcialResSel)
  val sel_NaN :: sel_Zero :: sel_Inf :: Nil = specialResSel.asBools().reverse
  val specialResult = RegEnable(
    Mux(sel_NaN,
      Mux(isDouble,
        Float64.defaultNaN,
        Float32.defaultNaN
      ),
      Mux(sel_Zero,
        Mux(isDouble,
          Cat(resSign, 0.U((Float64.getWidth-1).W)),
          Cat(resSign, 0.U((Float32.getWidth-1).W))
        ),
        Mux(isDouble,
          Cat(resSign, Float64.posInf.tail(1)),
          Cat(resSign, Float32.posInf.tail(1))
        )
      )
    ),
    io.in.fire()
  )


  // used in 's_norm' to normalize a subnormal number to normal
  val aMantLez = PriorityEncoder(aMantReg(51, 0).asBools().reverse)
  val bMantLez = PriorityEncoder(bMantReg(51, 0).asBools().reverse)

  // 53 + 2 + 2 = 57 bits are needed, but 57 % log2(4) != 0, use 58 bits instead
  val mantDivSqrt = Module(new MantDivSqrt(D_MANT_WIDTH+2+2+1))
  mantDivSqrt.io.out.ready := true.B
  mantDivSqrt.io.in.valid := state === s_start
  mantDivSqrt.io.in.bits.a := Mux(isDivReg || aIsOddExp, Cat(aMantReg, 0.U(5.W)), Cat(0.U(1.W), aMantReg, 0.U(4.W)))
  mantDivSqrt.io.in.bits.b := Cat(bMantReg, 0.U(5.W))
  mantDivSqrt.io.in.bits.isDiv := isDivReg

  /** Output format:
    *
    * 57  56  55              4   3  2  1  0
    *  0  x.  x   x   x  ...  x | x  x  x  x
    *
    */
  val mantDivSqrtResult = mantDivSqrt.io.out.bits.quotient
  val needNormalize = !mantDivSqrtResult(56)
  val mantNorm = Mux(needNormalize, mantDivSqrtResult<<1, mantDivSqrtResult)(56, 0)

  val expNorm = (aExpReg.asUInt() - (Mux(needNormalize, 2.U, 1.U) - isDivReg)).asSInt()

  val denormalizeShift = Mux(
    isDoubleReg,
    (-Float64.expBiasInt+1).S,
    (-Float32.expBiasInt+1).S
  ) - expNorm
  val denormShiftReg = RegEnable(denormalizeShift, mantDivSqrt.io.out.fire())

  val mantShifted = ShiftRightJam(mantNorm,
    Mux(denormalizeShift.head(1).asBool(), 0.U, denormalizeShift.asUInt()),
    D_MANT_WIDTH+3
  )

  val mantPostNorm = Mux(isDoubleReg,
    mantShifted.head(D_MANT_WIDTH),
    mantShifted.head(S_MANT_WIDTH)
  )
  val g = Mux(isDoubleReg,
    mantShifted.tail(D_MANT_WIDTH).head(1),
    mantShifted.tail(S_MANT_WIDTH).head(1)
  ).asBool()
  val r = Mux(isDoubleReg,
    mantShifted.tail(D_MANT_WIDTH+1).head(1),
    mantShifted.tail(S_MANT_WIDTH+1).head(1)
  ).asBool()
  val s = !mantDivSqrt.io.out.bits.isZeroRem || ORTree(Mux(isDoubleReg,
    mantShifted.tail(D_MANT_WIDTH+2),
    mantShifted.tail(S_MANT_WIDTH+2)
  ))

  val gReg = RegNext(g)
  val rReg = RegNext(r) // false.B
  val sReg = RegNext(s)

  /** state === s_round
    *
    */

  val rounding = Module(new RoundF64AndF32WithExceptions)
  rounding.io.isDouble := isDoubleReg
  rounding.io.denormShiftAmt := denormShiftReg
  rounding.io.sign := resSignReg
  rounding.io.expNorm := aExpReg
  rounding.io.mantWithGRS := Cat(aMantReg, gReg, rReg, sReg)
  rounding.io.rm := rmReg
  rounding.io.specialCaseHappen := false.B

  val expRounded = rounding.io.expRounded
  val mantRounded = rounding.io.mantRounded
  val overflowReg = RegEnable(rounding.io.overflow, state===s_round)
  val underflowReg = RegEnable(rounding.io.underflow, state===s_round)
  val inexactReg = RegEnable(rounding.io.inexact, state===s_round)
  val ovSetInfReg = RegEnable(rounding.io.ovSetInf, state===s_round)

  switch(state){
    is(s_idle){
      when(io.in.fire()){
        when(sqrtSpecial ||  divSepcial){
          state := s_finish
        }.elsewhen(aIsSubnormal || bIsSubnormal){
          state := s_norm
        }.otherwise({
          state := s_start
        })
      }
    }
    is(s_norm){
      state := s_start
    }
    is(s_start){
      state := s_compute
    }
    is(s_compute){
      when(mantDivSqrt.io.out.fire()){
        state := s_round
      }
    }
    is(s_round){
      state := s_finish
    }
    is(s_finish){
      when(io.out.fire()){
        state := s_idle
      }
    }
  }
  when(kill){ state := s_idle }

  switch(state){
    is(s_idle){
      when(io.in.fire()){
        aExpReg := aExp
        aMantReg := aMant
        bExpReg := bExp
        bMantReg := bMant
      }
    }
    is(s_norm){
      when(aIsSubnormalReg){
        aExpReg := (Mux(isDoubleReg, aExpReg, (-Float32.expBiasInt).S(SEXP_WIDTH.W)).asUInt() - aMantLez).asSInt()
        aMantReg := (aMantReg << aMantLez) << 1 // use 'Cat' instead ?
      }
      when(bIsSubnormalReg){
        bExpReg := (Mux(isDoubleReg, bExpReg, (-Float32.expBiasInt).S(SEXP_WIDTH.W)).asUInt() - bMantLez).asSInt()
        bMantReg := (bMantReg << bMantLez) << 1 // use 'Cat' instead ?
      }
    }
    is(s_start){
      aExpReg := Mux(isDivReg, aExpReg - bExpReg, (aExpReg >> 1).asSInt() + 1.S)
    }
    is(s_compute){
      when(mantDivSqrt.io.out.fire()){
        aExpReg := expNorm
        aMantReg := mantPostNorm
      }
    }
    is(s_round){
      aExpReg := expRounded
      aMantReg := mantRounded
    }
  }


  val commonResult = Mux(isDoubleReg,
    Cat(resSignReg, aExpReg(Float64.expWidth-1, 0), aMantReg(Float64.mantWidth-1, 0)),
    Cat(resSignReg, aExpReg(Float32.expWidth-1, 0), aMantReg(Float32.mantWidth-1, 0))
  )

  io.in.ready := (state === s_idle) && io.out.ready
  io.out.valid := (state === s_finish) && !kill
  io.out.bits.data := Mux(specialCaseHappenReg,
    specialResult,
    Mux(overflowReg,
      Mux(isDoubleReg,
        Cat(resSignReg, Mux(ovSetInfReg, Float64.posInf.tail(1), Float64.maxNorm.tail(1))),
        Cat(resSignReg, Mux(ovSetInfReg, Float32.posInf.tail(1), Float32.maxNorm.tail(1)))
      ),
      commonResult
    )
  )
  io.out.bits.uop := uopReg

  fflags.invalid := Mux(isDivReg, divInvalidReg, sqrtInvalidReg)
  fflags.underflow := !specialCaseHappenReg && underflowReg
  fflags.overflow := !specialCaseHappenReg && overflowReg
  fflags.infinite := Mux(isDivReg, divInfReg, false.B)
  fflags.inexact := !specialCaseHappenReg && (inexactReg || overflowReg || underflowReg)

//  FPUDebug() {
//    //    printf(p"$cnt in:${Hexadecimal(io.in.bits.src0)} \n")
//    when(io.in.fire()) {
//      printf(p"[In.fire] " +
//        p"a:${Hexadecimal(io.in.bits.a)} aexp:${aExp.asSInt()} amant:${Hexadecimal(aMant)} " +
//        p"b:${Hexadecimal(io.in.bits.b)} bexp:${bExp.asSInt()} bmant:${Hexadecimal(bMant)}\n")
//    }
////    when(state === s_norm) {
////      printf(p"[norm] lz:$aMantLez\n")
////    }
//    when(state === s_compute){
////      when(sqrt.io.out.fire()){
////        printf(p"[compute] ")
////      }
//    }
//    when(state === s_start) {
//      printf(p"[start] sign:$resSignReg mant:${Hexadecimal(aMantReg)} exp:${aExpReg.asSInt()}\n")
//    }
//    when(state === s_round){
//      printf(p"[round] exp before round:${aExpReg} g:$gReg r:$rReg s:$sReg mant:${Hexadecimal(aMantReg)}\n" +
//        p"[round] mantRounded:${Hexadecimal(mantRounded)}\n")
//    }
//    when(io.out.valid) {
//      printf(p"[Out.valid] " +
//        p"invalid:$sqrtInvalidReg  result:${Hexadecimal(commonResult)}\n" +
//        p"output:${Hexadecimal(io.out.bits.result)} " +
//        p"exp:${aExpReg.asSInt()} \n")
//    }
//  }
}
