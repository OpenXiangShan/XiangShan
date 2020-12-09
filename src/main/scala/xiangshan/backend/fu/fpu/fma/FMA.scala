package xiangshan.backend.fu.fpu.fma

import chisel3._
import chisel3.util._
import xiangshan.FuType
import xiangshan.backend.fu.{CertainLatency, FuConfig, FunctionUnit}
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.fpu.util.{CSA3_2, FPUDebug, ORTree, ShiftLeftJam, ShiftRightJam}


class FMA extends FPUPipelineModule {

  override def latency = FunctionUnit.fmacCfg.latency.latencyVal.get

  def UseRealArraryMult = false

  def SEXP_WIDTH: Int = Float64.expWidth + 2
  def D_MANT_WIDTH: Int = Float64.mantWidth + 1
  def S_MANT_WIDTH: Int = Float32.mantWidth + 1
  def INITIAL_EXP_DIFF: Int = Float64.mantWidth + 4
  def ADD_WIDTH: Int = 3*D_MANT_WIDTH + 2

  /******************************************************************
    * Stage 1: Decode Operands
    *****************************************************************/

  val rs0 = io.in.bits.src(0)
  val rs1 = io.in.bits.src(1)
  val rs2 = io.in.bits.src(2)
  val zero = 0.U(Float64.getWidth.W)
  val one = Mux(isDouble,
    Cat(0.U(1.W), Float64.expBiasInt.U(Float64.expWidth.W), 0.U(Float64.mantWidth.W)),
    Cat(0.U(1.W), Float32.expBiasInt.U(Float32.expWidth.W), 0.U(Float32.mantWidth.W))
  )

  val a = {
    val x = Mux(op(2),
      rs2,
      Mux(op(1),
        zero,
        rs1
      )
    )
    val sign = Mux(isDouble, x.head(1), x.tail(32).head(1)).asBool() ^ op(0)
    Mux(isDouble,
      Cat(sign, x.tail(1)),
      Cat(sign, x.tail(32).tail(1))
    )
  }
  val b = rs0
  val c = Mux(op(2,1) === 0.U, one, rs1)

  val operands = Seq(a, b, c).map(x => Mux(isDouble, x, extF32ToF64(x)))
  val classify = Array.fill(3)(Module(new Classify(Float64.expWidth, Float64.mantWidth)).io)
  classify.zip(operands).foreach({case (cls, x) => cls.in := x})

  def decode(x: UInt, isSubnormal: Bool, isZero: Bool) = {
    val f64 = Float64(x)
    val exp = Mux(isSubnormal,
      Mux(isDouble, (-Float64.expBiasInt+1).S, (-Float32.expBiasInt+1).S),
      f64.exp.toSInt - Float64.expBias.toSInt
    )
    val mantExt = Mux(isZero, 0.U, Cat(!isSubnormal, f64.mant))
    (f64.sign, exp, mantExt)
  }

  val signs = Array.fill(3)(Wire(Bool()))
  val exps = Array.fill(3)(Wire(SInt(SEXP_WIDTH.W)))
  val mants = Array.fill(3)(Wire(UInt(D_MANT_WIDTH.W)))
  for(i <- 0 until 3){
    val (s, e, m) = decode(operands(i), classify(i).isSubnormal, classify(i).isZero)
    signs(i) := s
    exps(i) := e
    mants(i) := m
  }

  val aIsSubnormal = classify(0).isSubnormal
  val bIsSubnormal = classify(1).isSubnormal
  val cIsSubnormal = classify(2).isSubnormal
  val prodHasSubnormal = bIsSubnormal || cIsSubnormal

  val aSign = signs(0)
  val aExpRaw = exps(0)


  val prodIsZero = classify.drop(1).map(_.isZero).reduce(_||_)
  val aIsZero = classify.head.isZero

  val prodSign = signs(1) ^ signs(2) ^ (op(2,1)==="b11".U)
  val prodExpRaw = Mux(prodIsZero,
    Mux(isDouble,
      (-Float64.expBiasInt).S,
      (-Float32.expBiasInt).S),
    exps(1) + exps(2)
  )

  val zeroResultSign = Mux(op(2,1) === "b01".U,
    prodSign,
    (aSign & prodSign) | ((aSign | prodSign) & rm===RoudingMode.RDN)
  )

  val hasNaN = classify.map(_.isNaN).reduce(_||_)
  val hasSNaN = classify.map(_.isSNaN).reduce(_||_)

  val isInf = classify.map(_.isInf)
  val aIsInf = isInf(0)
  val prodHasInf = isInf.drop(1).reduce(_||_)
  val hasInf = isInf(0) || prodHasInf

  val addInfInvalid = (aIsInf & prodHasInf & (aSign ^ prodSign)) & !(aIsInf ^ prodHasInf)
  val zeroMulInf = prodIsZero && prodHasInf

  val infInvalid = addInfInvalid || zeroMulInf

  val invalid = hasSNaN || infInvalid
  val specialCaseHappen = hasNaN || hasInf
  val specialOutput = PriorityMux(Seq(
    (hasNaN || infInvalid) -> Mux(isDouble,
      Float64.defaultNaN,
      Float32.defaultNaN
    ),
    aIsInf -> Mux(isDouble,
      Cat(aSign, Float64.posInf.tail(1)),
      Cat(aSign, Float32.posInf.tail(1))
    ),
    prodHasInf -> Mux(isDouble,
      Cat(prodSign, Float64.posInf.tail(1)),
      Cat(prodSign, Float32.posInf.tail(1))
    )
  ))
  val prodExpAdj = prodExpRaw + INITIAL_EXP_DIFF.S
  val expDiff = prodExpAdj - aExpRaw

  val mult = Module(new ArrayMultiplier(D_MANT_WIDTH+1, 0, UseRealArraryMult))
  mult.io.a := mants(1)
  mult.io.b := mants(2)
  mult.io.reg_en := io.in.fire()

  val s1_isDouble = S1Reg(isDouble)
  val s1_rm = S1Reg(rm)
  val s1_zeroSign = S1Reg(zeroResultSign)
  val s1_specialCaseHappen = S1Reg(specialCaseHappen)
  val s1_specialOutput = S1Reg(specialOutput)
  val s1_aSign = S1Reg(aSign)
  val s1_aExpRaw = S1Reg(aExpRaw)
  val s1_aMant = S1Reg(mants(0))
  val s1_prodSign = S1Reg(prodSign)
  val s1_prodExpAdj = S1Reg(prodExpAdj)
  val s1_expDiff = S1Reg(expDiff)
  val s1_discardProdMant = S1Reg(prodIsZero || expDiff.head(1).asBool()) //expDiff < 0.S
  val s1_discardAMant = S1Reg(aIsZero || expDiff > (ADD_WIDTH+3).S)
  val s1_invalid = S1Reg(invalid)

//  FPUDebug(){
//    when(valids(1) && ready){
//      printf(p"[s1] prodExp+56:${s1_prodExpAdj} aExp:${s1_aExpRaw} diff:${s1_expDiff}\n")
//    }
//  }


  /******************************************************************
    * Stage 2: align A | compute product (B*C)
    *****************************************************************/

  val alignedAMant = Wire(UInt((ADD_WIDTH+4).W))
  alignedAMant := Cat(
    0.U(1.W), // sign bit
    ShiftRightJam(s1_aMant, Mux(s1_discardProdMant, 0.U, s1_expDiff.asUInt()), ADD_WIDTH+3)
  )
  val alignedAMantNeg = -alignedAMant
  val effSub = s1_prodSign ^ s1_aSign

  val mul_prod = mult.io.carry.tail(1) + mult.io.sum.tail(1)

  val s2_isDouble = S2Reg(s1_isDouble)
  val s2_rm = S2Reg(s1_rm)
  val s2_zeroSign = S2Reg(s1_zeroSign)
  val s2_specialCaseHappen = S2Reg(s1_specialCaseHappen)
  val s2_specialOutput = S2Reg(s1_specialOutput)
  val s2_aSign = S2Reg(s1_aSign)
  val s2_prodSign = S2Reg(s1_prodSign)
  val s2_expPreNorm = S2Reg(Mux(s1_discardAMant || !s1_discardProdMant, s1_prodExpAdj, s1_aExpRaw))
  val s2_invalid = S2Reg(s1_invalid)

  val s2_prod = S2Reg(mul_prod)
  val s2_aMantNeg = S2Reg(alignedAMantNeg)
  val s2_aMant = S2Reg(alignedAMant)
  val s2_effSub = S2Reg(effSub)


//  FPUDebug(){
//    when(valids(1) && ready){
//      printf(p"[s2] discardAMant:${s1_discardAMant} discardProd:${s1_discardProdMant} \n")
//    }
//  }

  /******************************************************************
    * Stage 3: A + Prod => adder result
    *****************************************************************/

  val prodMinusA = Cat(s2_prod, 0.U(3.W)) + s2_aMantNeg
  val prodMinusA_Sign = prodMinusA.head(1).asBool()
  val aMinusProd = -prodMinusA
  val prodAddA = Cat(s2_prod, 0.U(3.W)) + s2_aMant

  val lza = Module(new LZA(ADD_WIDTH+4))
  lza.io.a := s2_aMant
  lza.io.b := Cat(s2_prod, 0.U(3.W))

  val effSubLez = lza.io.out - 1.U
  val effAddLez = PriorityEncoder(prodAddA.tail(1).asBools().reverse)
  val res = Mux(s2_effSub,
    Mux(prodMinusA_Sign,
      aMinusProd,
      prodMinusA
    ),
    prodAddA
  )
  val resSign = Mux(s2_prodSign,
    Mux(s2_aSign,
      true.B, // -(b*c) - a
      !prodMinusA_Sign        // -(b*c) + a
    ),
    Mux(s2_aSign,
      prodMinusA_Sign, // b*c - a
      false.B         // b*c + a
    )
  )
  val mantPreNorm = res.tail(1)
  val normShift = Mux(s2_effSub, effSubLez, effAddLez)

  val roundingInc = MuxLookup(s2_rm, "b10".U(2.W), Seq(
    RoudingMode.RDN -> Mux(resSign, "b11".U, "b00".U),
    RoudingMode.RUP -> Mux(resSign, "b00".U, "b11".U),
    RoudingMode.RTZ -> "b00".U
  ))
  val ovSetInf = rm === RoudingMode.RNE ||
    rm === RoudingMode.RMM ||
    (rm === RoudingMode.RDN && resSign) ||
    (rm === RoudingMode.RUP && !resSign)

  val s3_ovSetInf = S3Reg(ovSetInf)
  val s3_roundingInc = S3Reg(roundingInc)
  val s3_isDouble = S3Reg(s2_isDouble)
  val s3_rm = S3Reg(s2_rm)
  val s3_zeroSign = S3Reg(s2_zeroSign)
  val s3_specialCaseHappen = S3Reg(s2_specialCaseHappen)
  val s3_specialOutput = S3Reg(s2_specialOutput)
  val s3_resSign = S3Reg(resSign)
  val s3_mantPreNorm = S3Reg(mantPreNorm)
  val s3_expPreNorm = S3Reg(s2_expPreNorm)
  val s3_normShift = S3Reg(normShift)
  val s3_invalid = S3Reg(s2_invalid)


  /******************************************************************
    * Stage 4: Normalize/Denormalize Shift
    *****************************************************************/

  val expPostNorm = s3_expPreNorm - s3_normShift.toSInt
  val denormShift = Mux(
    s3_isDouble,
    (-Float64.expBiasInt+1).S,
    (-Float32.expBiasInt+1).S
  ) - expPostNorm

  val leftShift = s3_normShift.toSInt - Mux(denormShift.head(1).asBool(), 0.S, denormShift)
  val rightShift = denormShift - s3_normShift.toSInt

  val mantShifted = Mux(rightShift.head(1).asBool(), // < 0
    ShiftLeftJam(s3_mantPreNorm, leftShift.asUInt(), D_MANT_WIDTH+3),
    ShiftRightJam(s3_mantPreNorm, rightShift.asUInt(), D_MANT_WIDTH+3)
  )
  val s4_isDouble = S4Reg(s3_isDouble)
  val s4_rm = S4Reg(s3_rm)
  val s4_roundingInc = S4Reg(s3_roundingInc)
  val s4_zeroSign = S4Reg(s3_zeroSign)
  val s4_specialCaseHappen = S4Reg(s3_specialCaseHappen)
  val s4_specialOutput = S4Reg(s3_specialOutput)
  val s4_ovSetInf = S4Reg(s3_ovSetInf)
  val s4_resSign = S4Reg(s3_resSign)
  val s4_mantShifted = S4Reg(mantShifted)
  val s4_denormShift = S4Reg(denormShift)
  val s4_expPostNorm = S4Reg(expPostNorm)
  val s4_invalid = S4Reg(s3_invalid)

//  FPUDebug(){
//    when(valids(3) && ready){
//      printf(p"[s4] expPreNorm:${s3_expPreNorm} normShift:${s3_normShift} expPostNorm:${expPostNorm} " +
//        p"denormShift:${denormShift}" +
//        p"" +
//        p" \n")
//    }
//  }

  /******************************************************************
    * Stage 5: Rounding
    *****************************************************************/

  val mantUnrounded = Mux(s4_isDouble,
    s4_mantShifted.head(D_MANT_WIDTH),
    s4_mantShifted.head(S_MANT_WIDTH)
  )
  val g = Mux(s4_isDouble,
    s4_mantShifted.tail(D_MANT_WIDTH).head(1),
    s4_mantShifted.tail(S_MANT_WIDTH).head(1)
  ).asBool()
  val r = Mux(s4_isDouble,
    s4_mantShifted.tail(D_MANT_WIDTH+1).head(1),
    s4_mantShifted.tail(S_MANT_WIDTH+1).head(1)
  ).asBool()
  val s = ORTree(Mux(s4_isDouble,
    s4_mantShifted.tail(D_MANT_WIDTH+2),
    s4_mantShifted.tail(S_MANT_WIDTH+2)
  ))

  val rounding = Module(new RoundF64AndF32WithExceptions)
  rounding.io.isDouble := s4_isDouble
  rounding.io.denormShiftAmt := s4_denormShift
  rounding.io.sign := s4_resSign
  rounding.io.expNorm := s4_expPostNorm
  rounding.io.mantWithGRS := Cat(mantUnrounded, g, r, s)
  rounding.io.rm := s4_rm
  rounding.io.specialCaseHappen := s4_specialCaseHappen

  val isZeroResult = rounding.io.isZeroResult
  val expRounded = rounding.io.expRounded
  val mantRounded = rounding.io.mantRounded
  val overflow = rounding.io.overflow
  val underflow = rounding.io.underflow
  val inexact = rounding.io.inexact

  val s5_isDouble = S5Reg(s4_isDouble)
  val s5_sign = S5Reg(Mux(isZeroResult, s4_zeroSign, s4_resSign))
  val s5_exp = S5Reg(expRounded)
  val s5_mant = S5Reg(mantRounded)
  val s5_specialCaseHappen = S5Reg(s4_specialCaseHappen)
  val s5_specialOutput = S5Reg(s4_specialOutput)
  val s5_invalid = S5Reg(s4_invalid)
  val s5_overflow = S5Reg(overflow)
  val s5_underflow = S5Reg(underflow)
  val s5_inexact = S5Reg(inexact)
  val s5_ovSetInf = S5Reg(s4_ovSetInf)

//  FPUDebug(){
//    when(valids(4) && ready){
//      printf(p"[s5] expPostNorm:${s4_expPostNorm} expRounded:${expRounded}\n")
//    }
//  }

  /******************************************************************
    * Assign Outputs
    *****************************************************************/

  val commonResult = Mux(s5_isDouble,
    Cat(
      s5_sign,
      s5_exp(Float64.expWidth-1, 0),
      s5_mant(Float64.mantWidth-1, 0)
    ),
    Cat(
      s5_sign,
      s5_exp(Float32.expWidth-1, 0),
      s5_mant(Float32.mantWidth-1, 0)
    )
  )
  val result = Mux(s5_specialCaseHappen,
    s5_specialOutput,
    Mux(s5_overflow,
      Mux(s5_isDouble,
        Cat(s5_sign, Mux(s5_ovSetInf, Float64.posInf, Float64.maxNorm).tail(1)),
        Cat(s5_sign, Mux(s5_ovSetInf, Float32.posInf, Float32.maxNorm).tail(1))
      ),
      commonResult
    )
  )

  io.out.bits.data := result
  fflags.invalid := s5_invalid
  fflags.inexact := s5_inexact
  fflags.overflow := s5_overflow
  fflags.underflow := s5_underflow
  fflags.infinite := false.B

//  FPUDebug(){
//    //printf(p"v0:${valids(0)} v1:${valids(1)} v2:${valids(2)} v3:${valids(3)} v4:${valids(4)} v5:${valids(5)}\n")
//    when(io.in.fire()){
//      printf(p"[in] a:${Hexadecimal(a)} b:${Hexadecimal(b)} c:${Hexadecimal(c)}\n")
//    }
//    when(io.out.fire()){
//      printf(p"[out] res:${Hexadecimal(io.out.bits.result)}\n")
//    }
//  }


}
