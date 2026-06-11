package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.util.DecodeField

class VRegTypes extends Bundle {
  val useVs1 = Bool()     // vs1 is used
  val useVs2 = Bool()     // vs2 is used
  val useVs3 = Bool()     // vs3 is used
  val useVd = Bool()      // vd is used
  val emulVs1 = VLmul()   // EMUL for vs1
  val emulVs2 = VLmul()   // EMUL for vs2
  val emulVs3 = VLmul()   // EMUL for vs3
  val eewVs3EqVm = Bool() // vs3 EEW equals mask EEW
  val eewVs1EqVs3 = Bool()// vs1 EEW equals vs3 EEW
  val eewVs1LtVs3 = Bool()// vs1 EEW < vs3 EEW
  val eewVs2EqVs3 = Bool()// vs2 EEW equals vs3 EEW
  val eewVs2LtVs3 = Bool()// vs2 EEW < vs3 EEW
}

object VRegTypesField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  VRegTypes,
] {
  override def name: String = "vregTypes"
  override def chiselType: VRegTypes = new VRegTypes
  override def default: BitPat = BitPat.dontCare(chiselType.getWidth)

  override def genTable(
    op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  ): BitPat = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op

    instP match {
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => default
      case vmi: VecMemInstPattern => genMem(vmi, op)
      case vai: VecArithInstPattern =>
        val withTraits = VecArithInstPattern.apply()(vai.rawInst)
        genArith(withTraits, sewP, lmulP)
    }
  }

  // ============================================================
  //  Memory
  // ============================================================

  private def genMem(
    vmi: VecMemInstPattern,
    op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  ): BitPat = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op
    val sew = sewP.sewValue
    val lmul = lmulP.lmulValue
    val seg = nfP.segNum
    implicit val ctx = s"inst=${instP.bitPat} sew=$sew lmul=$lmul seg=$seg"

    val isIndex  = vmi.isInstanceOf[VecMemIndex]
    val isWhole  = vmi.isInstanceOf[VecMemWhole]
    val isMask   = vmi.isInstanceOf[VecMemMask]
    val isStore  = vmi.isInstanceOf[VecStoreInstPattern]
    val isLoad   = vmi.isInstanceOf[VecLoadInstPattern]

    // Whole register: EMUL from NF field, EEW from width field
    if (isWhole) {
      val nr = instP.rawInst(31, 29).rawString match {
        case "000" => 1
        case "001" => 2
        case "011" => 4
        case "111" => 8
        case _     => throw new IllegalArgumentException(s"Invalid nf for whole reg: ${instP.rawInst(31,29)}")
      }
      if (isLoad) {
        combine(N, N, N, Y)(
          D3, D3, emulToBits(nr))(
          N, D, D, D, D)
      } else {
        combine(N, N, Y, N)(
          D3, D3, emulToBits(nr))(
          N, D, D, D, D)
      }
    } else {
      val useVs1 = N
      val useVs2 = if (isIndex) Y else N
      val useVs3 = if (isStore) Y else N
      val useVd  = if (isLoad) Y else N

      val emulVs1 = D3
      val emulVs2 = if (isIndex) {
        val eew = vmi.eewValue
        emulToBits(eew * lmul / sew)
      } else D3
      val emulVs3 = if (isMask) R1
                else if (isIndex) lmulP.bitPat
                else emulToBits(vmi.eewValue * lmul / sew)

      val cmp3Vm  = N
      val cmp1Eq3 = D
      val cmp1Lt3 = D
      val cmp2Eq3 = if (isIndex) {
        val vs2Eew = vmi.eewValue
        if (vs2Eew == sew) Y else N
      } else D
      val cmp2Lt3 = if (isIndex) {
        val vs2Eew = vmi.eewValue
        if (vs2Eew < sew) Y else N
      } else D

      combine(useVs1, useVs2, useVs3, useVd)(
        emulVs1, emulVs2, emulVs3)(
        cmp3Vm, cmp1Eq3, cmp1Lt3, cmp2Eq3, cmp2Lt3)
    }
  }

  // ============================================================
  //  Arithmetic — derive EMUL from Vs*Type traits
  // ============================================================

  private def genArith(
    instP: VecInstPattern,
    sewP: SewPattern,
    lmulP: LmulPattern,
  ): BitPat = {
    val v1 = vs1Info(instP, sewP, lmulP)
    val v2 = vs2Info(instP, sewP, lmulP)
    val v3 = vs3Info(instP, sewP, lmulP)

    val vs3Used = v3._1 == Y || v3._4 == Y
    val cmp3Vm  = if (              !vs3Used) D else if (    0 == v3._3) Y else N
    val cmp1Eq3 = if (v1._1 == N || !vs3Used) D else if (v1._3 == v3._3) Y else N
    val cmp1Lt3 = if (v1._1 == N || !vs3Used) D else if (v1._3 <  v3._3) Y else N
    val cmp2Eq3 = if (v2._1 == N || !vs3Used) D else if (v2._3 == v3._3) Y else N
    val cmp2Lt3 = if (v2._1 == N || !vs3Used) D else if (v2._3 <  v3._3) Y else N

    combine(
      v1._1, v2._1, v3._1, v3._4)(
      v1._2, v2._2, v3._2)(
      cmp3Vm, cmp1Eq3, cmp1Lt3, cmp2Eq3, cmp2Lt3)
  }

  // ============================================================
  //  VsType → (used, emul, eewRatio)
  //  eewRatio: 0 = special (not applicable / mask EEW=1)
  //            1 = sew, 2 = sew*2, 0.5 = sew/2, 0.25 = sew/4, 0.125 = sew/8
  // ============================================================

  private def vs1Info(instP: VecInstPattern, sewP: SewPattern, lmulP: LmulPattern): (BitPat, BitPat, Double) = {
    val lmul = lmulP.lmulValue
    val sew = sewP.sewValue
    val LB = lmulP.bitPat
    implicit val ctx = s"inst=${instP.bitPat} sew=$sew lmul=$lmul"
    instP match {
      case _: V1_NotUsed       => (N, D3, 0)
      case _: V1_VecNormal     => (Y, LB, 1)
      case _: V1_VecFloat      => (Y, LB, 1)
      case _: V1_VecMask       => (Y, R1, 0)
      case _: V1_ScaNormal     => (Y, R1, 1)
      case _: V1_ScaWiden      => (Y, R1, 2)
      case _: V1_ScaFloat      => (Y, R1, 1)
      case _: V1_ScaFloatWiden => (Y, R1, 2)
      case _: V1_SpcGather16   => (Y, emulToBits(16.0 / sewP.sewValue * lmul), 16.0 / sewP.sewValue)
      case _: V1_CryptW128E32  => (Y, LB, 1)
      case _: V1_CryptW256E64  => (Y, LB, 1)
      case _: V1_CryptBoth     => (Y, LB, 1)
      case _ => throw new IllegalArgumentException(s"Unknown Vs1: $ctx")
    }
  }

  private def vs2Info(instP: VecInstPattern, sewP: SewPattern, lmulP: LmulPattern): (BitPat, BitPat, Double) = {
    val lmul = lmulP.lmulValue
    val sew = sewP.sewValue
    val LB = lmulP.bitPat
    implicit val ctx = s"inst=${instP.bitPat} sew=$sew lmul=$lmul"
    lazy val W2 = emulToBits(lmul * 2)
    lazy val N2 = emulToBits(lmul / 2)
    lazy val N4 = emulToBits(lmul / 4)
    lazy val N8 = emulToBits(lmul / 8)
    instP match {
      case _: V2_NotUsed       => (N, D3, 0)
      case _: V2_VecNormal     => (Y, LB, 1)
      case _: V2_VecWiden      => (Y, W2, 2)
      case _: V2_VecFloat      => (Y, LB, 1)
      case _: V2_VecFloatWiden => (Y, W2, 2)
      case _: V2_VecMask       => (Y, R1, 0)
      case _: V2_ScaNormal     => (Y, R1, 1)
      case _: V2_ScaFloat      => (Y, R1, 1)
      case _: V2_VecNarrow8    => (Y, N8, 0.125)
      case _: V2_VecNarrow4    => (Y, N4, 0.25)
      case _: V2_VecNarrow2    => (Y, N2, 0.5)
      case _: V2_SpcMV1        => (Y, R1, 1)
      case _: V2_SpcMV2        => (Y, R2, 1)
      case _: V2_SpcMV4        => (Y, R4, 1)
      case _: V2_SpcMV8        => (Y, R8, 1)
      case _: V2_CryptW128E32  => (Y, LB, 1)
      case _: V2_CryptW256E64  => (Y, LB, 1)
      case _: V2_CryptBoth     => (Y, LB, 1)
      case _ => throw new IllegalArgumentException(s"Unknown Vs2: $ctx")
    }
  }

  private def vs3Info(instP: VecInstPattern, sewP: SewPattern, lmulP: LmulPattern): (BitPat, BitPat, Double, BitPat) = {
    val lmul = lmulP.lmulValue
    val sew = sewP.sewValue
    val LB = lmulP.bitPat
    implicit val ctx = s"inst=${instP.bitPat} sew=$sew lmul=$lmul"
    lazy val W2 = emulToBits(lmul * 2)
    instP match {
      case _: V3_NotUsed                => (N, D3, 0, N)
      // 2-source: vs3 only used as dest, not source
      case _: V3_VecNormal_DestOnly     => (N, LB, 1, Y)
      case _: V3_VecWiden_DestOnly      => (N, W2, 2, Y)
      case _: V3_VecFloat_DestOnly      => (N, LB, 1, Y)
      case _: V3_VecFloatWiden_DestOnly => (N, W2, 2, Y)
      case _: V3_VecMask_DestOnly       => (N, R1, 0, Y)
      case _: V3_ScaNormal_DestOnly     => (N, R1, 1, Y)
      case _: V3_ScaWiden_DestOnly      => (N, R1, 2, Y)
      case _: V3_ScaFloat_DestOnly      => (N, R1, 1, Y)
      case _: V3_ScaFloatWiden_DestOnly => (N, R1, 2, Y)
      case _: V3_SpcMV1_DestOnly        => (N, R1, 1, Y)
      case _: V3_SpcMV2_DestOnly        => (N, R2, 1, Y)
      case _: V3_SpcMV4_DestOnly        => (N, R4, 1, Y)
      case _: V3_SpcMV8_DestOnly        => (N, R8, 1, Y)
      case _: V3_CryptW128E32_DestOnly  => (N, LB, 1, Y)
      case _: V3_CryptW256E64_DestOnly  => (N, LB, 1, Y)
      // 3-source: vs3 is both source and dest
      case _: V3_VecNormal_SrcDest      => (Y, LB, 1, Y)
      case _: V3_VecWiden_SrcDest       => (Y, W2, 2, Y)
      case _: V3_VecFloat_SrcDest       => (Y, LB, 1, Y)
      case _: V3_VecFloatWiden_SrcDest  => (Y, W2, 2, Y)
      case _: V3_CryptW128E32_SrcDest   => (Y, LB, 1, Y)
      case _: V3_CryptBoth_SrcDest      => (Y, LB, 1, Y)
      case _ => throw new IllegalArgumentException(s"Unknown Vs3: $ctx")
    }
  }

  // ============================================================
  //  Helpers
  // ============================================================

  private def Y = BitPat.Y(1)
  private def N = BitPat.N(1)
  private def D = BitPat.dontCare(1)
  private def D3 = BitPat.dontCare(3)
  private def R1 = BitPat("b000")
  private def R2 = BitPat("b001")
  private def R4 = BitPat("b010")
  private def R8 = BitPat("b011")

  private def emulToBits(emul: Double)(implicit ctx: String = ""): BitPat = emul match {
    case 0.125 => BitPat("b101")
    case 0.25  => BitPat("b110")
    case 0.5   => BitPat("b111")
    case 1     => BitPat("b000")
    case 2     => BitPat("b001")
    case 4     => BitPat("b010")
    case 8     => BitPat("b011")
    case _     => throw new IllegalArgumentException(s"Invalid EMUL($emul): $ctx")
  }

  private def combine(
    useVs1: BitPat, useVs2: BitPat, useVs3: BitPat, useVd: BitPat,
  )(
    emulVs1:  BitPat, emulVs2: BitPat, emulVs3: BitPat,
  )(
    eewVs3EqVm:  BitPat, eewVs1EqVs3: BitPat, eewVs1LtVs3: BitPat,
    eewVs2EqVs3: BitPat, eewVs2LtVs3: BitPat,
  ): BitPat = {
    require(useVs1.getWidth == 1, s"useVs1 width ${useVs1.getWidth} != 1")
    require(useVs2.getWidth == 1, s"useVs2 width ${useVs2.getWidth} != 1")
    require(useVs3.getWidth == 1, s"useVs3 width ${useVs3.getWidth} != 1")
    require(useVd .getWidth == 1, s"useVd  width ${useVd.getWidth} != 1")
    require(emulVs1.getWidth == 3, s"emulVs1 width ${emulVs1.getWidth} != 3")
    require(emulVs2.getWidth == 3, s"emulVs2 width ${emulVs2.getWidth} != 3")
    require(emulVs3.getWidth == 3, s"emulVs3 width ${emulVs3.getWidth} != 3")
    require(eewVs3EqVm .getWidth == 1, s"eewVs3EqVm  width ${eewVs3EqVm.getWidth} != 1")
    require(eewVs1EqVs3.getWidth == 1, s"eewVs1EqVs3 width ${eewVs1EqVs3.getWidth} != 1")
    require(eewVs1LtVs3.getWidth == 1, s"eewVs1LtVs3 width ${eewVs1LtVs3.getWidth} != 1")
    require(eewVs2EqVs3.getWidth == 1, s"eewVs2EqVs3 width ${eewVs2EqVs3.getWidth} != 1")
    require(eewVs2LtVs3.getWidth == 1, s"eewVs2LtVs3 width ${eewVs2LtVs3.getWidth} != 1")
    useVs1 ## useVs2 ## useVs3 ## useVd ##
      emulVs1 ## emulVs2 ## emulVs3 ##
      eewVs3EqVm ## eewVs1EqVs3 ## eewVs1LtVs3 ## eewVs2EqVs3 ## eewVs2LtVs3
  }
}
