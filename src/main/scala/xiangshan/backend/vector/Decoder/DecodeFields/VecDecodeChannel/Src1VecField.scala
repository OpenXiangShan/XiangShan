package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import freechips.rocketchip.rocket.Instructions.{VSETVL, VSETVLI}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.DecodeSrcType
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object Src1VecField extends DecodeField[InstPattern, Vec[SrcRenType]] {

  override def name: String = "src1Vec"

  override def chiselType: Vec[SrcRenType] = Vec(8, new SrcRenType)

  override def genTable(op: InstPattern): BitPat = ???

  def genTable(op: VecInstPattern): BitPat = {
    op match {
      case VecArithInstPattern() => genTable(op.asInstanceOf[VecArithInstPattern])
      case VecConfigInstPattern() => genTable(op.asInstanceOf[VecConfigInstPattern])
      case _: VecMemInstPattern => genTable(op.asInstanceOf[VecMemInstPattern])
    }
  }

  def genTable(op: VecArithInstPattern): BitPat = {
    var isImm = false
    var isGp = false
    var isVp = false
    var isFp = false
    var isNo = false
    var res: BitPat = BitPat.N()
    if (useIMM(op)) {
      isImm = true
      res = DecodeSrcType.IMM.toBitPat
    }
    if (useGP(op)) {
      isGp = true
      res = DecodeSrcType.GP.toBitPat
    }
    if (useVP(op)) {
      isVp = true
      res = DecodeSrcType.VP.toBitPat
    }
    if (useFP(op)) {
      isFp = true
      res = DecodeSrcType.FP.toBitPat
    }
    if (useNO(op)) {
      isNo = true
      res = DecodeSrcType.NO.toBitPat
    }
    require(Seq(isImm, isGp, isFp, isVp, isNo).map(x => if (x) 1 else 0).sum == 1,
      s"The inst ${op.name} not correct decoded, " +
        s"isImm=$isImm, isGp=$isGp, isVp=$isVp, isFp=$isFp, isNo=$isNo"
    )
    res
  }

  def genTable(op: VecMemInstPattern): BitPat = {
    DecodeSrcType.GP.toBitPat
  }

  def genTable(op: VecConfigInstPattern): BitPat = {
    if (getVariableNameSeq(VSETVLI, VSETVL).contains(op.name))
      DecodeSrcType.GP.toBitPat
    else
      DecodeSrcType.IMM.toBitPat
  }

  def useIMM(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VI",
      "_VIM",
      "_WI",
      "_V_I",
    )
    op.name.endsWithThese(suffixes)
  }

  def useGP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VX",
      "_VXM",
      "_WX",
      "_S_X",
      "_V_X",
    )
    op.name.endsWithThese(suffixes)
  }

  def useVP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VV",
      "_VVM",
      "_WV",
      "_VS",
      "_MM",
      "_V_V",
      "_VM",
    )
    op.name.endsWithThese(suffixes)
  }

  def useFP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VF",
      "_S_F",
      "_V_F",
      "_VFM",
      "_WF",
    )
    op.name.endsWithThese(suffixes)
  }

  def useNO(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "R_V", // VMVNR_V
      "_X_S",
      "_F_S",
      "_M",
      "_VF2",
      "_VF4",
      "_VF8",
      "VID_V",
    )
    val prefixes = Seq(
      "VFCVT",
      "VFNCVT",
      "VFWCVT",
      "VFCLASS",
      "VFREC7",
      "VFRSQRT7",
      "VFSQRT",
    )
    op.name.startsWithThese(prefixes) || op.name.endsWithThese(suffixes)
  }
}
