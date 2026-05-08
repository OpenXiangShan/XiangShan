package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import utils.NamedUInt
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.{FpInstPattern, InstPattern, VecInstPattern}
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object Frm extends NamedUInt(3) {
  val RNE = 0.U(width.W)
  val RTZ = 1.U(width.W)
  val RDN = 2.U(width.W)
  val RUP = 3.U(width.W)
  val RMM = 4.U(width.W)
  val ROD = 6.U(width.W)
  val DYN = 7.U(width.W)
}

object FrmField extends DecodeField[VecInstPattern, UInt] {

  override def name: String = "frm"

  override def chiselType: UInt = Frm()

  override def genTable(op: VecInstPattern): BitPat = {
    BitPat(
      if (rtzInsts.contains(op.name))
        Frm.RTZ
      else if (rodInsts.contains(op.name))
        Frm.ROD
      else
        Frm.DYN
    )
  }

  val rtzInsts = getVariableNameSeq(
    VFCVT_RTZ_XU_F_V,
    VFCVT_RTZ_X_F_V,
    VFWCVT_RTZ_XU_F_V,
    VFWCVT_RTZ_X_F_V,
    VFNCVT_RTZ_XU_F_W,
    VFNCVT_RTZ_X_F_W,
  ).toSet

  val rodInsts = getVariableNameSeq(
    VFNCVT_ROD_F_F_W,
  ).toSet
}
