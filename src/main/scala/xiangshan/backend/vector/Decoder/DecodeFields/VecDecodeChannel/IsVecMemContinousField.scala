package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecConfigInstPattern, VecInstPattern, VecLoadInstPattern, VecMemFF, VecMemIndex, VecMemInstPattern, VecMemMask, VecMemStrided, VecMemTrait, VecMemUnitStride, VecMemWhole, VecStoreInstPattern}
import xiangshan.backend.vector.Decoder.util.{BoolDecodeField, DecodeField}
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt

object IsVecMemContinousField extends BoolDecodeField[VecInstPattern] {

  override def name: String = "isVecMemContinous"

  override def genTable(op: VecInstPattern): BitPat = (
    op match {
      case vmi: VecMemInstPattern =>
        vmi.asInstanceOf[VecMemTrait] match {
          case _: VecMemUnitStride => true
          case _: VecMemStrided => false
          case _: VecMemIndex => false
          case _: VecMemWhole => true
          case _: VecMemMask => true
          case _: VecMemFF => true
        }
      case _ => false
    }
  ).toBitPat
}
