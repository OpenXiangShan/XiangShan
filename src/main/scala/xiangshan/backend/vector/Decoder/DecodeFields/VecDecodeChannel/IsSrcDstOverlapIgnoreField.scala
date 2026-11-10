package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsSrcDstOverlapIgnoreField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "isSrcDstOverlapIgnore"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => dc
      case _: VecIntVVMPattern | _: VecFpOp2VMPattern |
           _: VecCarryMPattern | _: VecIntMMMPattern |
           _: VecIntRedPattern | _: VecIntWRedPattern |
           _: VecFpRedPattern | _: VecFpWRedPattern => y
      case _ => n
    }
  }
}
