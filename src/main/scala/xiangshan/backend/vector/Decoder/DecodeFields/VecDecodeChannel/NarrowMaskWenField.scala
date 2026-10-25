package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.StringToExt

/** Classify partial mask-result families by name prefix, including compares and carry masks.
  * Boolean mask-logic instructions outside these prefixes use whole-register handling.
  */
object NarrowMaskWenField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "narrowMaskWen"

  override def genTable(op: VecInstPattern): BitPat = {
    if (op.name.startsWithThese(instPrefixes))
      y
    else
      n
  }

  val instPrefixes = Seq(
    "VMADC", "VMSBC",
    "VMS",
    "VMF",
  )
}
