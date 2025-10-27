package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.util.ScalaTypeExt.StringToExt

/**
 * This field is used to specify if the uop write narrow mask, like vmseq.vv. <br/>
 * The mask operation uops are excluded, since they write whole register.
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
