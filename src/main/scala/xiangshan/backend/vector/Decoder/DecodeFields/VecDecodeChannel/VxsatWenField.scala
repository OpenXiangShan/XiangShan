package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.util.ScalaTypeExt.StringToExt
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object VxsatWenField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "vxsatWen"

  override def genTable(op: VecInstPattern): BitPat = {
    if (op.name.startsWithThese(instPrefixes)) {
      y
    } else {
      n
    }}

  val instPrefixes = getVariableNameSeq(
    VSADD_VV,
    VSADDU_VV,
    VSSUB_VV,
    VSSUBU_VV,
    VNCLIP_WV,
    VNCLIPU_WV,
    VSMUL_VV,
  ).map(_.split('_').head)
}
