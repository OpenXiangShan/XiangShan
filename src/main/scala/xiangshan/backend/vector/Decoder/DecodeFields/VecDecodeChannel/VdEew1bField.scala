package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import freechips.rocketchip.rocket.Instructions.{VLM_V, VMADC_VV, VMSBC_VV, VMSBF_M, VMSEQ_VV, VMSGTU_VX, VMSGT_VX, VMSIF_M, VMSLEU_VV, VMSLE_VV, VMSLTU_VV, VMSLT_VV, VMSNE_VV, VMSOF_M}
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.util.ScalaTypeExt.StringToExt
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object VdEew1bField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "vdEew1b"

  override def genTable(op: VecInstPattern): BitPat = {
    if (op.name.startsWithThese(prefixes) || op.name.endsWithThese(suffixes))
      y
    else
      n
  }

  val prefixes: Seq[String] = getVariableNameSeq(
    VMADC_VV, VMSBC_VV,
    VMSEQ_VV, VMSNE_VV, VMSLE_VV, VMSLEU_VV, VMSGT_VX, VMSGTU_VX, VMSLT_VV, VMSLTU_VV,
    VMSBF_M, VMSIF_M, VMSOF_M,
    VLM_V,
  ).map(_.split('_').head) ++ Seq(
    "VMF",
  )

  val suffixes: Seq[String] = Seq(
    "_MM",
  )
}


