package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.DecodeField

object PrivExceptionCause extends ChiselEnum {
  val none, sfenceVMA, sfencePart, hfenceGVMA, hfenceVVMA, hlsv, wfi, wrsNto, cboZ, cboCF, cboI, aes64ks1i, amocasQ = Value
}

object PrivExceptionCauseField extends DecodeField[InstPattern, PrivExceptionCause.Type] {
  override def name: String = "privExceptionCause"

  override def chiselType: PrivExceptionCause.Type = PrivExceptionCause()

  override def default: BitPat = BitPat(PrivExceptionCause.none.asUInt)

  override def genTable(inst: InstPattern): BitPat = {
    val cause: PrivExceptionCause.Type = inst match {
      case _: SfenceVMAInstPattern        => PrivExceptionCause.sfenceVMA
      case _: SfenceOtherInstPattern      => PrivExceptionCause.sfencePart
      case _: HfenceGVMAInstPattern       => PrivExceptionCause.hfenceGVMA
      case _: HfenceVVMAInstPattern       => PrivExceptionCause.hfenceVVMA
      case _: WaitForInterruptInstPattern => PrivExceptionCause.wfi
      case _: ZawrsNtoPattern             => PrivExceptionCause.wrsNto
      case _: HyperLoadInstPattern        => PrivExceptionCause.hlsv
      case _: HyperStoreInstPattern       => PrivExceptionCause.hlsv
      case _: CboZInstPattern             => PrivExceptionCause.cboZ
      case _: CboCFInstPattern            => PrivExceptionCause.cboCF
      case _: CboIInstPattern             => PrivExceptionCause.cboI
      case _: Aes64ks1iIllInstPattern     => PrivExceptionCause.aes64ks1i
      case _: AmocasQIllInstPattern       => PrivExceptionCause.amocasQ
      case _                              => PrivExceptionCause.none
    }

    BitPat(cause.asUInt)
  }
}
