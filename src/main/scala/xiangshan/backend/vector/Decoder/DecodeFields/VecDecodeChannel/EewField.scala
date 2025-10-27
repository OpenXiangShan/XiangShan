package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.vector.Decoder.InstPattern.{VecInstPattern, VecMemInstPattern}
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object EewField extends DecodeField[VecInstPattern, UInt] {

  override def name: String = "eew"

  override def chiselType: UInt = VSew()

  override def genTable(op: VecInstPattern): BitPat = {
    val eew: UInt = op match {
      case x: VecMemInstPattern if x.mew.rawString == "0" =>
        x.width.rawString match {
          case "000" => VSew.e8
          case "101" => VSew.e16
          case "110" => VSew.e32
          case "111" => VSew.e64
        }
    }

    eew.toBitPat
  }
}
