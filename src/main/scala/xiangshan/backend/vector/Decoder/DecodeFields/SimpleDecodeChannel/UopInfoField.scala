package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.{BitPat, ValidIO}
import xiangshan.backend.decode.isa.Extensions.ExtBase
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameSimple
import xiangshan.backend.decode.opcode.Opcode.{Opcode, toOpcodeUtil}
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.BitPatToExt


class UopInfoField(uopIdx: Int, extensions: Seq[ExtBase]) extends DecodeField[InstPattern, ValidIO[UopInfoRenameSimple]] {

  override def name: String = s"uopInfo$uopIdx"

  override def chiselType: ValidIO[UopInfoRenameSimple] = ValidIO(new UopInfoRenameSimple)

  override def default: BitPat = BitPat.N() ## BitPat.dontCare(UopInfoRenameSimple.width)

  override def genTable(op: InstPattern): BitPat = {
    // try
    //   table(op.bitPat).encode.pad0To(Opcode.getWidth)
    // catch {
    //   case e: NoSuchElementException =>
    //     println(s"inst ${op.name} is not in uop table")
    //     throw e
    //   case e: Throwable => throw e
    // }

    if (UopInfoFieldSimple.genUopSeq(op, extensions).isDefinedAt(uopIdx)) {
      BitPat.Y(1) ## UopInfoFieldSimple.genUopSeq(op, extensions)(uopIdx).genUopInfoRenameSimpleBitPat
    } else {
      default
    }

  }
}

object UopInfoFieldSimple {

  def genUopSeq(op: InstPattern, extensions: Seq[ExtBase]): Seq[Opcode] = {
    this.genUopSeqImpl(op, extensions)
  }

  def genUopSeqImpl(instP: InstPattern, extensions: Seq[ExtBase]): Seq[Opcode] = {
    extensions.map(_.table).reduce(_ ++ _)(instP.bitPat)
  }

}
