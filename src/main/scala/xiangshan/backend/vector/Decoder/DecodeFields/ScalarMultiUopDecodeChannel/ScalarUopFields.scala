package xiangshan.backend.vector.Decoder.DecodeFields.ScalarMultiUopDecodeChannel

import chisel3._
import chisel3.util.{BitPat, MuxLookup, ValidIO}
import xiangshan.CommitType
import xiangshan.backend.decode.isa.Extensions.ExtBase
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel.{
  CanRobCompressField, CommitTypeField, NumWbField, SelImmField, UopInfoFieldSimple,
}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, NumWB}
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable._
import xiangshan.backend.vector.Decoder.util.{BoolDecodeField, DecodeField}
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

/** Per-uop logical register selectors used by split scalar instructions. */
class LogicalRegField(uopIdx: Int, operand: Int, extensions: Seq[ExtBase])
  extends DecodeField[InstPattern, UInt] {
  override def name: String = s"logicalReg${operand}_$uopIdx"
  override def chiselType: UInt = UInt(LogicalRegSel.width.W)

  override def genTable(op: InstPattern): BitPat = {
    UopInfoFieldSimple.genUopSeq(op, extensions).lift(uopIdx).map { uop =>
      val m = ScalaUopTable.metadata(uop)
      Seq(m.src1, m.src2, m.src3, m.dest)(operand).encode.U(LogicalRegSel.width.W).toBitPat
    }.getOrElse(default)
  }
}

object LogicalRegField {
  /** Decode the table's selector into an instruction register or a fixed logical register. */
  def decodeSelector(spec: UInt, instFields: XSInstBitFields): UInt = {
    MuxLookup(spec(8, 6), spec(5, 0))(Seq(
      1.U -> instFields.RS1,
      2.U -> (instFields.RS2 + spec(5, 0)),
      3.U -> instFields.FS3,
      4.U -> instFields.RD,
      5.U -> (instFields.RD + spec(5, 0)),
      7.U -> spec(5, 0),
    ))
  }
}

class UopImmInfo extends Bundle {
  val selImm = ValidIO(DecodeSelImm())
  val constant = ValidIO(UInt(32.W))
}

/** Use the instruction immediate unless the uop supplies a packed constant. */
class UopImmInfoField(uopIdx: Int, extensions: Seq[ExtBase], xlen: Int)
  extends DecodeField[InstPattern, UopImmInfo] {
  override def name: String = s"uopImmInfo$uopIdx"
  override def chiselType: UopImmInfo = new UopImmInfo

  override def genTable(op: InstPattern): BitPat = {
    val spec = UopInfoFieldSimple.genUopSeq(op, extensions).lift(uopIdx)
      .flatMap(uop => ScalaUopTable.metadata(uop).imm)
    spec match {
      case Some(s) =>
        val valid = if (s.sel.litValue == DecodeSelImm.NO.litValue) BitPat.N() else BitPat.Y()
        valid ## s.sel.toBitPat ## BitPat.Y() ## s.value(xlen).U(32.W).toBitPat
      case None => SelImmField.genTable(op) ## BitPat.N() ## BitPat.dontCare(32)
    }
  }
}

/** Ordinary uops inherit the instruction's commit classification. */
class UopCommitTypeField(uopIdx: Int, extensions: Seq[ExtBase])
  extends DecodeField[InstPattern, UInt] {
  override def name: String = s"uopCommitType$uopIdx"
  override def chiselType: UInt = CommitType()

  override def genTable(op: InstPattern): BitPat = {
    UopInfoFieldSimple.genUopSeq(op, extensions).lift(uopIdx)
      .flatMap(uop => ScalaUopTable.metadata(uop).commitType)
      .map(_.toBitPat).getOrElse(CommitTypeField.genTable(op))
  }
}

/** Split stores include separate address and data writebacks. */
object ScalarNumWbField extends DecodeField[InstPattern, UInt] {
  override def name: String = "numWb"
  override def chiselType: UInt = NumWB()

  override def genTable(inst: InstPattern): BitPat = inst match {
    case _: SspushInstPattern => 5.U(NumWB.width.W).toBitPat // Six writebacks, encoded as count - 1.
    case _: SspopchkInstPattern => 2.U(NumWB.width.W).toBitPat
    case _ => NumWbField.genTable(inst)
  }
}

object ScalarCanRobCompressField extends BoolDecodeField[InstPattern] {
  override def name: String = "canRobCompress"

  override def genTable(inst: InstPattern): BitPat = inst match {
    case _: SspushInstPattern | _: SspopchkInstPattern | _: SsrdpInstPattern | _: SsamoswapInstPattern => n
    case _ => CanRobCompressField.genTable(inst)
  }
}

object IsSsamoswapField extends BoolDecodeField[InstPattern] {
  override def name: String = "isSsamoswap"

  override def genTable(inst: InstPattern): BitPat = inst match {
    case _: SsamoswapInstPattern => y
    case _ => n
  }
}
