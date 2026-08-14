package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.{BitPat, MuxLookup, ValidIO}
import xiangshan.CommitType
import xiangshan.backend.decode.isa.Extensions.ExtBase
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Uop.UopInfoRenameSimple
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable._
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm
import xiangshan.backend.decode.opcode.Opcode.{Opcode, toOpcodeUtil}
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

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
