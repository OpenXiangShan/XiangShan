package xiangshan.backend.vector.Decoder.Uop

import chisel3._
import chisel3.util._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector.util.ChiselTypeExt._

class UopInfoRenameWithIllegal extends Bundle {
  val illegal = Bool()
  val uop = new UopInfoRename()
}

class UopInfoRename extends Bundle {
  // rename used
  val src1Ren = Bool()
  val src1Type = DecodeSrcType()
  val src2Ren = Bool()
  val src2Type = DecodeSrcType()
  val vlRen = Bool()
  val v0Ren = Bool()
  val maskType = MaskTypeChiselEnum()
  val intRmRen = Bool()
  val readVdAsSrc = Bool()

  val gpWen = Bool()
  val fpWen = Bool()
  val vpWen = Bool()
  val vlWen = Bool()
  val vxsatWen = Bool()
}

object UopInfoRename {
  lazy val width = (new UopInfoRename).getWidth

  def genBitPat(
    src1Type   : Option[OperandType],
    src2Type   : Option[OperandType],
    vlRen      : Boolean,
    v0Ren      : Boolean,
    maskType   : MaskType,
    intRmRen   : Boolean,
    readVdAsSrc: Boolean,
    gpWen      : Boolean,
    fpWen      : Boolean,
    vpWen      : Boolean,
    vlWen      : Boolean,
    vxsatWen   : Boolean,
  ): BitPat = {
    val src1: String = operandTypeToString(src1Type)
    val src2: String = operandTypeToString(src2Type)
    val vlR = booleanToString(vlRen)
    val v0R = booleanToString(v0Ren)
    val intRmR = booleanToString(intRmRen)
    val rdAsSrc = booleanToString(readVdAsSrc)
    val maskTy = maskType.toChiselEnum.toBitPat.rawString
    val gpW = booleanToString(gpWen)
    val fpW = booleanToString(fpWen)
    val vpW = booleanToString(vpWen)
    val vlW = booleanToString(vlWen)
    val vxsatW = booleanToString(vxsatWen)

    val bp = BitPat(s"b$src1$src2$vlR$v0R$maskTy$intRmR$rdAsSrc$gpW$fpW$vpW$vlW$vxsatW")
    require(
      bp.width == UopInfoRename.width,
      s"bitpat width is ${bp.width}, but ${UopInfoRename.width} is expected"
    )
    bp
  }

  def operandTypeToString(s: Option[OperandType]): String = {
    s match {
      case Some(value) => "1" + (value match {
        case Operand.IMM => DecodeSrcType.IMM
        case Operand.GP  => DecodeSrcType.GP
        case Operand.FP  => DecodeSrcType.FP
        case Operand.VP  => DecodeSrcType.VP
      }).toBitPat.rawString
      case None => "000"
    }
  }

  def booleanToString(b: Boolean): String = {
    b match {
      case true => "1"
      case false => "0"
    }
  }
}