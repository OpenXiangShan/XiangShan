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

class UopInfoRenameSimple extends Bundle {
  // rename used
  val src1Ren = Bool()
  val src1Type = DecodeSrcType()
  val src2Ren = Bool()
  val src2Type = DecodeSrcType()
  val src3Ren = Bool()
  val src3Type = DecodeSrcType()

  val gpWen = Bool()
  val fpWen = Bool()

  // pipeline control signal
  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
}

object UopInfoRenameSimple extends InfoToString {
  lazy val width = (new UopInfoRenameSimple).getWidth

  def genBitPat(
                 src1Type   : Option[OperandType],
                 src2Type   : Option[OperandType],
                 src3Type   : Option[OperandType],
                 gpWen      : Boolean,
                 fpWen      : Boolean,
                 noSpec     : Boolean,
                 blockBack  : Boolean,
                 flushPipe  : Boolean,
               ): BitPat = {
    val src1: String = operandTypeToString(src1Type)
    val src2: String = operandTypeToString(src2Type)
    val src3: String = operandTypeToString(src3Type)
    val gpW = booleanToString(gpWen)
    val fpW = booleanToString(fpWen)
    val noS = booleanToString(noSpec)
    val blockB = booleanToString(blockBack)
    val flushP = booleanToString(flushPipe)

    val bp = BitPat(s"b$src1$src2$src3$gpW$fpW$noS$blockB$flushP")
    require(
      bp.width == width,
      s"bitpat width is ${bp.width}, but ${width} is expected"
    )
    bp
  }
}

class UopInfoRename extends Bundle {
  // rename used
  val src1Ren = Bool()
  val src1Type = DecodeSrcType()
  val src2Ren = Bool()
  val src2Type = DecodeSrcType()
  val vlRen = Bool()
  val maskType = MaskTypeChiselEnum()
  val intRmRen = Bool()
  val readVdAsSrc = Bool()

  val gpWen = Bool()
  val fpWen = Bool()
  val vpWen = Bool()
  val vlWen = Bool()
  val vxsatWen = Bool()

  val noSpec = Bool()
  val blockBack = Bool()

  /**
   * If need alloc vd at rename stage
   */
  val vdAlloc = Bool()
}

object UopInfoRename extends InfoToString {
  lazy val width = (new UopInfoRename).getWidth

  def genBitPat(
    src1Type   : Option[OperandType],
    src2Type   : Option[OperandType],
    vlRen      : Boolean,
    maskType   : MaskType,
    intRmRen   : Boolean,
    readVdAsSrc: Boolean,
    gpWen      : Boolean,
    fpWen      : Boolean,
    vpWen      : Boolean,
    vlWen      : Boolean,
    vxsatWen   : Boolean,
    noSpec     : Boolean,
    blockBack  : Boolean,
    vdAlloc    : Boolean,
  ): BitPat = {
    val src1: String = operandTypeToString(src1Type)
    val src2: String = operandTypeToString(src2Type)
    val vlR = booleanToString(vlRen)
    val maskTy = maskType.chiselEnum.toBitPat.rawString
    val intRmR = booleanToString(intRmRen)
    val rdAsSrc = booleanToString(readVdAsSrc)
    val gpW = booleanToString(gpWen)
    val fpW = booleanToString(fpWen)
    val vpW = booleanToString(vpWen)
    val vlW = booleanToString(vlWen)
    val vxsatW = booleanToString(vxsatWen)
    val noS = booleanToString(noSpec)
    val blockB = booleanToString(blockBack)
    val vdAllocStr = booleanToString(vdAlloc)

    val bp = BitPat(s"b$src1$src2$vlR$maskTy$intRmR$rdAsSrc$gpW$fpW$vpW$vlW$vxsatW$noS$blockB$vdAllocStr")
    require(
      bp.width == width,
      s"bitpat width is ${bp.width}, but ${width} is expected"
    )
    bp
  }
}

trait InfoToString {
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
