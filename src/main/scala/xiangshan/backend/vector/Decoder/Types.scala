package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util.BitPat
import org.chipsalliance.cde.config.{Parameters => P}
import utils.NamedUInt
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.BString._
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.{NamedDef, NamedDefObj}

object Types {
  class DecodeSrcType extends Bundle {
    val value = UInt(DecodeSrcType.width.W)
  }

  object DecodeSrcType {
    def width = 2

    def apply(): DecodeSrcType = new DecodeSrcType()

    def IMM = b"00"
    def NO  = b"00"
    def GP  = b"01"
    def FP  = b"10"
    def VP  = b"11"
  }

  object SelImm extends NamedUInt(4) {
    def S  = b"0000"
    def SB = b"0001"
    def U  = b"0010"
    def UJ = b"0011"
    def I  = b"0100"
    def Z  = b"0101"
    def B6 = b"0110"
    def LUI32     = b"0111"
    def OPIVIS    = b"1000"
    def OPIVIU    = b"1001"
    def VSETVLI   = b"1010"
    def VSETIVLI  = b"1011"
    def VRORVI    = b"1100"
    def FI        = b"1111" // used by fli and it's fusion
  }

  class SplitSrcInfo(implicit val p: P) extends VecBundle {
    // if this src is used
    val valid = Bool()
    val lreg  = Lreg()
    val widthTy = new SrcWidthType

    def isWidth = this.widthTy.isWiden
    def notWidth = this.widthTy.notWiden
    def isExtF2 = this.widthTy.isExtF2
    def isExtF4 = this.widthTy.isExtF4
    def isExtF8 = this.widthTy.isExtF8
    def notExtF = this.widthTy.notExtF
  }

  object EnumLMUL extends ChiselEnum {
    val M1 = Value(b"000")
    val M2 = Value(b"001")
    val M4 = Value(b"010")
    val M8 = Value(b"011")
    val ILL = Value(b"100")
    val MF8 = Value(b"101")
    val MF4 = Value(b"110")
    val MF2 = Value(b"111")

    def mapToRegs(lmul: this.Type, seg: Int = 1)(times: Int = 1): UInt = {
      val ms = Seq(M2, M4, M8)
      val regs = Seq(2, 4, 8)

      Mux1HLookUp(lmul, 1.U)(Seq(
        M2 -> 2.U,
        M4 -> 4.U,
        M8 -> 8.U,
      ))
    }
  }

  class SrcWidthType extends Bundle {
    import SrcWidthType._

    private val value = UInt(2.W)

    def isWiden  = this.value(0) === WIDEN(0)
    def notWiden = this.value(0) === NORMAL(0)

    def isExtF2 = this.value === F2
    def isExtF4 = this.value === F4
    def isExtF8 = this.value === F8
    def notExtF = this.value === NORMAL
  }

  object SrcWidthType {
    // NOTICE: widen type will be never conflict with f8, since the former is used by vext uop while the latter is used
    // by other widen uops.
    private def NORMAL = b"00"  // EEW = SEW
    private def WIDEN  = b"01"  // EEW = SEW*2
    private def F8 = b"01"      // EEW = SEW/8
    private def F4 = b"10"      // EEW = SEW/4
    private def F2 = b"11"      // EEW = SEW/2
  }

  class MaskInfo {
    val maskType = MaskTypeChiselEnum()
    val maskSel = UInt(2.W)
  }

  abstract class MaskType(val chiselEnum: MaskTypeChiselEnum.Type) {

  }

  case object DestMask extends MaskType(MaskTypeChiselEnum.DestMask)
  case object Src2Mask extends MaskType(MaskTypeChiselEnum.Src2Mask)
  case object Src12Mask extends MaskType(MaskTypeChiselEnum.Src12Mask)
  case object NoMask extends MaskType(MaskTypeChiselEnum.DestMask)

  object MaskTypeChiselEnum extends ChiselEnum {
    val DestMask  = Value(b"00") // normal situation
    val Src2Mask  = Value(b"10") // src 1 use mask, mask index will be uopIdx or `uopIdx / 2`
    val Src12Mask = Value(b"11") // src 1 and 2 use mask, mask index of src2 will be lmul - 1

    // E.g. REDU
    //  uop_vv (r.vd, r.vs2 | 0.U, r.vs2 | 7.U ), Src12Mask
    //  uop_vv (r.vd, r.vs2 | 1.U, r.vd        ), Src1Mask
    //  uop_vv (r.vd, r.vs2 | 2.U, r.vd        ), Src1Mask
    //  uop_vv (r.vd, r.vs2 | 3.U, r.vd        ), Src1Mask
    //  uop_vv (r.vd, r.vs2 | 4.U, r.vd        ), Src1Mask
    //  uop_vv (r.vd, r.vs2 | 5.U, r.vd        ), Src1Mask
    //  uop_vv (r.vd, r.vs2 | 6.U, r.vd        ), Src1Mask
    //  uop_red(r.vd, r.vs1      , r.vd        ),
  }

  class MaskSel extends NamedDef(2) {
    def Mask0 = b"00"
    def Mask1 = b"01"
    def Mask3 = b"10"
    def Mask7 = b"11"
  }

  object MaskSel extends NamedDefObj(new MaskSel)

  object Operand extends Enumeration {
    val IMM = Value
    val GP = Value
    val FP = Value
    val VP = Value
  }

  object Sign extends Enumeration {
    val S = Value
    val U = Value
  }

  type SignType = Sign.Value
  type OperandType = Operand.Value

  object OperandType {
    def genBitPat(typ: Option[OperandType]): BitPat = {
      typ match {
        case None => BitPat.dontCare(DecodeSrcType.width)
        case Some(t) => t match {
          case Operand.IMM => DecodeSrcType.IMM.toBitPat
          case Operand.GP  => DecodeSrcType.GP.toBitPat
          case Operand.FP  => DecodeSrcType.FP.toBitPat
          case Operand.VP  => DecodeSrcType.VP.toBitPat
        }
      }
    }
  }

  object UopBufferNum extends NamedUInt(3)
}