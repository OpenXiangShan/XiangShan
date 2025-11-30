package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.{BitPat, ValidIO}
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types.SelImm
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.macros.InstanceNameMacro.{getVariableName, getVariableNameSeq}

import scala.language.implicitConversions

object SelImmField extends DecodeField[InstPattern, ValidIO[UInt]] {

  override def name: String = "selImm"

  override def chiselType: ValidIO[UInt] = ValidIO(SelImm())

  override def genTable(inst: InstPattern): BitPat = {
    val imm = inst match {
      case int: IntInstPattern => int match {
        case _: IntRTypePattern => null
        case intI: IntITypePattern => intI match {
          case IntImmInstPattern() => SelImm.I
          case IntLoadInstPattern() => SelImm.I
          case JalrPattern() => SelImm.I
          case SystemInstPattern() => SelImm.I
          case HyperLoadInstPattern() => SelImm.I
          case CSRInstPattern() => SelImm.Z
          case AmoLrInstPattern() => null
          case CboInstPattern() => SelImm.S
          case FenceInstPattern() => null
          case FenceiInstPattern() => null
        }
        case _: IntSTypePattern => SelImm.S
        case IntBTypePattern() => SelImm.SB
        case IntUTypePattern() => SelImm.U
        case IntJTypePattern() => SelImm.UJ
      }
      case fp: FpInstPattern => fp match {
        case fpI: FpITypeInstPattern => fpI match {
          case i2f: FpITypeI2fInstPattern => i2f match {
            case FpITypeImmInstPattern() => SelImm.FI
            case _ => null
          }
          case _ => null
        }
        case _ => null
      }
      case vec: VecInstPattern => vec match {
        case _: VecMemInstPattern => null
        case vecArith: VecArithInstPattern =>
          if (vecArith.category.rawString == Category.OPIVI.str) {
            if (getVariableName(VROR_VI) == vecArith.name)
              SelImm.VRORVI
            else if (unsignedImmVecInst.contains(vecArith.name))
              SelImm.OPIVIU
            else
              SelImm.OPIVIS
          } else {
            null
          }
        case vecCfg: VecConfigInstPattern =>
          if (getVariableName(VSETIVLI) == vecCfg.name)
            SelImm.VSETIVLI
          else if (getVariableName(VSETVLI) == vecCfg.name)
            SelImm.VSETVLI
          else
            null
      }
    }

    if (imm != null)
      BitPat.Y() ## imm
    else
      BitPat.N() ## BitPat.dontCare(SelImm.width)
  }

  val unsignedImmVecInst = getVariableNameSeq(
    VMSLEU_VI,
    VMSGTU_VI,
    VSADDU_VI,
  )
}
