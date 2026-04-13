package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.{BitPat, ValidIO}
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.macros.InstanceNameMacro.{getVariableName, getVariableNameSeq}

import scala.language.implicitConversions

object SelImmField extends DecodeField[InstPattern, ValidIO[UInt]] {

  override def name: String = "selImm"

  override def chiselType: ValidIO[UInt] = ValidIO(DecodeSelImm())

  override def genTable(inst: InstPattern): BitPat = {
    val imm = inst match {
      case int: IntInstPattern => int match {
        case _: IntRTypePattern => null
        case intI: IntITypePattern => intI match {
          case IntImmInstPattern() => DecodeSelImm.I
          case IntLoadInstPattern() => DecodeSelImm.I
          case JalrPattern() => DecodeSelImm.I
          case SystemInstPattern() => DecodeSelImm.I
          case HyperLoadInstPattern() => null
          case CSRInstPattern() => DecodeSelImm.Z
          case AmoLrInstPattern() => null
          case CboInstPattern() => DecodeSelImm.S
          case FenceInstPattern() => null
          case FenceiInstPattern() => null
          case CustomTrapPattern() => null
        }
        case intS: IntSTypePattern => intS match {
          case HyperStoreInstPattern() => null
          case _ => DecodeSelImm.S
        }
        case IntBTypePattern() => DecodeSelImm.SB
        case IntUTypePattern() => DecodeSelImm.U
        case IntJTypePattern() => DecodeSelImm.UJ
      }
      case fp: FpInstPattern => fp match {
        case fpI: FpITypeInstPattern => fpI match {
          case i2f: FpITypeI2fInstPattern => i2f match {
            case FpITypeImmInstPattern() => DecodeSelImm.FI
            case _ => null
          }
          case FpITypeLoadInstPattern() => DecodeSelImm.I
          case _ => null
        }
        case FpSTypeInstPattern() => DecodeSelImm.S
        case _ => null
      }
      case vec: VecInstPattern => vec match {
        case _: VecMemInstPattern => null
        case vecArith: VecArithInstPattern =>
          if (vecArith.category.rawString == Category.OPIVI.str) {
            if (getVariableName(VROR_VI) == vecArith.name)
              DecodeSelImm.VRORVI
            else if (unsignedImmVecInst.contains(vecArith.name))
              DecodeSelImm.OPIVIU
            else
              DecodeSelImm.OPIVIS
          } else {
            null
          }
        case vecCfg: VecConfigInstPattern =>
          if (getVariableName(VSETIVLI) == vecCfg.name)
            DecodeSelImm.VSETIVLI
          else if (getVariableName(VSETVLI) == vecCfg.name)
            DecodeSelImm.VSETVLI
          else
            null
      }
    }

    if (imm != null)
      BitPat.Y() ## imm
    else
      BitPat.N() ## BitPat.dontCare(DecodeSelImm.width)
  }

  val unsignedImmVecInst = getVariableNameSeq(
  )
}
