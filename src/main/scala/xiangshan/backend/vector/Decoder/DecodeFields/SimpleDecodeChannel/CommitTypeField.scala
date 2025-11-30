package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.CommitType
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object CommitTypeField extends DecodeField[InstPattern, UInt] {
  override def name: String = "commitType"

  override def chiselType: UInt = CommitType()

  override def genTable(op: InstPattern): BitPat = {
    val uint: UInt = op match {
      case intInst: IntInstPattern => intInst match {
        case _: IntRTypePattern => CommitType.NORMAL
        case iType: IntITypePattern => iType match {
          case IntLoadInstPattern() => CommitType.LOAD
          case HyperLoadInstPattern() => CommitType.LOAD
          case _ => CommitType.NORMAL
        }
        case sType: IntSTypePattern => sType match {
          case IntStoreInstPattern() => CommitType.STORE
          case HyperStoreInstPattern() => CommitType.STORE
        }
        case IntBTypePattern() => CommitType.BRANCH
        case IntUTypePattern() => CommitType.NORMAL
        case IntJTypePattern() => CommitType.BRANCH
      }
      case fpInst: FpInstPattern => fpInst match {
        case fpIType: FpITypeInstPattern => fpIType match {
          case FpITypeLoadInstPattern() => CommitType.LOAD
          case _ => CommitType.NORMAL
        }
        case FpSTypeInstPattern() => CommitType.STORE
        case _ => CommitType.NORMAL
      }
      case pattern: VecInstPattern => pattern match {
        case vmi: VecMemInstPattern => vmi match {
          case _: VecLoadInstPattern => CommitType.LOAD
          case _: VecStoreInstPattern => CommitType.STORE
        }
        case _ => CommitType.NORMAL
      }
    }
    uint.toBitPat
  }
}
