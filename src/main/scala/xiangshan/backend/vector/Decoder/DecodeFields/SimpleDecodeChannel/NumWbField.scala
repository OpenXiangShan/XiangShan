package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.DecodePatterns.RdZeroPattern
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2}
import xiangshan.backend.vector.Decoder.Types.NumWB
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object NumWbField extends DecodeField[InstPattern, UInt] {

  override def name: String = "numWb"

  override def chiselType: UInt = NumWB()

  override def genTable(instP: InstPattern): BitPat = {
    val numWb = instP match {
      case int: IntInstPattern =>
        int match {
          case s: IntSTypePattern => s match {
            // SB, SH, SW, SD,
            case IntStoreInstPattern() => 2
            // HSV_B, HSV_H, HSV_W, HSV_D,
            case HyperStoreInstPattern() => 2
          }
          // JAL, JALR, AUIPC if rd not zero (RD==0 is in PseudoDecodeChannel, not here)
          case inst if numWbIs2IfRdNotZeroInsts.contains(inst.name) => 2
          case _ => 1
        }
      case fp: FpInstPattern =>
        fp match {
          // FSH, FSW, FSD,
          case FpSTypeInstPattern() => 2
          case _ => 1
        }
      case _: VecInstPattern =>
        throw new IllegalArgumentException("VecInstPattern is not supported in NumUopField")
    }
    (numWb - 1).U(NumWB.width.W).toBitPat
  }

  val numWbIs2IfRdNotZeroInsts: Set[String] = getVariableNameSeq(
    JAL, JALR,
  ).toSet
}
