package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.DecodePatterns.RdZeroPattern
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2}
import xiangshan.backend.vector.Decoder.Types.NumUop
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object NumUopField extends DecodeField[
  DecodePatternComb2[InstPattern, RdZeroPattern],
  UInt,
] {

  override def name: String = "numUop"

  override def chiselType: UInt = NumUop()

  override def genTable(op: DecodePatternComb2[InstPattern, RdZeroPattern]): BitPat = {
    val DecodePatternComb(instP, rdZeroP) = op
    val numUop = instP match {
      case int: IntInstPattern =>
        int match {
          case s: IntSTypePattern => s match {
            // SB, SH, SW, SD,
            case IntStoreInstPattern() => 2
            // HSV_B, HSV_H, HSV_W, HSV_D,
            case HyperStoreInstPattern() => 2
          }
          // JAL, JALR, AUIPC if rd not zero
          case inst if numWbIs2IfRdNotZeroInsts.contains(inst.name) && !rdZeroP.rdZero.get => 2
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
    (numUop - 1).U(NumUop.width.W).toBitPat
  }

  val numWbIs2IfRdNotZeroInsts: Set[String] = getVariableNameSeq(
    JAL, JALR, AUIPC,
  ).toSet
}
