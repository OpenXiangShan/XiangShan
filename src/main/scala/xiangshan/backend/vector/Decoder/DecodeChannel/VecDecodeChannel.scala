package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.decode.isa.Instructions
import xiangshan.{CommitType, FuOpType}
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.{VLmul, VSew, VType}
import xiangshan.backend.vector.Decoder.DecodeChannel.SplitCtlDecoderUtil.InstSewLmulNfPattern
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{UopInfoField, _}
import xiangshan.backend.vector.Decoder.DecodeFields._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.{SplitType, SplitTypeOH}
import xiangshan.backend.vector.Decoder.Types.{EnumLMUL, NumWB, VdDepElim}
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector.Decoder.{util, _}
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog
import xiangshan.macros.InstanceNameMacro.getVariableName

object SplitCtlDecoderUtil {
  case class SplitTypeOHPattern(splitTypeOH: BitPat) extends DecodePattern {
    override def bitPat: BitPat = splitTypeOH

    lazy val value: Int = splitTypeOH.value.bitLength - 1
  }

  object SplitTypeOHPattern {
    def apply(splitTypeOH: SplitTypeOH.Type): SplitTypeOHPattern = new SplitTypeOHPattern(splitTypeOH.toBitPat)

    def apply(splitType: SplitType.Type): SplitTypeOHPattern = new SplitTypeOHPattern(
      new BitPat(
        value = BigInt(1) << splitType.litValue.toInt,
        mask = (BigInt(1) << (SplitType.maxValue + 1)) - 1,
        width = SplitType.maxValue + 1,
      )
    )
  }
  
  type UopLmulNfSplitOHPattern = DecodePatternComb3[LmulPattern, NfPattern, SplitTypeOHPattern]

  object UopLmulNfSplitOHPattern {
    def apply(lmul: LmulPattern, nf: NfPattern, splitTypeOH: SplitTypeOHPattern): DecodePatternComb3[LmulPattern, NfPattern, SplitTypeOHPattern] = {
      lmul ## nf ## splitTypeOH
    }

    def unapply(arg: UopLmulNfSplitOHPattern): Option[Tuple3[LmulPattern, NfPattern, SplitTypeOHPattern]] = {
      Some(Tuple3(arg.p1, arg.p2, arg.p3))
    }
  }

  type InstSewLmulNfPattern = DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]

  object InstSewLmulNfPattern {
    type Type = InstSewLmulNfPattern

    def apply(
      inst: VecInstPattern,
      sew: SewPattern,
      lmul: LmulPattern,
      nf: NfPattern,
    ): DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern] = inst ## sew ## lmul ## nf

    def unapply(arg: Type): Option[(VecInstPattern, SewPattern, LmulPattern, NfPattern)] = {
      Some((
        arg.p1,
        arg.p2,
        arg.p3,
        arg.p4,
      ))
    }
  }
}

class UopSrcBundle extends Bundle {
  val src1 = UInt(5.W)
  val src2 = UInt(5.W)
  val dest = UInt(5.W)
}
