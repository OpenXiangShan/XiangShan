package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import utils.NamedUInt
import xiangshan.backend.fu.vector.Bundles.{Nf, SewOH, VSew}
import xiangshan.backend.vector.Decoder.Split.{SplitType, SplitTypeOH}
import xiangshan.backend.vector.Decoder.Split.SplitType._
import xiangshan.backend.vector.Decoder.Types.EnumLMUL
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Decode.DecoderModule
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog

object UopNumUtil {
  def uopNumTimesSeg(numUopOH: UInt, nf: UInt): UInt = {
    val mod = Module(new UopNumSegDecoder)
    mod.in.nf := nf
    mod.in.numUopOH := numUopOH
    mod.out.numUopOH
  }

  def realValidForUopNumTimesSeg(numUopOH: UInt, nf: UInt): UInt = {
    val mod = Module(new UopNumSegDecoder)
    mod.in.nf := nf
    mod.in.numUopOH := numUopOH
    mod.out.validOH
  }
}

class UopNumSegDecoder extends Module with HasVectorSettings {
  val in = IO(Input(new Bundle {
    // b000: seg = 1
    // b111: seg = 8
    val nf = UInt(3.W)
    // b0001: numUop = 1
    // b0010: numUop = 2
    // b0100: numUop = 4
    // b1000: numUop = 8
    val numUopOH = NumUopOH()
  }))
  val out = IO(Output(new Bundle {
    // b0001: numUop = 1
    // b0010: numUop = 2
    // b0100: numUop = 4
    // b1000: numUop = 8
    // b0000: illegal or no input
    val numUopOH = NumUopOH()

    val validOH = UInt(8.W)
  }))

  val MuSeq = collection.mutable.Seq
  type MuSeq[T] = collection.mutable.Seq[T]

  val iUopNumIs = WireInit(VecInit.fill(9)(false.B))
  for ((i, j) <- Seq(1,2,4,8) zip (0 until 4)) {
    iUopNumIs(i) := in.numUopOH(j)
  }

  val segIs = WireInit(VecInit.fill(9)(false.B))
  for (i <- 1 to 8) {
    segIs(i) := in.nf === (i - 1).U
  }

  val uopNumOH = WireInit(VecInit.fill(9)(false.B))
  var uopNumIsNSeq: MuSeq[MuSeq[Bool]] = MuSeq.fill(9)(MuSeq())

  for (uopNum <- 1 to 8) {
    for (iUopNum <- Seq(1,2,4,8); seg <- 1 to 8) {
      if (uopNum == iUopNum * seg) {
        uopNumIsNSeq(uopNum) :+= (iUopNumIs(iUopNum) && segIs(seg))
      }
    }
    uopNumOH(uopNum) := Cat(uopNumIsNSeq(uopNum).toSeq).orR
  }

  val oNumUopIs1 = uopNumOH(1)
  val oNumUopIs2 = uopNumOH(2)
  val oNumUopIs4 = uopNumOH.asUInt(4,3).orR
  val oNumUopIs8 = uopNumOH.asUInt(8,5).orR

  out.numUopOH := Cat(oNumUopIs8, oNumUopIs4, oNumUopIs2, oNumUopIs1)
  out.validOH := Cat((1 to uopNumOH.length).map(i => Cat(uopNumOH.slice(1, i + 1).reverse).orR).reverse)

  dontTouch(segIs)
  dontTouch(uopNumOH)
  dontTouch(oNumUopIs1)
  dontTouch(oNumUopIs2)
  dontTouch(oNumUopIs4)
  dontTouch(oNumUopIs8)
}

class NewUopNumNoSegDecoderInput extends Bundle {
  val sew: UInt = VSew()
  val eew: UInt = VSew()
  val lmul: EnumLMUL.Type = EnumLMUL()
}

class NewUopNumNoSegDecoderOutput extends Bundle {
  val uopNumOH = NumUopOH()
}

/**
 * input sew, eew, lmul
 */
class UopNumNoSegDecoder extends DecoderModule (
  inB = new NewUopNumNoSegDecoderInput,
  outB = new NewUopNumNoSegDecoderOutput,
)(
  outFieldF = Seq(
    (pattern: Seq[BitPat]) => {
      val sew :: eew :: lmul :: Nil = pattern
      val iEew = Sews.decodeValue(eew)
      val dEew = Sews.decodeValue(sew)
      val dEmul: Double = Lmuls.decodeValue(lmul)
      val iEmul = dEmul * iEew / dEew
      val uopNum: Int = (1.0 max iEmul max dEmul).toInt

      BitPat("b" + (
        uopNum match {
          case 1 => "0001"
          case 2 => "0010"
          case 4 => "0100"
          case 8 => "1000"
          case _ => "0000" // illegal pattern
        }
      ))
    }
  ),
  inLegalSeq = Seq(Sews.all, Sews.all, Lmuls.all),
)

class UopNumDecoder(val splitTypeOneHot: Boolean = false) extends Module with HasVectorSettings {
  override def desiredName: String = s"UopNumDecoder_${if (splitTypeOneHot) "SplitTypeOH" else "SplitTypeNonOH"}"

  val in = IO(Input(new Bundle {
    val splitType = SplitType()
    val splitTypeOH = SplitTypeOH()
    val lmul: EnumLMUL.Type = EnumLMUL()
    val sew: UInt = VSew()
    val eew: UInt = VSew()
    val nf: UInt = Nf()
  }))
  val out = IO(Output(new Bundle {
    // b0001: numUop = 1
    // b0010: numUop = 2
    // b0100: numUop = 4
    // b1000: numUop = 8
    // b0000: illegal or no input
    val uopNumOH = NumUopOH()
  }))

  val vlsNoSegDecoder = Module(new UopNumNoSegDecoder)
  val vlsSegDecoder = Module(new UopNumSegDecoder)

  vlsNoSegDecoder.in.lmul := in.lmul
  vlsNoSegDecoder.in.sew  := in.sew
  vlsNoSegDecoder.in.eew  := in.eew

  vlsSegDecoder.in.nf := in.nf
  vlsSegDecoder.in.numUopOH := vlsNoSegDecoder.out.uopNumOH

  val lmulOH = Mux1HLookUp(in.lmul, NumUopOH.N1)(Seq(
    EnumLMUL.M2 -> NumUopOH.N2,
    EnumLMUL.M4 -> NumUopOH.N4,
    EnumLMUL.M8 -> NumUopOH.N8,
  ))

  val lmulTimes2OH = Mux1HLookUp(in.lmul, NumUopOH.N1)(Seq(
    EnumLMUL.M1 -> NumUopOH.N2,
    EnumLMUL.M2 -> NumUopOH.N4,
    EnumLMUL.M4 -> NumUopOH.N8,
  ))

  if (splitTypeOneHot) {
    out.uopNumOH := Mux1H(Seq(
      Cat(Seq(
        WVV,
        WVW,
        VVW,
        VWREDU,
        VWREDO
      ).map(_.litValue).map(i => in.splitTypeOH(i))).orR -> lmulTimes2OH,
      Cat(Seq(
        VLSIDX_DI_RATIO_1,
        VLSIDX_DI_RATIO_2,
        VLSIDX_DI_RATIO_4,
        VLSIDX_DI_RATIO_8,
        VLSIDX_DI_RATIO_F8,
        VLSIDX_DI_RATIO_F4,
        VLSIDX_DI_RATIO_F2,
      ).map(_.litValue).map(i => in.splitTypeOH(i))).orR -> vlsSegDecoder.out.numUopOH,
      Cat(Seq(
        NONE,
      ).map(_.litValue).map(i => in.splitTypeOH(i))).orR -> NumUopOH.N1,
    ))
  } else {
    out.uopNumOH := Mux1HLookUp(in.splitType, lmulOH, Seq(
      Seq(WVV, WVW, VVW, VWREDU, VWREDO) -> lmulTimes2OH,
      Seq(VLSIDX_DI_RATIO_1, VLSIDX_DI_RATIO_2, VLSIDX_DI_RATIO_4, VLSIDX_DI_RATIO_8,
        VLSIDX_DI_RATIO_F8, VLSIDX_DI_RATIO_F4, VLSIDX_DI_RATIO_F2) -> vlsSegDecoder.out.numUopOH,
      Seq(NONE) -> NumUopOH.N1,
    ))
  }

}

object Sews {
  import VSew._
  val all: Seq[UInt] = Seq(e8, e16, e32, e64).map(_.pad(3))

  def decodeValue(f: this.type => UInt): Int = {
    this.decodeValue(f(this))
  }

  def decodeValue(encode: UInt): Int = {
    require(encode.isLit)
    this.decodeValueImpl(encode.toBitPat.rawString)
  }

  def decodeValue(encode: BitPat): Int = {
    this.decodeValueImpl(encode.rawString)
  }

  def decodeValueImpl(str: String): Int = str match {
    case "00" | "000" => 8
    case "01" | "001" => 16
    case "10" | "010" => 32
    case "11" | "011" => 64
  }
}

object SewOHs {
  import SewOH._
  val all: Seq[UInt] = Seq(e8, e16, e32, e64)
}

object Lmuls {
  import xiangshan.backend.fu.vector.Bundles.VLmul._
  val all: Seq[UInt] = Seq(m1, m2, m4, m8, mf8, mf4, mf2)


  def decodeValue(f: this.type => UInt): Double = {
    this.decodeValue(f(this))
  }

  def decodeValue(encode: UInt): Double = {
    require(encode.isLit)
    this.decodeValueImpl(encode.toBitPat.rawString)
  }

  def decodeValue(encode: BitPat): Double = {
    this.decodeValueImpl(encode.rawString)
  }

  def decodeValueImpl(str: String): Double = str match {
    case "000" => 1
    case "001" => 2
    case "010" => 4
    case "011" => 8
    case "100" => 0
    case "101" => 0.125
    case "110" => 0.25
    case "111" => 0.5
  }
}

object NumUopOH extends NamedUInt(4) {
  def N0 = b"0000"
  def N1 = b"0001"
  def N2 = b"0010"
  def N4 = b"0100"
  def N8 = b"1000"

  def decodeValue(bitPat: BitPat): Int = {
    this.decodeValueImpl(bitPat.rawString)
  }

  private def decodeValueImpl(str: String): Int = {
    str match {
      case "0001" => 1
      case "0010" => 2
      case "0100" => 4
      case "1000" => 8
      case "0000" => 0
    }
  }
}

object UopNumTimesSegModuleMain extends App {
  Verilog.emitVerilog(
    new UopNumSegDecoder
  )
}

object UopNumNoSegDecoderMain extends App {
  Verilog.emitVerilog(
    new UopNumNoSegDecoder
  )
}

object UopNumDecoderMain extends App {
  Verilog.emitVerilog(
    new UopNumDecoder(splitTypeOneHot = true)
  )
  Verilog.emitVerilog(
    new UopNumDecoder(splitTypeOneHot = false)
  )
}