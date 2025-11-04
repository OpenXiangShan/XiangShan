package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodePattern
import xiangshan.backend.vector.Decoder.Split.SplitType
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.Decoder.InstPattern._

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object RVVDecodeUtil {
  class DecodePatternComb2[
    P1 <: DecodePattern,
    P2 <: DecodePattern,
  ](
    val p1: P1,
    val p2: P2,
  ) extends DecodePattern {
    override def bitPat: BitPat = p1.bitPat ## p2.bitPat

    def ##[T <: DecodePattern](that: T): DecodePatternComb3[P1, P2, T] = {
      new DecodePatternComb3[P1, P2, T](p1, p2, that)
    }
  }

  class DecodePatternComb3[
    P1 <: DecodePattern,
    P2 <: DecodePattern,
    P3 <: DecodePattern,
  ](
    val p1: P1,
    val p2: P2,
    val p3: P3,
  ) extends DecodePattern {
    override def bitPat: BitPat = p1.bitPat ## p2.bitPat ## p3.bitPat

    def ##[T <: DecodePattern](that: T): DecodePatternComb4[P1, P2, P3, T] = {
      new DecodePatternComb4[P1, P2, P3, T](p1, p2, p3, that)
    }
  }

  class DecodePatternComb4[
    P1 <: DecodePattern,
    P2 <: DecodePattern,
    P3 <: DecodePattern,
    P4 <: DecodePattern,
  ](
    val p1: P1,
    val p2: P2,
    val p3: P3,
    val p4: P4,
  ) extends DecodePattern {
    override def bitPat: BitPat = p1.bitPat ## p2.bitPat ## p3.bitPat ## p4.bitPat
    def ##[T <: DecodePattern](that: T): DecodePatternComb5[P1, P2, P3, P4, T] = {
      new DecodePatternComb5[P1, P2, P3, P4, T](p1, p2, p3, p4, that)
    }
  }

  class DecodePatternComb5[
    P1 <: DecodePattern,
    P2 <: DecodePattern,
    P3 <: DecodePattern,
    P4 <: DecodePattern,
    P5 <: DecodePattern,
  ](
    val p1: P1,
    val p2: P2,
    val p3: P3,
    val p4: P4,
    val p5: P5,
  ) extends DecodePattern {
    override def bitPat: BitPat = p1.bitPat ## p2.bitPat ## p3.bitPat ## p4.bitPat ## p5.bitPat
  }

  object DecodePatternComb2 {
    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
    ](arg: DecodePatternComb2[P1, P2]): Option[(P1, P2)] = Some((arg.p1, arg.p2))
  }

  object DecodePatternComb3 {
    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
      P3 <: DecodePattern,
    ](arg: DecodePatternComb3[P1, P2, P3]): Option[(P1, P2, P3)] = Some((arg.p1, arg.p2, arg.p3))
  }

  object DecodePatternComb4 {
    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
      P3 <: DecodePattern,
      P4 <: DecodePattern,
    ](arg: DecodePatternComb4[P1, P2, P3, P4]): Option[(P1, P2, P3, P4)] = Some((arg.p1, arg.p2, arg.p3, arg.p4))
  }

  object DecodePatternComb5 {
    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
      P3 <: DecodePattern,
      P4 <: DecodePattern,
      P5 <: DecodePattern,
    ](arg: DecodePatternComb5[P1, P2, P3, P4, P5]): Option[(P1, P2, P3, P4, P5)] =
      Some((arg.p1, arg.p2, arg.p3, arg.p4, arg.p5))
  }

  object DecodePatternComb {
    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
    ](arg: DecodePatternComb2[P1, P2]): Option[(P1, P2)] = DecodePatternComb2.unapply(arg)

    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
      P3 <: DecodePattern,
    ](arg: DecodePatternComb3[P1, P2, P3]): Option[(P1, P2, P3)] = DecodePatternComb3.unapply(arg)

    def unapply[
      P1 <: DecodePattern,
      P2 <: DecodePattern,
      P3 <: DecodePattern,
      P4 <: DecodePattern,
    ](arg: DecodePatternComb4[P1, P2, P3, P4]): Option[(P1, P2, P3, P4)] = DecodePatternComb4.unapply(arg)
  }

  case class LmulPattern(
    vlmul: BitPat
  ) extends DecodePattern {
    def bitPat: BitPat = vlmul

    def lmulValue: Double = {
      vlmul.rawString match {
        case "000" => 1
        case "001" => 2
        case "010" => 4
        case "011" => 8
        case "101" => 0.125
        case "110" => 0.25
        case "111" => 0.5
        case "100" => 0
      }
    }

    def dumpString: String = {
      val str = vlmul.rawString match {
        case "000" => "m1"
        case "001" => "m2"
        case "010" => "m4"
        case "011" => "m8"
        case "101" => "mf8"
        case "110" => "mf4"
        case "111" => "mf2"
        case _     => "???"
      }
      f"$str%3s"
    }
  }

  object LmulPattern {
    def apply(uint: UInt): LmulPattern = new LmulPattern(uint.toBitPat)

    def apply(lmul: Double): LmulPattern = {
      val bpStr = lmul match {
        case 1 => "000"
        case 2 => "001"
        case 4 => "010"
        case 8 => "011"
        case 0.125 => "101"
        case 0.25 => "110"
        case 0.5 => "111"
      }
      new LmulPattern(BitPat("b" + bpStr))
    }

    def dontCare: LmulPattern = LmulPattern(BitPat.dontCare(3))

    def all: Seq[LmulPattern] = {
      Lmuls.all.map(LmulPattern.apply)
    }
  }

  case class NfPattern(bitpat: BitPat) extends DecodePattern {
    override def bitPat: BitPat = bitpat

    def segNum: Int = bitpat.rawString match {
      case "000" => 1
      case "001" => 2
      case "010" => 3
      case "011" => 4
      case "100" => 5
      case "101" => 6
      case "110" => 7
      case "111" => 8
      case _ => 0
    }

    def dumpString: String = {
      if (this.segNum == 0)
        "????"
      else
        f"seg${this.segNum}"
    }
  }

  object NfPattern {
    def apply(uint: UInt): NfPattern = NfPattern(uint.toBitPat)

    def dontCare: NfPattern = NfPattern(BitPat.dontCare(3))

    def all: Seq[NfPattern] = Seq(
      BitPat("b000"),
      BitPat("b001"),
      BitPat("b010"),
      BitPat("b011"),
      BitPat("b100"),
      BitPat("b101"),
      BitPat("b110"),
      BitPat("b111"),
    ).map(NfPattern.apply)
  }

  case class SewPattern(bitpat: BitPat) extends DecodePattern {

    override def bitPat: BitPat = bitpat

    def sewValue: Int = bitpat.rawString match {
      case "000" => 8
      case "001" => 16
      case "010" => 32
      case "011" => 64
      case _ => 0
    }

    def dumpString: String = {
      if (this.sewValue == 0)
        "???"
      else
        f"${"e" + this.sewValue.toString}%3s"
    }
  }

  object SewPattern {
    def apply(uint: UInt): SewPattern = SewPattern(uint.toBitPat)

    def apply(int: Int): SewPattern = {
      val bpStr = int match {
        case 8  => "000"
        case 16 => "001"
        case 32 => "010"
        case 64 => "011"
      }
      SewPattern(BitPat("b" + bpStr))
    }

    def dontCare: SewPattern = SewPattern(BitPat.dontCare(3))

    def all: Seq[SewPattern] = Sews.all.map(SewPattern.apply)
  }

  case class UopNumOHsPattern(
    uopNumSeq: Seq[Int],
    mopWidth : Int,
  ) extends DecodePattern {
    override def bitPat: BitPat = {
      this.uopNumSeq.map(x => genBitPat(x))
        .padTo(mopWidth, BitPat.dontCare(4))
        .ensuring(_.forall(_.getWidth == 4))
        .reverse
        .reduce(_ ## _)
    }

    private def genBitPat(x: Int): BitPat = {
      if (x == 0b1111)
        BitPat.dontCare(4)
      else
        x.U(4.W).toBitPat
    }
  }

  object UopNumOHsPatterns {
    def simplify(patterns: Seq[UopNumOHsPattern]): Seq[UopNumOHsPattern] = {
      val res = ArrayBuffer.empty[UopNumOHsPattern]
      val tail0Indices: Seq[Int] = patterns.zipWithIndex.collect { case (x: UopNumOHsPattern, idx: Int) if x.uopNumSeq.last == 0 => idx }
      res ++= patterns.slice(0, tail0Indices.head)
      for ((idx, i) <- tail0Indices.zipWithIndex) {
        // i is the index of tail0Indices
        val needCompress =
          patterns.isDefinedAt(idx + 1) && patterns(idx + 1).uopNumSeq.last == 1 &&
          patterns.isDefinedAt(idx + 2) && patterns(idx + 2).uopNumSeq.last == 2 &&
          patterns.isDefinedAt(idx + 3) && patterns(idx + 3).uopNumSeq.last == 4 &&
          patterns.isDefinedAt(idx + 4) && patterns(idx + 4).uopNumSeq.last == 8
        if (needCompress) {
          res += patterns(idx).copy(uopNumSeq = patterns(idx).uopNumSeq.dropRight(1) :+ 0b1111)
          res ++= patterns.slice(
            if (tail0Indices.isDefinedAt(i + 1)) idx + 5 else patterns.length,
            if (tail0Indices.isDefinedAt(i + 1)) tail0Indices(i + 1) else patterns.length
          )
        } else {
          res ++= patterns.slice(idx, if (tail0Indices.isDefinedAt(i + 1)) tail0Indices(i + 1) else patterns.length)
        }
      }
      if (res.last.uopNumSeq.last == 0b1111) {
        res ++= patterns.slice(tail0Indices.last + 5, patterns.length)
      }
      res.toSeq
    }
  }


  case class RVVInstWithConfigPattern(
    instPattern: VecInstPattern,
    sew: BitPat,
  ) extends DecodePattern {
    require(sew.getWidth == 3)

    override def bitPat: BitPat = instPattern.bitPat ## sew

    def name = instPattern.getName()
  }

  case class UopLmulNfSplitPattern(
    lmul      : BitPat,
    nf        : BitPat,
    splitType : BitPat,
  ) extends DecodePattern {
    require(splitType.getWidth == SplitType.getWidth)
    require(lmul.getWidth == Lmuls.all.head.getWidth)

    override def bitPat: BitPat = lmul ## nf ## splitType
  }

  class SrcNumSelect extends Bundle {
    // useVs1/Vs2/Vd should be OH or all 0s
    val useVs2  = Bool()
    val useVs1  = Bool()
    val useVd   = Bool()
    val bias = UInt(3.W) // hold 0~7
  }

  object SrcNumSelect {
    def genBitPat(select: this.type => RegSelect, bias: Int): BitPat = {
      (select(this) match {
        case VS1 => BitPat("b010")
        case VS2 => BitPat("b100")
        case VD => BitPat("b001")
        case NO => BitPat("b000")
      }) ## BitPat(bias.U(3.W))
    }

    sealed abstract class RegSelect
    case object VS1 extends RegSelect
    case object VS2 extends RegSelect
    case object VD extends RegSelect
    case object NO extends RegSelect
  }

  class SrcDestSelect extends Bundle {
    val src1 = new SrcNumSelect
    val src2 = new SrcNumSelect
    val dest = new SrcNumSelect
  }

  object SrcDestSelect {
    lazy val width = (new SrcDestSelect).getWidth
  }
}

