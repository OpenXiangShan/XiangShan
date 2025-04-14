package xiangshan.backend.vector.util

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utils.SeqUtils.Seq2

object Decode {
  abstract class DecodeModuleBase extends Module {
    type Pattern <: DecodePattern

    val allPatterns: Seq[Pattern]

    val allFields: Seq[_ <: DecodeField[Pattern, _ <: Data]]

    val decodeTable = new DecodeTable(allPatterns, allFields)

    val decodeResult = decodeTable.decode(this.makeDecodeInput)

    def makeDecodeInput: UInt
  }

  abstract class DecoderModule[IB <: Bundle, OB <: Bundle](
    inB: IB,
    outB: OB,
  )(
    inLegalSeq: Seq[Seq[Bits]] = Seq(),
    outFieldF : Seq[Seq[BitPat] => BitPat],
    inPatterns: Seq[Seq[BitPat]] = Seq(),
  ) extends Module {
    require(inLegalSeq.forall(_.forall(_.asInstanceOf[Data].isLit)))
    require(inLegalSeq.nonEmpty ^ inPatterns.nonEmpty)
    require(outB.elements.size == outFieldF.size)
    val in = IO(Input(inB))
    val out = IO(Output(outB))

    val allDecodeFields: Seq[OutField] = outFieldF.zipWithIndex.map{case (f, i) =>  new OutField(f, i)}
    val allBitPats: Seq[Seq[BitPat]] =
      if (inPatterns.nonEmpty)
        inPatterns
      else
        SeqProduct(inLegalSeq).map(_.map(x => BitPat(x.asUInt)))

    val patterns = allBitPats.map(InPattern)

    val decodeTable = new DecodeTable(patterns, allDecodeFields)

    val decodeResult: DecodeBundle = decodeTable.decode(in.asUInt)

    (out.getElements zip allDecodeFields).foreach { case (o, field) =>
      o := decodeResult(field)
    }

    def SeqProduct[T](seq2: Seq2[T]): Seq[Seq[T]] = {
      seq2.foldLeft[Seq[Seq[T]]](Seq())(SeqProductFold)
    }

    def SeqProductFold[T](seqA: Seq[Seq[T]], seqB: Seq[T]): Seq[Seq[T]] = {
      if (seqA.isEmpty)
        return seqB.map(b => Seq(b))
      for {
        a: Seq[T] <- seqA
        b <- seqB
      } yield {
        a :+ b
      }
    }

    case class InPattern(in: Seq[BitPat]) extends DecodePattern {
      override def bitPat: BitPat = in.reduce(_ ## _)
    }

    class OutField(val f: Seq[BitPat] => BitPat, i: Int) extends DecodeField[InPattern, Data] {
      override def name: String = "out"

      override def chiselType: Data = out.getElements(i).cloneType

      override def genTable(inP: InPattern): BitPat = {
        this.f(inP.in)
      }
    }
  }
}
