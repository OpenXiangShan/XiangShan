package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.SplitTable
import xiangshan.backend.vector.Decoder.Uop.UopInfoRename
import xiangshan.backend.vector.Decoder.Uop.UopTrait.UopBase
import xiangshan.backend.vector.Decoder.Uop.VecUopDefines._
import xiangshan.backend.vector.Decoder.util.DecodeField

object UopInfoField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  Vec[ValidIO[UopInfoRename]]
] {
  type Pattern = DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]

  override def name: String = "uopInfo"

  override def chiselType: Vec[ValidIO[UopInfoRename]] = Vec(8, ValidIO(new UopInfoRename))

  override def genTable(op: Pattern): BitPat = {
    val uopSeq: Seq[UopBase] = genUopSeq(op)

    uopSeq.map {
      x: UopBase =>
        BitPat.Y(1) ## x.genUopInfoRenameBitPat
    }.padTo(8, emptyUopBitPat).reverse.reduce(_ ## _)
  }

  val emptyUopBitPat: BitPat = BitPat.Y() ## BitPat.dontCare(UopInfoRename.width)

  private val buffer = collection.mutable.Map[Pattern, Seq[UopBase]]()

  def genUopSeq(op: Pattern): Seq[UopBase] = {
    buffer.getOrElseUpdate(op, this.genUopSeqImpl(op))
  }

  def genUopSeqImpl(op: Pattern): Seq[UopBase] = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op

    instP match {
      case vai: VecArithInstPattern =>
        val instBP = instP.rawInst
        SplitTable.table(instBP)(lmulP)

      case vci: VecConfigInstPattern =>
        throw new IllegalArgumentException(s"inst ${vci} pattern is not supported in UopInfoField")

      case vmi: VecMemInstPattern if vmi.isInstanceOf[VecMemWhole] =>
        val seg = NfPattern(vmi.nf).segNum
        vmi match {
          case pattern: VecLoadInstPattern => Seq.fill(seg)(vlnr)
          case pattern: VecStoreInstPattern => Seq.fill(seg)(vsnr)
        }

      case vmi: VecMemInstPattern if vmi.isInstanceOf[VecMemMask] =>
        vmi match {
          case pattern: VecLoadInstPattern => Seq(vlm)
          case pattern: VecStoreInstPattern => Seq(vsm)
        }

      case vmi: VecMemInstPattern =>
        val seg = nfP.segNum
        val lmul = lmulP.lmulValue
        val sew = sewP.sewValue
        val eew = vmi.eewValue
        val emul = (lmul * eew / sew).max(1.0).toInt

        vmi.asInstanceOf[VecMemTrait] match {
          case stride: VecMemUnitStride if (seg * emul <= 8) =>
            vmi match {
              case pattern: VecLoadInstPattern => Seq.fill(seg * emul)(vle)
              case pattern: VecStoreInstPattern => Seq.fill(seg * emul)(vse)
            }

          case strided: VecMemStrided if (seg * emul <= 8) =>
            vmi match {
              case pattern: VecLoadInstPattern => Seq.fill(seg * emul)(vlse)
              case pattern: VecStoreInstPattern => Seq.fill(seg * emul)(vsse)
            }

          case index: VecMemIndex =>
            val dEmul = lmul
            val iEmul = emul
            val uopNum = (1.0 max iEmul max dEmul).toInt * seg
            if (iEmul >= 0.125 && uopNum <= 8) {
              vmi match {
                case _: VecLoadUnorderIndex  => Seq.fill(uopNum)(vluxe)
                case _: VecLoadOrderIndex    => Seq.fill(uopNum)(vloxe)
                case _: VecStoreUnorderIndex => Seq.fill(uopNum)(vsuxe)
                case _: VecStoreOrderIndex   => Seq.fill(uopNum)(vsoxe)
              }
            } else {
              Seq()
            }

          case f: VecMemFF if (seg * emul <= 8) =>
            Seq.fill(seg * emul)(vleff)

          case _ =>
            Seq()
        }
    }
  }
}
