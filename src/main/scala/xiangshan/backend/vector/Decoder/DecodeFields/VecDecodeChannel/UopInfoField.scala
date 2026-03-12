package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util._
import xiangshan.backend.decode.opcode.Opcode.LduOpcodes._
import xiangshan.backend.decode.opcode.Opcode.StuOpcodes._
import xiangshan.backend.decode.opcode.Opcode.{Opcode, toOpcodeUtil}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.SplitTable
import xiangshan.backend.vector.Decoder.Uop.UopInfoRename
import xiangshan.backend.vector.Decoder.Uop.UopTrait.{UopBase, VecLoadUop}
import xiangshan.backend.vector.Decoder.util.DecodeField

class UopInfoField(uopIdx: Int) extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  ValidIO[UopInfoRename],
] {
  type Pattern = DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]

  override def name: String = s"uopInfo$uopIdx"

  override def chiselType: ValidIO[UopInfoRename] = ValidIO(new UopInfoRename)

  override def default: BitPat = BitPat.N() ## BitPat.dontCare(UopInfoRename.width)

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    if (UopInfoFieldVec.genUopSeq(op).isDefinedAt(uopIdx)) {
      BitPat.Y(1) ## UopInfoFieldVec.genUopSeq(op)(uopIdx).genUopInfoRenameBitPat
    } else {
      default
    }
  }
}

object UopInfoFieldVec extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  Vec[ValidIO[UopInfoRename]]
] {
  type Pattern = DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]

  override def name: String = "uopInfo"

  override def chiselType: Vec[ValidIO[UopInfoRename]] = Vec(8, ValidIO(new UopInfoRename))

  override def genTable(op: Pattern): BitPat = {
    val uopSeq: Seq[Opcode] = genUopSeq(op)

    uopSeq.map {
      x: Opcode =>
        BitPat.Y(1) ## x.genUopInfoRenameBitPat
    }.padTo(8, emptyUopBitPat).reverse.reduce(_ ## _)
  }

  val emptyUopBitPat: BitPat = BitPat.Y() ## BitPat.dontCare(UopInfoRename.width)

  def genUopSeq(op: Pattern): Seq[Opcode] = {
    this.genUopSeqImpl(op)
  }

  def genUopSeqImpl(op: Pattern): Seq[Opcode] = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op

    instP match {
      case vai: VecArithInstPattern =>
        val instBP = instP.rawInst
        vai match {
          case pattern: VecIntArithInstPattern if pattern.name.startsWith("VADD") =>
            println(s"[tmp-genUopSeqImpl] ${instP} ${sewP} ${lmulP} ${nfP}")
            println(SplitTable.table(instBP)(sewP ## lmulP))

          case _ => ()
        }
        SplitTable.table(instBP)(sewP ## lmulP)

      case vci: VecConfigInstPattern =>
        throw new IllegalArgumentException(s"inst ${vci} pattern is not supported in UopInfoField")

      case vmi: VecMemInstPattern if vmi.isInstanceOf[VecMemWhole] =>
        val seg = nfP.segNum
        val lmul = lmulP.lmulValue
        val sew = sewP.sewValue
        val eew = vmi.eewValue
        val emul = (lmul * eew / sew).max(1.0).toInt
        vmi match {
          case pattern: VecLoadInstPattern => Seq.fill(seg)(getIndexMemUop(vlnrUops, eew))
          case pattern: VecStoreInstPattern => Seq.fill(seg)(getIndexMemUop(vsnrUops, eew))
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
          case f: VecMemFF if (seg * emul <= 8) =>
            Seq.fill(seg * emul)(getIndexMemUop(vleffUops, eew))

          case stride: VecMemUnitStride if (seg * emul <= 8) =>
            vmi match {
              case pattern: VecLoadInstPattern => Seq.fill(seg * emul)(getIndexMemUop(vleUops, eew))
              case pattern: VecStoreInstPattern => Seq.fill(seg * emul)(getIndexMemUop(vseUops, eew))
            }

          case strided: VecMemStrided if (seg * emul <= 8) =>
            vmi match {
              case pattern: VecLoadInstPattern => Seq.fill(seg * emul)(getIndexMemUop(vlseUops, eew))
              case pattern: VecStoreInstPattern => Seq.fill(seg * emul)(getIndexMemUop(vsseUops, eew))
            }

          case index: VecMemIndex =>
            val dEmul = lmul
            val iEmul = emul
            val uopNum = (1.0 max iEmul max dEmul).toInt * seg
            if (iEmul >= 0.125 && uopNum <= 8) {
              vmi match {
                case _: VecLoadUnorderIndex  => Seq.fill(uopNum)(getIndexMemUop(vluxeiUops, sew, eew))
                case _: VecLoadOrderIndex    => Seq.fill(uopNum)(getIndexMemUop(vloxeiUops, sew, eew))
                case _: VecStoreUnorderIndex => Seq.fill(uopNum)(getIndexMemUop(vsuxeiUops, sew, eew))
                case _: VecStoreOrderIndex   => Seq.fill(uopNum)(getIndexMemUop(vsoxeiUops, sew, eew))
              }
            } else {
              Seq()
            }

          case _ =>
            Seq()
        }
    }
  }

  val vleUops = Seq(vle8, vle16, vle32, vle64)
  val vleffUops = Seq(vle8ff, vle16ff, vle32ff, vle64ff)
  val vseUops = Seq(vse8, vse16, vse32, vse64)
  val vlseUops = Seq(vlse8, vlse16, vlse32, vlse64)
  val vsseUops = Seq(vsse8, vsse16, vsse32, vsse64)
  val vlnrUops = Seq(vlnre8, vlnre16, vlnre32, vlnre64)
  val vsnrUops = Seq(vsnre8, vsnre16, vsnre32, vsnre64)

  val vluxeiUops: Seq[Seq[Opcode]] = Seq(
    Seq(vluxei8e8 , vluxei8e16 , vluxei8e32 , vluxei8e64 ),
    Seq(vluxei16e8, vluxei16e16, vluxei16e32, vluxei16e64),
    Seq(vluxei32e8, vluxei32e16, vluxei32e32, vluxei32e64),
    Seq(vluxei64e8, vluxei64e16, vluxei64e32, vluxei64e64),
  )

  val vloxeiUops: Seq[Seq[Opcode]] = Seq(
    Seq(vloxei8e8 , vloxei8e16 , vloxei8e32 , vloxei8e64 ),
    Seq(vloxei16e8, vloxei16e16, vloxei16e32, vloxei16e64),
    Seq(vloxei32e8, vloxei32e16, vloxei32e32, vloxei32e64),
    Seq(vloxei64e8, vloxei64e16, vloxei64e32, vloxei64e64),
  )

  val vsuxeiUops: Seq[Seq[Opcode]] = Seq(
    Seq(vsuxei8e8 , vsuxei8e16 , vsuxei8e32 , vsuxei8e64 ),
    Seq(vsuxei16e8, vsuxei16e16, vsuxei16e32, vsuxei16e64),
    Seq(vsuxei32e8, vsuxei32e16, vsuxei32e32, vsuxei32e64),
    Seq(vsuxei64e8, vsuxei64e16, vsuxei64e32, vsuxei64e64),
  )

  val vsoxeiUops: Seq[Seq[Opcode]] = Seq(
    Seq(vsoxei8e8 , vsoxei8e16 , vsoxei8e32 , vsoxei8e64 ),
    Seq(vsoxei16e8, vsoxei16e16, vsoxei16e32, vsoxei16e64),
    Seq(vsoxei32e8, vsoxei32e16, vsoxei32e32, vsoxei32e64),
    Seq(vsoxei64e8, vsoxei64e16, vsoxei64e32, vsoxei64e64),
  )

  def getIndexMemUop(uops: Seq[Seq[Opcode]], eew: Int, sew: Int) = {
    val i = getI(eew)
    val j = getI(sew)

    uops(i)(j)
  }

  def getIndexMemUop(uops: Seq[Opcode], eew: Int) = {
    val i = getI(eew)

    uops(i)
  }

  private def getI(width: Int) = width match {
    case 8 => 0
    case 16 => 1
    case 32 => 2
    case 64 => 3
  }
}
