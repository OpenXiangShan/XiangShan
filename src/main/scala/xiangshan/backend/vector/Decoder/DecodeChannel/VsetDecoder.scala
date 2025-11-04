package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import chisel3.util.experimental.decode._
import freechips.rocketchip.rocket.Instructions
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2, DecodePatternComb5, LmulPattern, SewPattern}
import xiangshan.backend.vector.Decoder.Uop.UopTrait.UopBase
import xiangshan.backend.vector.Decoder.Uop.{UopInfoRename, VecUopDefines}
import xiangshan.backend.vector.Decoder.{Lmuls, Sews}
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._
import xiangshan.backend.vector.util.Verilog

class VsetDecoder extends Module {
  import VsetDecoderUtil._

  val in = IO(Input(new Bundle {
    val rawInst = UInt(32.W)
  }))
  val out = IO(Output(new Bundle {
    val renameInfo = ValidIO(new UopInfoRename)
    val src = new UopSrcBundle
    val isVSETVL = Bool()
  }))

  val rawInst = in.rawInst
  val instFields = rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val isVSETVLI = rawInst(31) === b"0"
  val isVSETIVLI = rawInst(31, 30) === b"11"
  val isVSETVL = rawInst(31, 25) === b"1000000"

  val vsetvliVill = instFields.ZIMM_VSETVLI.drop(8) =/= 0.U
  val vsetivliVill = instFields.ZIMM_VSETIVLI.drop(8) =/= 0.U

  val rs1IsZero = instFields.RS1 === 0.U
  val rdIsZero = instFields.RD === 0.U

  val sewLmulPatterns: Seq[DecodePatternComb2[SewPattern, LmulPattern]] = for {
    sew <- SewPattern.all
    lmul <- LmulPattern.all
  } yield {
    sew ## lmul
  }

  val sewLmulDecodeTable = new DecodeTable(sewLmulPatterns, Seq(SewLmulIllegalField))

  val sewLmulDecodeBundle = sewLmulDecodeTable.decode(instFields.ZIMM_VSETVLI.take(6))

  val sewLmulIllegal = sewLmulDecodeBundle(SewLmulIllegalField)

  val uop = Wire(new UopInfoRename)

  uop := Mux1H(Seq(
    isVSETVL -> Mux1H(Seq(
      (rdIsZero && rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypex_vll.genUopInfoRenameBitPat),
      (!rdIsZero && rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypex_vlmax.genUopInfoRenameBitPat),
      (!rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypex_vlx.genUopInfoRenameBitPat),
    )),
    isVSETVLI -> Mux(
      sewLmulIllegal || vsetvliVill,
      bitPatToUInt(VecUopDefines.vset_vtypei_ill.genUopInfoRenameBitPat),
      Mux1H(Seq(
        (rdIsZero && rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypei_nop.genUopInfoRenameBitPat),
        (!rdIsZero && rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypei_vlmax.genUopInfoRenameBitPat),
        (!rs1IsZero) -> bitPatToUInt(VecUopDefines.vset_vtypei_vlx.genUopInfoRenameBitPat),
      )),
    ),
    isVSETIVLI -> Mux(
      sewLmulIllegal || vsetivliVill,
      bitPatToUInt(VecUopDefines.vset_vtypei_ill.genUopInfoRenameBitPat),
      bitPatToUInt(VecUopDefines.vset_vtypei_vli.genUopInfoRenameBitPat)
    ),
  )).asTypeOf(uop)

  val legalInst: Bool = new DecodeTable(VecInstPattern.set, Seq(IsLegalInstField)).decode(in.rawInst)(IsLegalInstField)

  out.renameInfo.valid := legalInst
  out.renameInfo.bits := uop
  out.src.src1 := instFields.RS1
  out.src.src2 := instFields.RS2
  out.src.dest := instFields.RD
  out.isVSETVL := instFields.ALL(31, 25) === b"1_000000"
}

case class ConfigInstDetailPattern(
  rawInst: BitPat
) extends DecodePattern {
  def zimm10_0 = rawInst(30, 20)

  def zimm9_0 = rawInst(29, 20)

  def vtypei = rawInst(27, 20)

  def isVSETVLI  = rawInst(31, 31).rawString == "0"
  def isVSETIVLI = rawInst(31, 30).rawString == "11"
  def isVSETVL   = rawInst(31, 25).rawString == "1000000"

  def isIllegalVSETVLI: Boolean = this.isSewAndLmulIllegal(this.sewValue, this.lmulValue)
  def isIllegalVSETIVLI: Boolean = this.isSewAndLmulIllegal(this.sewValue, this.lmulValue)

  def rd : BitPat = this.rawInst(11, 7)
  def rs1: BitPat = this.rawInst(19, 15)
  def rs2: BitPat = this.rawInst(24, 20)

  def vma: BitPat = this.vtypei(7)
  def vta: BitPat = this.vtypei(6)
  def vsew: BitPat = this.vtypei(5, 3)
  def vlmul: BitPat = this.vtypei(2, 0)

  def sewValue: Int = this.vsew.take(3).rawString match {
    case "000" => 8
    case "001" => 16
    case "010" => 32
    case "011" => 64
  }

  def lmulValue: Double = this.vlmul.rawString match {
    case "000" => 1
    case "001" => 2
    case "010" => 4
    case "011" => 8
    case "100" => 0
    case "101" => 0.125
    case "110" => 0.25
    case "111" => 0.5
  }

  def isSewAndLmulIllegal(sew: Int, lmul: Double): Boolean = {
    lmul == 0 | (sew > lmul.min(1) * 64)
  }

  override def bitPat: BitPat = rawInst
}

object VsetDecoderUtil {
  abstract class BoolPattern(
    bool: Option[Boolean]
  ) extends DecodePattern {
    override def bitPat: BitPat = bool match {
      case Some(value) => value.toBitPat
      case None => BitPat("b?")
    }
  }

  case class RdZeroPattern(
    rdZero: Option[Boolean]
  ) extends BoolPattern(rdZero)

  case class Rs1ZeroPattern(
    rs1Zero: Option[Boolean]
  ) extends BoolPattern(rs1Zero)

  case class VsetvliVtypeiLegalHead(
    zimm11bHead3Zero: Option[Boolean]
  ) extends BoolPattern(zimm11bHead3Zero) {
    def vill: Boolean = zimm11bHead3Zero.exists(!_)
  }

  case class VsetivliVtypeiLegalHead(
    zimm10bHead2Zero: Option[Boolean]
  ) extends BoolPattern(zimm10bHead2Zero){
    def vill: Boolean = zimm10bHead2Zero.exists(!_)
  }

  type InstPatternWithRdRs1Zero = DecodePatternComb5[
    ConfigInstDetailPattern,
    VsetvliVtypeiLegalHead,
    VsetivliVtypeiLegalHead,
    RdZeroPattern,
    Rs1ZeroPattern,
  ]

  object UopInfoField extends DecodeField[InstPatternWithRdRs1Zero, UopInfoRename] {

    override def name: String = "uopInfo"

    override def chiselType: UopInfoRename = new UopInfoRename

    override def genTable(op: InstPatternWithRdRs1Zero): BitPat = {
      val uopSeq: Seq[_ <: UopBase] = genUop(op).toSeq

      val bitPatSeq: Seq[BitPat] = uopSeq.map(_.genUopInfoRenameBitPat).padTo(1, BitPat.dontCare(UopInfoRename.width))

      bitPatSeq.reverse.reduce(_ ## _)
    }

    def genUop(op: InstPatternWithRdRs1Zero): Option[UopBase] = {
      val DecodePatternComb5(
        instP,
        vsetvliVtypeiLegalHead,
        vsetivliVtypeiLegalHead,
        rdZero,
        rs1Zero
      ) = op

      if (instP.isVSETVL) {
        Some((
          if (rdZero.rdZero.get && rs1Zero.rs1Zero.get)
            VecUopDefines.vset_vtypex_vll
          else if (rs1Zero.rs1Zero.get)
            VecUopDefines.vset_vtypex_vlmax
          else
            VecUopDefines.vset_vtypex_vlx
        ).asInstanceOf[UopBase])
      }
      else if (instP.isVSETVLI) {
        Some((
          if (instP.isIllegalVSETVLI || !vsetvliVtypeiLegalHead.zimm11bHead3Zero.get)
            VecUopDefines.vset_vtypei_ill
          else if (rdZero.rdZero.get && rs1Zero.rs1Zero.get)
            VecUopDefines.vset_vtypei_nop
          else if (rs1Zero.rs1Zero.get)
            VecUopDefines.vset_vtypei_vlmax
          else
            VecUopDefines.vset_vtypei_vlx
        ).asInstanceOf[UopBase])
      }
      else if (instP.isVSETIVLI) {
        Some(
          if (instP.isIllegalVSETIVLI || !vsetivliVtypeiLegalHead.zimm10bHead2Zero.get)
            VecUopDefines.vset_vtypei_ill
          else
            VecUopDefines.vset_vtypei_vli
        )
      }
      else {
        None
      }
    }
  }

  object IsLegalInstField extends BoolDecodeField[VecConfigInstPattern] {

    override def name: String = "isLegalInst"

    override def default: BitPat = this.n

    override def genTable(op: VecConfigInstPattern): BitPat = this.y
  }

  object VillField extends BoolDecodeField[InstPatternWithRdRs1Zero] {

    override def name: String = "vill"

    override def genTable(op: InstPatternWithRdRs1Zero): BitPat = {
      val DecodePatternComb5(
        instP,
        vsetvliVtypeiLegalHead,
        vsetivliVtypeiLegalHead,
        rdZero,
        rs1Zero
      ) = op
      if (instP.isVSETVLI)
        if (instP.isIllegalVSETVLI || vsetvliVtypeiLegalHead.vill)
          this.y
        else
          this.n
      else if (instP.isVSETIVLI)
        if (instP.isIllegalVSETIVLI || vsetivliVtypeiLegalHead.vill)
          this.y
        else
          this.n
      else
        this.n
    }
  }

  object SewLmulIllegalField extends BoolDecodeField[
    DecodePatternComb2[SewPattern, LmulPattern]
  ] {
    override def name: String = "sewLmulIllegal"

    override def genTable(op: DecodePatternComb2[SewPattern, LmulPattern]): BitPat = {
      val DecodePatternComb(sewP, lmulP) = op

      val sew = sewP.sewValue
      val lmul = lmulP.lmulValue

      if (sew > lmul.min(1) * 64) y else n
    }
  }

  val vsews: Seq[BitPat] = Sews.all.map(uint => uint.toBitPat.pad0To(3))

  val vlmuls: Seq[BitPat] = Lmuls.all.map(uint => uint.toBitPat)

  val allSewLmulSeq: Seq[BitPat] = {
    for (vsew <- vsews; vlmul <- vlmuls) yield {
      vsew ## vlmul
    }
  }

  val allInstVSETVLI: Seq[BitPat] = allSewLmulSeq.map {
    x => BitPat(s"b0_???_??_${x.rawString}_?????_111_?????_1010111")
  }

  val allInstVSETIVLI: Seq[BitPat] = allSewLmulSeq.map {
    x => BitPat(s"b11_??_??_${x.rawString}_?????_111_?????_1010111")
  }

  val allInstVSETVL: Seq[BitPat] = Seq(
    Instructions.VSETVL,
  )

  val allInstVSET: Seq[BitPat] = allInstVSETVLI ++ allInstVSETIVLI ++ allInstVSETVL

  val boolSeq = Seq(false, true)

  val vsetvlPattern: Seq[InstPatternWithRdRs1Zero] = for {
    inst <- allInstVSETVL
    rdZero <- boolSeq
    rs1Zero <- boolSeq
  } yield {
    ConfigInstDetailPattern(inst) ##
      VsetvliVtypeiLegalHead(None) ##
      VsetivliVtypeiLegalHead(None) ##
      RdZeroPattern(Some(rdZero)) ##
      Rs1ZeroPattern(Some(rs1Zero))
  }

  val vsetvliPattern: Seq[InstPatternWithRdRs1Zero] = for {
    inst <- allInstVSETVLI
    rdZero <- boolSeq
    rs1Zero <- boolSeq
    legalHead <- boolSeq
  } yield {
    ConfigInstDetailPattern(inst) ##
      VsetvliVtypeiLegalHead(Some(legalHead)) ##
      VsetivliVtypeiLegalHead(None) ##
      RdZeroPattern(Some(rdZero)) ##
      Rs1ZeroPattern(Some(rs1Zero))
  }

  val vsetivliPattern: Seq[InstPatternWithRdRs1Zero] = for {
    inst <- allInstVSETIVLI
    rdZero <- boolSeq
    rs1Zero <- boolSeq
    legalHead <- boolSeq
  } yield {
    ConfigInstDetailPattern(inst) ##
      VsetvliVtypeiLegalHead(None) ##
      VsetivliVtypeiLegalHead(Some(legalHead)) ##
      RdZeroPattern(Some(rdZero)) ##
      Rs1ZeroPattern(Some(rs1Zero))
  }
}

object VsetDecoderMain {
  def main(args: Array[String]): Unit = {
    import VsetDecoderUtil._

//    val patterns = vsetvlPattern ++ vsetvliPattern ++ vsetivliPattern
//
//    for (pattern <- patterns) {
//      val uop: Option[UopBase] = UopInfoField.genUop(pattern)
//      uop.foreach(
//        uop => println(
//          s"${pattern.p1.rawInst}, " +
//            s"ill: ${pattern.p2.zimm11bHead3Zero.exists(!_) || pattern.p3.zimm10bHead2Zero.exists(!_)}, " +
//            s"rdZero:${pattern.p4.rdZero.get}, rs1Zero:${pattern.p5.rs1Zero.get}: ${uop.uopInfoRenameString}"))
//    }

    Verilog.emitVerilog(
      new VsetDecoder,
      Array(
        "--throw-on-first-error",
        "--full-stacktrace",
        "--target-dir", "build/decoder"
      )
    )
  }
}

