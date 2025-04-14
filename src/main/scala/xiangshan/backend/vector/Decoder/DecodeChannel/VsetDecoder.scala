package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import freechips.rocketchip.rocket.Instructions
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.DecodePatternComb
import xiangshan.backend.vector.Decoder.Uop.UopType.UopBase
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

  val inst = in.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec)

  val allPatterns = vsetvlPattern ++ vsetvliPattern ++ vsetivliPattern

  val allFields = Seq(
    UopInfoField,
  )

  val decodeTable: DecodeTable[InstPatternWithRdRs1Zero] = new DecodeTable(allPatterns, allFields)
  println(s"length of decodeTable in VsetDecoder: ${decodeTable.table.table.length}")

  val legalInst: Bool = new DecodeTable(VecInstPattern.set, Seq(IsLegalInstField)).decode(in.rawInst)(IsLegalInstField)

  val vtypeiWidth = 8
  val vsetvliLegal = inst.ZIMM_VSETVLI.drop(vtypeiWidth) === b"000"
  val vsetivliLegal = inst.ZIMM_VSETIVLI.drop(vtypeiWidth) === b"00"
  val rdZero = inst.RD === 0.U
  val rs1Zero = inst.RS1 === 0.U

  val decodeResult = decodeTable.decode(in.rawInst ## vsetvliLegal ## vsetivliLegal ## rdZero ## rs1Zero)

  out.renameInfo.valid := legalInst
  out.renameInfo.bits := decodeResult(UopInfoField)
  out.src.src1 := inst.RS1
  out.src.src2 := inst.RS2
  out.src.dest := inst.RD
  out.isVSETVL := inst.ALL(31, 25) === b"1_000000"
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

  type InstPatternWithRdRs1Zero = DecodePatternComb[
    DecodePatternComb[
      ConfigInstDetailPattern,
      DecodePatternComb[
        VsetvliVtypeiLegalHead,
        VsetivliVtypeiLegalHead,
      ],
    ],
    DecodePatternComb[
      RdZeroPattern,
      Rs1ZeroPattern,
    ]
  ]

  object InstPatternWithRdRs1Zero {
    def apply(
      inst: BitPat,
      vsetvliLegal: Option[Boolean],
      vsetivliLegal: Option[Boolean],
      rdZero: Option[Boolean],
      rs1Zero: Option[Boolean],
    ): InstPatternWithRdRs1Zero = {
      ConfigInstDetailPattern(inst) ## (
        VsetvliVtypeiLegalHead(vsetvliLegal) ##
        VsetivliVtypeiLegalHead(vsetivliLegal)
      ) ## (
        RdZeroPattern(rdZero) ##
        Rs1ZeroPattern(rs1Zero)
      )
    }
  }

  object UopInfoField extends DecodeField[InstPatternWithRdRs1Zero, UopInfoRename] {

    override def name: String = "uopInfo"

    override def chiselType: UopInfoRename = new UopInfoRename

    override def genTable(op: InstPatternWithRdRs1Zero): BitPat = {
      val uopSeq: Seq[_ <: UopBase] = genUop(op).toSeq

      val bitPatSeq: Seq[BitPat] = uopSeq.map(_.genUopInfoRenameBitPat).padTo(1, BitPat.dontCare(UopInfoRename.width))

      bitPatSeq.reverse.reduce(_ ## _)
    }

    def genUop(op: InstPatternWithRdRs1Zero): Option[UopBase] = {
      val vsetvliVtypeiLegalHead: VsetvliVtypeiLegalHead = op.p1.p2.p1
      val vsetivliVtypeiLegalHead: VsetivliVtypeiLegalHead = op.p1.p2.p2
      val instP: ConfigInstDetailPattern = op.p1.p1
      val rdZero = op.p2.p1.rdZero
      val rs1Zero = op.p2.p2.rs1Zero

      if (instP.isVSETVL) {
        Some((
          if (rdZero.get && rs1Zero.get)
            VecUopDefines.vset_vtypex_vll.x
          else if (rs1Zero.get)
            VecUopDefines.vset_vtypex_vlmax.x
          else
            VecUopDefines.vset_vtypex_vlx.xx
        ).asInstanceOf[UopBase])
      }
      else if (instP.isVSETVLI) {
        Some((
          if (instP.isIllegalVSETVLI || !vsetvliVtypeiLegalHead.zimm11bHead3Zero.get)
            VecUopDefines.vset_vtypei_ill
          else if (rdZero.get && rs1Zero.get)
            VecUopDefines.vset_vtypei_nop
          else if (rs1Zero.get)
            VecUopDefines.vset_vtypei_vlmax
          else
            VecUopDefines.vset_vtypei_vlx.x
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
      val vsetvliVtypeiLegalHead: VsetvliVtypeiLegalHead = op.p1.p2.p1
      val vsetivliVtypeiLegalHead: VsetivliVtypeiLegalHead = op.p1.p2.p2
      val instP: ConfigInstDetailPattern = op.p1.p1
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
    InstPatternWithRdRs1Zero(inst, None, None, Some(rdZero), Some(rs1Zero))
  }

  val vsetvliPattern: Seq[InstPatternWithRdRs1Zero] = for {
    inst <- allInstVSETVLI
    rdZero <- boolSeq
    rs1Zero <- boolSeq
    legalHead <- boolSeq
  } yield {
    InstPatternWithRdRs1Zero(
      inst,
      vsetvliLegal = Some(legalHead),
      vsetivliLegal = None,
      rdZero = Some(rdZero),
      rs1Zero = Some(rs1Zero)
    )
  }

  val vsetivliPattern: Seq[InstPatternWithRdRs1Zero] = for {
    inst <- allInstVSETIVLI
    rdZero <- boolSeq
    rs1Zero <- boolSeq
    legalHead <- boolSeq
  } yield {
    InstPatternWithRdRs1Zero(
      inst,
      vsetvliLegal = None,
      vsetivliLegal = Some(legalHead),
      rdZero = Some(rdZero),
      rs1Zero = Some(rs1Zero)
    )
  }
}

object VsetDecoderMain extends App {
  import VsetDecoderUtil._

  val patterns = vsetvlPattern ++ vsetvliPattern ++ vsetivliPattern

  for (pattern <- patterns) {
    val uop: Option[UopBase] = UopInfoField.genUop(pattern)
    uop.foreach(
      uop => println(
        s"${pattern.p1.p1.rawInst}, " +
        s"ill: ${pattern.p1.p2.p1.zimm11bHead3Zero.exists(!_) || pattern.p1.p2.p2.zimm10bHead2Zero.exists(!_)}, rdZero:${pattern.p2.p1.rdZero.get}, rs1Zero:${pattern.p2.p2.rs1Zero.get}: ${uop.uopInfoRenameString}"))
  }

  Verilog.emitVerilog(new VsetDecoder)
}

