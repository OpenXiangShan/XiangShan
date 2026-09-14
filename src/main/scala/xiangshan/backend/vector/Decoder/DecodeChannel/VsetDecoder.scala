package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.decode.isa.Instructions
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.VSetOpcodes._
import xiangshan.backend.decode.opcode.Opcode.{Opcode, toOpcodeUtil}
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodePatterns.{RdZeroPattern, Rs1ZeroPattern}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Types.DecodeSelImm
import xiangshan.backend.vector.Decoder.Uop.UopInfoRename
import xiangshan.backend.vector.Decoder.util.{BoolDecodeField, DecodeField, DecodePattern, DecodeTable}
import xiangshan.backend.vector.Decoder.{Lmuls, Sews}
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._
import xiangshan.backend.vector.util.Verilog

import scala.language.implicitConversions

@instantiable
class VsetDecoder extends Module {
  import VsetDecoder._
  import VsetDecoderUtil._

  @public
  val in = IO(Input(new In))
  @public
  val out = IO(Output(ValidIO(new Out)))

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
      (rdIsZero && rs1IsZero) -> bitPatToUInt(uvset_vtypex_vll.genUopInfoRenameBitPat),
      (!rdIsZero && rs1IsZero) -> bitPatToUInt(uvset_vtypex_vlmax.genUopInfoRenameBitPat),
      (!rs1IsZero) -> bitPatToUInt(uvset_vtypex_vlx.genUopInfoRenameBitPat),
    )),
    isVSETVLI -> Mux(
      sewLmulIllegal || vsetvliVill,
      bitPatToUInt(uvset_ill.genUopInfoRenameBitPat),
      Mux1H(Seq(
        (rdIsZero && rs1IsZero) -> bitPatToUInt(uvset_vtypei_nop.genUopInfoRenameBitPat),
        (!rdIsZero && rs1IsZero) -> bitPatToUInt(uvset_vtypei_vlmax.genUopInfoRenameBitPat),
        (!rs1IsZero) -> bitPatToUInt(uvset_vtypei_vlx.genUopInfoRenameBitPat),
      )),
    ),
    isVSETIVLI -> Mux(
      sewLmulIllegal || vsetivliVill,
      bitPatToUInt(uvset_ill.genUopInfoRenameBitPat),
      bitPatToUInt(uvset_vtypei_vli.genUopInfoRenameBitPat)
    ),
  )).asTypeOf(uop)

  val opcode = Wire(Opcode())
  opcode := Mux1H(Seq(
    isVSETVL -> Mux1H(Seq(
      (rdIsZero && rs1IsZero) -> uvset_vtypex_vll.encode,
      (!rdIsZero && rs1IsZero) -> uvset_vtypex_vlmax.encode,
      (!rs1IsZero) -> uvset_vtypex_vlx.encode,
    )),
    isVSETVLI -> Mux(
      sewLmulIllegal || vsetvliVill,
      uvset_ill.encode,
      Mux1H(Seq(
        (rdIsZero && rs1IsZero) -> uvset_vtypei_nop.encode,
        (!rdIsZero && rs1IsZero) -> uvset_vtypei_vlmax.encode,
        (!rs1IsZero) -> uvset_vtypei_vlx.encode,
      )),
    ),
    isVSETIVLI -> Mux(
      sewLmulIllegal || vsetivliVill,
      uvset_ill.encode,
      uvset_vtypei_vli.encode,
    ),
  ))

  val legalInst: Bool = new DecodeTable(VecInstPattern.set, Seq(IsLegalInstField)).decode(in.rawInst)(IsLegalInstField)

  out.valid := legalInst
  out.bits.renameInfo.valid := legalInst
  out.bits.renameInfo.bits := uop
  out.bits.renameInfo.bits.gpWen := uop.gpWen && instFields.RD =/= 0.U
  out.bits.illegal := legalInst && in.vsIsOff
  out.bits.src.src1 := instFields.RS1
  out.bits.src.src2 := instFields.RS2
  out.bits.src.dest := instFields.RD
  out.bits.fuType := FuType.vset.U
  out.bits.opcode := opcode
  out.bits.selImm.valid := isVSETVLI || isVSETIVLI
  out.bits.selImm.bits := DecodeSelImm.VSETIVLI
  out.bits.imm := Mux1H(Seq(
    isVSETIVLI -> ImmUnion.VSETIVLI.minBitsFromInstr(rawInst),
    isVSETVLI -> ImmUnion.VSETVLI.minBitsFromInstr(rawInst),
  ))
  out.bits.flushPipe := isVSETVL
  out.bits.isVSETVL := isVSETVL

  implicit def castToUInt(bp: BitPat): UInt = bitPatToUInt(bp)
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
      val uopSeq: Seq[Opcode] = genUop(op).toSeq

      val bitPatSeq: Seq[BitPat] = uopSeq.map(_.genUopInfoRenameBitPat).padTo(1, BitPat.dontCare(UopInfoRename.width))

      bitPatSeq.reverse.reduce(_ ## _)
    }

    def genUop(op: InstPatternWithRdRs1Zero): Option[Opcode] = {
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
            uvset_vtypex_vll
          else if (rs1Zero.rs1Zero.get)
            uvset_vtypex_vlmax
          else
            uvset_vtypex_vlx
        ))
      }
      else if (instP.isVSETVLI) {
        Some(
          if (instP.isIllegalVSETVLI || !vsetvliVtypeiLegalHead.zimm11bHead3Zero.get)
            uvset_ill
          else if (rdZero.rdZero.get && rs1Zero.rs1Zero.get)
            uvset_vtypei_nop
          else if (rs1Zero.rs1Zero.get)
            uvset_vtypei_vlmax
          else
            uvset_vtypei_vlx
        )
      }
      else if (instP.isVSETIVLI) {
        Some(
          if (instP.isIllegalVSETIVLI || !vsetivliVtypeiLegalHead.zimm10bHead2Zero.get)
            uvset_ill
          else
            uvset_vtypei_vli
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

  object OpcodeField extends DecodeField[InstPatternWithRdRs1Zero, UInt] {

    override def name: String = "opcode"

    override def chiselType: UInt = Opcode()

    override def genTable(op: InstPatternWithRdRs1Zero): BitPat = {
      val uop = UopInfoField.genUop(op)
      uop.map(_.encode.pad0To(Opcode.getWidth)).getOrElse(default)
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

object VsetDecoder {
  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new VsetDecoder,
      Array(
        "--throw-on-first-error",
        "--full-stacktrace",
        "--target-dir", "build/decoder"
      )
    )
  }

  class In extends Bundle {
    val rawInst = UInt(32.W)
    val vsIsOff = Bool()
  }

  class Out extends Bundle {
    val renameInfo = ValidIO(new UopInfoRename)
    val src = new UopSrcBundle
    val fuType = FuType()
    val opcode = Opcode()
    val selImm = ValidIO(DecodeSelImm())
    val imm = UInt(32.W)
    val flushPipe = Bool()
    val isVSETVL = Bool()
    val illegal = Bool()
  }
}

