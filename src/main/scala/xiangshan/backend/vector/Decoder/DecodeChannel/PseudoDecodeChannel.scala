package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util.{BitPat, ValidIO}
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.decode.opcode.OpcodeTraits._
import sourcecode.{Name => SourceName}
import xiangshan.backend.decode.isa.PseudoInstructions
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.{AluOpcodes, VSetOpcodes}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.{DecodeSelImm, DecodeSrcType}
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector.util.ChiselTypeExt.{BitPatToExt, UIntToUIntField}
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog

import scala.collection.SeqMap
import scala.language.implicitConversions

@instantiable
class PseudoDecodeChannel(instSeq: Seq[InstPattern] = PseudoDecodeChannel.uopTable.keys.toSeq) extends Module {
  import PseudoDecodeChannel._

  @public
  val in = IO(Input(new In))
  @public
  val out = IO(Output(ValidIO(new Out)))

  val instBitFields: XSInstBitFields = in.rawInst.asTypeOf(new XSInstBitFields)

  val patterns = instSeq

  val fields = Seq(
    legalField,
    fuTypeField,
    opcodeField,
    gpWenField,
    fpWenField,
    noSpecField,
    blockBackField,
    flushPipeField,
    selImmField,
    src1Field,
    src2Field,
    needVecEnableField,
    canRobCompressField,
  )

  val table = new DecodeTable(patterns, fields)

  val bundle = table.decode(in.rawInst)

  out.valid := bundle(legalField)
  out.bits.fuType := bundle(fuTypeField)
  out.bits.opcode := bundle(opcodeField)
  out.bits.src1RenType := bundle(src1Field)
  out.bits.src2RenType := bundle(src2Field)
  out.bits.lsrc1 := instBitFields.RS1
  out.bits.lsrc2 := instBitFields.RS2
  out.bits.ldest := instBitFields.RD
  out.bits.gpWen := bundle(gpWenField)
  out.bits.fpWen := bundle(fpWenField)
  out.bits.noSpec := bundle(noSpecField)
  out.bits.blockBack := bundle(blockBackField)
  out.bits.flushPipe := bundle(flushPipeField)
  out.bits.selImm := bundle(selImmField)
  out.bits.canRobCompress := !bundle(canRobCompressField)
  out.bits.exceptionII := bundle(needVecEnableField) && in.fromCSR.illegalInst.vsIsOff
}

object PseudoDecodeChannel {
  def main(args: Array[String]): Unit = {
    val targetDir = "build/decoder"

    Verilog.emitVerilog(
      new PseudoDecodeChannel(),
      Array("--full-stacktrace", "--target-dir", targetDir),
    )
  }

  import InstPatterns._

  class In extends Bundle {
    val rawInst = UInt(32.W)
    val fromCSR = new CSRToDecode
  }

  class Out extends Bundle {
    val fuType: UInt = FuType()
    val opcode: UInt = Opcode()
    val src1RenType = new SrcRenType
    val src2RenType = new SrcRenType
    val lsrc1 = UInt(5.W)
    val lsrc2 = UInt(5.W)
    val ldest = UInt(5.W)
    val gpWen = Bool()
    val fpWen = Bool()
    val noSpec = Bool()
    val blockBack = Bool()
    val flushPipe = Bool()
    val selImm = ValidIO(DecodeSelImm())
    val canRobCompress = Bool()
    val exceptionII = Bool()
  }

  object InstPatterns {
    val CSRRVL     = PseudoInstPattern(makeCSRRBitPat(CSRs.vl))

    val CSRRVLENB  = PseudoInstPattern(makeCSRRBitPat(CSRs.vlenb))

    val PREFETCH_I = PseudoInstPattern(PseudoInstructions.PREFETCH_I)
    val PREFETCH_R = PseudoInstPattern(PseudoInstructions.PREFETCH_R)
    val PREFETCH_W = PseudoInstPattern(PseudoInstructions.PREFETCH_W)
  }

  class DecodeFieldGen[-T <: InstPattern, +D <: Data](
    gen: => D,
    tableFunc: T => BitPat,
    defaultFunc: Option[BitPat] = None,
  )(
    implicit val _name: SourceName
  ) extends DecodeField[T, D] {
    override def name: String = _name.value

    override def chiselType: D = gen

    override def genTable(op: T): BitPat = {
      tableFunc(op)
    }

    override def default: BitPat = defaultFunc.getOrElse(super.default)

    override def toString: String = {
      name + "@" + this.getClass.getSimpleName
    }
  }

  val uopTable: SeqMap[InstPattern, Opcode] = SeqMap(
    CSRRVL    -> (VSetOpcodes.readvl + NeedVecEnable),
    CSRRVLENB -> (AluOpcodes.add.copy() - Src1Gp - Src2En - Src2Gp + Src2Imm(DecodeSelImm.I) + NeedVecEnable),
  )

  uopTable.foreach(println)

  val legalField = new DecodeFieldGen(
    Bool(),
    (_: InstPattern) => BitPat.Y(),
    defaultFunc = Option(BitPat.N()), // only the pattern passed in is legal
  )

  val fuTypeField = new DecodeFieldGen(
    FuType(),
    (op: InstPattern) => {
      val fuType = uopTable(op).factory match {
        case _: Opcode.AluOpcodes.type => FuType.alu.U
        case _: Opcode.BruOpcodes.type => ???
        case _: Opcode.MulOpcodes.type => ???
        case _: Opcode.DivOpcodes.type => ???
        case _: Opcode.LduOpcodes.type => ???
        case _: Opcode.StuOpcodes.type => ???
        case _: Opcode.AmoOpcodes.type => ???
        case _: Opcode.BkuOpcodes.type => ???
        case _: Opcode.CsrOpcodes.type => ???
        case _: Opcode.FenceOpcodes.type => ???
        case _: Opcode.FMacOpcodes.type => ???
        case _: Opcode.FDivOpcodes.type => ???
        case _: Opcode.FCvtOpcodes.type => ???
        case _: Opcode.FMiscOpcodes.type => ???
        case _: Opcode.VSetOpcodes.type => FuType.vset.U
        case _: Opcode.VIAluOpcodes.type => ???
        case _: Opcode.VMAluOpcodes.type => ???
        case _: Opcode.VIMacOpcodes.type => ???
        case _: Opcode.VIRedOpcodes.type => ???
        case _: Opcode.VIPermOpcodes.type => ???
        case _: Opcode.VIDivOpcodes.type => ???
      }
      fuType.pad(FuType.width).toBitPat
    }
  )

  val opcodeField = new DecodeFieldGen(
    Opcode(),
    (op: InstPattern) => {
      uopTable(op).encode.pad0To(Opcode.getWidth)
    }
  )

  val gpWenField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(GpWen).toBitPat
  )

  val fpWenField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(FpWen).toBitPat
  )

  val src1Field = new DecodeFieldGen(
    new SrcRenType,
    (op: InstPattern) => uopTable(op).getTraits.collect { case x: Src1Trait => x }.ensuring(_.size <= 1).headOption.map(_.srcType match {
      case Vp | Vm | Vs | Vw | Vws => DecodeSrcType.VP
      case Fp => DecodeSrcType.FP
      case Gp => DecodeSrcType.GP
      case Imm => DecodeSrcType.IMM
    }).map((x: UInt) => BitPat.Y() ## x.toBitPat).getOrElse(BitPat.N() ## BitPat.dontCare(DecodeSrcType.width))
  )

  val src2Field = new DecodeFieldGen(
    new SrcRenType,
    (op: InstPattern) => uopTable(op).getTraits.collect { case x: Src2Trait => x }
      .ensuring(_.size <= 1, s"${uopTable(op)} has more than 1 Src2Trait(${uopTable(op).getTraits})")
      .headOption
      .map(
        _.srcType match {
          case Vp | Vm | Vs | Vw | Vws => DecodeSrcType.VP
          case Fp => DecodeSrcType.FP
          case Gp => DecodeSrcType.GP
          case Imm => DecodeSrcType.IMM
        }
      )
      .map((x: UInt) => BitPat.Y() ## x.toBitPat)
      .getOrElse(BitPat.N() ## BitPat.dontCare(DecodeSrcType.width))
  )

  val noSpecField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(NoSpec).toBitPat,
  )

  val blockBackField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(BlockBack).toBitPat
  )

  val flushPipeField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(FlushPipe).toBitPat
  )

  val needVecEnableField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => uopTable(op).getTraits.contains(NeedVecEnable).toBitPat
  )

  val selImmField = new DecodeFieldGen(
    ValidIO(DecodeSelImm()),
    (op: InstPattern) => uopTable(op).getTraits.collect { case Src2Imm(selImm) => selImm }
      .ensuring(_.size <= 1)
      .headOption
      .map(BitPat.Y() ## _)
      .getOrElse(BitPat.N() ## BitPat.dontCare(DecodeSelImm.width))
  )

  val canRobCompressField = new DecodeFieldGen(
    Bool(),
    (op: InstPattern) => (!uopTable(op).getTraits.contains(CannotRobCompress)).toBitPat
  )

  def makeCSRRBitPat(csrno: Int): BitPat = {
    new BitPat(csrno, 0xFFF, 12) ## BitPat("b00000_010_?????_1110011")
  }

  implicit def StringToBitPat(str: String): BitPat = BitPat(str)
}
