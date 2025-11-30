package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util.{BitPat, ValidIO}
import utility.LookupTree
import xiangshan.CommitType
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodePatterns.RdZeroPattern
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.{NumUop, SelImm}
import xiangshan.backend.vector.Decoder.util._
import xiangshan.backend.vector.util.Verilog

@instantiable
class SimpleDecodeChannel(instSeq: Seq[InstPattern], opcodeTable: Map[BitPat, Opcode]) extends Module {
  import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(ValidIO(new SimpleDecodeChannelOutput)))

  val rawInst = in.rawInst
  val instFields = rawInst.asTypeOf(new XSInstBitFields)
  val rdZero = instFields.RD === 0.U

  val patterns = instSeq
  val patternsRd: Seq[DecodePatternComb2[InstPattern, RdZeroPattern]] = instSeq.flatMap {
    case p if p.hasRd => Seq(true, false).map(b => p ## RdZeroPattern(Some(b)))
    case p if !p.hasRd => Seq(p ## RdZeroPattern(None))
  }

  val fuTypeField = new FuTypeField(opcodeTable)
  val opcodeField = new OpcodeField(opcodeTable)

  val fields = Seq(
    fuTypeField,
    opcodeField,
    LegalField,
    Src1Field,
    Src2Field,
    Src3Field,
    FrmRenField,
    GpWenField,
    FpWenField,
    FFlagsWenField,
    NoSpecField,
    BlockBackField,
    FlushPipeField,
    SelImmField,
    CommitTypeField,
    CanRobCompressField,
  )

  println(s"The length of DecodeTable in SimpleDecodeChannel: ${patterns.length}")
  val table = new DecodeTable(patterns, fields)
  val instRdTable = new DecodeTable(patternsRd, Seq(NumUopField))

  val result = table.decode(in.rawInst)
  val resultInstRd = instRdTable.decode(in.rawInst ## rdZero)

  val selImm = result(SelImmField)

  val imm = LookupTree(selImm.bits, ImmUnion.immSelMap.map {
    case (sel, enum) =>
      sel -> enum.minBitsFromInstr(in.rawInst).ensuring(_.getWidth == enum.len)
  })

  out.valid := result(LegalField)
  out.bits.fuType := result(fuTypeField)
  out.bits.opcode := result(opcodeField)
  out.bits.src1RenType := result(Src1Field)
  out.bits.src2RenType := result(Src2Field)
  out.bits.src3RenType := result(Src3Field)
  out.bits.lsrc1 := instFields.RS1
  out.bits.lsrc2 := instFields.RS2
  out.bits.lsrc3 := instFields.FS3
  out.bits.frmRen := result(FrmRenField)
  out.bits.gpWen := result(GpWenField) && instFields.RD =/= 0.U
  out.bits.fpWen := result(FpWenField)
  out.bits.fflagsWen := result(FFlagsWenField)
  out.bits.ldest := instFields.RD
  out.bits.noSpec := result(NoSpecField)
  out.bits.blockBack := result(BlockBackField)
  out.bits.flushPipe := result(FlushPipeField)
  out.bits.selImm := selImm
  out.bits.imm := imm
  out.bits.commitType := result(CommitTypeField)
  out.bits.canRobCompress := result(CanRobCompressField)
  out.bits.numUop := resultInstRd(NumUopField)
}

class SimpleDecodeChannelOutput() extends Bundle {
  val fuType: UInt = FuType()
  val opcode: UInt = Opcode()
  val src1RenType = new SrcRenType
  val src2RenType = new SrcRenType
  val src3RenType = new SrcRenType
  val lsrc1 = UInt(5.W)
  val lsrc2 = UInt(5.W)
  val lsrc3 = UInt(5.W)
  val frmRen = Bool()
  val gpWen = Bool()
  val fpWen = Bool()
  val fflagsWen = Bool()
  val ldest = UInt(5.W)
  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = ValidIO(SelImm())
  val imm = UInt(32.W)
  val commitType = CommitType()
  val canRobCompress = Bool()
  val numUop = NumUop()
}

object SimpleDecodeChannelMain extends App {
  import xiangshan.backend.decode.isa.Extensions._

  val extensions: Seq[ExtBase] = Seq(
    I, M, A, F, D, Zicsr,
    System, S,
    Za64rs, /*Zacas,*/ Zawrs,
    Zba, Zbb, Zbc, Zbs, Zbkb, Zbkc, Zbkx,
    // Zcb, Zcmop,
    // Zfa, Zfh, ZfaZfh, ZfaF, ZfaD, Zfhmin,
  )

  val insts: Seq[InstPattern] = InstPattern.extensionInsts(extensions: _*).collect { case x if !x.isInstanceOf[VecInstPattern] => x }
  val table: Map[BitPat, Opcode] = extensions.map(_.table).reduce(_ ++ _)

  println(s"number of insts: ${insts.size}")
  println(s"number of table: ${table.size}")

  val targetDir = "build/decoder"

  Verilog.emitVerilog(
    new SimpleDecodeChannel(insts, table),
    Array("--full-stacktrace", "--target-dir", targetDir),
  )

  println(s"Generate SimpleDecodeChannel in dir $targetDir")
}
