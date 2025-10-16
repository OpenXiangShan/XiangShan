package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import chisel3.util.ValidIO
import chisel3.util.experimental.decode.DecodeTable
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.SelImm
import xiangshan.backend.vector.util.Verilog

@instantiable
class SimpleDecodeChannel(instSeq: Seq[InstPattern]) extends Module {
  import xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(ValidIO(new SimpleDecodeChannelOutput)))

  val instFields = in.rawInst.asTypeOf(new XSInstBitFields)

  val patterns = instSeq

  val fields = Seq(
    LegalField,
    Src1Field,
    Src2Field,
    Src3Field,
    GpWenField,
    FpWenField,
    NoSpecField,
    BlockBackField,
    FlushPipeField,
    SelImmField,
  )

  println(s"The length of DecodeTable in SimpleDecodeChannel: ${patterns.length}")
  val table = new DecodeTable(patterns, fields)

  val result = table.decode(in.rawInst)

  out.valid := result(LegalField)
  out.bits.src1RenType := result(Src1Field)
  out.bits.src2RenType := result(Src2Field)
  out.bits.src3RenType := result(Src3Field)
  out.bits.lsrc1 := instFields.RS1
  out.bits.lsrc2 := instFields.RS2
  out.bits.lsrc3 := instFields.FS3
  out.bits.gpWen := result(GpWenField)
  out.bits.fpWen := result(FpWenField)
  out.bits.ldest := instFields.RD
  out.bits.noSpec := result(NoSpecField)
  out.bits.blockBack := result(BlockBackField)
  out.bits.flushPipe := result(FlushPipeField)
  out.bits.selImm := result(SelImmField)
}

class SimpleDecodeChannelOutput() extends Bundle {
  val src1RenType = new SrcRenType
  val src2RenType = new SrcRenType
  val src3RenType = new SrcRenType
  val lsrc1 = UInt(5.W)
  val lsrc2 = UInt(5.W)
  val lsrc3 = UInt(5.W)
  val gpWen = Bool()
  val fpWen = Bool()
  val ldest = UInt(5.W)
  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = ValidIO(SelImm())
}

object SimpleDecodeChannelMain extends App {
  val insts: Seq[InstPattern] = InstPattern.all.collect { case x if !x.isInstanceOf[VecInstPattern] => x }

  Verilog.emitVerilog(
    new SimpleDecodeChannel(insts),
    Array("--full-stacktrace", "--target-dir", "build/decoder"),
  )
}
