package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import xiangshan.backend.decode.opcode.Opcode._
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.util.{DecodeField, DecodePattern, DecodeTable}
import xiangshan.backend.vector.util.ChiselTypeExt.BitPatToExt
import xiangshan.backend.vector.util.Verilog
import xiangshan.backend.decode.opcode.Latency
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.FuTypeField


class LatDecoder(opcodesSeq: Seq[Opcodes]) extends Module {
  val in = IO(Input(new LatDecoder.In))
  val out = IO(Output(new LatDecoder.Out))

  val patterns: Seq[Seq[LatDecoder.OpcodePattern]] = opcodesSeq.map(LatDecoder.genDecodePattern)
  val fields: Seq[LatDecoder.LatField] = opcodesSeq.map(x => new LatDecoder.LatField(x))

  val tables = patterns lazyZip fields map { case (pattern, field) => new DecodeTable(pattern, Seq(field)) }

  val bundles: Seq[UInt] = tables lazyZip fields map (_.decode(in.opcode)(_))

  out.lat := Mux1H(
    opcodesSeq lazyZip bundles map {
      case (opcodes, bundle) =>
        (FuTypeField.genFuType(opcodes).U & in.fuType).orR -> bundle
    }
  )
}

object LatDecoder {
  def apply(fuType: UInt, opcode: UInt): UInt = {
    val mod = Module(new LatDecoder(Seq(VIAluOpcodes, VFMacOpcodes, VMoveOpcodes, VFCvtOpcodes)))
    mod.in.fuType := fuType
    mod.in.opcode := opcode
    mod.out.lat
  }

  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new LatDecoder(Seq(VIAluOpcodes)),
      Array(
        "--full-stacktrace",
        "--target-dir", "build/LatDecoder",
      )
    )
  }

  class In extends Bundle {
    val fuType: UInt = FuType()
    val opcode: UInt = Opcodes()
  }

  class Out extends Bundle {
    val lat: UInt = Latency()
  }

  def genDecodePattern(opcodes: Opcodes): Seq[OpcodePattern] = {
    opcodes.all.map(OpcodePattern)
  }

  case class OpcodePattern(opcode: Opcode) extends DecodePattern {
    override def bitPat: BitPat = opcode.encode
  }

  class LatField(opcodes: Opcodes) extends DecodeField[OpcodePattern, UInt] {
    override def name: String = "lat"

    override def chiselType: UInt = UInt(opcodes.maxLat.U.getWidth.W)

    override def genTable(op: OpcodePattern): BitPat = {
      BitPat(op.opcode.getLat.U).pad0To(this.width)
    }
  }
}
