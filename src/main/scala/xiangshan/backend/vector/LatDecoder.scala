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
  // One group per opcode space; keep in sync with the vector exe units in `vecSchdParams`.
  // Groups sharing a fuType would be OR-ed together by the Mux1H below.
  val opcodes = Seq(
    VIAluOpcodes,
    VIMacOpcodes,
    VMoveOpcodes,
    VFCvtOpcodes,
    VFMacOpcodes,
    StuOpcodes,
  )

  def apply(fuType: UInt, opcode: UInt): UInt = {
    val mod = Module(new LatDecoder(opcodes))
    mod.in.fuType := fuType
    mod.in.opcode := opcode
    mod.out.lat
  }

  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new LatDecoder(opcodes),
      Array(
        "--full-stacktrace",
        "--target-dir", "build/LatDecoder",
      )
    )
    println(
      s"[LatDecoder] fuType=${new In().fuType.getWidth}b opcode=${new In().opcode.getWidth}b " +
        s"latency=${new Out().lat.getWidth}b"
    )
    dumpLatencies()
  }

  /** One line per opcode pattern with the latency its table row gets, i.e. `<group>.getLat(opcode)`. */
  private def dumpLatencies(): Unit = {
    opcodes.foreach { g =>
      val name = g.getClass.getSimpleName.stripSuffix("$")
      val fu = FuTypeField.genFuType(g)
      println(s"=== $name fuType=${fu.getName} (${g.all.size} patterns) ===")
      g.all.sortBy(_.encode.value).foreach { op =>
        val lat = op.getLat
        val note = if (lat == Latency.uncertainLitVal()) "  (uncertain)" else ""
        println(f"  ${op.getName()}%-38s ${op.encode.rawString}  lat = $lat%2d$note")
      }
    }
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

    override def chiselType: UInt = Latency()

    override def genTable(op: OpcodePattern): BitPat = {
      val lat = op.opcode.getLat
      if (lat != Latency.uncertainLitVal()) {
        BitPat(lat.U).pad0To(this.width)
      } else {
        opcodes match {
          case _: StuOpcodes => BitPat(0.U)
          case _ => BitPat(op.opcode.getLat.U).pad0To(this.width)
        }
      }
    }
  }
}
