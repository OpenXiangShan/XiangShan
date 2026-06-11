package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.util.{DecodeBundle, DecodeTable}
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.Verilog
import xiangshan.{XSCoreParameters, XSCoreParamsKey}
import top.ArgParser

@instantiable
class SrcDstSelectDecoder(
  instSeq: Seq[DecodePatternComb2[VecInstPattern, SewPattern]],
)(implicit p: Parameters) extends Module with HasVectorSettings {
  import SrcDstSelectDecoder._

  @public val in = IO(Input(new DecodeChannelInput))
  @public val out = IO(Output(new Out))

  val allInstsWithSew = instSeq

  val allFields = Seq(
    Src2SelectField,
    Src1SelectField,
    DestSelectField,
  )

  println(s"The length of decodeTable in VecDecodeChannel: ${allInstsWithSew.length}")

  val decodeTable = new DecodeTable(allInstsWithSew, allFields)
  val decodeResult: DecodeBundle = decodeTable.decode(in.rawInst)

  out.src2SelType := decodeResult(Src2SelectField)
  out.src1SelType := decodeResult(Src1SelectField)
  out.destSelType := decodeResult(DestSelectField)
}

object SrcDstSelectDecoder {
  def main(args: Array[String]): Unit = {
    val (config, _, _) = ArgParser.parse(args)
    val defaultConfig = config.alterPartial({ case XSCoreParamsKey => XSCoreParameters })
    val instPatterns = VecInstPattern.withSew(VecInstPattern.all)
    Verilog.emitVerilog(
      new SrcDstSelectDecoder(instPatterns)(defaultConfig),
      Array("--target-dir", "build/decoder")
    )
  }

  class Out extends Bundle {
    val src2SelType = Src2SelectEnum.UInt()
    val src1SelType = Src1SelectEnum.UInt()
    val destSelType = DestSelectEnum.UInt()
  }
}
