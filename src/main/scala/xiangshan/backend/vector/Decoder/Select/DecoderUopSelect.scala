package xiangshan.backend.vector.Decoder.Select

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeField, DecodeTable}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{UopNumOHsPattern, UopNumOHsPatterns}
import xiangshan.backend.vector.Decoder.Types.UopBufferNum
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.HasVectorSettings
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.Verilog
import xiangshan.backend.vector.util.Select.Mux1HLookUp


class DecoderUopSelectModule[T <: Data](
  uopWidth: Int = 8,
  uopBufferSize: Int = 7,
  gen: => T,
) extends Module with HasVectorSettings {
  val bundle = gen
  val width = gen.getWidth

  override def desiredName: String = s"DecoderUopSelectModule_${bundle}"

  val in = IO(Input(new Bundle {
    val uopFromBuffer = Vec(uopBufferSize, ValidIO(UInt(width.W)))
    val uopFromChannel = Vec(uopWidth + uopBufferSize, ValidIO(UInt(width.W)))
    val uopBufferNum = UopBufferNum()
  }))

  val out = IO(Output(new Bundle {
    val uopToRename = Vec(uopWidth, ValidIO(bundle))
    val uopToBuffer = Vec(uopBufferSize, ValidIO(bundle))
  }))

  private val uopBufferNum = in.uopBufferNum

  for (i <- 0 until uopWidth) {
    out.uopToRename(i) := (
      if (in.uopFromBuffer.isDefinedAt(i))
        Mux(uopBufferNum >= (i + 1).U, in.uopFromBuffer(i), in.uopFromChannel(i.U - uopBufferNum)).asTypeOf(ValidIO(bundle))
      else
        in.uopFromChannel(i.U - uopBufferNum).asTypeOf(ValidIO(bundle))
    )
  }

  for (i <- 0 until uopBufferSize) {
    out.uopToBuffer(i) := in.uopFromChannel((i + uopWidth).U(log2Up(uopWidth + uopBufferSize).W) - uopBufferNum).asTypeOf(ValidIO(bundle))
  }
}
