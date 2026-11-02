package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ifu.PreDecodeHelper

class TracePreDecoderIO(implicit p: Parameters) extends TraceBundle {
  val compact = Input(Vec(IBufferEnqueueWidth, new TraceCompactEntry))
  val out     = Output(Vec(IBufferEnqueueWidth, new TraceDecodedEntry))
}

class TracePreDecoder(implicit p: Parameters) extends TraceModule with PreDecodeHelper {
  val io = IO(new TracePreDecoderIO())

  for (i <- 0 until IBufferEnqueueWidth) {
    val trace     = io.compact(i).traceInfo
    val inst      = trace.inst
    val curIsRVC  = isRVC(inst)
    val jalOffset = getJalOffset(inst, curIsRVC)
    val brOffset  = getBrOffset(inst, curIsRVC)

    io.out(i)                := 0.U.asTypeOf(new TraceDecodedEntry)
    io.out(i).compact        := io.compact(i)
    io.out(i).pd.valid       := io.compact(i).valid
    io.out(i).pd.isRVC       := curIsRVC
    io.out(i).pd.brAttribute := BranchAttribute.decode(inst, io.compact(i).valid)
    io.out(i).jumpOffset     := Mux(io.out(i).pd.isBr, brOffset, jalOffset)
  }
}
