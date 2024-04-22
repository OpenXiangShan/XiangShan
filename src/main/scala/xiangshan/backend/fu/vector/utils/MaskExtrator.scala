package xiangshan.backend.fu.vector.utils

import chisel3._
import chisel3.util._
import xiangshan.backend.fu.vector.Bundles.VSew

class MaskExtractorIO(vlen: Int) extends Bundle {
  private val numBytes = vlen / 8

  val in = Input(new Bundle {
    val mask = UInt(numBytes.W)
    val vsew = VSew()
  })
  val out = Output(new Bundle {
    val mask = UInt(numBytes.W)
  })
}

class MaskExtractor(vlen: Int) extends Module {
  private val numBytes = vlen / 8

  val io = IO(new MaskExtractorIO(vlen))

  private val mask = io.in.mask
  private val vsew = io.in.vsew
  private val extractedMask = Wire(UInt(vlen.W))

  extractedMask := Mux1H(Seq(
    (vsew === VSew.e8)  -> mask,
    (vsew === VSew.e16) -> VecInit(mask.asBools.flatMap(Seq.fill(2)(_))).asUInt,
    (vsew === VSew.e32) -> VecInit(mask.asBools.flatMap(Seq.fill(4)(_))).asUInt,
    (vsew === VSew.e64) -> VecInit(mask.asBools.flatMap(Seq.fill(8)(_))).asUInt,
  ))

  io.out.mask := extractedMask
}

object MaskExtractor {
  def apply(vlen: Int)(mask: UInt, vsew: UInt): UInt = {
    val maskExtractor = Module(new MaskExtractor(vlen))
    maskExtractor.io.in.mask := mask
    maskExtractor.io.in.vsew := vsew
    maskExtractor.io.out.mask
  }
}

object VerilogMaskExtrator extends App {
  println("Generating the MaskExtractor hardware")
  emitVerilog(new MaskExtractor(128), Array("--full-stacktrace", "--target-dir", "build/MaskExtractor"))
}
