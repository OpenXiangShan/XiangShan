package xiangshan.backend.vector.datapath

import chisel3._
import chisel3.util.Fill
import utility.LookupTree
import xiangshan.SelImm
import xiangshan.backend.decode.{Imm, Imm_OPIVIS, Imm_OPIVIU}
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.vector.Decoder.Sews
import xiangshan.backend.vector.datapath.VecImmExtractor._
import xiangshan.backend.vector.util.Verilog

class VecImmExtractor(vlen: Int, immTypeSeq: Set[Imm]) extends Module {
  val in = IO(Input(new In))
  val out = IO(Output(new Out(vlen)))

  val extractMap: Seq[(UInt, UInt)] =
    for {
      vsew <- Sews.all
      immType <- immTypeSeq
    } yield {
      val sew = Sews.decodeValue(vsew)
      (vsew ## immType.typEncode) -> Fill(vlen / sew, immType.extract(sew)(in.imm))
    }

  out.data := LookupTree(in.vsew ## in.immType, extractMap)
}

object VecImmExtractor {
  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new VecImmExtractor(128, Set(Imm_OPIVIS(), Imm_OPIVIU())),
      Array(
        "--full-stacktrace",
        "--target-dir", "build-VecImmExtractor",
      )
    )
  }

  def apply(vlen: Int, immTypeSet: Set[Imm])(imm: UInt, immType: UInt, vsew: UInt): UInt = {
    val mod = Module(new VecImmExtractor(vlen, immTypeSet))
    mod.in.imm := imm
    mod.in.immType := immType
    mod.in.vsew := vsew
    mod.out.data
  }

  class In extends Bundle {
    val imm = UInt(32.W)
    val immType = SelImm()
    val vsew = VSew()
  }

  class Out(vlen: Int) extends Bundle {
    val data = UInt(vlen.W)
  }
}
