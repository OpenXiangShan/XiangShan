package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import fudian.utils.SignExt
import utility.LookupTree
import xiangshan.SelImm
import xiangshan.backend.decode.{Imm, ImmUnion}
import xiangshan.backend.datapath.DataConfig._

import scala.collection.MapView

class ImmExtractorIO(dataBits: Int) extends Bundle {
  val in = Input(new Bundle {
    val imm = UInt(32.W)
    val immType = SelImm()
  })
  val out = Output(new Bundle {
    val imm = UInt(dataBits.W)
  })
}

class ImmExtractor(dataBits: Int, immTypeSet: Set[Imm]) extends Module {
  val io = IO(new ImmExtractorIO(dataBits))

  if (immTypeSet.nonEmpty) {
    io.out.imm := LookupTree(
      io.in.immType,
      immTypeSet.toSeq.map(
        immType => immType.typEncode -> SignExt(immType.toImm32(io.in.imm), dataBits)
      )
    )
  }
  else {
    io.out.imm := 0.U
  }
}

object ImmExtractor {
  def apply(imm: UInt, immType: UInt, dataBits: Int, immTypeSet: Set[Imm]): UInt = {
    val mod = Module(new ImmExtractor(dataBits, immTypeSet))
    mod.io.in.imm := imm
    mod.io.in.immType := immType
    mod.io.out.imm
  }
}
