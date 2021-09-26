package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class TagReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val tag = UInt(tagBits.W)
}

class NewTagArray(readPorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(tagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })
}
