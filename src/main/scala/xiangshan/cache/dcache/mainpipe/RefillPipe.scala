package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class RefillPipeReq(implicit p: Parameters) extends DCacheBundle {
  val addr = UInt(PAddrBits.W)
  val way_en = UInt(DCacheWays.W)
  // val wmask = UInt(DCacheBanks.W)
  val data = Vec(DCacheBanks, UInt(DCacheSRAMRowBits.W))
  val meta = new Meta
  val alias = UInt(2.W) // TODO: parameterize
}

class RefillPipe(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new RefillPipeReq))
    val data_write = DecoupledIO(new L1BankedDataWriteReq)
    val meta_write = DecoupledIO(new MetaWriteReq)
    val tag_write = DecoupledIO(new TagWriteReq)
  })
  // TODO
}
