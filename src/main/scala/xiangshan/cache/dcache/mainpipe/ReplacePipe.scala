package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class ReplacePipeReq(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)

  // if dcache size > 32KB, vaddr is also needed for store
  // vaddr is used to get extra index bits
  val vaddr  = UInt(VAddrBits.W)
}

class ReplacePipeResp(implicit p: Parameters) extends DCacheBundle {
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
}

class ReplacePipe(implicit p: Parameters) extends  DCacheModule {
  val io = IO(new Bundle() {
    // miss queue
    val req = Flipped(DecoupledIO(new ReplacePipeReq))
    val resp = ValidIO(new ReplacePipeResp)

    // write-back queue
    val wb = DecoupledIO(new WritebackReq)

    // read data array, invalid meta array
    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult))
    val meta_write = DecoupledIO(new MetaWriteReq)
  })
}
