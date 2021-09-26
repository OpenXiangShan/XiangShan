package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import utils._
import freechips.rocketchip.tilelink.{ClientMetadata, ClientStates, TLPermissions}

class NewMainPipeReq(implicit p: Parameters) extends DCacheBundle {
  val probe = Bool()
  val probe_param = UInt(TLPermissions.bdWidth.W)
  val probe_need_data = Bool()

  // request info
  // reqs from Store, AMO use this
  // probe does not use this
  val source = UInt(sourceTypeWidth.W)
  val cmd = UInt(M_SZ.W)
  // if dcache size > 32KB, vaddr is also needed for store
  // vaddr is used to get extra index bits
  val vaddr  = UInt(VAddrBits.W)
  // must be aligned to block
  val addr   = UInt(PAddrBits.W)

  // store
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(cfg.blockBytes * 8 / DataBits).W)
  val amo_data   = UInt(DataBits.W)
  val amo_mask   = UInt((DataBits / 8).W)

  val id = UInt(reqIdWidth.W)
}

class NewMainPipe(implicit p: Parameters) extends DCacheModule {
  val encMetaBits = cacheParams.tagCode.width((new MetaAndTag).getWidth) - tagBits

  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new NewMainPipeReq))
    // miss queue
    val miss_req = DecoupledIO(new NewMissReq)
    // store buffer
    val store_resp = ValidIO(new DCacheLineResp)
    // write-back queue
    val wb = DecoupledIO(new WritebackReq)

    val data_read = DecoupledIO(new L1BankedDataReadLineReq)
    val data_resp = Input(Vec(DCacheBanks, new L1BankedDataReadResult()))
    val data_write = DecoupledIO(new L1BankedDataWriteReq)

    val meta_read = DecoupledIO(new MetaReadReq)
    val meta_resp = Input(Vec(nWays, UInt(encMetaBits.W)))
    val meta_write = DecoupledIO(new MetaWriteReq)

    val tag_read = DecoupledIO(new TagReadReq)
    val tag_resp = Input(Vec(nWays, UInt(tagBits.W)))
    val tag_write = DecoupledIO(new TagWriteReq)

    // update state vec in replacement algo
    val replace_access = Flipped(Vec(LoadPipelineWidth, ValidIO(new ReplacementAccessBundle)))

    // load fast wakeup should be disabled when data read is not ready
    val disable_ld_fast_wakeup = Output(Vec(LoadPipelineWidth, Bool()))
  })
}
