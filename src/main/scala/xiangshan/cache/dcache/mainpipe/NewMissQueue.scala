package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.tilelink._
import bus.tilelink.TLMessages._
import difftest._
import huancun.{AliasKey, DirtyKey, PreferCacheKey, PrefetchKey}

class NewMissReq(implicit p: Parameters) extends DCacheBundle {
  val source = UInt(sourceTypeWidth.W)
  val cmd = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
  val way_en = UInt(DCacheWays.W)

  // store
  val store_data = UInt((cfg.blockBytes * 8).W)
  val store_mask = UInt(cfg.blockBytes.W)

  // which word does amo work on?
  val word_idx = UInt(log2Up(blockWords).W)
  val amo_data = UInt(DataBits.W)
  val amo_mask = UInt((DataBits / 8).W)

  val coh = new ClientMetadata
  val id = UInt(reqIdWidth.W)

  def isLoad = source === LOAD_SOURCE.U
  def isStore = source === STORE_SOURCE.U
}

class NewMissEntry(edge: TLEdgeOut)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    // MSHR ID
    val id = Input(UInt())
    // client requests
    // this entry is free and can be allocated to new reqs
    val primary_ready = Output(Bool())
    // this entry is busy, but it can merge the new req
    val secondary_ready = Output(Bool())
    // this entry is busy and it can not merge the new req
    val secondary_reject = Output(Bool())
    val req    = Flipped(ValidIO(new NewMissReq))
    val refill_to_ldq = ValidIO(new Refill)
    // TODO: bypass refill data to load pipe

    // bus
    val mem_acquire = DecoupledIO(new TLBundleA(edge.bundle))
    val mem_grant = Flipped(DecoupledIO(new TLBundleD(edge.bundle)))
    val mem_finish = DecoupledIO(new TLBundleE(edge.bundle))

    // refill pipe
    val refill_pipe_req = DecoupledIO(new RefillPipeReq)

    // replace pipe
    val replace_pipe_req = DecoupledIO(new ReplacePipeReq)
    val replace_pipe_resp = Flipped(ValidIO(new ReplacePipeResp))

  })
}
