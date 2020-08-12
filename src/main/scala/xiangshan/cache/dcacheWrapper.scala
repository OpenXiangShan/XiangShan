package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._
import xiangshan.{MicroOp}

// Meta data for dcache requests
// anything that should go with reqs and resps goes here
class DCacheMeta extends DCacheBundle {
  val id      = UInt(reqIdWidth.W)
  val vaddr   = UInt(VAddrBits.W) // maybe we should use VAddrBits?
  val paddr   = UInt(PAddrBits.W)
  val uop     = new MicroOp //FIXME: opt data width
  val mmio    = Bool()
  val tlb_miss = Bool()
  // dcache request id
  // master uses id to correlate resps to reqs
  // different master can allocate and free ids independently
  // as long as they do not share resp  
  val mask    = UInt((DataBits/8).W)
  val replay  = Bool() // whether it's a replayed request?
}

// ordinary load and special memory operations(lr/sc, atomics)
class DCacheLoadReq extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val meta   = new DCacheMeta
}

// ordinary store
class DCacheStoreReq extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val meta   = new DCacheMeta
}

class DCacheResp extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  val meta         = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val nack         = Bool()
}

class DCacheLoadIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLoadReq)
  val resp = Flipped(DecoupledIO(new DCacheResp))
}

class DCacheStoreIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheStoreReq)
  val resp = Flipped(DecoupledIO(new DCacheResp))
}

class DCacheToLsuIO extends DCacheBundle {
  val load     = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsroq    = Flipped(new DCacheLoadIO)  // lsroq load/store
  val store    = Flipped(new DCacheStoreIO) // for sbuffer
}

class DCacheIO extends DCacheBundle {
  val lsu = new DCacheToLsuIO
  val bus = new TLCached(cfg.busParams)
}

class DCache extends DCacheModule {
  val io = IO(new DCacheIO)

  //----------------------------------------
  // core data structures
  val dataArray = Module(new DuplicatedDataArray)
  val metaArray = Module(new DuplicatedMetaArray)


  //----------------------------------------
  // core modules
  val ldu = Seq.fill(LoadPipelineWidth) { Module(new LoadPipe) }
  val stu = Module(new StorePipe)
  val loadMissQueue = Module(new LoadMissQueue)
  val storeMissQueue = Module(new StoreMissQueue)
  val missQueue = Module(new MissQueue)
  val wb = Module(new WritebackUnit)


  //----------------------------------------
  // meta array
  val MetaWritePortCount = 2
  val MissQueueMetaWritePort = 0
  val ProberMetaWritePort = 1
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, MetaWritePortCount))

  metaWriteArb.io.in(MissQueueMetaWritePort)    <> missQueue.io.meta_write
  metaWriteArb.io.in(ProberMetaWritePort).valid := false.B
  metaWriteArb.io.in(ProberMetaWritePort).bits  := DontCare

  metaArray.io.write <> metaWriteArb.io.out

  // To simplify port arbitration
  // MissQueue, Prober and StorePipe all use port 0
  val MetaReadPortCount = 4
  val MissQueueMetaReadPort = 0
  val ProberMetaReadPort = 1
  val StorePipeMetaReadPort = 2
  val LoadPipeMetaReadPort = 3

  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, MetaReadPortCount))

  metaReadArb.io.in(MissQueueMetaReadPort)    <> missQueue.io.meta_read
  metaReadArb.io.in(ProberMetaReadPort).valid := false.B
  metaReadArb.io.in(ProberMetaReadPort).bits  := DontCare
  metaReadArb.io.in(StorePipeMetaReadPort)    <> stu.io.meta_read
  metaReadArb.io.in(LoadPipeMetaReadPort)     <> ldu(0).io.meta_read

  metaArray.io.read(0) <> metaReadArb.io.out

  metaArray.io.resp(0) <> missQueue.io.meta_resp
  // metaArray.io.resp(0) <> prober.io.meta_resp
  metaArray.io.resp(0) <> stu.io.meta_resp
  metaArray.io.resp(0) <> ldu(0).io.meta_resp

  for (w <- 1 until LoadPipelineWidth) {
    metaArray.io.read(w) <> ldu(w).io.meta_read
    metaArray.io.resp(w) <> ldu(w).io.meta_resp
  }

  //----------------------------------------
  // meta array
  val DataWritePortCount = 2
  val MissQueueDataWritePort = 0
  val StorePipeDataWritePort = 1

  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, DataWritePortCount))

  dataWriteArb.io.in(MissQueueDataWritePort) <> missQueue.io.refill
  dataWriteArb.io.in(StorePipeDataWritePort) <> stu.io.data_write

  dataArray.io.write <> dataWriteArb.io.out

  // To simplify port arbitration
  // WritebackUnit and StorePipe use port 0
  val DataReadPortCount = 3
  val WritebackDataReadPort = 0
  val StorePipeDataReadPort = 1
  val LoadPipeDataReadPort = 2

  val dataReadArb = Module(new Arbiter(new L1DataReadReq, DataReadPortCount))

  dataReadArb.io.in(WritebackDataReadPort) <> wb.io.data_req
  dataReadArb.io.in(StorePipeDataReadPort) <> stu.io.data_read
  dataReadArb.io.in(LoadPipeDataReadPort)  <> ldu(0).io.data_read

  dataArray.io.read(0) <> dataReadArb.io.out
  dataArray.io.resp(0) <> wb.io.data_resp
  dataArray.io.resp(0) <> stu.io.data_resp
  dataArray.io.resp(0) <> ldu(0).io.data_resp

  for (w <- 1 until LoadPipelineWidth) {
    dataArray.io.read(w) <> ldu(w).io.data_read
    dataArray.io.resp(w) <> ldu(w).io.data_resp
  }

  //----------------------------------------
  // load pipe and load miss queue
  // load miss queue replays on ldu 0
  val loadArb = Module(new Arbiter(new DCacheLoadReq, 2))
  val loadReplay = loadMissQueue.io.replay
  val lsu_0 = io.lsu.load(0)
  val ldu_0 = ldu(0).io.lsu
  loadArb.io.in(0) <> loadReplay.req
  loadArb.io.in(1) <> lsu_0.req
  assert(!(lsu_0.req.fire() && lsu_0.req.bits.meta.replay), "LSU should not replay requests")
  assert(!(loadReplay.req.fire() && !loadReplay.req.bits.meta.replay), "LoadMissQueue should replay requests")
  val ldu_0_block = block_load(loadArb.io.out.bits.addr)
  block_decoupled(loadArb.io.out, ldu_0.req, ldu_0_block)

  ldu_0.resp.ready := false.B

  val isReplay = ldu_0.resp.bits.meta.replay
  loadReplay.resp.valid := ldu_0.resp.valid && isReplay
  loadReplay.resp.bits  := ldu_0.resp.bits
  when (loadReplay.resp.valid) {
    ldu_0.resp.ready := loadReplay.resp.ready
  }

  lsu_0.resp.valid := ldu_0.resp.valid && !isReplay
  lsu_0.resp.bits  := ldu_0.resp.bits
  when (lsu_0.resp.valid) {
    ldu_0.resp.ready := lsu_0.resp.ready
  }

  for (w <- 1 until LoadPipelineWidth) {
    val load_w_block = block_load(io.lsu.load(w).req.bits.addr)
    block_decoupled(io.lsu.load(w).req, ldu(w).io.lsu.req, load_w_block)
    ldu(w).io.lsu.resp <> io.lsu.load(w).resp
  }

  // load miss queue
  loadMissQueue.io.lsu <> io.lsu.lsroq

  //----------------------------------------
  // store pipe and store miss queue
  storeMissQueue.io.lsu    <> io.lsu.store

  val store_block = block_store(storeMissQueue.io.replay.req.bits.addr)
  block_decoupled(storeMissQueue.io.replay.req, stu.io.lsu.req, store_block)
  storeMissQueue.io.replay.resp <> stu.io.lsu.resp

  //----------------------------------------
  // miss queue
  val loadMissQueueClientId = 0.U(clientIdWidth.W)
  val storeMissQueueClientId = 1.U(clientIdWidth.W)

  // Request
  val missReqArb = Module(new Arbiter(new MissReq, 2))

  val missReq      = missQueue.io.req
  val loadMissReq  = loadMissQueue.io.miss_req
  val storeMissReq = storeMissQueue.io.miss_req

  missReqArb.io.in(0).valid          := loadMissReq.valid
  loadMissReq.ready                  := missReqArb.io.in(0).ready
  missReqArb.io.in(0).bits.cmd       := loadMissReq.bits.cmd
  missReqArb.io.in(0).bits.addr      := loadMissReq.bits.addr
  missReqArb.io.in(0).bits.client_id := Cat(loadMissQueueClientId,
    loadMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(1).valid          := storeMissReq.valid
  storeMissReq.ready                 := missReqArb.io.in(1).ready
  missReqArb.io.in(1).bits.cmd       := storeMissReq.bits.cmd
  missReqArb.io.in(1).bits.addr      := storeMissReq.bits.addr
  missReqArb.io.in(1).bits.client_id := Cat(storeMissQueueClientId,
    storeMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  val miss_block = block_miss(missReqArb.io.out.bits.addr)
  block_decoupled(missReqArb.io.out, missReq, miss_block)

  // Response
  val missResp        = missQueue.io.resp
  val loadMissResp    = loadMissQueue.io.miss_resp
  val storeMissResp   = storeMissQueue.io.miss_resp

  val clientId = missResp.bits.client_id(entryIdMSB, entryIdLSB)

  val isLoadMissResp = clientId === loadMissQueueClientId
  loadMissResp.valid := missResp.valid && isLoadMissResp
  loadMissResp.bits  := missResp.bits

  val isStoreMissResp = clientId === storeMissQueueClientId
  storeMissResp.valid := missResp.valid && isStoreMissResp
  storeMissResp.bits  := missResp.bits

  // Finish
  val missFinish        = missQueue.io.finish
  val loadMissFinish    = loadMissQueue.io.miss_finish
  val storeMissFinish   = storeMissQueue.io.miss_finish

  val missFinishArb = Module(new Arbiter(new MissFinish, 2))
  missFinishArb.io.in(0).valid          := loadMissFinish.valid
  loadMissFinish.ready                  := missFinishArb.io.in(0).ready
  missFinishArb.io.in(0).bits.entry_id  := loadMissFinish.bits.entry_id
  missFinishArb.io.in(0).bits.client_id := Cat(loadMissQueueClientId,
    loadMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

  missFinishArb.io.in(1).valid          := storeMissFinish.valid
  storeMissFinish.ready                 := missFinishArb.io.in(1).ready
  missFinishArb.io.in(1).bits.entry_id  := storeMissFinish.bits.entry_id
  missFinishArb.io.in(1).bits.client_id := Cat(storeMissQueueClientId,
    storeMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

  missFinish                            <> missFinishArb.io.out

  // tilelink stuff
  io.bus.a <> missQueue.io.mem_acquire
  io.bus.e <> missQueue.io.mem_finish

  when (io.bus.d.bits.source === cfg.nMissEntries.U) {
    // This should be ReleaseAck
    io.bus.d.ready := true.B
    missQueue.io.mem_grant.valid := false.B
    missQueue.io.mem_grant.bits  := DontCare
  } .otherwise {
    // This should be GrantData
    missQueue.io.mem_grant <> io.bus.d
  }


  //----------------------------------------
  // prober
  io.bus.b.ready        := false.B

  //----------------------------------------
  // wb
  // 0 goes to prober, 1 goes to missQueue evictions
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  wbArb.io.in(0).valid := false.B
  wbArb.io.in(0).bits  := DontCare
  wbArb.io.in(1)       <> missQueue.io.wb_req
  wb.io.req            <> wbArb.io.out
  missQueue.io.wb_resp := wb.io.resp
  io.bus.c             <> wb.io.release
  wb.io.mem_grant      := io.bus.d.fire() && io.bus.d.bits.source === cfg.nMissEntries.U

  // synchronization stuff
  def block_load(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_addr_match || miss_idx_match
  }

  def block_store(addr: UInt) = {
    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    miss_idx_match
  }

  def block_miss(addr: UInt) = {
    val store_idx_matches = VecInit(stu.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val store_idx_match = store_idx_matches.reduce(_||_)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_idx_match || miss_idx_match
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }
}
