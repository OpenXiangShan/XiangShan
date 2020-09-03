package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters, TLMasterParameters, TLMasterPortParameters, TLArbiter}
import xiangshan.MicroOp

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
class DCacheWordReq  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val meta   = new DCacheMeta
}

// ordinary store
class DCacheLineReq  extends DCacheBundle
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
  val req  = DecoupledIO(new DCacheWordReq )
  val resp = Flipped(DecoupledIO(new DCacheResp))
  // kill previous cycle's req
  val s1_kill = Output(Bool())
}

class DCacheStoreIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq )
  val resp = Flipped(DecoupledIO(new DCacheResp))
}

class DCacheToLsuIO extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsroq = Flipped(new DCacheLoadIO)  // lsroq load/store
  val store = Flipped(new DCacheStoreIO) // for sbuffer
  val misc  = Flipped(new DCacheLoadIO)  // misc reqs
}

class DCacheIO extends DCacheBundle {
  val lsu = new DCacheToLsuIO
  // TODO: remove ptw port, it directly connect to L2
  val ptw = Flipped(new DCacheLoadIO)
}


class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, cfg.nMissEntries+1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head

  //----------------------------------------
  // core data structures
  val dataArray = Module(new DuplicatedDataArray)
  val metaArray = Module(new DuplicatedMetaArray)


  //----------------------------------------
  // core modules
  val ldu = Seq.fill(LoadPipelineWidth) { Module(new LoadPipe) }
  val stu = Module(new StorePipe)
  val misc = Module(new MiscPipe)
  val loadMissQueue = Module(new LoadMissQueue)
  val storeMissQueue = Module(new StoreMissQueue)
  val miscMissQueue = Module(new MiscMissQueue)
  val missQueue = Module(new MissQueue(edge))
  val wb = Module(new WritebackUnit(edge))
  val prober = Module(new ProbeUnit(edge))


  //----------------------------------------
  // meta array
  val MetaWritePortCount = 2
  val MissQueueMetaWritePort = 0
  val ProberMetaWritePort = 1
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, MetaWritePortCount))

  metaWriteArb.io.in(MissQueueMetaWritePort)    <> missQueue.io.meta_write
  metaWriteArb.io.in(ProberMetaWritePort)       <> prober.io.meta_write

  metaArray.io.write <> metaWriteArb.io.out

  // To simplify port arbitration
  // MissQueue, Prober and StorePipe all use port 0
  // if contention got severe, considering load balancing on two ports?
  val MetaReadPortCount = 5
  val MissQueueMetaReadPort = 0
  val ProberMetaReadPort = 1
  val StorePipeMetaReadPort = 2
  val LoadPipeMetaReadPort = 3
  val MiscPipeMetaReadPort = 4

  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, MetaReadPortCount))

  metaReadArb.io.in(MissQueueMetaReadPort)    <> missQueue.io.meta_read
  metaReadArb.io.in(ProberMetaReadPort)       <> prober.io.meta_read
  metaReadArb.io.in(StorePipeMetaReadPort)    <> stu.io.meta_read
  metaReadArb.io.in(LoadPipeMetaReadPort)     <> ldu(0).io.meta_read
  metaReadArb.io.in(MiscPipeMetaReadPort)     <> misc.io.meta_read

  metaArray.io.read(0) <> metaReadArb.io.out

  missQueue.io.meta_resp <>  metaArray.io.resp(0)
  prober.io.meta_resp    <>  metaArray.io.resp(0)
  stu.io.meta_resp       <>  metaArray.io.resp(0)
  ldu(0).io.meta_resp    <>  metaArray.io.resp(0)
  misc.io.meta_resp      <>  metaArray.io.resp(0)

  for (w <- 1 until LoadPipelineWidth) {
    metaArray.io.read(w) <> ldu(w).io.meta_read
    ldu(w).io.meta_resp <> metaArray.io.resp(w)
  }

  //----------------------------------------
  // data array
  val DataWritePortCount = 3
  val StorePipeDataWritePort = 0
  val MissQueueDataWritePort = 1
  val MiscPipeDataWritePort = 2

  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, DataWritePortCount))

  dataWriteArb.io.in(StorePipeDataWritePort) <> stu.io.data_write
  dataWriteArb.io.in(MissQueueDataWritePort) <> missQueue.io.refill
  dataWriteArb.io.in(MiscPipeDataWritePort)  <> misc.io.data_write

  dataArray.io.write <> dataWriteArb.io.out

  // To simplify port arbitration
  // WritebackUnit and StorePipe use port 0
  val DataReadPortCount = 4
  val WritebackDataReadPort = 0
  val StorePipeDataReadPort = 1
  val LoadPipeDataReadPort = 2
  val MiscPipeDataReadPort = 3

  val dataReadArb = Module(new Arbiter(new L1DataReadReq, DataReadPortCount))

  dataReadArb.io.in(WritebackDataReadPort) <> wb.io.data_req
  dataReadArb.io.in(StorePipeDataReadPort) <> stu.io.data_read
  dataReadArb.io.in(MiscPipeDataReadPort)  <> misc.io.data_read
  dataReadArb.io.in(LoadPipeDataReadPort)  <> ldu(0).io.data_read

  dataArray.io.read(0) <> dataReadArb.io.out
  dataArray.io.resp(0) <> wb.io.data_resp
  dataArray.io.resp(0) <> stu.io.data_resp
  dataArray.io.resp(0) <> misc.io.data_resp
  dataArray.io.resp(0) <> ldu(0).io.data_resp

  for (w <- 1 until LoadPipelineWidth) {
    dataArray.io.read(w) <> ldu(w).io.data_read
    dataArray.io.resp(w) <> ldu(w).io.data_resp
  }

  //----------------------------------------
  // load pipe and load miss queue
  // load miss queue replays on ldu 0
  val loadArb = Module(new Arbiter(new DCacheWordReq , 2))
  val loadReplay = loadMissQueue.io.replay
  val lsu_0 = io.lsu.load(0)
  val ldu_0 = ldu(0).io.lsu
  loadArb.io.in(0) <> loadReplay.req
  loadArb.io.in(1) <> lsu_0.req
  assert(!(lsu_0.req.fire() && lsu_0.req.bits.meta.replay), "LSU should not replay requests")
  assert(!(loadReplay.req.fire() && !loadReplay.req.bits.meta.replay), "LoadMissQueue should replay requests")
  val ldu_0_nack = nack_load(loadArb.io.out.bits.addr)
  // do not nack replayed reqs
  ldu_0.req <> loadArb.io.out
  ldu(0).io.nack := ldu_0_nack && !loadArb.io.out.bits.meta.replay

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

  // the s1 kill signal
  // only lsu uses this, replay never kills
  ldu_0.s1_kill := lsu_0.s1_kill

  for (w <- 1 until LoadPipelineWidth) {
    val load_w_nack = nack_load(io.lsu.load(w).req.bits.addr)
    ldu(w).io.lsu.req <> io.lsu.load(w).req
    ldu(w).io.nack := load_w_nack

    ldu(w).io.lsu.resp <> io.lsu.load(w).resp
    ldu(w).io.lsu.s1_kill <> io.lsu.load(w).s1_kill
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.replay), "LSU should not replay requests")
  }

  for (w <- 0 until LoadPipelineWidth) {
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.mmio), "MMIO requests should not go to cache")
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.tlb_miss), "TLB missed requests should not go to cache")
  }

  // load miss queue
  loadMissQueue.io.lsu <> io.lsu.lsroq
  assert(!io.lsu.lsroq.s1_kill, "Lsroq should never use s1 kill on loadMissQueue")

  //----------------------------------------
  // store pipe and store miss queue
  storeMissQueue.io.lsu    <> io.lsu.store
  /*
  assert(!(storeMissQueue.io.replay.req.fire() && !storeMissQueue.io.replay.req.bits.meta.replay),
    "StoreMissQueue should replay requests")
  */
  assert(!(io.lsu.store.req.fire() && io.lsu.store.req.bits.meta.replay),
    "Sbuffer should not should replay requests")
  assert(!(io.lsu.store.req.fire() && io.lsu.store.req.bits.meta.mmio),
    "MMIO requests should not go to cache")
  assert(!(io.lsu.store.req.fire() && io.lsu.store.req.bits.meta.tlb_miss),
    "TLB missed requests should not go to cache")

  val store_block = block_store(storeMissQueue.io.replay.req.bits.addr)
  block_decoupled(storeMissQueue.io.replay.req, stu.io.lsu.req, store_block && !storeMissQueue.io.replay.req.bits.meta.replay)
  storeMissQueue.io.replay.resp <> stu.io.lsu.resp

  //----------------------------------------
  // misc pipe
  misc.io.wb_invalidate_lrsc := wb.io.inflight_addr
  miscMissQueue.io.replay <> misc.io.lsu
  val miscClientIdWidth = 1
  val lsuMiscClientId = 0.U(miscClientIdWidth.W)
  val ptwMiscClientId = 1.U(miscClientIdWidth.W)
  val miscClientIdMSB = reqIdWidth - 1
  val miscClientIdLSB = reqIdWidth - miscClientIdWidth

  // Request
  val miscReqArb = Module(new Arbiter(new DCacheWordReq, 2))

  val miscReq    = miscMissQueue.io.lsu.req
  val lsuMiscReq = io.lsu.misc.req
  val ptwMiscReq = io.ptw.req

  miscReqArb.io.in(0).valid        := lsuMiscReq.valid
  lsuMiscReq.ready                 := miscReqArb.io.in(0).ready
  miscReqArb.io.in(0).bits         := lsuMiscReq.bits
  miscReqArb.io.in(0).bits.meta.id := Cat(lsuMiscClientId,
    lsuMiscReq.bits.meta.id(miscClientIdLSB - 1, 0))

  miscReqArb.io.in(1).valid        := ptwMiscReq.valid
  ptwMiscReq.ready                 := miscReqArb.io.in(1).ready
  miscReqArb.io.in(1).bits         := ptwMiscReq.bits
  miscReqArb.io.in(1).bits.meta.id := Cat(ptwMiscClientId,
    ptwMiscReq.bits.meta.id(miscClientIdLSB - 1, 0))

  val misc_block = block_misc(miscReqArb.io.out.bits.addr)
  block_decoupled(miscReqArb.io.out, miscReq, misc_block)

  // Response
  val miscResp    = miscMissQueue.io.lsu.resp
  val lsuMiscResp = io.lsu.misc.resp
  val ptwMiscResp = io.ptw.resp

  miscResp.ready  := false.B

  val miscClientId = miscResp.bits.meta.id(miscClientIdMSB, miscClientIdLSB)

  val isLsuMiscResp  = miscClientId === lsuMiscClientId
  lsuMiscResp.valid := miscResp.valid && isLsuMiscResp
  lsuMiscResp.bits  := miscResp.bits
  lsuMiscResp.bits.meta.id := miscResp.bits.meta.id(miscClientIdLSB - 1, 0)
  when (lsuMiscResp.valid) {
    miscResp.ready := lsuMiscResp.ready
  }

  val isPTWMiscResp  = miscClientId === ptwMiscClientId
  ptwMiscResp.valid := miscResp.valid && isPTWMiscResp
  ptwMiscResp.bits  := miscResp.bits
  ptwMiscResp.bits.meta.id := miscResp.bits.meta.id(miscClientIdLSB - 1, 0)
  when (ptwMiscResp.valid) {
    miscResp.ready := ptwMiscResp.ready
  }

  // some other stuff
  miscMissQueue.io.lsu.s1_kill := false.B

  assert(!(miscReq.fire() && miscReq.bits.meta.replay),
    "Misc does not support request replay")
  assert(!(miscReq.fire() && miscReq.bits.meta.mmio),
    "MMIO requests should not go to cache")
  assert(!(miscReq.fire() && miscReq.bits.meta.tlb_miss),
    "TLB missed requests should not go to cache")
  assert(!io.lsu.misc.s1_kill, "Lsroq should never use s1 kill on misc")
  assert(!io.ptw.s1_kill, "Lsroq should never use s1 kill on misc") // TODO: ptw wanna use s1_kill


  //----------------------------------------
  // miss queue
  val loadMissQueueClientId  = 0.U(clientIdWidth.W)
  val storeMissQueueClientId = 1.U(clientIdWidth.W)
  val miscMissQueueClientId  = 2.U(clientIdWidth.W)

  // Request
  val missReqArb = Module(new Arbiter(new MissReq, 3))

  val missReq      = missQueue.io.req
  val loadMissReq  = loadMissQueue.io.miss_req
  val storeMissReq = storeMissQueue.io.miss_req
  val miscMissReq  = miscMissQueue.io.miss_req

  missReqArb.io.in(0).valid          := loadMissReq.valid
  loadMissReq.ready                  := missReqArb.io.in(0).ready
  missReqArb.io.in(0).bits           := loadMissReq.bits
  missReqArb.io.in(0).bits.client_id := Cat(loadMissQueueClientId,
    loadMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(1).valid          := storeMissReq.valid
  storeMissReq.ready                 := missReqArb.io.in(1).ready
  missReqArb.io.in(1).bits           := storeMissReq.bits
  missReqArb.io.in(1).bits.client_id := Cat(storeMissQueueClientId,
    storeMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(2).valid          := miscMissReq.valid
  miscMissReq.ready                  := missReqArb.io.in(2).ready
  missReqArb.io.in(2).bits           := miscMissReq.bits
  missReqArb.io.in(2).bits.client_id := Cat(miscMissQueueClientId,
    miscMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  val miss_block = block_miss(missReqArb.io.out.bits.addr)
  block_decoupled(missReqArb.io.out, missReq, miss_block)

  // Response
  val missResp        = missQueue.io.resp
  val loadMissResp    = loadMissQueue.io.miss_resp
  val storeMissResp   = storeMissQueue.io.miss_resp
  val miscMissResp    = miscMissQueue.io.miss_resp

  val clientId = missResp.bits.client_id(clientIdMSB, clientIdLSB)

  val isLoadMissResp = clientId === loadMissQueueClientId
  loadMissResp.valid := missResp.valid && isLoadMissResp
  loadMissResp.bits.entry_id := missResp.bits.entry_id
  loadMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  val isStoreMissResp = clientId === storeMissQueueClientId
  storeMissResp.valid := missResp.valid && isStoreMissResp
  storeMissResp.bits.entry_id := missResp.bits.entry_id
  storeMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  val isMiscMissResp = clientId === miscMissQueueClientId
  miscMissResp.valid := missResp.valid && isMiscMissResp
  miscMissResp.bits.entry_id := missResp.bits.entry_id
  miscMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  // Finish
  val missFinish        = missQueue.io.finish
  val loadMissFinish    = loadMissQueue.io.miss_finish
  val storeMissFinish   = storeMissQueue.io.miss_finish
  val miscMissFinish    = miscMissQueue.io.miss_finish

  val missFinishArb = Module(new Arbiter(new MissFinish, 3))
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

  missFinishArb.io.in(2).valid          := miscMissFinish.valid
  miscMissFinish.ready                  := missFinishArb.io.in(2).ready
  missFinishArb.io.in(2).bits.entry_id  := miscMissFinish.bits.entry_id
  missFinishArb.io.in(2).bits.client_id := Cat(miscMissQueueClientId,
    miscMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

  missFinish                            <> missFinishArb.io.out

  // tilelink stuff
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish

  when (bus.d.bits.source === cfg.nMissEntries.U) {
    // This should be ReleaseAck
    bus.d.ready := true.B
    missQueue.io.mem_grant.valid := false.B
    missQueue.io.mem_grant.bits  := DontCare
  } .otherwise {
    // This should be GrantData
    missQueue.io.mem_grant <> bus.d
  }


  //----------------------------------------
  // prober
  prober.io.block := block_probe(prober.io.inflight_req_block_addr.bits)
  prober.io.req <> bus.b

  //----------------------------------------
  // wb
  // 0 goes to prober, 1 goes to missQueue evictions
  val wbArb = Module(new Arbiter(new WritebackReq(edge.bundle.sourceBits), 2))
  wbArb.io.in(0)       <> prober.io.wb_req
  wbArb.io.in(1)       <> missQueue.io.wb_req
  wb.io.req            <> wbArb.io.out
  missQueue.io.wb_resp := wb.io.resp
  prober.io.wb_resp    := wb.io.resp
  wb.io.mem_grant      := bus.d.fire() && bus.d.bits.source === cfg.nMissEntries.U

  TLArbiter.lowestFromSeq(edge, bus.c, Seq(prober.io.rep, wb.io.release))

  // synchronization stuff
  def nack_load(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val misc_addr_matches = VecInit(misc.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val misc_addr_match = misc_addr_matches.reduce(_||_)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_addr_match || misc_addr_match || miss_idx_match
  }

  def block_store(addr: UInt) = {
    val misc_addr_matches = VecInit(misc.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val misc_addr_match = misc_addr_matches.reduce(_||_)

    val prober_addr_match = prober.io.inflight_req_block_addr.valid && prober.io.inflight_req_block_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    misc_addr_match || prober_addr_match || miss_idx_match
  }

  def block_misc(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val prober_addr_match = prober.io.inflight_req_block_addr.valid && prober.io.inflight_req_block_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    store_addr_match || prober_addr_match || miss_idx_match
  }

  def block_miss(addr: UInt) = {
    val store_idx_matches = VecInit(stu.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val store_idx_match = store_idx_matches.reduce(_||_)

    val misc_idx_matches = VecInit(misc.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val misc_idx_match = misc_idx_matches.reduce(_||_)

    val prober_idx_match = prober.io.inflight_req_idx.valid && prober.io.inflight_req_idx.bits === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_idx_match || misc_idx_match || prober_idx_match || miss_idx_match
  }

  def block_probe(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val misc_addr_matches = VecInit(misc.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val misc_addr_match = misc_addr_matches.reduce(_||_)

    val lrsc_addr_match = misc.io.block_probe_addr.valid && misc.io.block_probe_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.block_probe_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_addr_match || misc_addr_match || lrsc_addr_match || miss_idx_match
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }
}
