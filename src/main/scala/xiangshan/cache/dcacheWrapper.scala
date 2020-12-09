package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink.{TLClientNode, TLClientParameters, TLMasterParameters, TLMasterPortParameters, TLArbiter}

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

// for load from load unit
// cycle 0: vaddr
// cycle 1: paddr
class DCacheLoadReq  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(VAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val meta   = new DCacheMeta
}

// special memory operations(lr/sc, atomics)
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

class DCacheLoadResp extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  val meta         = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val nack         = Bool()
}

class DCacheWordResp extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  val meta         = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val nack         = Bool()
}

class DCacheLineResp extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  val meta   = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val nack   = Bool()
}

class DCacheLoadIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
  // kill previous cycle's req
  val s1_kill  = Output(Bool())
  val s1_paddr   = Output(UInt(PAddrBits.W))
}

class DCacheWordIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}

class DCacheLineIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq )
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToLsuIO extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = Flipped(new DCacheLineIO)  // lsq load/store
  val store = Flipped(new DCacheLineIO) // for sbuffer
  val atomics  = Flipped(new DCacheWordIO)  // atomics reqs
}

class DCacheIO extends DCacheBundle {
  val lsu = new DCacheToLsuIO
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


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters with HasXSLog {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCache: tilelink width does not match")

  //----------------------------------------
  // core data structures
  val dataArray = Module(new DuplicatedDataArray)
  val metaArray = Module(new DuplicatedMetaArray)
  dataArray.dump()
  metaArray.dump()


  //----------------------------------------
  // core modules
  val ldu = Seq.fill(LoadPipelineWidth) { Module(new LoadPipe) }
  val stu = Module(new StorePipe)
  val atomics = Module(new AtomicsPipe)
  val loadMissQueue = Module(new LoadMissQueue)
  val storeMissQueue = Module(new StoreMissQueue)
  val atomicsMissQueue = Module(new AtomicsMissQueue)
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
  val AtomicsPipeMetaReadPort = 4

  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, MetaReadPortCount))

  metaReadArb.io.in(MissQueueMetaReadPort)    <> missQueue.io.meta_read
  metaReadArb.io.in(ProberMetaReadPort)       <> prober.io.meta_read
  metaReadArb.io.in(StorePipeMetaReadPort)    <> stu.io.meta_read
  metaReadArb.io.in(LoadPipeMetaReadPort)     <> ldu(0).io.meta_read
  metaReadArb.io.in(AtomicsPipeMetaReadPort)  <> atomics.io.meta_read

  metaArray.io.read(0) <> metaReadArb.io.out

  missQueue.io.meta_resp <>  metaArray.io.resp(0)
  prober.io.meta_resp    <>  metaArray.io.resp(0)
  stu.io.meta_resp       <>  metaArray.io.resp(0)
  ldu(0).io.meta_resp    <>  metaArray.io.resp(0)
  atomics.io.meta_resp      <>  metaArray.io.resp(0)

  for (w <- 1 until LoadPipelineWidth) {
    metaArray.io.read(w) <> ldu(w).io.meta_read
    ldu(w).io.meta_resp <> metaArray.io.resp(w)
  }

  //----------------------------------------
  // data array
  val DataWritePortCount = 3
  val StorePipeDataWritePort = 0
  val AtomicsPipeDataWritePort = 1
  val MissQueueDataWritePort = 2

  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, DataWritePortCount))

  dataWriteArb.io.in(StorePipeDataWritePort) <> stu.io.data_write
  dataWriteArb.io.in(MissQueueDataWritePort) <> missQueue.io.refill
  dataWriteArb.io.in(AtomicsPipeDataWritePort)  <> atomics.io.data_write

  dataArray.io.write <> dataWriteArb.io.out

  // To simplify port arbitration
  // WritebackUnit and StorePipe use port 0
  val DataReadPortCount = 5
  val WritebackDataReadPort = 0
  val StorePipeDataReadPort = 1
  val LoadPipeDataReadPort = 2
  val AtomicsPipeDataReadPort = 3
  val LoadMissDataReadPort = 4

  val dataReadArb = Module(new Arbiter(new L1DataReadReq, DataReadPortCount))

  dataReadArb.io.in(WritebackDataReadPort) <> wb.io.data_req
  dataReadArb.io.in(StorePipeDataReadPort) <> stu.io.data_read
  dataReadArb.io.in(LoadPipeDataReadPort)  <> ldu(0).io.data_read
  dataReadArb.io.in(AtomicsPipeDataReadPort) <> atomics.io.data_read
  dataReadArb.io.in(LoadMissDataReadPort)    <> loadMissQueue.io.data_req

  dataArray.io.read(0) <> dataReadArb.io.out
  dataArray.io.resp(0) <> wb.io.data_resp
  dataArray.io.resp(0) <> stu.io.data_resp
  dataArray.io.resp(0) <> atomics.io.data_resp
  dataArray.io.resp(0) <> ldu(0).io.data_resp
  dataArray.io.resp(0) <> loadMissQueue.io.data_resp

  for (w <- 1 until LoadPipelineWidth) {
    dataArray.io.read(w) <> ldu(w).io.data_read
    dataArray.io.resp(w) <> ldu(w).io.data_resp
  }

  //----------------------------------------
  // load pipe and load miss queue
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    val load_w_nack = nack_load(io.lsu.load(w).req.bits.addr)
    ldu(w).io.lsu.req <> io.lsu.load(w).req
    ldu(w).io.lsu.s1_paddr <> io.lsu.load(w).s1_paddr
    ldu(w).io.nack := load_w_nack
    XSDebug(load_w_nack, s"LoadUnit $w nacked\n")

    ldu(w).io.lsu.resp <> io.lsu.load(w).resp
    ldu(w).io.lsu.s1_kill <> io.lsu.load(w).s1_kill
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.replay), "LSU should not replay requests")
  }

  for (w <- 0 until LoadPipelineWidth) {
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.mmio), "MMIO requests should not go to cache")
    assert(!(io.lsu.load(w).req.fire() && io.lsu.load(w).req.bits.meta.tlb_miss), "TLB missed requests should not go to cache")
  }

  // load miss queue
  loadMissQueue.io.lsu <> io.lsu.lsq

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
  XSDebug(store_block, "StorePipe blocked\n")

  //----------------------------------------
  // atomics pipe
  atomics.io.wb_invalidate_lrsc := wb.io.inflight_addr
  atomicsMissQueue.io.lsu <> io.lsu.atomics
  atomicsMissQueue.io.replay <> atomics.io.lsu

  val atomics_block = block_atomics(atomicsMissQueue.io.replay.req.bits.addr)
  block_decoupled(atomicsMissQueue.io.replay.req, atomics.io.lsu.req, atomics_block && !atomicsMissQueue.io.replay.req.bits.meta.replay)
  XSDebug(atomics_block, "AtomicsPipe blocked\n")

  // when atomics are in flight, there should be no load or store in flight
  // so atomics and store should not show up at the same time
  val atomics_inflight = VecInit(atomics.io.inflight_req_block_addrs map (entry => entry.valid)).reduce(_||_)
  val store_inflight = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid)).reduce(_||_)
  assert(!(atomics_inflight && store_inflight))


  // some other stuff
  val atomicsReq = io.lsu.atomics.req
  assert(!(atomicsReq.fire() && atomicsReq.bits.meta.replay),
    "Atomics does not support request replay")
  assert(!(atomicsReq.fire() && atomicsReq.bits.meta.mmio),
    "MMIO requests should not go to cache")
  assert(!(atomicsReq.fire() && atomicsReq.bits.meta.tlb_miss),
    "TLB missed requests should not go to cache")

  //----------------------------------------
  // miss queue
  val loadMissQueueClientId  = 0.U(clientIdWidth.W)
  val storeMissQueueClientId = 1.U(clientIdWidth.W)
  val atomicsMissQueueClientId  = 2.U(clientIdWidth.W)

  // Request
  val missReqArb = Module(new Arbiter(new MissReq, 3))

  val missReq      = missQueue.io.req
  val loadMissReq  = loadMissQueue.io.miss_req
  val storeMissReq = storeMissQueue.io.miss_req
  val atomicsMissReq  = atomicsMissQueue.io.miss_req

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

  missReqArb.io.in(2).valid          := atomicsMissReq.valid
  atomicsMissReq.ready                  := missReqArb.io.in(2).ready
  missReqArb.io.in(2).bits           := atomicsMissReq.bits
  missReqArb.io.in(2).bits.client_id := Cat(atomicsMissQueueClientId,
    atomicsMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  val miss_block = block_miss(missReqArb.io.out.bits.addr)
  block_decoupled(missReqArb.io.out, missReq, miss_block)
  XSDebug(miss_block, "MissQueue blocked\n")

  // Response
  val missResp        = missQueue.io.resp
  val loadMissResp    = loadMissQueue.io.miss_resp
  val storeMissResp   = storeMissQueue.io.miss_resp
  val atomicsMissResp    = atomicsMissQueue.io.miss_resp

  val clientId = missResp.bits.client_id(clientIdMSB, clientIdLSB)

  val isLoadMissResp = clientId === loadMissQueueClientId
  loadMissResp.valid := missResp.valid && isLoadMissResp
  loadMissResp.bits  := missResp.bits
  loadMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  val isStoreMissResp = clientId === storeMissQueueClientId
  storeMissResp.valid := missResp.valid && isStoreMissResp
  storeMissResp.bits  := missResp.bits
  storeMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  val isAtomicsMissResp = clientId === atomicsMissQueueClientId
  atomicsMissResp.valid := missResp.valid && isAtomicsMissResp
  atomicsMissResp.bits  := missResp.bits
  atomicsMissResp.bits.client_id := missResp.bits.client_id(entryIdMSB, entryIdLSB)

  // Finish
  val missFinish        = missQueue.io.finish
  val loadMissFinish    = loadMissQueue.io.miss_finish
  val storeMissFinish   = storeMissQueue.io.miss_finish
  val atomicsMissFinish    = atomicsMissQueue.io.miss_finish

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

  missFinishArb.io.in(2).valid          := atomicsMissFinish.valid
  atomicsMissFinish.ready                  := missFinishArb.io.in(2).ready
  missFinishArb.io.in(2).bits.entry_id  := atomicsMissFinish.bits.entry_id
  missFinishArb.io.in(2).bits.client_id := Cat(atomicsMissQueueClientId,
    atomicsMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

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


  // sync with prober
  missQueue.io.probe_wb_req.valid := prober.io.wb_req.fire()
  missQueue.io.probe_wb_req.bits  := prober.io.wb_req.bits
  missQueue.io.probe_active       := prober.io.inflight_req_block_addr

  //----------------------------------------
  // prober
  prober.io.req.valid := bus.b.valid && !block_probe(get_block_addr(bus.b.bits.address))
  bus.b.ready         := prober.io.req.ready && !block_probe(get_block_addr(bus.b.bits.address))
  prober.io.req.bits  := bus.b.bits

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

  // dcache should only deal with DRAM addresses
  when (bus.a.fire()) {
    assert(bus.a.bits.address >= 0x80000000L.U)
  }
  when (bus.b.fire()) {
    assert(bus.b.bits.address >= 0x80000000L.U)
  }
  when (bus.c.fire()) {
    assert(bus.c.bits.address >= 0x80000000L.U)
  }

  // synchronization stuff
  def nack_load(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val atomics_addr_matches = VecInit(atomics.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val atomics_addr_match = atomics_addr_matches.reduce(_||_)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_addr_match || atomics_addr_match || miss_idx_match
  }

  def block_store(addr: UInt) = {
    val prober_addr_match = prober.io.inflight_req_block_addr.valid && prober.io.inflight_req_block_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    prober_addr_match || miss_idx_match
  }

  def block_atomics(addr: UInt) = {
    val prober_addr_match = prober.io.inflight_req_block_addr.valid && prober.io.inflight_req_block_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    prober_addr_match || miss_idx_match
  }

  def block_miss(addr: UInt) = {
    val store_idx_matches = VecInit(stu.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val store_idx_match = store_idx_matches.reduce(_||_)

    val atomics_idx_matches = VecInit(atomics.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val atomics_idx_match = atomics_idx_matches.reduce(_||_)

    val prober_idx_match = prober.io.inflight_req_idx.valid && prober.io.inflight_req_idx.bits === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_idx_match || atomics_idx_match || prober_idx_match || miss_idx_match
  }

  def block_probe(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val atomics_addr_matches = VecInit(atomics.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val atomics_addr_match = atomics_addr_matches.reduce(_||_)

    val lrsc_addr_match = atomics.io.block_probe_addr.valid && atomics.io.block_probe_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.block_probe_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    // the missed req
    val miss_req_idx_match = missReq.fire() && get_idx(missReq.bits.addr) === get_idx(addr)

    store_addr_match || atomics_addr_match || lrsc_addr_match || miss_idx_match || miss_req_idx_match
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }
}
