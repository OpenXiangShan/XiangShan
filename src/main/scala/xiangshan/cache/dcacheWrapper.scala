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

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val meta   = new DCacheMeta
}

// memory request in word granularity(store)
class DCacheLineReq  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val meta   = new DCacheMeta
}

class DCacheWordResp extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  val meta         = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
}

class DCacheLineResp extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  val meta   = new DCacheMeta
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
}

class Refill extends DCacheBundle
{
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
}

class DCacheWordIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}

// used by load unit
class DCacheLoadIO extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill  = Output(Bool())
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr   = Output(UInt(PAddrBits.W))
}

class DCacheLineIO extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq )
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToLsuIO extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val store = Flipped(new DCacheLineIO) // for sbuffer
  val atomics  = Flipped(new DCacheWordIO)  // atomics reqs
}

class DCacheIO extends DCacheBundle {
  val lsu = new DCacheToLsuIO
  val prefetch = DecoupledIO(new MissReq)
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
  /*
  dataArray.dump()
  metaArray.dump()
  */


  //----------------------------------------
  // core modules
  val ldu = Seq.fill(LoadPipelineWidth) { Module(new LoadPipe) }
  val stu = Module(new StorePipe)
  val atomics = Module(new AtomicsPipe)
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
  val MetaReadPortCount = 4
  val ProberMetaReadPort = 0
  val StorePipeMetaReadPort = 1
  val LoadPipeMetaReadPort = 2
  val AtomicsPipeMetaReadPort = 3

  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, MetaReadPortCount))

  metaReadArb.io.in(ProberMetaReadPort)       <> prober.io.meta_read
  metaReadArb.io.in(StorePipeMetaReadPort)    <> stu.io.meta_read
  metaReadArb.io.in(LoadPipeMetaReadPort)     <> ldu(0).io.meta_read
  metaReadArb.io.in(AtomicsPipeMetaReadPort)  <> atomics.io.meta_read

  metaArray.io.read(0) <> metaReadArb.io.out

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
  dataWriteArb.io.in(MissQueueDataWritePort) <> missQueue.io.data_write
  dataWriteArb.io.in(AtomicsPipeDataWritePort)  <> atomics.io.data_write

  dataArray.io.write <> dataWriteArb.io.out

  // To simplify port arbitration
  // WritebackUnit and StorePipe use port 0
  val DataReadPortCount = 4
  val WritebackDataReadPort = 0
  val StorePipeDataReadPort = 1
  val LoadPipeDataReadPort = 2
  val AtomicsPipeDataReadPort = 3

  val dataReadArb = Module(new Arbiter(new L1DataReadReq, DataReadPortCount))

  dataReadArb.io.in(WritebackDataReadPort) <> wb.io.data_req
  dataReadArb.io.in(StorePipeDataReadPort) <> stu.io.data_read
  dataReadArb.io.in(LoadPipeDataReadPort)  <> ldu(0).io.data_read
  dataReadArb.io.in(AtomicsPipeDataReadPort) <> atomics.io.data_read

  dataArray.io.read(0) <> dataReadArb.io.out
  dataArray.io.resp(0) <> wb.io.data_resp
  dataArray.io.resp(0) <> stu.io.data_resp
  dataArray.io.resp(0) <> atomics.io.data_resp
  dataArray.io.resp(0) <> ldu(0).io.data_resp

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
  require(LoadPipelineWidth == 2, "We hard code the number of load misses")
  val loadMissQueueClientId_0  = 0.U(clientIdWidth.W)
  val loadMissQueueClientId_1  = 1.U(clientIdWidth.W)
  val storeMissQueueClientId   = 2.U(clientIdWidth.W)
  val atomicsMissQueueClientId = 3.U(clientIdWidth.W)

  // Request
  val missReqArb = Module(new Arbiter(new MissReq, nClientMissQueues))

  val missReq      = missQueue.io.req
  val loadMissReq_0  = ldu(0).io.miss_req
  val loadMissReq_1  = ldu(1).io.miss_req
  val storeMissReq  = stu.io.miss_req
  val atomicsMissReq  = atomics.io.miss_req

  missReqArb.io.in(0) <> loadMissReq_0
  missReqArb.io.in(0).bits.client_id := Cat(loadMissQueueClientId_0,
    loadMissReq_0.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(1) <> loadMissReq_1
  missReqArb.io.in(1).bits.client_id := Cat(loadMissQueueClientId_1,
    loadMissReq_0.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(2).valid          := storeMissReq.valid
  storeMissReq.ready                 := missReqArb.io.in(2).ready
  missReqArb.io.in(2).bits           := storeMissReq.bits
  missReqArb.io.in(2).bits.client_id := Cat(storeMissQueueClientId,
    storeMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  missReqArb.io.in(3).valid          := atomicsMissReq.valid
  atomicsMissReq.ready               := missReqArb.io.in(3).ready
  missReqArb.io.in(3).bits           := atomicsMissReq.bits
  missReqArb.io.in(3).bits.client_id := Cat(atomicsMissQueueClientId,
    atomicsMissReq.bits.client_id(entryIdMSB, entryIdLSB))

  val miss_block = block_miss(missReqArb.io.out.bits.addr)
  block_decoupled(missReqArb.io.out, missReq, miss_block)
  XSDebug(miss_block, "MissQueue blocked\n")

  // Response
  // store and atomics wait for miss queue responses
  val missResp        = missQueue.io.resp
  val storeMissResp   = storeMissQueue.io.miss_resp
  val atomicsMissResp = atomicsMissQueue.io.miss_resp

  val clientId = missResp.bits.client_id(clientIdMSB, clientIdLSB)

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
  val storeMissFinish   = storeMissQueue.io.miss_finish
  val atomicsMissFinish = atomicsMissQueue.io.miss_finish

  val missFinishArb = Module(new Arbiter(new MissFinish, 2))
  missFinishArb.io.in(0).valid          := storeMissFinish.valid
  storeMissFinish.ready                 := missFinishArb.io.in(0).ready
  missFinishArb.io.in(0).bits.entry_id  := storeMissFinish.bits.entry_id
  missFinishArb.io.in(0).bits.client_id := Cat(storeMissQueueClientId,
      storeMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

  missFinishArb.io.in(1).valid          := atomicsMissFinish.valid
  atomicsMissFinish.ready                  := missFinishArb.io.in(1).ready
  missFinishArb.io.in(1).bits.entry_id  := atomicsMissFinish.bits.entry_id
  missFinishArb.io.in(1).bits.client_id := Cat(atomicsMissQueueClientId,
    atomicsMissFinish.bits.client_id(entryIdMSB, entryIdLSB))

  missFinish                            <> missFinishArb.io.out

  // refill to load queue
  io.lsu.lsq <> missQueue.io.refill

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

  io.prefetch.valid := missQueue.io.req.fire()
  io.prefetch.bits := missQueue.io.req.bits

  // synchronization stuff
  def nack_load(addr: UInt) = {
    val store_addr_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val store_addr_match = store_addr_matches.reduce(_||_)

    val atomics_addr_matches = VecInit(atomics.io.inflight_req_block_addrs map (entry => entry.valid && entry.bits === get_block_addr(addr)))
    val atomics_addr_match = atomics_addr_matches.reduce(_||_)

    val prober_idx_match = prober.io.inflight_req_block_addr.valid && get_idx(prober.io.inflight_req_block_addr.bits) === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    store_addr_match || atomics_addr_match || prober_idx_match || miss_idx_match
  }

  def block_store(addr: UInt) = {
    val prober_idx_match = prober.io.inflight_req_block_addr.valid && get_idx(prober.io.inflight_req_block_addr.bits) === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    prober_idx_match || miss_idx_match
  }

  def block_atomics(addr: UInt) = {
    val prober_idx_match = prober.io.inflight_req_block_addr.valid && get_idx(prober.io.inflight_req_block_addr.bits) === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)
    prober_idx_match || miss_idx_match
  }

  def block_miss(addr: UInt) = {
    val prober_idx_match = prober.io.inflight_req_idx.valid && prober.io.inflight_req_idx.bits === get_idx(addr)

    val miss_idx_matches = VecInit(missQueue.io.inflight_req_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    prober_idx_match || miss_idx_match
  }

  def block_probe(addr: UInt) = {
    val store_idx_matches = VecInit(stu.io.inflight_req_block_addrs map (entry => entry.valid && get_idx(entry.bits) === get_idx(addr)))
    val store_idx_match = store_idx_matches.reduce(_||_)

    val atomics_idx_matches = VecInit(atomics.io.inflight_req_block_addrs map (entry => entry.valid && get_idx(entry.bits) === get_idx(addr)))
    val atomics_idx_match = atomics_idx_matches.reduce(_||_)

    val lrsc_addr_match = atomics.io.block_probe_addr.valid && atomics.io.block_probe_addr.bits === get_block_addr(addr)

    val miss_idx_matches = VecInit(missQueue.io.block_probe_idxes map (entry => entry.valid && entry.bits === get_idx(addr)))
    val miss_idx_match = miss_idx_matches.reduce(_||_)

    // the missed req
    val miss_req_idx_match = missReq.fire() && get_idx(missReq.bits.addr) === get_idx(addr)

    store_idx_match || atomics_idx_match || lrsc_addr_match || miss_idx_match || miss_req_idx_match
  }

  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }
}
