/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import system.L1CacheErrorInfo
import device.RAMHelper

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters)  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheWordReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

// memory request in word granularity(store)
class DCacheLineReq(implicit p: Parameters)  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

class DCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheWordResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class DCacheLineResp(implicit p: Parameters) extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class Refill(implicit p: Parameters) extends DCacheBundle
{
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  def dump() = {
    XSDebug("Refill: addr: %x data: %x\n", addr, data)
  }
}

class DCacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}

// used by load unit
class DCacheLoadIO(implicit p: Parameters) extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill  = Output(Bool())
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr = Output(UInt(PAddrBits.W))
  val s1_hit_way = Input(UInt(nWays.W))
  val s1_disable_fast_wakeup = Input(Bool())
}

class DCacheLineIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq )
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val store = Flipped(new DCacheLineIO) // for sbuffer
  val atomics  = Flipped(new DCacheWordIO)  // atomics reqs
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val lsu = new DCacheToLsuIO
  val error = new L1CacheErrorInfo
  val mshrFull = Output(Bool())
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
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCache: tilelink width does not match")

  //----------------------------------------
  // core data structures
  val dataArray = Module(new DuplicatedDataArray)
  val metaArray = Module(new DuplicatedMetaArray)
  /*
  dataArray.dump()
  metaArray.dump()
  */
  val errors = dataArray.io.errors ++ metaArray.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.ecc_error.valid -> e)))

  //----------------------------------------
  // core modules
  val ldu = Seq.fill(LoadPipelineWidth) { Module(new LoadPipe) }
  val storeReplayUnit = Module(new StoreReplayQueue)
  val atomicsReplayUnit = Module(new AtomicsReplayEntry)

  val mainPipe   = Module(new MainPipe)
  val missQueue  = Module(new MissQueue(edge))
  val probeQueue = Module(new ProbeQueue(edge))
  val wb         = Module(new WritebackQueue(edge))


  //----------------------------------------
  // meta array
  val MetaWritePortCount = 1
  val MainPipeMetaWritePort = 0
  metaArray.io.write <> mainPipe.io.meta_write

  // MainPipe contend MetaRead with Load 0
  // give priority to MainPipe
  val MetaReadPortCount = 2
  val MainPipeMetaReadPort = 0
  val LoadPipeMetaReadPort = 1

  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, MetaReadPortCount))

  metaReadArb.io.in(LoadPipeMetaReadPort) <> ldu(LoadPipelineWidth - 1).io.meta_read
  metaReadArb.io.in(MainPipeMetaReadPort) <> mainPipe.io.meta_read

  metaArray.io.read(LoadPipelineWidth - 1) <> metaReadArb.io.out

  ldu(LoadPipelineWidth - 1).io.meta_resp <> metaArray.io.resp(LoadPipelineWidth - 1)
  mainPipe.io.meta_resp <> metaArray.io.resp(LoadPipelineWidth - 1)

  for (w <- 0 until (LoadPipelineWidth - 1)) {
    metaArray.io.read(w) <> ldu(w).io.meta_read
    ldu(w).io.meta_resp <> metaArray.io.resp(w)
  }

  //----------------------------------------
  // data array
  val DataWritePortCount = 1
  val MainPipeDataWritePort = 0

  dataArray.io.write <> mainPipe.io.data_write

  // give priority to MainPipe
  val DataReadPortCount = 2
  val MainPipeDataReadPort = 0
  val LoadPipeDataReadPort = 1

  val dataReadArb = Module(new Arbiter(new L1DataReadReq, DataReadPortCount))

  dataReadArb.io.in(LoadPipeDataReadPort)  <> ldu(LoadPipelineWidth - 1).io.data_read
  dataReadArb.io.in(MainPipeDataReadPort)  <> mainPipe.io.data_read

  dataArray.io.read(LoadPipelineWidth - 1) <> dataReadArb.io.out

  dataArray.io.resp(LoadPipelineWidth - 1) <> ldu(LoadPipelineWidth - 1).io.data_resp
  dataArray.io.resp(LoadPipelineWidth - 1) <> mainPipe.io.data_resp

  for (w <- 0 until (LoadPipelineWidth - 1)) {
    dataArray.io.read(w) <> ldu(w).io.data_read
    dataArray.io.resp(w) <> ldu(w).io.data_resp
  }

  //----------------------------------------
  // load pipe
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    ldu(w).io.lsu <> io.lsu.load(w)

    // replay and nack not needed anymore
    // TODO: remove replay and nack
    ldu(w).io.nack := false.B

    ldu(w).io.disable_ld_fast_wakeup := mainPipe.io.disable_ld_fast_wakeup(w)
  }

  //----------------------------------------
  // store pipe and store miss queue
  storeReplayUnit.io.lsu    <> io.lsu.store

  //----------------------------------------
  // atomics
  // atomics not finished yet
  io.lsu.atomics <> atomicsReplayUnit.io.lsu

  //----------------------------------------
  // miss queue
  val MissReqPortCount = LoadPipelineWidth + 1
  val MainPipeMissReqPort = 0

  // Request
  val missReqArb = Module(new RRArbiter(new MissReq, MissReqPortCount))

  missReqArb.io.in(MainPipeMissReqPort) <> mainPipe.io.miss_req
  for (w <- 0 until LoadPipelineWidth) { missReqArb.io.in(w + 1) <> ldu(w).io.miss_req }

  wb.io.miss_req.valid := missReqArb.io.out.valid
  wb.io.miss_req.bits  := missReqArb.io.out.bits.addr

  block_decoupled(missReqArb.io.out, missQueue.io.req, wb.io.block_miss_req)

  // refill to load queue
  io.lsu.lsq <> missQueue.io.refill

  // tilelink stuff
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish
  missQueue.io.probe_req := bus.b.bits.address

  //----------------------------------------
  // probe
  // probeQueue.io.mem_probe <> bus.b
  block_decoupled(bus.b, probeQueue.io.mem_probe, missQueue.io.probe_block)

  //----------------------------------------
  // mainPipe
  val MainPipeReqPortCount = 4
  val MissMainPipeReqPort = 0
  val StoreMainPipeReqPort = 1
  val AtomicsMainPipeReqPort = 2
  val ProbeMainPipeReqPort = 3

  val mainPipeReqArb = Module(new RRArbiter(new MainPipeReq, MainPipeReqPortCount))
  mainPipeReqArb.io.in(MissMainPipeReqPort)    <> missQueue.io.pipe_req
  mainPipeReqArb.io.in(StoreMainPipeReqPort)   <> storeReplayUnit.io.pipe_req
  mainPipeReqArb.io.in(AtomicsMainPipeReqPort) <> atomicsReplayUnit.io.pipe_req
  mainPipeReqArb.io.in(ProbeMainPipeReqPort)   <> probeQueue.io.pipe_req

  // add a stage to break the Arbiter bits.addr to ready path
  val mainPipeReq_valid = RegInit(false.B)
  val mainPipeReq_fire  = mainPipeReq_valid && mainPipe.io.req.ready
  val mainPipeReq_req   = RegEnable(mainPipeReqArb.io.out.bits, mainPipeReqArb.io.out.fire())

  mainPipeReqArb.io.out.ready := mainPipeReq_fire || !mainPipeReq_valid
  mainPipe.io.req.valid := mainPipeReq_valid
  mainPipe.io.req.bits  := mainPipeReq_req

  when (mainPipeReqArb.io.out.fire()) { mainPipeReq_valid := true.B }
  when (!mainPipeReqArb.io.out.fire() && mainPipeReq_fire) { mainPipeReq_valid := false.B }

  missQueue.io.pipe_resp         <> mainPipe.io.miss_resp
  storeReplayUnit.io.pipe_resp   <> mainPipe.io.store_resp
  atomicsReplayUnit.io.pipe_resp <> mainPipe.io.amo_resp

  probeQueue.io.lrsc_locked_block <> mainPipe.io.lrsc_locked_block

  for(i <- 0 until LoadPipelineWidth) {
    mainPipe.io.replace_access(i) <> ldu(i).io.replace_access
  }

  //----------------------------------------
  // wb
  // add a queue between MainPipe and WritebackUnit to reduce MainPipe stalls due to WritebackUnit busy
  wb.io.req <> mainPipe.io.wb_req
  bus.c     <> wb.io.mem_release

  // connect bus d
  missQueue.io.mem_grant.valid := false.B
  missQueue.io.mem_grant.bits  := DontCare

  wb.io.mem_grant.valid := false.B
  wb.io.mem_grant.bits  := DontCare

  // in L1DCache, we ony expect Grant[Data] and ReleaseAck
  bus.d.ready := false.B
  when (bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData) {
    missQueue.io.mem_grant <> bus.d
  } .elsewhen (bus.d.bits.opcode === TLMessages.ReleaseAck) {
    wb.io.mem_grant <> bus.d
  } .otherwise {
    assert (!bus.d.fire())
  }

  //----------------------------------------
  // assertions
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

  //----------------------------------------
  // utility functions
  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  //----------------------------------------
  // performance counters
  val num_loads = PopCount(ldu.map(e => e.io.lsu.req.fire()))
  XSPerfAccumulate("num_loads", num_loads)

  io.mshrFull := missQueue.io.full
}

class AMOHelper() extends BlackBox {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val enable = Input(Bool())
    val cmd    = Input(UInt(5.W))
    val addr   = Input(UInt(64.W))
    val wdata  = Input(UInt(64.W))
    val mask   = Input(UInt(8.W))
    val rdata  = Output(UInt(64.W))
  })
}

class FakeDCache()(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new DCacheIO)

  io := DontCare
  // to LoadUnit
  for (i <- 0 until LoadPipelineWidth) {
    val fakeRAM = Module(new RAMHelper(64L * 1024 * 1024 * 1024))
    fakeRAM.io.clk   := clock
    fakeRAM.io.en    := io.lsu.load(i).resp.valid && !reset.asBool
    fakeRAM.io.rIdx  := RegNext((io.lsu.load(i).s1_paddr - "h80000000".U) >> 3)
    fakeRAM.io.wIdx  := 0.U
    fakeRAM.io.wdata := 0.U
    fakeRAM.io.wmask := 0.U
    fakeRAM.io.wen   := false.B

    io.lsu.load(i).req.ready := true.B
    io.lsu.load(i).resp.valid := RegNext(RegNext(io.lsu.load(i).req.valid) && !io.lsu.load(i).s1_kill)
    io.lsu.load(i).resp.bits.data := fakeRAM.io.rdata
    io.lsu.load(i).resp.bits.miss := false.B
    io.lsu.load(i).resp.bits.replay := false.B
    io.lsu.load(i).resp.bits.id := DontCare
    io.lsu.load(i).s1_hit_way := 1.U
    io.lsu.load(i).s1_disable_fast_wakeup := false.B
  }
  // to LSQ
  io.lsu.lsq.valid := false.B
  io.lsu.lsq.bits := DontCare
  // to Store Buffer
  io.lsu.store.req.ready := true.B
  io.lsu.store.resp := DontCare
  io.lsu.store.resp.valid := RegNext(io.lsu.store.req.valid)
  io.lsu.store.resp.bits.id := RegNext(io.lsu.store.req.bits.id)
  // to atomics
  val amoHelper = Module(new AMOHelper)
  amoHelper.io.clock := clock
  amoHelper.io.enable := io.lsu.atomics.req.valid && !reset.asBool
  amoHelper.io.cmd := io.lsu.atomics.req.bits.cmd
  amoHelper.io.addr := io.lsu.atomics.req.bits.addr
  amoHelper.io.wdata := io.lsu.atomics.req.bits.data
  amoHelper.io.mask := io.lsu.atomics.req.bits.mask
  io.lsu.atomics.req.ready := true.B
  io.lsu.atomics.resp.valid := RegNext(io.lsu.atomics.req.valid)
  assert(!io.lsu.atomics.resp.valid || io.lsu.atomics.resp.ready)
  io.lsu.atomics.resp.bits.data := amoHelper.io.rdata
  io.lsu.atomics.resp.bits.replay := false.B
  io.lsu.atomics.resp.bits.id := 1.U
}

class DCacheWrapper()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {

  val clientNode = if (!useFakeDCache) TLIdentityNode() else null
  val dcache = if (!useFakeDCache) LazyModule(new DCache()) else null
  if (!useFakeDCache) {
    clientNode := dcache.clientNode
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new DCacheIO)
    if (useFakeDCache) {
      val fake_dcache = Module(new FakeDCache())
      io <> fake_dcache.io
    }
    else {
      io <> dcache.module.io
    }
  }
}
