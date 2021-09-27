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
import chisel3.experimental.ExtModule
import chisel3.util._
import xiangshan._
import utils._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import system.L1CacheErrorInfo
import device.RAMHelper
import huancun.{AliasField, AliasKey, PreferCacheField, PrefetchField, DirtyField}

// DCache specific parameters
case class DCacheParameters
(
  nSets: Int = 256,
  nWays: Int = 8,
  rowBits: Int = 128,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("random"),
  nMissEntries: Int = 1,
  nProbeEntries: Int = 1,
  nReleaseEntries: Int = 1,
  nStoreReplayEntries: Int = 1,
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64
) extends L1CacheParameters {
  // if sets * blockBytes > 4KB(page size),
  // cache alias will happen,
  // we need to avoid this by recoding additional bits in L2 cache
  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    PreferCacheField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(DirtyField())

  def tagCode: Code = Code.fromString(tagECC)

  def dataCode: Code = Code.fromString(dataECC)
}

//           Physical Address
// --------------------------------------
// |   Physical Tag |  PIndex  | Offset |
// --------------------------------------
//                  |
//                  DCacheTagOffset
// 
//           Virtual Address
// --------------------------------------
// | Above index  | Set | Bank | Offset |
// --------------------------------------
//                |     |      |        |
//                |     |      |        DCacheWordOffset
//                |     |      DCacheBankOffset
//                |     DCacheSetOffset
//                DCacheAboveIndexOffset

// Default DCache size = 64 sets * 8 ways * 8 banks * 8 Byte = 32K Byte

trait HasDCacheParameters extends HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def encWordBits = cacheParams.dataCode.width(wordBits)

  def encRowBits = encWordBits * rowWords // for DuplicatedDataArray only
  def eccBits = encWordBits - wordBits

  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant

  def nSourceType = 3
  def sourceTypeWidth = log2Up(nSourceType)
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = 64

  // banked dcache support
  val DCacheSets = cacheParams.nSets
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 8
  val DCacheSRAMRowBits = 64 // hardcoded

  val DCacheLineBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheLineBytes = DCacheLineBits / 8
  val DCacheLineWords = DCacheLineBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordOffset = 0
  val DCacheBankOffset = DCacheWordOffset + log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheIndexOffset = DCacheBankOffset

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset-1, DCacheBankOffset)
  }

  def addr_to_dcache_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset-1, DCacheSetOffset)
  }

  def get_data_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def get_mask_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(full_divide(rowBits, wordBits), s"rowBits($rowBits) must be multiple of wordBits($wordBits)")
  require(full_divide(beatBits, rowBits), s"beatBits($beatBits) must be multiple of rowBits($rowBits)")
}

abstract class DCacheModule(implicit p: Parameters) extends L1CacheModule
  with HasDCacheParameters

abstract class DCacheBundle(implicit p: Parameters) extends L1CacheBundle
  with HasDCacheParameters

class ReplacementAccessBundle(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(log2Up(nSets).W)
  val way = UInt(log2Up(nWays).W)
}

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
  val vaddr  = UInt(VAddrBits.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt((cfg.blockBytes * 8).W)
  val mask   = UInt(cfg.blockBytes.W)
  val id     = UInt(reqIdWidth.W)
  def dump() = {
    XSDebug("DCacheLineReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

class DCacheWordReqWithVaddr(implicit p: Parameters) extends DCacheWordReq {
  val vaddr = UInt(VAddrBits.W)
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
  val data   = UInt(l1BusDataWidth.W)
  // for debug usage
  val data_raw = UInt((cfg.blockBytes * 8).W)
  val hasdata = Bool()
  val refill_done = Bool()
  def dump() = {
    XSDebug("Refill: addr: %x data: %x\n", addr, data)
  }
}

class DCacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}

class DCacheWordIOWithVaddr(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReqWithVaddr)
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
  val req  = DecoupledIO(new DCacheLineReq)
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val store = Flipped(new DCacheLineIO) // for sbuffer
  val atomics  = Flipped(new DCacheWordIOWithVaddr)  // atomics reqs
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
    )),
    requestFields = cacheParams.reqFields,
    echoFields = cacheParams.echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCache: tilelink width does not match")

  println("DCache:") 
  println("  DCacheSets: " + DCacheSets) 
  println("  DCacheWays: " + DCacheWays) 
  println("  DCacheBanks: " + DCacheBanks) 
  println("  DCacheSRAMRowBits: " + DCacheSRAMRowBits) 
  println("  DCacheWordOffset: " + DCacheWordOffset) 
  println("  DCacheBankOffset: " + DCacheBankOffset) 
  println("  DCacheSetOffset: " + DCacheSetOffset) 
  println("  DCacheTagOffset: " + DCacheTagOffset) 
  println("  DCacheAboveIndexOffset: " + DCacheAboveIndexOffset) 

  //----------------------------------------
  // core data structures
  val bankedDataArray = Module(new BankedDataArray)
  val metaArray = Module(new DuplicatedMetaArray(numReadPorts = 3))
  bankedDataArray.dump()

  val errors = bankedDataArray.io.errors ++ metaArray.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.ecc_error.valid -> e)))
  // assert(!io.error.ecc_error.valid)

  //----------------------------------------
  // core modules
  val ldu = Seq.tabulate(LoadPipelineWidth)({ i => Module(new LoadPipe(i))})
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

  metaArray.io.read(LoadPipelineWidth) <> mainPipe.io.meta_read
  mainPipe.io.meta_resp <> metaArray.io.resp(LoadPipelineWidth)

  for (w <- 0 until LoadPipelineWidth) {
    metaArray.io.read(w) <> ldu(w).io.meta_read
    ldu(w).io.meta_resp <> metaArray.io.resp(w)
  }

  //----------------------------------------
  // data array

  bankedDataArray.io.write <> mainPipe.io.banked_data_write
  bankedDataArray.io.read(0) <> ldu(0).io.banked_data_read
  bankedDataArray.io.read(1) <> ldu(1).io.banked_data_read
  bankedDataArray.io.readline <> mainPipe.io.banked_data_read

  ldu(0).io.banked_data_resp := bankedDataArray.io.resp
  ldu(1).io.banked_data_resp := bankedDataArray.io.resp
  mainPipe.io.banked_data_resp := bankedDataArray.io.resp

  ldu(0).io.bank_conflict_fast := bankedDataArray.io.bank_conflict_fast(0)
  ldu(1).io.bank_conflict_fast := bankedDataArray.io.bank_conflict_fast(1)
  ldu(0).io.bank_conflict_slow := bankedDataArray.io.bank_conflict_slow(0)
  ldu(1).io.bank_conflict_slow := bankedDataArray.io.bank_conflict_slow(1)

  //----------------------------------------
  // load pipe
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    ldu(w).io.lsu <> io.lsu.load(w)

    // replay and nack not needed anymore
    // TODO: remove replay and nack
    ldu(w).io.nack := false.B

    ldu(w).io.disable_ld_fast_wakeup := 
      mainPipe.io.disable_ld_fast_wakeup(w) || 
      bankedDataArray.io.bank_conflict_fast(w) // load pipe fast wake up should be disabled when bank conflict
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

class AMOHelper() extends ExtModule {
//  val io = IO(new Bundle {
    val clock  = IO(Input(Clock()))
    val enable = IO(Input(Bool()))
    val cmd    = IO(Input(UInt(5.W)))
    val addr   = IO(Input(UInt(64.W)))
    val wdata  = IO(Input(UInt(64.W)))
    val mask   = IO(Input(UInt(8.W)))
    val rdata  = IO(Output(UInt(64.W)))
//  })
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
