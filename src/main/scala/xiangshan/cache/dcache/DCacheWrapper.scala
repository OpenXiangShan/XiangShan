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
import device.RAMHelper
import huancun.{AliasField, AliasKey, PreferCacheField, PrefetchField, DirtyField}
import scala.math.max

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
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64,
  alwaysReleaseData: Boolean = true
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
//                |     |      |        0
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

  def encTagBits = cacheParams.tagCode.width(tagBits)
  def eccTagBits = encTagBits - tagBits

  def lrscCycles = LRSCCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant

  def nSourceType = 3
  def sourceTypeWidth = log2Up(nSourceType)
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2
  def SOFT_PREFETCH = 3

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = 64

  require(isPow2(cfg.nMissEntries))
  require(isPow2(cfg.nReleaseEntries))
  val nEntries = max(cfg.nMissEntries, cfg.nReleaseEntries) << 1
  val releaseIdBase = max(cfg.nMissEntries, cfg.nReleaseEntries)

  // banked dcache support
  val DCacheSets = cacheParams.nSets
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 8
  val DCacheSRAMRowBits = 64 // hardcoded
  val DCacheWordBits = 64 // hardcoded
  val DCacheWordBytes = DCacheWordBits / 8

  val DCacheSizeBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheSizeBytes = DCacheSizeBits / 8
  val DCacheSizeWords = DCacheSizeBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordOffset = log2Up(DCacheWordBytes)

  val DCacheBankOffset = log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheLineOffset = DCacheSetOffset
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

  val numReplaceRespPorts = 2

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

class ReplacementWayReqIO(implicit p: Parameters) extends DCacheBundle {
  val set = ValidIO(UInt(log2Up(nSets).W))
  val way = Input(UInt(log2Up(nWays).W))
}

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters)  extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(DataBits.W)
  val mask   = UInt((DataBits/8).W)
  val id     = UInt(reqIdWidth.W)
  val instrtype   = UInt(sourceTypeWidth.W)
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
  def idx: UInt = get_idx(vaddr)
}

class DCacheWordReqWithVaddr(implicit p: Parameters) extends DCacheWordReq {
  val vaddr = UInt(VAddrBits.W)
  val wline = Bool()
}

class DCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data         = UInt(DataBits.W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val miss_enter = Bool()
  // cache miss, and enter the missqueue successfully. just for softprefetch
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

class Release(implicit p: Parameters) extends DCacheBundle
{
  val paddr  = UInt(PAddrBits.W)
  def dump() = {
    XSDebug("Release: paddr: %x\n", paddr(PAddrBits-1, DCacheTagOffset))
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
  val s2_kill  = Output(Bool())
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr = Output(UInt(PAddrBits.W))
  val s1_hit_way = Input(UInt(nWays.W))
  val s1_disable_fast_wakeup = Input(Bool())
  val s1_bank_conflict = Input(Bool())
}

class DCacheLineIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq)
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToSbufferIO(implicit p: Parameters) extends DCacheBundle { 
  // sbuffer will directly send request to dcache main pipe
  val req = Flipped(Decoupled(new DCacheLineReq))

  val main_pipe_hit_resp = ValidIO(new DCacheLineResp)
  val refill_hit_resp = ValidIO(new DCacheLineResp)

  val replay_resp = ValidIO(new DCacheLineResp)

  def hit_resps: Seq[ValidIO[DCacheLineResp]] = Seq(main_pipe_hit_resp, refill_hit_resp)
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val store = new DCacheToSbufferIO // for sbuffer
  val atomics  = Flipped(new DCacheWordIOWithVaddr)  // atomics reqs
  val release = ValidIO(new Release) // cacheline release hint for ld-ld violation check 
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val lsu = new DCacheToLsuIO
  val csr = new L1CacheToCsrIO
  val error = new L1CacheErrorInfo
  val mshrFull = Output(Bool())
}


class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, nEntries + 1),
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
  val metaArray = Module(new AsynchronousMetaArray(readPorts = 4, writePorts = 3))
  val tagArray = Module(new DuplicatedTagArray(readPorts = LoadPipelineWidth + 1))
  bankedDataArray.dump()

  val errors = bankedDataArray.io.errors ++ metaArray.io.errors
  io.error <> RegNext(Mux1H(errors.map(e => e.ecc_error.valid -> e)))
  // assert(!io.error.ecc_error.valid)

  //----------------------------------------
  // core modules
  val ldu = Seq.tabulate(LoadPipelineWidth)({ i => Module(new LoadPipe(i))})
  val atomicsReplayUnit = Module(new AtomicsReplayEntry)
  val mainPipe   = Module(new MainPipe)
  val refillPipe = Module(new RefillPipe)
  val replacePipe = Module(new ReplacePipe)
  val missQueue  = Module(new MissQueue(edge))
  val probeQueue = Module(new ProbeQueue(edge))
  val wb         = Module(new WritebackQueue(edge))

  //----------------------------------------
  // meta array
  val meta_read_ports = ldu.map(_.io.meta_read) ++
    Seq(mainPipe.io.meta_read,
      replacePipe.io.meta_read)
  val meta_resp_ports = ldu.map(_.io.meta_resp) ++
    Seq(mainPipe.io.meta_resp,
      replacePipe.io.meta_resp)
  val meta_write_ports = Seq(
    mainPipe.io.meta_write,
    refillPipe.io.meta_write,
    replacePipe.io.meta_write
  )
  meta_read_ports.zip(metaArray.io.read).foreach { case (p, r) => r <> p }
  meta_resp_ports.zip(metaArray.io.resp).foreach { case (p, r) => p := r }
  meta_write_ports.zip(metaArray.io.write).foreach { case (p, w) => w <> p }

  //----------------------------------------
  // tag array
  require(tagArray.io.read.size == (ldu.size + 1))
  ldu.zipWithIndex.foreach {
    case (ld, i) =>
      tagArray.io.read(i) <> ld.io.tag_read
      ld.io.tag_resp := tagArray.io.resp(i)
  }
  tagArray.io.read.last <> mainPipe.io.tag_read
  mainPipe.io.tag_resp := tagArray.io.resp.last

  val tag_write_arb = Module(new Arbiter(new TagWriteReq, 2))
  tag_write_arb.io.in(0) <> refillPipe.io.tag_write
  tag_write_arb.io.in(1) <> mainPipe.io.tag_write
  tagArray.io.write <> tag_write_arb.io.out

  //----------------------------------------
  // data array

  val dataReadLineArb = Module(new Arbiter(new L1BankedDataReadLineReq, 2))
  dataReadLineArb.io.in(0) <> replacePipe.io.data_read
  dataReadLineArb.io.in(1) <> mainPipe.io.data_read

  val dataWriteArb = Module(new Arbiter(new L1BankedDataWriteReq, 2))
  dataWriteArb.io.in(0) <> refillPipe.io.data_write
  dataWriteArb.io.in(1) <> mainPipe.io.data_write

  bankedDataArray.io.write <> dataWriteArb.io.out
  bankedDataArray.io.read(0) <> ldu(0).io.banked_data_read
  bankedDataArray.io.read(1) <> ldu(1).io.banked_data_read
  bankedDataArray.io.readline <> dataReadLineArb.io.out

  ldu(0).io.banked_data_resp := bankedDataArray.io.resp
  ldu(1).io.banked_data_resp := bankedDataArray.io.resp
  mainPipe.io.data_resp := bankedDataArray.io.resp
  replacePipe.io.data_resp := bankedDataArray.io.resp

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
      bankedDataArray.io.bank_conflict_fast(w) // load pipe fast wake up should be disabled when bank conflict
  }

  //----------------------------------------
  // atomics
  // atomics not finished yet
  io.lsu.atomics <> atomicsReplayUnit.io.lsu
  atomicsReplayUnit.io.pipe_resp := mainPipe.io.atomic_resp

  //----------------------------------------
  // miss queue
  val MissReqPortCount = LoadPipelineWidth + 1
  val MainPipeMissReqPort = 0

  // Request
  val missReqArb = Module(new RRArbiter(new MissReq, MissReqPortCount))

  missReqArb.io.in(MainPipeMissReqPort) <> mainPipe.io.miss
  for (w <- 0 until LoadPipelineWidth) { missReqArb.io.in(w + 1) <> ldu(w).io.miss_req }

  wb.io.miss_req.valid := missReqArb.io.out.valid
  wb.io.miss_req.bits  := missReqArb.io.out.bits.addr

  block_decoupled(missReqArb.io.out, missQueue.io.req, wb.io.block_miss_req)

  // refill to load queue
  io.lsu.lsq <> missQueue.io.refill_to_ldq

  // tilelink stuff
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish
  missQueue.io.probe_addr := bus.b.bits.address

  missQueue.io.main_pipe_resp := mainPipe.io.atomic_resp

  //----------------------------------------
  // probe
  // probeQueue.io.mem_probe <> bus.b
  block_decoupled(bus.b, probeQueue.io.mem_probe, missQueue.io.probe_block)
  probeQueue.io.lrsc_locked_block <> mainPipe.io.lrsc_locked_block

  //----------------------------------------
  // mainPipe
  // when a req enters main pipe, if it is set-conflict with replace pipe or refill pipe,
  // block the req in main pipe
  val refillPipeStatus, replacePipeStatusS0 = Wire(Valid(UInt(idxBits.W)))
  refillPipeStatus.valid := refillPipe.io.req.valid
  refillPipeStatus.bits := get_idx(refillPipe.io.req.bits.paddrWithVirtualAlias)
  replacePipeStatusS0.valid := replacePipe.io.req.valid
  replacePipeStatusS0.bits := get_idx(replacePipe.io.req.bits.vaddr)
  val blockMainPipeReqs = Seq(
    refillPipeStatus,
	replacePipeStatusS0,
    replacePipe.io.status.s1_set,
    replacePipe.io.status.s2_set
  )
  val storeShouldBeBlocked = Cat(blockMainPipeReqs.map(r => r.valid && r.bits === io.lsu.store.req.bits.idx)).orR
  val probeShouldBeBlocked = Cat(blockMainPipeReqs.map(r => r.valid && r.bits === get_idx(probeQueue.io.pipe_req.bits.vaddr))).orR

  block_decoupled(probeQueue.io.pipe_req, mainPipe.io.probe_req, probeShouldBeBlocked)
  block_decoupled(io.lsu.store.req, mainPipe.io.store_req, storeShouldBeBlocked)

  io.lsu.store.replay_resp := mainPipe.io.store_replay_resp
  io.lsu.store.main_pipe_hit_resp := mainPipe.io.store_hit_resp

  val mainPipeAtomicReqArb = Module(new Arbiter(new MainPipeReq, 2))
  mainPipeAtomicReqArb.io.in(0) <> missQueue.io.main_pipe_req
  mainPipeAtomicReqArb.io.in(1) <> atomicsReplayUnit.io.pipe_req
  mainPipe.io.atomic_req <> mainPipeAtomicReqArb.io.out

  mainPipe.io.invalid_resv_set := wb.io.req.fire && wb.io.req.bits.addr === mainPipe.io.lrsc_locked_block.bits

  //----------------------------------------
  // replace pipe
  val mpStatus = mainPipe.io.status
  val replaceSet = addr_to_dcache_set(missQueue.io.replace_pipe_req.bits.vaddr)
  val replaceWayEn = missQueue.io.replace_pipe_req.bits.way_en
  val replaceShouldBeBlocked = // mpStatus.s0_set.valid && replaceSet === mpStatus.s0_set.bits ||
    Cat(Seq(mpStatus.s1, mpStatus.s2, mpStatus.s3).map(s =>
      s.valid && s.bits.set === replaceSet && s.bits.way_en === replaceWayEn
    )).orR()
  block_decoupled(missQueue.io.replace_pipe_req, replacePipe.io.req, replaceShouldBeBlocked)
  missQueue.io.replace_pipe_resp := replacePipe.io.resp

  //----------------------------------------
  // refill pipe
  val refillShouldBeBlocked = (mpStatus.s1.valid && mpStatus.s1.bits.set === missQueue.io.refill_pipe_req.bits.idx) ||
    Cat(Seq(mpStatus.s2, mpStatus.s3).map(s =>
      s.valid &&
        s.bits.set === missQueue.io.refill_pipe_req.bits.idx &&
        s.bits.way_en === missQueue.io.refill_pipe_req.bits.way_en
    )).orR
  block_decoupled(missQueue.io.refill_pipe_req, refillPipe.io.req, refillShouldBeBlocked)
  io.lsu.store.refill_hit_resp := refillPipe.io.store_resp

  //----------------------------------------
  // wb
  // add a queue between MainPipe and WritebackUnit to reduce MainPipe stalls due to WritebackUnit busy
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  wbArb.io.in.zip(Seq(mainPipe.io.wb, replacePipe.io.wb)).foreach { case (arb, pipe) => arb <> pipe }
  wb.io.req <> wbArb.io.out
  bus.c     <> wb.io.mem_release
  wb.io.release_wakeup := refillPipe.io.release_wakeup
  wb.io.release_update := mainPipe.io.release_update
  io.lsu.release.valid := bus.c.fire()
  io.lsu.release.bits.paddr := bus.c.bits.address

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
  // replacement algorithm
  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)

  val replWayReqs = ldu.map(_.io.replace_way) ++ Seq(mainPipe.io.replace_way)
  replWayReqs.foreach{
    case req =>
      req.way := DontCare
      when (req.set.valid) { req.way := replacer.way(req.set.bits) }
  }

  val replAccessReqs = ldu.map(_.io.replace_access) ++ Seq(
    mainPipe.io.replace_access,
    refillPipe.io.replace_access
  )
  val touchWays = Seq.fill(replAccessReqs.size)(Wire(ValidIO(UInt(log2Up(nWays).W))))
  touchWays.zip(replAccessReqs).foreach {
    case (w, req) =>
      w.valid := req.valid
      w.bits := req.bits.way
  }
  val touchSets = replAccessReqs.map(_.bits.set)
  replacer.access(touchSets, touchWays)

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
  // Customized csr cache op support
  val cacheOpDecoder = Module(new CSRCacheOpDecoder("dcache", CacheInstrucion.COP_ID_DCACHE))
  cacheOpDecoder.io.csr <> io.csr
  bankedDataArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  metaArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  tagArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  cacheOpDecoder.io.cache.resp.valid := bankedDataArray.io.cacheOp.resp.valid ||
    metaArray.io.cacheOp.resp.valid ||
    tagArray.io.cacheOp.resp.valid
  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
    bankedDataArray.io.cacheOp.resp.valid -> bankedDataArray.io.cacheOp.resp.bits,
    metaArray.io.cacheOp.resp.valid -> metaArray.io.cacheOp.resp.bits,
    tagArray.io.cacheOp.resp.valid -> tagArray.io.cacheOp.resp.bits,
  ))
  assert(!((bankedDataArray.io.cacheOp.resp.valid +& metaArray.io.cacheOp.resp.valid +& tagArray.io.cacheOp.resp.valid) > 1.U))

  //----------------------------------------
  // performance counters
  val num_loads = PopCount(ldu.map(e => e.io.lsu.req.fire()))
  XSPerfAccumulate("num_loads", num_loads)

  io.mshrFull := missQueue.io.full

  // performance counter
  val ld_access = Wire(Vec(LoadPipelineWidth, missQueue.io.debug_early_replace.last.cloneType))
  val st_access = Wire(ld_access.last.cloneType)
  ld_access.zip(ldu).foreach {
    case (a, u) =>
      a.valid := RegNext(u.io.lsu.req.fire()) && !u.io.lsu.s1_kill
      a.bits.idx := RegNext(get_idx(u.io.lsu.req.bits.addr))
      a.bits.tag := get_tag(u.io.lsu.s1_paddr)
  }
  st_access.valid := RegNext(mainPipe.io.store_req.fire())
  st_access.bits.idx := RegNext(get_idx(mainPipe.io.store_req.bits.vaddr))
  st_access.bits.tag := RegNext(get_tag(mainPipe.io.store_req.bits.addr))
  val access_info = ld_access.toSeq ++ Seq(st_access)
  val early_replace = RegNext(missQueue.io.debug_early_replace)
  val access_early_replace = access_info.map {
    case acc =>
      Cat(early_replace.map {
        case r =>
          acc.valid && r.valid &&
            acc.bits.tag === r.bits.tag &&
            acc.bits.idx === r.bits.idx
      })
  }
  XSPerfAccumulate("access_early_replace", PopCount(Cat(access_early_replace)))

  val wb_perf      = wb.perfEvents.map(_._1).zip(wb.perfinfo.perfEvents.perf_events)
  val mainp_perf     = mainPipe.perfEvents.map(_._1).zip(mainPipe.perfinfo.perfEvents.perf_events)
  val missq_perf     = missQueue.perfEvents.map(_._1).zip(missQueue.perfinfo.perfEvents.perf_events)
  val probq_perf     = probeQueue.perfEvents.map(_._1).zip(probeQueue.perfinfo.perfEvents.perf_events)
  val ldu_0_perf     = ldu(0).perfEvents.map(_._1).zip(ldu(0).perfinfo.perfEvents.perf_events)
  val ldu_1_perf     = ldu(1).perfEvents.map(_._1).zip(ldu(1).perfinfo.perfEvents.perf_events)
  val perfEvents = wb_perf ++ mainp_perf ++ missq_perf ++ probq_perf ++ ldu_0_perf ++ ldu_1_perf
  val perflist = wb.perfinfo.perfEvents.perf_events ++ mainPipe.perfinfo.perfEvents.perf_events ++
                 missQueue.perfinfo.perfEvents.perf_events ++ probeQueue.perfinfo.perfEvents.perf_events ++
                 ldu(0).perfinfo.perfEvents.perf_events ++ ldu(1).perfinfo.perfEvents.perf_events
  val perf_length = perflist.length
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(perflist.length))
  })
  perfinfo.perfEvents.perf_events := perflist

}

class AMOHelper() extends ExtModule {
  val clock  = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val cmd    = IO(Input(UInt(5.W)))
  val addr   = IO(Input(UInt(64.W)))
  val wdata  = IO(Input(UInt(64.W)))
  val mask   = IO(Input(UInt(8.W)))
  val rdata  = IO(Output(UInt(64.W)))
}

class DCacheWrapper()(implicit p: Parameters) extends LazyModule with HasXSParameter {

  val useDcache = coreParams.dcacheParametersOpt.nonEmpty
  val clientNode = if (useDcache) TLIdentityNode() else null
  val dcache = if (useDcache) LazyModule(new DCache()) else null
  if (useDcache) {
    clientNode := dcache.clientNode
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new DCacheIO)
    val perfinfo = IO(new Bundle(){
      val perfEvents = Output(new PerfEventsBundle(dcache.asInstanceOf[DCache].module.perf_length))
    })
    val perfEvents = dcache.asInstanceOf[DCache].module.perfEvents.map(_._1).zip(dcache.asInstanceOf[DCache].module.perfinfo.perfEvents.perf_events)
    if (!useDcache) {
      // a fake dcache which uses dpi-c to access memory, only for debug usage!
      val fake_dcache = Module(new FakeDCache())
      io <> fake_dcache.io
    }
    else {
      io <> dcache.module.io
      perfinfo := dcache.asInstanceOf[DCache].module.perfinfo
    }
  }
}
