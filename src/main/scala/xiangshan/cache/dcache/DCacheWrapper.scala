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

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import coupledL2.VaddrField
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.{AliasField, PrefetchField}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rob.RobDebugRollingIO
import xiangshan.cache.wpu._
import xiangshan.mem.{AddPipelineReg, HasL1PrefetchSourceParameter}
import xiangshan.mem.prefetch._

// DCache specific parameters
case class DCacheParameters
(
  nSets: Int = 256,
  nWays: Int = 8,
  rowBits: Int = 64,
  tagECC: Option[String] = None,
  dataECC: Option[String] = None,
  replacer: Option[String] = Some("setplru"),
  updateReplaceOn2ndmiss: Boolean = true,
  nMissEntries: Int = 1,
  nProbeEntries: Int = 1,
  nReleaseEntries: Int = 1,
  nMMIOEntries: Int = 1,
  nMMIOs: Int = 1,
  blockBytes: Int = 64,
  nMaxPrefetchEntry: Int = 1,
  alwaysReleaseData: Boolean = false
) extends L1CacheParameters {
  // if sets * blockBytes > 4KB(page size),
  // cache alias will happen,
  // we need to avoid this by recoding additional bits in L2 cache
  val setBytes = nSets * blockBytes
  val aliasBitsOpt = if(setBytes > pageSize) Some(log2Ceil(setBytes / pageSize)) else None

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

trait HasDCacheParameters extends HasL1CacheParameters with HasL1PrefetchSourceParameter{
  val cacheParams = dcacheParameters
  val cfg = cacheParams

  def encWordBits = cacheParams.dataCode.width(wordBits)

  def encRowBits = encWordBits * rowWords // for DuplicatedDataArray only
  def eccBits = encWordBits - wordBits

  def encTagBits = cacheParams.tagCode.width(tagBits)
  def eccTagBits = encTagBits - tagBits

  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant

  def nSourceType = 10
  def sourceTypeWidth = log2Up(nSourceType)
  // non-prefetch source < 3
  def LOAD_SOURCE = 0
  def STORE_SOURCE = 1
  def AMO_SOURCE = 2
  // prefetch source >= 3
  def DCACHE_PREFETCH_SOURCE = 3
  def SOFT_PREFETCH = 4
  // the following sources are only used inside SMS
  def HW_PREFETCH_AGT = 5
  def HW_PREFETCH_PHT_CUR = 6
  def HW_PREFETCH_PHT_INC = 7
  def HW_PREFETCH_PHT_DEC = 8
  def HW_PREFETCH_BOP = 9
  def HW_PREFETCH_STRIDE = 10

  def BLOOM_FILTER_ENTRY_NUM = 4096

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = log2Up(nEntries) max log2Up(StoreBufferSize)

  require(isPow2(cfg.nMissEntries)) // TODO
  // require(isPow2(cfg.nReleaseEntries))
  require(cfg.nMissEntries < cfg.nReleaseEntries)
  val nEntries = cfg.nMissEntries + cfg.nReleaseEntries
  val releaseIdBase = cfg.nMissEntries

  // banked dcache support
  val DCacheSetDiv = 1
  val DCacheSets = cacheParams.nSets
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 8 // hardcoded
  val DCacheDupNum = 16
  val DCacheSRAMRowBits = cacheParams.rowBits // hardcoded
  val DCacheWordBits = 64 // hardcoded
  val DCacheWordBytes = DCacheWordBits / 8
  val MaxPrefetchEntry = cacheParams.nMaxPrefetchEntry
  val DCacheVWordBytes = VLEN / 8
  require(DCacheSRAMRowBits == 64)

  val DCacheSetDivBits = log2Ceil(DCacheSetDiv)
  val DCacheSetBits = log2Ceil(DCacheSets)
  val DCacheSizeBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheSizeBytes = DCacheSizeBits / 8
  val DCacheSizeWords = DCacheSizeBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordOffset = log2Up(DCacheWordBytes)
  val DCacheVWordOffset = log2Up(DCacheVWordBytes)

  val DCacheBankOffset = log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheLineOffset = DCacheSetOffset

  // uncache
  val uncacheIdxBits = log2Up(StoreQueueSize + 1) max log2Up(VirtualLoadQueueSize + 1)
  // hardware prefetch parameters
  // high confidence hardware prefetch port
  val HighConfHWPFLoadPort = LoadPipelineWidth - 1 // use the last load port by default
  val IgnorePrefetchConfidence = false

  // parameters about duplicating regs to solve fanout
  // In Main Pipe:
    // tag_write.ready -> data_write.valid * 8 banks
    // tag_write.ready -> meta_write.valid
    // tag_write.ready -> tag_write.valid
    // tag_write.ready -> err_write.valid
    // tag_write.ready -> wb.valid
  val nDupTagWriteReady = DCacheBanks + 4
  // In Main Pipe:
    // data_write.ready -> data_write.valid * 8 banks
    // data_write.ready -> meta_write.valid
    // data_write.ready -> tag_write.valid
    // data_write.ready -> err_write.valid
    // data_write.ready -> wb.valid
  val nDupDataWriteReady = DCacheBanks + 4
  val nDupWbReady = DCacheBanks + 4
  val nDupStatus = nDupTagWriteReady + nDupDataWriteReady
  val dataWritePort = 0
  val metaWritePort = DCacheBanks
  val tagWritePort = metaWritePort + 1
  val errWritePort = tagWritePort + 1
  val wbPort = errWritePort + 1

  def set_to_dcache_div(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    if (DCacheSetDivBits == 0) 0.U else set(DCacheSetDivBits-1, 0)
  }

  def set_to_dcache_div_set(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    set(DCacheSetBits - 1, DCacheSetDivBits)
  }

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset-1, DCacheBankOffset)
  }

  def addr_to_dcache_div(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    if(DCacheSetDivBits == 0) 0.U else addr(DCacheSetOffset + DCacheSetDivBits - 1, DCacheSetOffset)
  }

  def addr_to_dcache_div_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset - 1, DCacheSetOffset + DCacheSetDivBits)
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
  
  def get_alias(vaddr: UInt): UInt ={
    if(blockOffBits + idxBits > pgIdxBits){
      vaddr(blockOffBits + idxBits - 1, pgIdxBits)
    }else{
      0.U
    }
  }

  def is_alias_match(vaddr0: UInt, vaddr1: UInt): Bool = {
    require(vaddr0.getWidth == VAddrBits && vaddr1.getWidth == VAddrBits)
    if(blockOffBits + idxBits > pgIdxBits) {
      vaddr0(blockOffBits + idxBits - 1, pgIdxBits) === vaddr1(blockOffBits + idxBits - 1, pgIdxBits)
    }else {
      // no alias problem
      true.B
    }
  }

  def get_direct_map_way(addr:UInt): UInt = {
    addr(DCacheAboveIndexOffset + log2Up(DCacheWays) - 1, DCacheAboveIndexOffset)
  }

  def arbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def arbiter_with_pipereg[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def arbiter_with_pipereg_N_dup[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    dups: Seq[DecoupledIO[T]],
    name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    for (dup <- dups) {
      AddPipelineReg(arb.io.out, dup, false.B)
    }
    AddPipelineReg(arb.io.out, out, false.B)
  }

  def rrArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new RRArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
  }

  def fastArbiter[T <: Bundle](
    in: Seq[DecoupledIO[T]],
    out: DecoupledIO[T],
    name: Option[String] = None): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) {
      a <> req
    }
    out <> arb.io.out
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
  val dmWay = Output(UInt(log2Up(nWays).W))
  val way = Input(UInt(log2Up(nWays).W))
}

class DCacheExtraMeta(implicit p: Parameters) extends DCacheBundle
{
  val error = Bool() // cache line has been marked as corrupted by l2 / ecc error detected when store
  val prefetch = UInt(L1PfSourceBits.W) // cache line is first required by prefetch
  val access = Bool() // cache line has been accessed by load / store

  // val debug_access_timestamp = UInt(64.W) // last time a load / store / refill access that cacheline
}

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  val cmd    = UInt(M_SZ.W)
  val vaddr  = UInt(VAddrBits.W)
  val data   = UInt(VLEN.W)
  val mask   = UInt((VLEN/8).W)
  val id     = UInt(reqIdWidth.W)
  val instrtype   = UInt(sourceTypeWidth.W)
  val isFirstIssue = Bool()
  val replayCarry = new ReplayCarry(nWays)

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("DCacheWordReq: cmd: %x vaddr: %x data: %x mask: %x id: %d\n",
      cmd, vaddr, data, mask, id)
  }
}

// memory request in word granularity(store)
class DCacheLineReq(implicit p: Parameters) extends DCacheBundle
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
  val addr = UInt(PAddrBits.W)
  val wline = Bool()
}

class DCacheWordReqWithVaddrAndPfFlag(implicit p: Parameters) extends DCacheWordReqWithVaddr {
  val prefetch = Bool()

  def toDCacheWordReqWithVaddr() = {
    val res = Wire(new DCacheWordReqWithVaddr)
    res.vaddr := vaddr
    res.wline := wline
    res.cmd := cmd
    res.addr := addr
    res.data := data
    res.mask := mask
    res.id := id
    res.instrtype := instrtype
    res.replayCarry := replayCarry
    res.isFirstIssue := isFirstIssue
    res.debug_robIdx := debug_robIdx

    res
  }
}

class BaseDCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  // read in s2
  val data = UInt(VLEN.W)
  // select in s3
  val data_delayed = UInt(VLEN.W)
  val id     = UInt(reqIdWidth.W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache miss, and failed to enter the missqueue, replay from RS is needed
  val replay = Bool()
  val replayCarry = new ReplayCarry(nWays)
  // data has been corrupted
  val tag_error = Bool() // tag error
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("DCacheWordResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class DCacheWordResp(implicit p: Parameters) extends BaseDCacheWordResp
{
  val meta_prefetch = UInt(L1PfSourceBits.W)
  val meta_access = Bool()
  // s2
  val handled = Bool()
  val real_miss = Bool()
  // s3: 1 cycle after data resp
  val error_delayed = Bool() // all kinds of errors, include tag error
  val replacementUpdated = Bool()
}

class BankedDCacheWordResp(implicit p: Parameters) extends DCacheWordResp
{
  val bank_data = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
  val bank_oh = UInt(DCacheBanks.W)
}

class DCacheWordRespWithError(implicit p: Parameters) extends BaseDCacheWordResp
{
  val error = Bool() // all kinds of errors, include tag error
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
  val error  = Bool() // refilled data has been corrupted
  // for debug usage
  val data_raw = UInt((cfg.blockBytes * 8).W)
  val hasdata = Bool()
  val refill_done = Bool()
  def dump() = {
    XSDebug("Refill: addr: %x data: %x\n", addr, data)
  }
  val id     = UInt(log2Up(cfg.nMissEntries).W)
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


class UncacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  val cmd  = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val data = UInt(XLEN.W)
  val mask = UInt((XLEN/8).W)
  val id   = UInt(uncacheIdxBits.W)
  val instrtype = UInt(sourceTypeWidth.W)
  val atomic = Bool()
  val isFirstIssue = Bool()
  val replayCarry = new ReplayCarry(nWays)

  def dump() = {
    XSDebug("UncacheWordReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

class UncacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data      = UInt(XLEN.W)
  val data_delayed = UInt(XLEN.W)
  val id        = UInt(uncacheIdxBits.W)
  val miss      = Bool()
  val replay    = Bool()
  val tag_error = Bool()
  val error     = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val mshr_id = UInt(log2Up(cfg.nMissEntries).W)  // FIXME: why uncacheWordResp is not merged to baseDcacheResp

  val debug_robIdx = UInt(log2Ceil(RobSize).W)
  def dump() = {
    XSDebug("UncacheWordResp: data: %x id: %d miss: %b replay: %b, tag_error: %b, error: %b\n",
      data, id, miss, replay, tag_error, error)
  }
}

class UncacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new UncacheWordReq)
  val resp = Flipped(DecoupledIO(new UncacheWordResp))
}

class AtomicsResp(implicit p: Parameters) extends DCacheBundle {
  val data    = UInt(DataBits.W)
  val miss    = Bool()
  val miss_id = UInt(log2Up(cfg.nMissEntries).W)
  val replay  = Bool()
  val error   = Bool()

  val ack_miss_queue = Bool()

  val id     = UInt(reqIdWidth.W)
}

class AtomicWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new MainPipeReq)
  val resp = Flipped(ValidIO(new AtomicsResp))
  val block_lr = Input(Bool())
}

// used by load unit
class DCacheLoadIO(implicit p: Parameters) extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill  = Output(Bool())
  val s2_kill  = Output(Bool())
  val s0_pc = Output(UInt(VAddrBits.W))
  val s1_pc = Output(UInt(VAddrBits.W))
  val s2_pc = Output(UInt(VAddrBits.W))
  // cycle 0: load has updated replacement before
  val replacementUpdated = Output(Bool())
  // cycle 0: prefetch source bits
  val pf_source = Output(UInt(L1PfSourceBits.W))
  // cycle 0: virtual address: req.addr
  // cycle 1: physical address: s1_paddr
  val s1_paddr_dup_lsu = Output(UInt(PAddrBits.W)) // lsu side paddr
  val s1_paddr_dup_dcache = Output(UInt(PAddrBits.W)) // dcache side paddr
  val s1_disable_fast_wakeup = Input(Bool())
  // cycle 2: hit signal
  val s2_hit = Input(Bool()) // hit signal for lsu,
  val s2_first_hit = Input(Bool())
  val s2_bank_conflict = Input(Bool())
  val s2_wpu_pred_fail = Input(Bool())
  val s2_mq_nack = Input(Bool())

  // debug
  val debug_s1_hit_way = Input(UInt(nWays.W))
  val debug_s2_pred_way_num = Input(UInt(XLEN.W))
  val debug_s2_dm_way_num = Input(UInt(XLEN.W))
  val debug_s2_real_way_num = Input(UInt(XLEN.W))
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

// forward tilelink channel D's data to ldu
class DcacheToLduForwardIO(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val data = UInt(l1BusDataWidth.W)
  val mshrid = UInt(log2Up(cfg.nMissEntries).W)
  val last = Bool()

  def apply(req_valid : Bool, req_data : UInt, req_mshrid : UInt, req_last : Bool) = {
    valid := req_valid
    data := req_data
    mshrid := req_mshrid
    last := req_last
  }

  def dontCare() = {
    valid := false.B
    data := DontCare
    mshrid := DontCare
    last := DontCare
  }

  def forward(req_valid : Bool, req_mshr_id : UInt, req_paddr : UInt) = {
    val all_match = req_valid && valid &&
                req_mshr_id === mshrid &&
                req_paddr(log2Up(refillBytes)) === last

    val forward_D = RegInit(false.B)
    val forwardData = RegInit(VecInit(List.fill(VLEN/8)(0.U(8.W))))

    val block_idx = req_paddr(log2Up(refillBytes) - 1, 3)
    val block_data = Wire(Vec(l1BusDataWidth / 64, UInt(64.W)))
    (0 until l1BusDataWidth / 64).map(i => {
      block_data(i) := data(64 * i + 63, 64 * i)
    })
    val selected_data = Wire(UInt(128.W))
    selected_data := Mux(req_paddr(3), Fill(2, block_data(block_idx)), Cat(block_data(block_idx + 1.U), block_data(block_idx)))

    forward_D := all_match
    for (i <- 0 until VLEN/8) {
      forwardData(i) := selected_data(8 * i + 7, 8 * i)
    }

    (forward_D, forwardData)
  }
}

class MissEntryForwardIO(implicit p: Parameters) extends DCacheBundle {
  val inflight = Bool()
  val paddr = UInt(PAddrBits.W)
  val raw_data = Vec(blockRows, UInt(rowBits.W))
  val firstbeat_valid = Bool()
  val lastbeat_valid = Bool()

  def apply(mshr_valid : Bool, mshr_paddr : UInt, mshr_rawdata : Vec[UInt], mshr_first_valid : Bool, mshr_last_valid : Bool) = {
    inflight := mshr_valid
    paddr := mshr_paddr
    raw_data := mshr_rawdata
    firstbeat_valid := mshr_first_valid
    lastbeat_valid := mshr_last_valid
  }

  // check if we can forward from mshr or D channel
  def check(req_valid : Bool, req_paddr : UInt) = {
    RegNext(req_valid && inflight && req_paddr(PAddrBits - 1, blockOffBits) === paddr(PAddrBits - 1, blockOffBits))
  }

  def forward(req_valid : Bool, req_paddr : UInt) = {
    val all_match = (req_paddr(log2Up(refillBytes)) === 0.U && firstbeat_valid) ||
                    (req_paddr(log2Up(refillBytes)) === 1.U && lastbeat_valid)

    val forward_mshr = RegInit(false.B)
    val forwardData = RegInit(VecInit(List.fill(VLEN/8)(0.U(8.W))))

    val block_idx = req_paddr(log2Up(refillBytes), 3)
    val block_data = raw_data

    val selected_data = Wire(UInt(128.W))
    selected_data := Mux(req_paddr(3), Fill(2, block_data(block_idx)), Cat(block_data(block_idx + 1.U), block_data(block_idx)))

    forward_mshr := all_match
    for (i <- 0 until VLEN/8) {
      forwardData(i) := selected_data(8 * i + 7, 8 * i)
    }

    (forward_mshr, forwardData)
  }
}

// forward mshr's data to ldu
class LduToMissqueueForwardIO(implicit p: Parameters) extends DCacheBundle {
  // req
  val valid = Input(Bool())
  val mshrid = Input(UInt(log2Up(cfg.nMissEntries).W))
  val paddr = Input(UInt(PAddrBits.W))
  // resp
  val forward_mshr = Output(Bool())
  val forwardData = Output(Vec(VLEN/8, UInt(8.W)))
  val forward_result_valid = Output(Bool())

  def connect(sink: LduToMissqueueForwardIO) = {
    sink.valid := valid
    sink.mshrid := mshrid
    sink.paddr := paddr
    forward_mshr := sink.forward_mshr
    forwardData := sink.forwardData
    forward_result_valid := sink.forward_result_valid
  }

  def forward() = {
    (forward_result_valid, forward_mshr, forwardData)
  }
}

class StorePrefetchReq(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val sta   = Vec(StorePipelineWidth, Flipped(new DCacheStoreIO)) // for non-blocking store
  val lsq = ValidIO(new Refill)  // refill to load queue, wake up load misses
  val tl_d_channel = Output(new DcacheToLduForwardIO)
  val store = new DCacheToSbufferIO // for sbuffer
  val atomics  = Flipped(new AtomicWordIO)  // atomics reqs
  val release = ValidIO(new Release) // cacheline release hint for ld-ld violation check
  val forward_D = Output(Vec(LoadPipelineWidth, new DcacheToLduForwardIO))
  val forward_mshr = Vec(LoadPipelineWidth, new LduToMissqueueForwardIO)
}

class DCacheTopDownIO(implicit p: Parameters) extends DCacheBundle {
  val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  val robHeadMissInDCache = Output(Bool())
  val robHeadOtherReplay = Input(Bool())
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt(8.W))
  val l2_pf_store_only = Input(Bool())
  val lsu = new DCacheToLsuIO
  val csr = new L1CacheToCsrIO
  val error = new L1CacheErrorInfo
  val mshrFull = Output(Bool())
  val memSetPattenDetected = Output(Bool())
  val lqEmpty = Input(Bool())
  val pf_ctrl = Output(new PrefetchControlBundle)
  val force_write = Input(Bool())
  val debugTopDown = new DCacheTopDownIO
  val debugRolling = Flipped(new RobDebugRollingIO)
}

class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  override def shouldBeInlined: Boolean = false

  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField(),
    VaddrField(VAddrBits - blockOffBits),
  ) ++ cacheParams.aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Nil

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dcache",
      sourceId = IdRange(0, nEntries + 1),
      supportsProbe = TransferSizes(cfg.blockBytes)
    )),
    requestFields = reqFields,
    echoFields = echoFields
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters with HasPerfEvents with HasL1PrefetchSourceParameter {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "DCache: tilelink width does not match")

  println("DCache:")
  println("  DCacheSets: " + DCacheSets)
  println("  DCacheSetDiv: " + DCacheSetDiv)
  println("  DCacheWays: " + DCacheWays)
  println("  DCacheBanks: " + DCacheBanks)
  println("  DCacheSRAMRowBits: " + DCacheSRAMRowBits)
  println("  DCacheWordOffset: " + DCacheWordOffset)
  println("  DCacheBankOffset: " + DCacheBankOffset)
  println("  DCacheSetOffset: " + DCacheSetOffset)
  println("  DCacheTagOffset: " + DCacheTagOffset)
  println("  DCacheAboveIndexOffset: " + DCacheAboveIndexOffset)
  println("  DcacheMaxPrefetchEntry: " + MaxPrefetchEntry)
  println("  WPUEnable: " + dwpuParam.enWPU)
  println("  WPUEnableCfPred: " + dwpuParam.enCfPred)
  println("  WPUAlgorithm: " + dwpuParam.algoName)

  // Enable L1 Store prefetch
  val StorePrefetchL1Enabled = EnableStorePrefetchAtCommit || EnableStorePrefetchAtIssue || EnableStorePrefetchSPB
  val MetaReadPort = if(StorePrefetchL1Enabled) LoadPipelineWidth + 1 + StorePipelineWidth else LoadPipelineWidth + 1
  val TagReadPort = if(StorePrefetchL1Enabled) LoadPipelineWidth + 1 + StorePipelineWidth else LoadPipelineWidth + 1

  // Enable L1 Load prefetch
  val LoadPrefetchL1Enabled = true
  val AccessArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1
  val PrefetchArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1

  //----------------------------------------
  // core data structures
  val bankedDataArray = if(dwpuParam.enWPU) Module(new SramedDataArray) else Module(new BankedDataArray)
  val metaArray = Module(new L1CohMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 2))
  val errorArray = Module(new L1FlagMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 2))
  val prefetchArray = Module(new L1PrefetchSourceArray(readPorts = PrefetchArrayReadPort, writePorts = 2 + LoadPipelineWidth)) // prefetch flag array
  val accessArray = Module(new L1FlagMetaArray(readPorts = AccessArrayReadPort, writePorts = LoadPipelineWidth + 2))
  val tagArray = Module(new DuplicatedTagArray(readPorts = TagReadPort))
  val prefetcherMonitor = Module(new PrefetcherMonitor)
  val fdpMonitor =  Module(new FDPrefetcherMonitor)
  val bloomFilter =  Module(new BloomFilter(BLOOM_FILTER_ENTRY_NUM, true))
  val counterFilter = Module(new CounterFilter)
  bankedDataArray.dump()

  //----------------------------------------
  // core modules
  val ldu = Seq.tabulate(LoadPipelineWidth)({ i => Module(new LoadPipe(i))})
  val stu = Seq.tabulate(StorePipelineWidth)({ i => Module(new StorePipe(i))})
  val mainPipe     = Module(new MainPipe)
  val refillPipe   = Module(new RefillPipe)
  val missQueue    = Module(new MissQueue(edge))
  val probeQueue   = Module(new ProbeQueue(edge))
  val wb           = Module(new WritebackQueue(edge))

  missQueue.io.lqEmpty := io.lqEmpty
  missQueue.io.hartId := io.hartId
  missQueue.io.l2_pf_store_only := RegNext(io.l2_pf_store_only, false.B)
  missQueue.io.debugTopDown <> io.debugTopDown
  io.memSetPattenDetected := missQueue.io.memSetPattenDetected

  val errors = ldu.map(_.io.error) ++ // load error
    Seq(mainPipe.io.error) // store / misc error
  io.error <> RegNext(Mux1H(errors.map(e => RegNext(e.valid) -> RegNext(e))))

  //----------------------------------------
  // meta array

  // read / write coh meta
  val meta_read_ports = ldu.map(_.io.meta_read) ++
    Seq(mainPipe.io.meta_read) ++
    stu.map(_.io.meta_read)

  val meta_resp_ports = ldu.map(_.io.meta_resp) ++
    Seq(mainPipe.io.meta_resp) ++
    stu.map(_.io.meta_resp)

  val meta_write_ports = Seq(
    mainPipe.io.meta_write,
    refillPipe.io.meta_write
  )
  if(StorePrefetchL1Enabled) {
    meta_read_ports.zip(metaArray.io.read).foreach { case (p, r) => r <> p }
    meta_resp_ports.zip(metaArray.io.resp).foreach { case (p, r) => p := r }
  }else {
    meta_read_ports.take(LoadPipelineWidth + 1).zip(metaArray.io.read).foreach { case (p, r) => r <> p }
    meta_resp_ports.take(LoadPipelineWidth + 1).zip(metaArray.io.resp).foreach { case (p, r) => p := r }

    meta_read_ports.drop(LoadPipelineWidth + 1).foreach { case p => p.ready := false.B }
    meta_resp_ports.drop(LoadPipelineWidth + 1).foreach { case p => p := 0.U.asTypeOf(p) }
  }
  meta_write_ports.zip(metaArray.io.write).foreach { case (p, w) => w <> p }

  // read extra meta (exclude stu)
  meta_read_ports.take(LoadPipelineWidth + 1).zip(errorArray.io.read).foreach { case (p, r) => r <> p }
  meta_read_ports.take(LoadPipelineWidth + 1).zip(prefetchArray.io.read).foreach { case (p, r) => r <> p }
  meta_read_ports.take(LoadPipelineWidth + 1).zip(accessArray.io.read).foreach { case (p, r) => r <> p }
  val extra_meta_resp_ports = ldu.map(_.io.extra_meta_resp) ++
    Seq(mainPipe.io.extra_meta_resp)
  extra_meta_resp_ports.zip(errorArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).error := r(i) })
  }}
  extra_meta_resp_ports.zip(prefetchArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).prefetch := r(i) })
  }}
  extra_meta_resp_ports.zip(accessArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).access := r(i) })
  }}

  if(LoadPrefetchL1Enabled) {
    // use last port to read prefetch and access flag
    prefetchArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
    prefetchArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
    prefetchArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en

    accessArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
    accessArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
    accessArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en

    val extra_flag_valid = RegNext(refillPipe.io.prefetch_flag_write.valid)
    val extra_flag_way_en = RegEnable(refillPipe.io.prefetch_flag_write.bits.way_en, refillPipe.io.prefetch_flag_write.valid)
    val extra_flag_prefetch = Mux1H(extra_flag_way_en, prefetchArray.io.resp.last)
    val extra_flag_access = Mux1H(extra_flag_way_en, accessArray.io.resp.last)

    prefetcherMonitor.io.validity.good_prefetch := extra_flag_valid && isFromL1Prefetch(extra_flag_prefetch) && extra_flag_access
    prefetcherMonitor.io.validity.bad_prefetch := extra_flag_valid && isFromL1Prefetch(extra_flag_prefetch) && !extra_flag_access
  }

  // write extra meta
  val error_flag_write_ports = Seq(
    mainPipe.io.error_flag_write, // error flag generated by corrupted store
    refillPipe.io.error_flag_write // corrupted signal from l2
  )
  error_flag_write_ports.zip(errorArray.io.write).foreach { case (p, w) => w <> p }

  val prefetch_flag_write_ports = ldu.map(_.io.prefetch_flag_write) ++ Seq(
    mainPipe.io.prefetch_flag_write, // set prefetch_flag to false if coh is set to Nothing
    refillPipe.io.prefetch_flag_write // refill required by prefetch will set prefetch_flag
  )
  prefetch_flag_write_ports.zip(prefetchArray.io.write).foreach { case (p, w) => w <> p }

  val same_cycle_update_pf_flag = ldu(0).io.prefetch_flag_write.valid && ldu(1).io.prefetch_flag_write.valid && (ldu(0).io.prefetch_flag_write.bits.idx === ldu(1).io.prefetch_flag_write.bits.idx) && (ldu(0).io.prefetch_flag_write.bits.way_en === ldu(1).io.prefetch_flag_write.bits.way_en)
  XSPerfAccumulate("same_cycle_update_pf_flag", same_cycle_update_pf_flag)

  val access_flag_write_ports = ldu.map(_.io.access_flag_write) ++ Seq(
    mainPipe.io.access_flag_write,
    refillPipe.io.access_flag_write
  )
  access_flag_write_ports.zip(accessArray.io.write).foreach { case (p, w) => w <> p }

  //----------------------------------------
  // tag array
  if(StorePrefetchL1Enabled) {
    require(tagArray.io.read.size == (ldu.size + stu.size + 1))
  }else {
    require(tagArray.io.read.size == (ldu.size + 1))
  }
  val tag_write_intend = missQueue.io.refill_pipe_req.valid || mainPipe.io.tag_write_intend
  assert(!RegNext(!tag_write_intend && tagArray.io.write.valid))
  ldu.zipWithIndex.foreach {
    case (ld, i) =>
      tagArray.io.read(i) <> ld.io.tag_read
      ld.io.tag_resp := tagArray.io.resp(i)
      ld.io.tag_read.ready := !tag_write_intend
  }
  if(StorePrefetchL1Enabled) {
    stu.zipWithIndex.foreach {
      case (st, i) =>
        tagArray.io.read(ldu.size + i) <> st.io.tag_read
        st.io.tag_resp := tagArray.io.resp(ldu.size + i)
        st.io.tag_read.ready := !tag_write_intend
    }
  }else {
    stu.foreach {
      case st =>
        st.io.tag_read.ready := false.B
        st.io.tag_resp := 0.U.asTypeOf(st.io.tag_resp)
    }
  }
  tagArray.io.read.last <> mainPipe.io.tag_read
  mainPipe.io.tag_resp := tagArray.io.resp.last

  val fake_tag_read_conflict_this_cycle = PopCount(ldu.map(ld=> ld.io.tag_read.valid))
  XSPerfAccumulate("fake_tag_read_conflict", fake_tag_read_conflict_this_cycle)

  val tag_write_arb = Module(new Arbiter(new TagWriteReq, 2))
  tag_write_arb.io.in(0) <> refillPipe.io.tag_write
  tag_write_arb.io.in(1) <> mainPipe.io.tag_write
  tagArray.io.write <> tag_write_arb.io.out

  ldu.map(m => {
    m.io.vtag_update.valid := tagArray.io.write.valid
    m.io.vtag_update.bits := tagArray.io.write.bits
  })

  //----------------------------------------
  // data array
  mainPipe.io.data_read.zip(ldu).map(x => x._1 := x._2.io.lsu.req.valid)

  val dataWriteArb = Module(new Arbiter(new L1BankedDataWriteReq, 2))
  dataWriteArb.io.in(0) <> refillPipe.io.data_write
  dataWriteArb.io.in(1) <> mainPipe.io.data_write

  bankedDataArray.io.write <> dataWriteArb.io.out

  for (bank <- 0 until DCacheBanks) {
    val dataWriteArb_dup = Module(new Arbiter(new L1BankedDataWriteReqCtrl, 2))
    dataWriteArb_dup.io.in(0).valid := refillPipe.io.data_write_dup(bank).valid
    dataWriteArb_dup.io.in(0).bits := refillPipe.io.data_write_dup(bank).bits
    dataWriteArb_dup.io.in(1).valid := mainPipe.io.data_write_dup(bank).valid
    dataWriteArb_dup.io.in(1).bits := mainPipe.io.data_write_dup(bank).bits

    bankedDataArray.io.write_dup(bank) <> dataWriteArb_dup.io.out
  }

  bankedDataArray.io.readline <> mainPipe.io.data_readline
  bankedDataArray.io.readline_intend := mainPipe.io.data_read_intend
  mainPipe.io.readline_error_delayed := bankedDataArray.io.readline_error_delayed
  mainPipe.io.data_resp := bankedDataArray.io.readline_resp

  (0 until LoadPipelineWidth).map(i => {
    bankedDataArray.io.read(i) <> ldu(i).io.banked_data_read
    bankedDataArray.io.is128Req(i) <> ldu(i).io.is128Req
    bankedDataArray.io.read_error_delayed(i) <> ldu(i).io.read_error_delayed

    ldu(i).io.banked_data_resp := bankedDataArray.io.read_resp_delayed(i)

    ldu(i).io.bank_conflict_slow := bankedDataArray.io.bank_conflict_slow(i)
  })

  (0 until LoadPipelineWidth).map(i => {
    val (_, _, done, _) = edge.count(bus.d)
    when(bus.d.bits.opcode === TLMessages.GrantData) {
      io.lsu.forward_D(i).apply(bus.d.valid, bus.d.bits.data, bus.d.bits.source, done)
    }.otherwise {
      io.lsu.forward_D(i).dontCare()
    }
  })
  // tl D channel wakeup
  val (_, _, done, _) = edge.count(bus.d)
  when (bus.d.bits.opcode === TLMessages.GrantData || bus.d.bits.opcode === TLMessages.Grant) {
    io.lsu.tl_d_channel.apply(bus.d.valid, bus.d.bits.data, bus.d.bits.source, done)
  } .otherwise {
    io.lsu.tl_d_channel.dontCare()
  }
  mainPipe.io.force_write <> io.force_write

  /** dwpu */
  val dwpu = Module(new DCacheWpuWrapper(LoadPipelineWidth))
  for(i <- 0 until LoadPipelineWidth){
    dwpu.io.req(i) <> ldu(i).io.dwpu.req(0)
    dwpu.io.resp(i) <> ldu(i).io.dwpu.resp(0)
    dwpu.io.lookup_upd(i) <> ldu(i).io.dwpu.lookup_upd(0)
    dwpu.io.cfpred(i) <> ldu(i).io.dwpu.cfpred(0)
  }
  dwpu.io.tagwrite_upd.valid := tagArray.io.write.valid
  dwpu.io.tagwrite_upd.bits.vaddr := tagArray.io.write.bits.vaddr
  dwpu.io.tagwrite_upd.bits.s1_real_way_en := tagArray.io.write.bits.way_en

  //----------------------------------------
  // load pipe
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    ldu(w).io.lsu <> io.lsu.load(w)

    // TODO:when have load128Req
    ldu(w).io.load128Req := false.B

    // replay and nack not needed anymore
    // TODO: remove replay and nack
    ldu(w).io.nack := false.B

    ldu(w).io.disable_ld_fast_wakeup :=
      bankedDataArray.io.disable_ld_fast_wakeup(w) // load pipe fast wake up should be disabled when bank conflict
  }

  prefetcherMonitor.io.timely.total_prefetch := ldu.map(_.io.prefetch_info.naive.total_prefetch).reduce(_ || _)
  prefetcherMonitor.io.timely.late_hit_prefetch := ldu.map(_.io.prefetch_info.naive.late_hit_prefetch).reduce(_ || _)
  prefetcherMonitor.io.timely.late_miss_prefetch := missQueue.io.prefetch_info.naive.late_miss_prefetch
  prefetcherMonitor.io.timely.prefetch_hit := PopCount(ldu.map(_.io.prefetch_info.naive.prefetch_hit))
  io.pf_ctrl <> prefetcherMonitor.io.pf_ctrl
  XSPerfAccumulate("useless_prefetch", ldu.map(_.io.prefetch_info.naive.total_prefetch).reduce(_ || _) && !(ldu.map(_.io.prefetch_info.naive.useful_prefetch).reduce(_ || _)))
  XSPerfAccumulate("useful_prefetch", ldu.map(_.io.prefetch_info.naive.useful_prefetch).reduce(_ || _))
  XSPerfAccumulate("late_prefetch_hit", ldu.map(_.io.prefetch_info.naive.late_prefetch_hit).reduce(_ || _))
  XSPerfAccumulate("late_load_hit", ldu.map(_.io.prefetch_info.naive.late_load_hit).reduce(_ || _))

  /** LoadMissDB: record load miss state */
  val isWriteLoadMissTable = WireInit(Constantin.createRecord("isWriteLoadMissTable" + p(XSCoreParamsKey).HartId.toString))
  val isFirstHitWrite = WireInit(Constantin.createRecord("isFirstHitWrite" + p(XSCoreParamsKey).HartId.toString))
  val tableName = "LoadMissDB" + p(XSCoreParamsKey).HartId.toString
  val siteName = "DcacheWrapper" + p(XSCoreParamsKey).HartId.toString
  val loadMissTable = ChiselDB.createTable(tableName, new LoadMissEntry)
  for( i <- 0 until LoadPipelineWidth){
    val loadMissEntry = Wire(new LoadMissEntry)
    val loadMissWriteEn =
      (!ldu(i).io.lsu.resp.bits.replay && ldu(i).io.miss_req.fire) ||
      (ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid && isFirstHitWrite.orR)
    loadMissEntry.timeCnt := GTimer()
    loadMissEntry.robIdx := ldu(i).io.lsu.resp.bits.debug_robIdx
    loadMissEntry.paddr := ldu(i).io.miss_req.bits.addr
    loadMissEntry.vaddr := ldu(i).io.miss_req.bits.vaddr
    loadMissEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.miss_req.fire & ldu(i).io.miss_resp.merged,
      ldu(i).io.miss_req.fire & !ldu(i).io.miss_resp.merged,
      ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid
    )))
    loadMissTable.log(
      data = loadMissEntry,
      en = isWriteLoadMissTable.orR && loadMissWriteEn,
      site = siteName,
      clock = clock,
      reset = reset
    )
  }

  val isWriteLoadAccessTable = WireInit(Constantin.createRecord("isWriteLoadAccessTable" + p(XSCoreParamsKey).HartId.toString))
  val loadAccessTable = ChiselDB.createTable("LoadAccessDB" + p(XSCoreParamsKey).HartId.toString, new LoadAccessEntry)
  for (i <- 0 until LoadPipelineWidth) {
    val loadAccessEntry = Wire(new LoadAccessEntry)
    loadAccessEntry.timeCnt := GTimer()
    loadAccessEntry.robIdx := ldu(i).io.lsu.resp.bits.debug_robIdx
    loadAccessEntry.paddr := ldu(i).io.miss_req.bits.addr
    loadAccessEntry.vaddr := ldu(i).io.miss_req.bits.vaddr
    loadAccessEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.miss_req.fire & ldu(i).io.miss_resp.merged,
      ldu(i).io.miss_req.fire & !ldu(i).io.miss_resp.merged,
      ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid
    )))
    loadAccessEntry.pred_way_num := ldu(i).io.lsu.debug_s2_pred_way_num
    loadAccessEntry.real_way_num := ldu(i).io.lsu.debug_s2_real_way_num
    loadAccessEntry.dm_way_num := ldu(i).io.lsu.debug_s2_dm_way_num
    loadAccessTable.log(
      data = loadAccessEntry,
      en = isWriteLoadAccessTable.orR && ldu(i).io.lsu.resp.valid,
      site = siteName + "_loadpipe" + i.toString,
      clock = clock,
      reset = reset
    )
  }

  //----------------------------------------
  // Sta pipe
  for (w <- 0 until StorePipelineWidth) {
    stu(w).io.lsu <> io.lsu.sta(w)
  }

  //----------------------------------------
  // atomics
  // atomics not finished yet
  // io.lsu.atomics <> atomicsReplayUnit.io.lsu
  io.lsu.atomics.resp := RegNext(mainPipe.io.atomic_resp)
  io.lsu.atomics.block_lr := mainPipe.io.block_lr
  // atomicsReplayUnit.io.pipe_resp := RegNext(mainPipe.io.atomic_resp)
  // atomicsReplayUnit.io.block_lr <> mainPipe.io.block_lr

  //----------------------------------------
  // miss queue
  // missReqArb port:
  // enableStorePrefetch: main pipe * 1 + load pipe * 2 + store pipe * 2; disable: main pipe * 1 + load pipe * 2
  // higher priority is given to lower indices
  val MissReqPortCount = if(StorePrefetchL1Enabled) LoadPipelineWidth + 1 + StorePipelineWidth else LoadPipelineWidth + 1
  val MainPipeMissReqPort = 0

  // Request
  val missReqArb = Module(new ArbiterFilterByCacheLineAddr(new MissReq, MissReqPortCount, blockOffBits, PAddrBits))

  missReqArb.io.in(MainPipeMissReqPort) <> mainPipe.io.miss_req
  for (w <- 0 until LoadPipelineWidth)  { missReqArb.io.in(w + 1) <> ldu(w).io.miss_req }

  for (w <- 0 until LoadPipelineWidth) { ldu(w).io.miss_resp := missQueue.io.resp }
  mainPipe.io.miss_resp := missQueue.io.resp

  if(StorePrefetchL1Enabled) {
    for (w <- 0 until StorePipelineWidth) { missReqArb.io.in(w + 1 + LoadPipelineWidth) <> stu(w).io.miss_req }
  }else {
    for (w <- 0 until StorePipelineWidth) { stu(w).io.miss_req.ready := false.B }
  }

  wb.io.miss_req.valid := missReqArb.io.out.valid
  wb.io.miss_req.bits  := missReqArb.io.out.bits.addr

  // block_decoupled(missReqArb.io.out, missQueue.io.req, wb.io.block_miss_req)
  missReqArb.io.out <> missQueue.io.req
  when(wb.io.block_miss_req) {
    missQueue.io.req.bits.cancel := true.B
    missReqArb.io.out.ready := false.B
  }

  for (w <- 0 until LoadPipelineWidth) { ldu(w).io.mq_enq_cancel := missQueue.io.mq_enq_cancel }

  XSPerfAccumulate("miss_queue_fire", PopCount(VecInit(missReqArb.io.in.map(_.fire))) >= 1.U)
  XSPerfAccumulate("miss_queue_muti_fire", PopCount(VecInit(missReqArb.io.in.map(_.fire))) > 1.U)

  XSPerfAccumulate("miss_queue_has_enq_req", PopCount(VecInit(missReqArb.io.in.map(_.valid))) >= 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_req", PopCount(VecInit(missReqArb.io.in.map(_.valid))) > 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_but_not_fire", PopCount(VecInit(missReqArb.io.in.map(_.valid))) > 1.U && PopCount(VecInit(missReqArb.io.in.map(_.fire))) === 0.U)

  // forward missqueue
  (0 until LoadPipelineWidth).map(i => io.lsu.forward_mshr(i).connect(missQueue.io.forward(i)))

  // refill to load queue
  io.lsu.lsq <> missQueue.io.refill_to_ldq

  // tilelink stuff
  bus.a <> missQueue.io.mem_acquire
  bus.e <> missQueue.io.mem_finish
  missQueue.io.probe_addr := bus.b.bits.address

  missQueue.io.main_pipe_resp := RegNext(mainPipe.io.atomic_resp)

  //----------------------------------------
  // probe
  // probeQueue.io.mem_probe <> bus.b
  block_decoupled(bus.b, probeQueue.io.mem_probe, missQueue.io.probe_block)
  probeQueue.io.lrsc_locked_block <> mainPipe.io.lrsc_locked_block
  probeQueue.io.update_resv_set <> mainPipe.io.update_resv_set

  //----------------------------------------
  // mainPipe
  // when a req enters main pipe, if it is set-conflict with replace pipe or refill pipe,
  // block the req in main pipe
  block_decoupled(probeQueue.io.pipe_req, mainPipe.io.probe_req, missQueue.io.refill_pipe_req.valid)
  block_decoupled(io.lsu.store.req, mainPipe.io.store_req, refillPipe.io.req.valid)

  io.lsu.store.replay_resp := RegNext(mainPipe.io.store_replay_resp)
  io.lsu.store.main_pipe_hit_resp := mainPipe.io.store_hit_resp

  arbiter_with_pipereg(
    in = Seq(missQueue.io.main_pipe_req, io.lsu.atomics.req),
    out = mainPipe.io.atomic_req,
    name = Some("main_pipe_atomic_req")
  )

  mainPipe.io.invalid_resv_set := RegNext(wb.io.req.fire && wb.io.req.bits.addr === mainPipe.io.lrsc_locked_block.bits)

  //----------------------------------------
  // replace (main pipe)
  val mpStatus = mainPipe.io.status
  mainPipe.io.replace_req <> missQueue.io.replace_pipe_req
  missQueue.io.replace_pipe_resp := mainPipe.io.replace_resp

  //----------------------------------------
  // refill pipe
  val refillShouldBeBlocked = (mpStatus.s1.valid && mpStatus.s1.bits.set === missQueue.io.refill_pipe_req.bits.idx) ||
    Cat(Seq(mpStatus.s2, mpStatus.s3).map(s =>
      s.valid &&
        s.bits.set === missQueue.io.refill_pipe_req.bits.idx &&
        s.bits.way_en === missQueue.io.refill_pipe_req.bits.way_en
    )).orR
  block_decoupled(missQueue.io.refill_pipe_req, refillPipe.io.req, refillShouldBeBlocked)

  val mpStatus_dup = mainPipe.io.status_dup
  val mq_refill_dup = missQueue.io.refill_pipe_req_dup
  val refillShouldBeBlocked_dup = VecInit((0 until nDupStatus).map { case i =>
    mpStatus_dup(i).s1.valid && mpStatus_dup(i).s1.bits.set === mq_refill_dup(i).bits.idx ||
    Cat(Seq(mpStatus_dup(i).s2, mpStatus_dup(i).s3).map(s =>
      s.valid &&
        s.bits.set === mq_refill_dup(i).bits.idx &&
        s.bits.way_en === mq_refill_dup(i).bits.way_en
    )).orR
  })
  dontTouch(refillShouldBeBlocked_dup)

  refillPipe.io.req_dup_for_data_w.zipWithIndex.foreach { case (r, i) =>
    r.bits := (mq_refill_dup.drop(dataWritePort).take(DCacheBanks))(i).bits
  }
  refillPipe.io.req_dup_for_meta_w.bits := mq_refill_dup(metaWritePort).bits
  refillPipe.io.req_dup_for_tag_w.bits := mq_refill_dup(tagWritePort).bits
  refillPipe.io.req_dup_for_err_w.bits := mq_refill_dup(errWritePort).bits
  refillPipe.io.req_dup_for_data_w.zipWithIndex.foreach { case (r, i) =>
    r.valid := (mq_refill_dup.drop(dataWritePort).take(DCacheBanks))(i).valid &&
      !(refillShouldBeBlocked_dup.drop(dataWritePort).take(DCacheBanks))(i)
  }
  refillPipe.io.req_dup_for_meta_w.valid := mq_refill_dup(metaWritePort).valid && !refillShouldBeBlocked_dup(metaWritePort)
  refillPipe.io.req_dup_for_tag_w.valid := mq_refill_dup(tagWritePort).valid && !refillShouldBeBlocked_dup(tagWritePort)
  refillPipe.io.req_dup_for_err_w.valid := mq_refill_dup(errWritePort).valid && !refillShouldBeBlocked_dup(errWritePort)

  val refillPipe_io_req_valid_dup = VecInit(mq_refill_dup.zip(refillShouldBeBlocked_dup).map(
    x => x._1.valid && !x._2
  ))
  val refillPipe_io_data_write_valid_dup = VecInit(refillPipe_io_req_valid_dup.slice(0, nDupDataWriteReady))
  val refillPipe_io_tag_write_valid_dup = VecInit(refillPipe_io_req_valid_dup.slice(nDupDataWriteReady, nDupStatus))
  dontTouch(refillPipe_io_req_valid_dup)
  dontTouch(refillPipe_io_data_write_valid_dup)
  dontTouch(refillPipe_io_tag_write_valid_dup)
  mainPipe.io.data_write_ready_dup := VecInit(refillPipe_io_data_write_valid_dup.map(v => !v))
  mainPipe.io.tag_write_ready_dup := VecInit(refillPipe_io_tag_write_valid_dup.map(v => !v))
  mainPipe.io.wb_ready_dup := wb.io.req_ready_dup

  mq_refill_dup.zip(refillShouldBeBlocked_dup).foreach { case (r, block) =>
    r.ready := refillPipe.io.req.ready && !block
  }

  missQueue.io.refill_pipe_resp := refillPipe.io.resp
  io.lsu.store.refill_hit_resp := RegNext(refillPipe.io.store_resp)

  //----------------------------------------
  // wb
  // add a queue between MainPipe and WritebackUnit to reduce MainPipe stalls due to WritebackUnit busy

  wb.io.req <> mainPipe.io.wb
  bus.c     <> wb.io.mem_release
  wb.io.release_wakeup := refillPipe.io.release_wakeup
  wb.io.release_update := mainPipe.io.release_update
  wb.io.probe_ttob_check_req <> mainPipe.io.probe_ttob_check_req
  wb.io.probe_ttob_check_resp <> mainPipe.io.probe_ttob_check_resp

  io.lsu.release.valid := RegNext(wb.io.req.fire)
  io.lsu.release.bits.paddr := RegNext(wb.io.req.bits.addr)
  // Note: RegNext() is required by:
  // * load queue released flag update logic
  // * load / load violation check logic
  // * and timing requirements
  // CHANGE IT WITH CARE

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
    assert (!bus.d.fire)
  }

  //----------------------------------------
  // Feedback Direct Prefetch Monitor
  fdpMonitor.io.refill := missQueue.io.prefetch_info.fdp.prefetch_monitor_cnt
  fdpMonitor.io.timely.late_prefetch := missQueue.io.prefetch_info.fdp.late_miss_prefetch
  fdpMonitor.io.accuracy.total_prefetch := missQueue.io.prefetch_info.fdp.total_prefetch
  for (w <- 0 until LoadPipelineWidth)  {
    if(w == 0) {
      fdpMonitor.io.accuracy.useful_prefetch(w) := ldu(w).io.prefetch_info.fdp.useful_prefetch
    }else {
      fdpMonitor.io.accuracy.useful_prefetch(w) := Mux(same_cycle_update_pf_flag, false.B, ldu(w).io.prefetch_info.fdp.useful_prefetch)
    }
  }
  for (w <- 0 until LoadPipelineWidth)  { fdpMonitor.io.pollution.cache_pollution(w) :=  ldu(w).io.prefetch_info.fdp.pollution }
  for (w <- 0 until LoadPipelineWidth)  { fdpMonitor.io.pollution.demand_miss(w) :=  ldu(w).io.prefetch_info.fdp.demand_miss }
  fdpMonitor.io.debugRolling := io.debugRolling

  //----------------------------------------
  // Bloom Filter
  bloomFilter.io.set <> missQueue.io.bloom_filter_query.set
  bloomFilter.io.clr <> missQueue.io.bloom_filter_query.clr

  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.query(w) <> ldu(w).io.bloom_filter_query.query }
  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.resp(w) <> ldu(w).io.bloom_filter_query.resp }

  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.ld_in(w) <> ldu(w).io.counter_filter_enq }
  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.query(w) <> ldu(w).io.counter_filter_query }

  //----------------------------------------
  // replacement algorithm
  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)
  val replWayReqs = ldu.map(_.io.replace_way) ++ Seq(mainPipe.io.replace_way) ++ stu.map(_.io.replace_way)

  val victimList = VictimList(nSets)
  if (dwpuParam.enCfPred) {
    when(missQueue.io.replace_pipe_req.valid) {
      victimList.replace(get_idx(missQueue.io.replace_pipe_req.bits.vaddr))
    }
    replWayReqs.foreach {
      case req =>
        req.way := DontCare
        when(req.set.valid) {
          when(victimList.whether_sa(req.set.bits)) {
            req.way := replacer.way(req.set.bits)
          }.otherwise {
            req.way := req.dmWay
          }
        }
    }
  } else {
    replWayReqs.foreach {
      case req =>
        req.way := DontCare
        when(req.set.valid) {
          req.way := replacer.way(req.set.bits)
        }
    }
  }

  val replAccessReqs = ldu.map(_.io.replace_access) ++ Seq(
    mainPipe.io.replace_access
  ) ++ stu.map(_.io.replace_access)
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
  when (bus.a.fire) {
    assert(bus.a.bits.address >= 0x80000000L.U)
  }
  when (bus.b.fire) {
    assert(bus.b.bits.address >= 0x80000000L.U)
  }
  when (bus.c.fire) {
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
  // dup cacheOp_req_valid
  bankedDataArray.io.cacheOp_req_dup.zipWithIndex.map{ case(dup, i) => dup := cacheOpDecoder.io.cache_req_dup(i) }
  // dup cacheOp_req_bits_opCode
  bankedDataArray.io.cacheOp_req_bits_opCode_dup.zipWithIndex.map{ case (dup, i) => dup := cacheOpDecoder.io.cacheOp_req_bits_opCode_dup(i) }

  tagArray.io.cacheOp.req := cacheOpDecoder.io.cache.req
  // dup cacheOp_req_valid
  tagArray.io.cacheOp_req_dup.zipWithIndex.map{ case(dup, i) => dup := cacheOpDecoder.io.cache_req_dup(i) }
  // dup cacheOp_req_bits_opCode
  tagArray.io.cacheOp_req_bits_opCode_dup.zipWithIndex.map{ case (dup, i) => dup := cacheOpDecoder.io.cacheOp_req_bits_opCode_dup(i) }

  cacheOpDecoder.io.cache.resp.valid := bankedDataArray.io.cacheOp.resp.valid ||
    tagArray.io.cacheOp.resp.valid
  cacheOpDecoder.io.cache.resp.bits := Mux1H(List(
    bankedDataArray.io.cacheOp.resp.valid -> bankedDataArray.io.cacheOp.resp.bits,
    tagArray.io.cacheOp.resp.valid -> tagArray.io.cacheOp.resp.bits,
  ))
  cacheOpDecoder.io.error := io.error
  assert(!((bankedDataArray.io.cacheOp.resp.valid +& tagArray.io.cacheOp.resp.valid) > 1.U))

  //----------------------------------------
  // performance counters
  val num_loads = PopCount(ldu.map(e => e.io.lsu.req.fire))
  XSPerfAccumulate("num_loads", num_loads)

  io.mshrFull := missQueue.io.full

  // performance counter
  val ld_access = Wire(Vec(LoadPipelineWidth, missQueue.io.debug_early_replace.last.cloneType))
  val st_access = Wire(ld_access.last.cloneType)
  ld_access.zip(ldu).foreach {
    case (a, u) =>
      a.valid := RegNext(u.io.lsu.req.fire) && !u.io.lsu.s1_kill
      a.bits.idx := RegNext(get_idx(u.io.lsu.req.bits.vaddr))
      a.bits.tag := get_tag(u.io.lsu.s1_paddr_dup_dcache)
  }
  st_access.valid := RegNext(mainPipe.io.store_req.fire)
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

  val perfEvents = (Seq(wb, mainPipe, missQueue, probeQueue) ++ ldu).flatMap(_.getPerfEvents)
  generatePerfEvent()
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
  override def shouldBeInlined: Boolean = false

  val useDcache = coreParams.dcacheParametersOpt.nonEmpty
  val clientNode = if (useDcache) TLIdentityNode() else null
  val dcache = if (useDcache) LazyModule(new DCache()) else null
  if (useDcache) {
    clientNode := dcache.clientNode
  }

  class DCacheWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val io = IO(new DCacheIO)
    val perfEvents = if (!useDcache) {
      // a fake dcache which uses dpi-c to access memory, only for debug usage!
      val fake_dcache = Module(new FakeDCache())
      io <> fake_dcache.io
      Seq()
    }
    else {
      io <> dcache.module.io
      dcache.module.getPerfEvents
    }
    generatePerfEvent()
  }

  lazy val module = new DCacheWrapperImp(this)
}
