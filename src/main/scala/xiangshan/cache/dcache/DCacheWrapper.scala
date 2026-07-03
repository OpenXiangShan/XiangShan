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
import xscache.coupledL2.{IsKeywordKey, IsKeywordField, MemBackTypeMMField, MemPageTypeNCField, PCField, VaddrField}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import xscache.common.{AliasField, PrefetchField}
import org.chipsalliance.cde.config.Parameters
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rob.{RobDebugRollingIO, RobPtr}
import xiangshan.cache.wpu._
import xiangshan.mem.prefetch._
import xiangshan.mem.Bundles.SbufferForwardReq
import xiangshan.mem.{AddPipelineReg, HasL1PrefetchSourceParameter, HasMemBlockParameters, LqPtr, MemorySize}
import freechips.rocketchip.tilelink.TLMessages.GrantData
import xiangshan.mem.L1PrefetchReq

// DCache specific parameters
case class DCacheParameters
(
  nSets: Int = 128,
  nWays: Int = 8,
  rowBits: Int = 16,
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
  alwaysReleaseData: Boolean = false,
  isKeywordBitsOpt: Option[Boolean] = Some(true),
  enableDataEcc: Boolean = false,
  enableTagEcc: Boolean = false,
  cacheCtrlAddressOpt: Option[AddressSet] = None,
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


// TODO: do we really need so many traits?
trait HasDCacheParameters
  extends HasMemBlockParameters
  with HasL1PrefetchSourceParameter
  with HasL1CacheParameters {
  val cacheParams = dcacheParameters
  val cfg = cacheParams
  def l2ClientPcBitsOpt: Option[Int] = p(XSCoreParamsKey).L2CacheParamsOpt
    .flatMap(_.clientCaches.find(_.name == "dcache"))
    .flatMap(_.pcBitOpt)

  def GenLatencyArray: Boolean = hasBerti

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
  def TIMESTAMP_WIDTH = 16
  def LATENCY_WIDTH = 16 // FIXME lyq: here should be 12, test for 16

  // each source use a id to distinguish its multiple reqs
  def reqIdWidth = log2Up(nEntries) max log2Up(StoreBufferSize)

  require(isPow2(cfg.nMissEntries)) // TODO
  // require(isPow2(cfg.nReleaseEntries))
  require(cfg.nMissEntries < cfg.nReleaseEntries)
  val nEntries = cfg.nMissEntries + cfg.nReleaseEntries + 1 // nMissEntries + nReleaseEntries + 1CMO_Entry
  val releaseIdBase = cfg.nMissEntries + 1
  val EnableDataEcc = cacheParams.enableDataEcc
  val EnableTagEcc = cacheParams.enableTagEcc

  // banked dcache support
  val DCacheSetDiv = 1
  val DCacheSets = cacheParams.nSets
  val DCacheWayDiv = 2
  val DCacheWays = cacheParams.nWays
  val DCacheBanks = 32
  val DCacheDupNum = 16
  val DCacheSRAMRealRowBits = DCacheSRAMRowBits * DCacheWays // 1 real Bank = vitural_bank * way_nums
  val DCacheSRAMRowBits = 16 
  val DCacheWordBits = 64 // hardcoded
  val DCacheWordBytes = DCacheWordBits / 8
  val MaxPrefetchEntry = cacheParams.nMaxPrefetchEntry
  def DCacheVWordBytes = VLEN / 8

  val DCacheSetDivBits = log2Ceil(DCacheSetDiv)
  val DCacheSetBits = log2Ceil(DCacheSets)
  val DCacheSizeBits = DCacheSRAMRowBits * DCacheBanks * DCacheWays * DCacheSets
  val DCacheSizeBytes = DCacheSizeBits / 8
  val DCacheSizeWords = DCacheSizeBits / 64 // TODO

  val DCacheSameVPAddrLength = 12

  val DCacheSRAMRowBytes = DCacheSRAMRowBits / 8
  val DCacheWordBankCount = DCacheWordBytes / DCacheSRAMRowBytes
  val DCacheVWordBankCount = VLEN / DCacheSRAMRowBits
  val DCacheQuadWordBankCount = QuadWordBytes / DCacheSRAMRowBytes
  val DCacheWordOffset = log2Up(DCacheWordBytes)
  def DCacheVWordOffset = log2Up(DCacheVWordBytes)

  val DCacheBankOffset = log2Up(DCacheSRAMRowBytes)
  val DCacheSetOffset = DCacheBankOffset + log2Up(DCacheBanks)
  val DCacheAboveIndexOffset = DCacheSetOffset + log2Up(DCacheSets)
  val DCacheTagOffset = DCacheAboveIndexOffset min DCacheSameVPAddrLength
  val DCacheLineOffset = DCacheSetOffset

  def encWordBits = cacheParams.dataCode.width(wordBits)
  def encRowBits  = encWordBits * rowWords // for DuplicatedDataArray only
  def eccBits     = encWordBits - wordBits

  def encTagBits = if (EnableTagEcc) cacheParams.tagCode.width(tagBits) else tagBits
  def tagECCBits = encTagBits - tagBits

  def encDataBits = if (EnableDataEcc) cacheParams.dataCode.width(DCacheSRAMRowBits) else DCacheSRAMRowBits
  def dataECCBits = encDataBits - DCacheSRAMRowBits
  def pseudoErrorMaskBits = ((tagBits + 7) / 8) * 8

  // L1 DCache controller
  val cacheCtrlParamsOpt  = OptionWrapper(
                              cacheParams.cacheCtrlAddressOpt.nonEmpty,
                              L1CacheCtrlParams(
                                address = cacheParams.cacheCtrlAddressOpt.get,
                                tagMaskRegWidth = pseudoErrorMaskBits,
                                dataMaskRegWidth = DCacheSRAMRowBits
                              )
                            )
  // uncache
  val uncacheIdxBits = log2Up(VirtualLoadQueueMaxStoreQueueSize + 1)
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

  def setToDcacheDiv(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    if (DCacheSetDivBits == 0) 0.U else set(DCacheSetDivBits-1, 0)
  }

  def setToDcacheDivSet(set: UInt) = {
    require(set.getWidth >= DCacheSetBits)
    set(DCacheSetBits - 1, DCacheSetDivBits)
  }

  def addrToDcacheBank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset-1, DCacheBankOffset)
  }

  def addrToDcacheDiv(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    if(DCacheSetDivBits == 0) 0.U else addr(DCacheSetOffset + DCacheSetDivBits - 1, DCacheSetOffset)
  }

  def addrToDcacheDivSet(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    modeId match {
      case 1 => Cat(
                 hashBitPairs(addr, PAddrBits - 1, pgIdxBits),
                 addr(DCacheAboveIndexOffset- 1 - (untagBits-pgUntagBits), DCacheSetOffset + DCacheSetDivBits)
                )(idxBits - DCacheSetDivBits - 1, 0)
      case 2 => addr(DCacheAboveIndexOffset - 1, DCacheSetOffset + DCacheSetDivBits)
      case _ => throw new IllegalArgumentException(s"Invalid L1DCache index modeId: $modeId")
    }
  }

  def addrToDcacheSet(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    modeId match {
      case 1 => Cat(
                 hashBitPairs(addr, PAddrBits - 1, pgIdxBits),
                 addr(DCacheAboveIndexOffset- 1 - (untagBits-pgUntagBits), DCacheSetOffset)
                )(DCacheAboveIndexOffset - DCacheSetOffset - 1, 0)
      case 2 => addr(DCacheAboveIndexOffset - 1, DCacheSetOffset)
      case _ => throw new IllegalArgumentException(s"Invalid L1DCache index modeId: $modeId")
    }
  }

  def getDataOfBank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def getMaskOfBank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank+1)*DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }

  def getAlias(vaddr: UInt, modeId: Int = modeId): UInt ={
    // require(blockOffBits + idxBits > pgIdxBits)
    if(blockOffBits + idxBits > pgIdxBits){
      modeId match {
        case 1 => hashBitPairs(vaddr, PAddrBits - 1, pgIdxBits)(blockOffBits + idxBits - pgIdxBits - 1, 0)
        case 2 => vaddr(blockOffBits + idxBits - 1, pgIdxBits)
        case _ => throw new IllegalArgumentException(s"Invalid L1DCache alias modeId: $modeId")
      }
    }else{
      0.U
    }
  }

  def isAliasMatch(vaddr0: UInt, vaddr1: UInt): Bool = {
    require(vaddr0.getWidth == VAddrBits && vaddr1.getWidth == VAddrBits)
    if(blockOffBits + idxBits > pgIdxBits) {
      getAlias(vaddr0, modeId) === getAlias(vaddr1, modeId)
    }else {
      // no alias problem
      true.B
    }
  }

  def getDirectMapWay(addr:UInt): UInt = {
    addr(DCacheAboveIndexOffset + log2Up(DCacheWays) - 1, DCacheAboveIndexOffset)
  }

  def bankMaskFromBase(baseBank: UInt, bankCount: Int): UInt = {
    val baseOH = UIntToOH(baseBank, DCacheBanks)
    (0 until bankCount).map(i => (baseOH << i)(DCacheBanks - 1, 0)).reduce(_ | _)
  }

  def byteMaskToBankMask(vaddr: UInt, byteMask: UInt): UInt = {
    val bankMaskInVWord = VecInit((0 until DCacheVWordBankCount).map(i => {
      byteMask(DCacheSRAMRowBytes * (i + 1) - 1, DCacheSRAMRowBytes * i).orR
    })).asUInt
    val bankOffsetInLine = Cat(vaddr(DCacheLineOffset - 1, DCacheVWordOffset), 0.U(log2Ceil(DCacheVWordBankCount).W))
    val bankMaskInLine = Cat(0.U((DCacheBanks - DCacheVWordBankCount).W), bankMaskInVWord)
    (bankMaskInLine << bankOffsetInLine)(DCacheBanks - 1, 0)
  }
  def addrToVWordBankBase(addr: UInt): UInt = {
    val bank = addrToDcacheBank(addr)
    val vwordBankOffsetBits = log2Ceil(DCacheVWordBankCount)
    Cat(bank(log2Up(DCacheBanks) - 1, vwordBankOffsetBits), 0.U(vwordBankOffsetBits.W))
  }

  def bankMaskToReadErrorLaneMask(bankMask: UInt, vwordBankBase: UInt): UInt = {
    VecInit((0 until DCacheVWordBankCount).map { i =>
      val bank = (vwordBankBase + i.U)(log2Up(DCacheBanks) - 1, 0)
      bankMask(bank)
    }).asUInt
  }

  def wordBankBase(wordIdx: UInt): UInt = {
    (wordIdx << log2Ceil(DCacheWordBankCount))(log2Up(DCacheBanks) - 1, 0)
  }

  def quadWordBankBase(quadWordIdx: UInt): UInt = {
    (quadWordIdx << log2Ceil(DCacheQuadWordBankCount))(log2Up(DCacheBanks) - 1, 0)
  }

  def assembleBankData(data: Vec[UInt], baseBank: UInt, bankCount: Int): UInt = {
    Cat((0 until bankCount).reverse.map(i => data((baseBank + i.U)(log2Up(DCacheBanks) - 1, 0))))
  }

  def selectDataPiece(data: UInt, sel: Seq[Bool], bankCount: Int): UInt = {
    Mux1H((0 until bankCount).map(i => sel(i) -> data(DCacheSRAMRowBits * (i + 1) - 1, DCacheSRAMRowBits * i)))
  }

  def selectMaskPiece(mask: UInt, sel: Seq[Bool], bankCount: Int): UInt = {
    Mux1H((0 until bankCount).map(i => sel(i) -> mask(DCacheSRAMRowBytes * (i + 1) - 1, DCacheSRAMRowBytes * i)))
  }

  def selectFullMask(sel: Seq[Bool]): UInt = {
    Mux(sel.reduce(_ || _), ~0.U(DCacheSRAMRowBytes.W), 0.U(DCacheSRAMRowBytes.W))
  }
  val numReplaceRespPorts = 2

  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
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
  val error = new TLError() // cache line has been marked as denieded/corrupted by
  val prefetch = UInt(L1PfSourceBits.W) // cache line is first required by prefetch
  val access = Bool() // cache line has been accessed by load / store
  val latency = UInt(LATENCY_WIDTH.W)

  // val debug_access_timestamp = UInt(64.W) // last time a load / store / refill access that cacheline
}

// memory request in word granularity(load, mmio, lr/sc, atomics)
class DCacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  /**
    * TODO:
    * remove data, mask, id, either cmd or instrtype
    */
  val cmd    = UInt(M_SZ.W)
  val vaddr  = UInt(VAddrBits.W)
  val vaddrDup = UInt(VAddrBits.W)
  val data   = UInt(VLEN.W)
  val mask   = UInt((VLEN/8).W)
  val id     = UInt(reqIdWidth.W)
  val instrtype   = UInt(sourceTypeWidth.W)
  val isFirstIssue = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val lqIdx = new LqPtr

  val debugRobIdx = UInt(log2Ceil(RobSize).W)
  def dump(cond: Bool) = {
    XSDebug(cond, "DCacheWordReq: cmd: %x vaddr: %x data: %x mask: %x id: %d\n",
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
  def dump(cond: Bool) = {
    XSDebug(cond, "DCacheLineReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
  def idx: UInt = get_dcache_idx(vaddr)
}

class DCacheWordReqWithVaddr(implicit p: Parameters) extends DCacheWordReq {
  val addr = UInt(PAddrBits.W)
  val wline = Bool()
}

class DCacheWordReqWithVaddrAndPfFlag(implicit p: Parameters) extends DCacheWordReqWithVaddr {
  val prefetch = Bool()
  val vecValid = Bool()
  val sqNeedDeq = Bool()

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
    res.debugRobIdx := debugRobIdx

    res
  }
}

class BaseDCacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  // read in s2
  val data = UInt(VLEN.W)
  // select in s3
  val dataDelayed = UInt(VLEN.W)
  val id     = UInt(reqIdWidth.W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache miss, and failed to enter the missqueue, replay from RS is needed
  val replay = Bool()
  val replayCarry = new ReplayCarry(nWays)
  // data has been corrupted
  val tagError = Bool() // tag error
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)

  val debugRobIdx = UInt(log2Ceil(RobSize).W)
  def dump(cond: Bool) = {
    XSDebug(cond, "DCacheWordResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class DCacheWordResp(implicit p: Parameters) extends BaseDCacheWordResp
{
  // TODO: Signals from different stages should not be in the same bundle
  val metaPrefetch = UInt(L1PfSourceBits.W)
  val metaAccess = Bool()
  val refillLatency = UInt(LATENCY_WIDTH.W)
  // s2
  val handled = Bool()
  val realMiss = Bool()
  // s3: 1 cycle after data resp
  val errorDelayed = Bool() // all kinds of errors, include tag error
  val tlErrorDelayed = new TLError()
  val replacementUpdated = Bool()
}

class BankedDCacheWordResp(implicit p: Parameters) extends DCacheWordResp
{
  val bankData = Vec(DCacheBanks, Bits(DCacheSRAMRowBits.W))
  val bankOh = UInt(DCacheBanks.W)
}

class DCacheWordRespWithError(implicit p: Parameters) extends BaseDCacheWordResp
{
  val error = Bool() // all kinds of errors, include tag error
  val nderr = Bool()
}

class DCacheLineResp(implicit p: Parameters) extends DCacheBundle
{
  val data   = UInt((cfg.blockBytes * 8).W)
  // cache req missed, send it to miss queue
  val miss   = Bool()
  // cache req nacked, replay it later
  val replay = Bool()
  val id     = UInt(reqIdWidth.W)
  def dump(cond: Bool) = {
    XSDebug(cond, "DCacheLineResp: data: %x id: %d miss: %b replay: %b\n",
      data, id, miss, replay)
  }
}

class Refill(implicit p: Parameters) extends DCacheBundle
{
  val addr   = UInt(PAddrBits.W)
  val data   = UInt(l1BusDataWidth.W)
  val error  = Bool() // refilled data has been corrupted
  // for debug usage
  val dataRaw = UInt((cfg.blockBytes * 8).W)
  val hasdata = Bool()
  val refillDone = Bool()
  def dump(cond: Bool) = {
    XSDebug(cond, "Refill: addr: %x data: %x\n", addr, data)
  }
  val id     = UInt(log2Up(cfg.nMissEntries).W)
}

class Release(implicit p: Parameters) extends DCacheBundle
{
  val paddr  = UInt(PAddrBits.W)
  def dump(cond: Bool) = {
    XSDebug(cond, "Release: paddr: %x\n", paddr(PAddrBits-1, DCacheTagOffset))
  }
}

class DCacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheWordReq)
  val resp = Flipped(DecoupledIO(new DCacheWordResp))
}


class UncacheWordReq(implicit p: Parameters) extends DCacheBundle
{
  val robIdx = new RobPtr
  val cmd  = UInt(M_SZ.W)
  val addr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W) // for uncache buffer forwarding
  val data = UInt(XLEN.W)
  val mask = UInt((XLEN/8).W)
  val id   = UInt(uncacheIdxBits.W)
  val nc = Bool()
  val memBackTypeMM = Bool()

  def dump(cond: Bool) = {
    XSDebug(cond, "UncacheWordReq: cmd: %x addr: %x data: %x mask: %x id: %d\n",
      cmd, addr, data, mask, id)
  }
}

class UncacheIdResp(implicit p: Parameters) extends DCacheBundle {
  val mid = UInt(uncacheIdxBits.W)
  val sid = UInt(UncacheBufferIndexWidth.W)
  val is2lq = Bool()
  val nc = Bool()
}

class UncacheWordResp(implicit p: Parameters) extends DCacheBundle
{
  val data      = UInt(XLEN.W)
  val dataDelayed = UInt(XLEN.W)
  val id        = UInt(UncacheBufferIndexWidth.W) // resp identified signals
  val nc        = Bool() // resp identified signals
  val is2lq     = Bool() // resp identified signals
  val miss      = Bool()
  val replay    = Bool()
  val tagError = Bool()
  val error     = Bool()
  val nderr     = Bool()
  val denied    = Bool()
  val corrupt   = Bool()
  val replayCarry = new ReplayCarry(nWays)
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)  // FIXME: why uncacheWordResp is not merged to baseDcacheResp

  val debugRobIdx = UInt(log2Ceil(RobSize).W)
  def dump(cond: Bool) = {
    XSDebug(cond, "UncacheWordResp: data: %x id: %d miss: %b replay: %b, tag_error: %b, error: %b\n",
      data, id, miss, replay, tagError, error)
  }
}

class UncacheWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new UncacheWordReq)
  val idResp = Flipped(ValidIO(new UncacheIdResp))
  val resp = Flipped(DecoupledIO(new UncacheWordResp))
}

class MainPipeResp(implicit p: Parameters) extends DCacheBundle {
  //distinguish amo
  val source  = UInt(sourceTypeWidth.W)
  val data    = UInt(QuadWordBits.W)
  val miss    = Bool()
  val missId = UInt(log2Up(cfg.nMissEntries).W)
  val replay  = Bool()
  val error   = Bool()
  val tlError = new TLError()

  val ackMissQueue = Bool()

  val id     = UInt(reqIdWidth.W)

  def isAMO: Bool = source === AMO_SOURCE.U
  def isStore: Bool = source === STORE_SOURCE.U
}

class AtomicWordIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new MainPipeReq)
  val resp = Flipped(ValidIO(new MainPipeResp))
  val blockLr = Input(Bool())
}

class CMOReq(implicit p: Parameters) extends Bundle {
  val opcode = UInt(3.W)   // 0-cbo.clean, 1-cbo.flush, 2-cbo.inval, 3-cbo.zero
  val address = UInt(64.W)
}

class CMOResp(implicit p: Parameters) extends Bundle {
  val address = UInt(64.W)
  val nderr   = Bool()
  val denied  = Bool()
  val corrupt = Bool()
}

// used by load unit
class DCacheLoadIO(implicit p: Parameters) extends DCacheWordIO
{
  // kill previous cycle's req
  val s1_kill           = Output(Bool()) // kill loadpipe req at s1
  val s2_kill           = Output(Bool())
  val s0_pc             = Output(UInt(VAddrBits.W))
  val s1_pc             = Output(UInt(VAddrBits.W))
  val s2_pc             = Output(UInt(VAddrBits.W))
  // cycle 0: load has updated replacement before
  val replacementUpdated = Output(Bool())
  val is128Req = Bool()
  // cycle 0: prefetch source bits
  val pfSource = Output(UInt(L1PfSourceBits.W))
  // cycle0: load microop
 // val s0_uop = Output(new MicroOp)
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
  val debugS1HitWay = Input(UInt(nWays.W))
  val debugS2PredWayNum = Input(UInt(XLEN.W))
  val debugS2DmWayNum = Input(UInt(XLEN.W))
  val debugS2RealWayNum = Input(UInt(XLEN.W))
}

class DCacheLineIO(implicit p: Parameters) extends DCacheBundle
{
  val req  = DecoupledIO(new DCacheLineReq)
  val resp = Flipped(DecoupledIO(new DCacheLineResp))
}

class DCacheToSbufferIO(implicit p: Parameters) extends DCacheBundle {
  // sbuffer will directly send request to dcache main pipe
  val req = Flipped(Decoupled(new DCacheLineReq))

  val mainPipeHitResp = ValidIO(new DCacheLineResp)
  //val refill_hit_resp = ValidIO(new DCacheLineResp)

  val replayResp = ValidIO(new DCacheLineResp)

  //def hit_resps: Seq[ValidIO[DCacheLineResp]] = Seq(main_pipe_hit_resp, refill_hit_resp)
  def hitResps: Seq[ValidIO[DCacheLineResp]] = Seq(mainPipeHitResp)
}

class MissEntryForwardIO(implicit p: Parameters) extends DCacheBundle {
  val inflight = Bool()
  val paddr = UInt(PAddrBits.W)
  val rawData = Vec(blockRows, UInt(rowBits.W))
  val isFromStore = Bool()
  val storeMask = UInt(cfg.blockBytes.W)
  val firstbeatValid = Bool()
  val lastbeatValid = Bool()
  val denied = Bool()
  val corrupt = Bool()

  // check if we can forward from mshr or D channel
  def check(reqValid : Bool, req_paddr : UInt) = {
    RegNext(reqValid && inflight && req_paddr(PAddrBits - 1, blockOffBits) === paddr(PAddrBits - 1, blockOffBits)) // TODO: clock gate(1-bit)
  }

  def forward(reqValid : Bool, req_paddr : UInt) = {
    val allMatch = (req_paddr(log2Up(refillBytes)) === 0.U && firstbeatValid) ||
                    (req_paddr(log2Up(refillBytes)) === 1.U && lastbeatValid)

    val forwardMshr = RegInit(false.B)
    val forwardData = RegInit(VecInit(List.fill(VLEN/8)(0.U(8.W))))

    val blockIdx = req_paddr(log2Up(refillBytes), 3)
    val blockData = rawData

    val selectedData = Wire(UInt(128.W))
    selectedData := Mux(req_paddr(3), Fill(2, blockData(blockIdx)), Cat(blockData(blockIdx + 1.U), blockData(blockIdx)))

    forwardMshr := allMatch
    for (i <- 0 until VLEN/8) {
      forwardData(i) := selectedData(8 * i + 7, 8 * i)
    }

    (forwardMshr, forwardData)
  }
}

class DCacheForwardReqS0(implicit p: Parameters) extends DCacheBundle {
  val vaddr = UInt(VAddrBits.W)
  val size = UInt(MemorySize.Size.width.W)
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)
}

class DCacheForwardReqS1(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
}

class DCacheForwardResp(implicit p: Parameters) extends DCacheBundle {
  val matchInvalid = Bool()
  val forwardData = Vec((VLEN/8), UInt(8.W))
  val forwardMask = Vec((VLEN/8), Bool())
  // denied and corrupt are only valid when forwarding matches
  val denied = Bool()
  val corrupt = Bool()
}

class DCacheForward(implicit p: Parameters) extends DCacheBundle {
  val s0_req = ValidIO(new DCacheForwardReqS0)
  val s1_req = Output(new DCacheForwardReqS1)
  val s1_kill = Output(Bool())
  val s2_resp = Flipped(ValidIO(new DCacheForwardResp))
}

class DCacheLoadWakeup(implicit p: Parameters) extends DCacheBundle {
  val mshrId = UInt(log2Up(cfg.nMissEntries).W)
}

// forward mshr's data to ldu
class LduToMissqueueForwardIO(implicit p: Parameters) extends DCacheBundle {
  // TODO: use separate Bundles for req and resp
  // req
  val valid = Input(Bool())
  val mshrid = Input(UInt(log2Up(cfg.nMissEntries).W))
  val paddr = Input(UInt(PAddrBits.W))
  // resp
  val forwardMshr = Output(Bool())
  val forwardData = Output(Vec(VLEN/8, UInt(8.W)))
  val forwardResultValid = Output(Bool())
  val denied = Output(Bool())
  val corrupt = Output(Bool())

  // Why? What is the purpose of `connect`???
  def connect(sink: LduToMissqueueForwardIO) = {
    sink.valid := valid
    sink.mshrid := mshrid
    sink.paddr := paddr
    forwardMshr := sink.forwardMshr
    forwardData := sink.forwardData
    forwardResultValid := sink.forwardResultValid
    denied := sink.denied
    corrupt := sink.corrupt
  }

  def forward() = {
    (forwardResultValid, forwardMshr, forwardData, denied, corrupt)
  }
}

class StorePrefetchReq(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
  val vaddr = UInt(VAddrBits.W)
}

class DCacheToLsuIO(implicit p: Parameters) extends DCacheBundle {
  val load  = Vec(LoadPipelineWidth, Flipped(new DCacheLoadIO)) // for speculative load
  val sta   = Vec(StorePipelineWidth, Flipped(new DCacheStoreIO)) // for non-blocking store
  val loadWakeup = ValidIO(new DCacheLoadWakeup())
  val store = new DCacheToSbufferIO // for sbuffer
  val atomics  = Flipped(new AtomicWordIO)  // atomics reqs
  val release = ValidIO(new Release) // cacheline release hint for ld-ld violation check
  val forwardD = Flipped(Vec(LoadPipelineWidth, new DCacheForward))
  val forwardMshr = Flipped(Vec(LoadPipelineWidth, new DCacheForward))
  // If a store is miss and accepted by mshr, Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
  val forwardMshrStData = Flipped(Vec(LoadPipelineWidth, new SbufferForwardReq))
}

class DCacheTopDownIO(implicit p: Parameters) extends DCacheBundle {
  val robHeadVaddr = Flipped(Valid(UInt(VAddrBits.W)))
  val robHeadMissInDCache = Output(Bool())
  val robHeadOtherReplay = Input(Bool())
}

class DCacheIO(implicit p: Parameters) extends DCacheBundle {
  val hartId = Input(UInt(hartIdLen.W))
  val l2PfStoreOnly = Input(Bool())
  val lsu = new DCacheToLsuIO
  val error = ValidIO(new L1CacheErrorInfo)
  val mshrFull = Output(Bool())
  val mshrStoreEmpty = Output(Bool())
  val memSetPattenDetected = Output(Bool())
  val lqEmpty = Input(Bool())
  val pfCtrl = Output(Vec(L1PrefetcherNum, new PrefetchControlBundle))
  val refillTrain = ValidIO(new TrainReqBundle)
  val forceWrite = Input(Bool())
  val smsAgtEvictReq = DecoupledIO(new AGTEvictReq)
  val debugTopDown = new DCacheTopDownIO
  val debugRolling = Flipped(new RobDebugRollingIO)
  val l2Hint = Input(Valid(new L2ToL1Hint()))
  val cmoOpReq = Flipped(DecoupledIO(new CMOReq))
  val cmoOpResp = DecoupledIO(new CMOResp)
  val l1Miss = Output(Bool())
  val wfi = Flipped(new WfiReqBundle)
  val prefetchReq = Flipped(DecoupledIO(new L1PrefetchReq))
}

private object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

class TreeArbiter[T <: MissReqWoStoreData](val gen: T, val n: Int) extends Module{
  val io = IO(new ArbiterIO(gen, n))

  def selectTree(in: Vec[Valid[T]], sIdx: UInt): Tuple2[UInt, T] = {
    if (in.length == 1) {
      (sIdx, in(0).bits)
    } else if (in.length == 2) {
      (
        Mux(in(0).valid, sIdx, sIdx + 1.U),
        Mux(in(0).valid, in(0).bits, in(1).bits)
      )
    } else {
      val half = in.length / 2
      val leftValid = in.slice(0, half).map(_.valid).reduce(_ || _)
      val (leftIdx, leftSel) = selectTree(VecInit(in.slice(0, half)), sIdx)
      val (rightIdx, rightSel) = selectTree(VecInit(in.slice(half, in.length)), sIdx + half.U)
      (
        Mux(leftValid, leftIdx, rightIdx),
        Mux(leftValid, leftSel, rightSel)
      )
    }
  }
  val ins = Wire(Vec(n, Valid(gen)))
  for (i <- 0 until n) {
    ins(i).valid := io.in(i).valid
    ins(i).bits  := io.in(i).bits
  }
  val (idx, sel) = selectTree(ins, 0.U)
  // NOTE: io.chosen is very slow, dont use it
  io.chosen := idx
  io.out.bits := sel

  val grant = ArbiterCtrl(io.in.map(_.valid))
  for ((in, g) <- io.in.zip(grant))
    in.ready := g && io.out.ready
  io.out.valid := !grant.last || io.in.last.valid
}

class DCacheMEQueryIOBundle(implicit p: Parameters) extends DCacheBundle
{
  val req              = ValidIO(new MissReqWoStoreData)
  val primaryReady    = Input(Bool())
  val secondaryReady  = Input(Bool())
  val secondaryReject = Input(Bool())
  val blockMatch      = Input(Bool())
}

class DCacheMQQueryIOBundle(implicit p: Parameters) extends DCacheBundle
{
  val req    = ValidIO(new MissReq)
  val ready  = Input(Bool())
}

class MissReadyGen(val n: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(new MissReq)))
    val queryMQ = Vec(n, new DCacheMQQueryIOBundle)
  })

  val mqReadyVec = io.queryMQ.map(_.ready)

  io.queryMQ.zipWithIndex.foreach{
    case (q, idx) => {
      q.req.valid := io.in(idx).valid
      q.req.bits  := io.in(idx).bits
    }
  }
  io.in.zipWithIndex.map {
    case (r, idx) => {
      r.ready := mqReadyVec(idx)
    }
  }
}

class DCache()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  override def shouldBeInlined: Boolean = false

  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField(),
    VaddrField(VAddrBits - blockOffBits),
    MemBackTypeMMField(),
    MemPageTypeNCField()
  //  IsKeywordField()
  ) ++ l2ClientPcBitsOpt.map(PCField(_)).toSeq ++ cacheParams.aliasBitsOpt.map(AliasField)
  val echoFields: Seq[BundleFieldBase] = Seq(
    IsKeywordField()
  )

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
  val cacheCtrlOpt = cacheCtrlParamsOpt.map(params => LazyModule(new CtrlUnit(params)))

  lazy val module = new DCacheImp(this)
}


class DCacheImp(outer: DCache) extends LazyModuleImp(outer) with HasDCacheParameters with HasPerfEvents with
  HasL1PrefetchSourceParameter {

  val io = IO(new DCacheIO)

  val (bus, edge) = outer.clientNode.out.head
  require(pseudoErrorMaskBits >= tagBits, "pseudo-error masks must cover tagBits")
  require(pseudoErrorMaskBits >= DCacheSRAMRowBits, "pseudo-error masks must cover data-bank row width")
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
  println("  HasCMO: " + HasCMO)

  // HybridUnit is no longer supported
  require(backendParams.HyuCnt == 0)
  // Enable L1 Store prefetch
  val StorePrefetchL1Enabled = EnableStorePrefetchAtCommit || EnableStorePrefetchAtIssue || EnableStorePrefetchSPB
  val MetaReadPort =
        if (StorePrefetchL1Enabled)
          1 + backendParams.LduCnt + backendParams.StaCnt
        else
          1 + backendParams.LduCnt
  val TagReadPort =
        if (StorePrefetchL1Enabled)
          1 + backendParams.LduCnt + backendParams.StaCnt
        else
          1 + backendParams.LduCnt

  // Enable L1 Load prefetch
  val LoadPrefetchL1Enabled = true
  val AccessArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1
  val PrefetchArrayReadPort = if(LoadPrefetchL1Enabled) LoadPipelineWidth + 1 + 1 else LoadPipelineWidth + 1

  //----------------------------------------
  // core data structures
  val bankedDataArray = if(dwpuParam.enWPU) Module(new SramedDataArray) else Module(new BankedDataArray)
  val metaArray = Module(new L1CohMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 1))
  val errorArray = Module(new L1ErrorMetaArray(readPorts = LoadPipelineWidth + 1, writePorts = 1, enableBypass = true))
  val prefetchArray = Module(new L1PrefetchSourceArray(
    readPorts = PrefetchArrayReadPort, writePorts = 1 + LoadPipelineWidth
  )) // prefetch flag array
  val latencyArray = Option.when(GenLatencyArray)(Module(new L1RefillLatencyArray(
    readPorts = PrefetchArrayReadPort, writePorts = 1 + LoadPipelineWidth
  )))

  val accessArray = Module(new L1FlagMetaArray(readPorts = AccessArrayReadPort, writePorts = LoadPipelineWidth + 1))
  val tagArray = Module(new DuplicatedTagArray(readPorts = TagReadPort))
  val prefetcherMonitor = Module(new PrefetcherMonitor)
  val bloomFilter =  Module(new BloomFilter(BLOOM_FILTER_ENTRY_NUM, true))
  val counterFilter = Module(new CounterFilter)
  bankedDataArray.dump()

  //----------------------------------------
  // miss queue
  // missReq source:
  // enableStorePrefetch: main pipe * 1 + load pipe * 3 + store pipe * 1
  //             disable: main pipe * 1 + load pipe * 3
  // higher priority is given to lower indices
  val MainPipeMissReqPort = 0

  //----------------------------------------
  // core modules
  val ldu = Seq.tabulate(LoadPipelineWidth)({ i => Module(new LoadPipe(i))})
  val stu = Seq.tabulate(StorePipelineWidth)({ i => Module(new StorePipe(i))})
  val mainPipe     = Module(new MainPipe)
  // val refillPipe   = Module(new RefillPipe)
  val missQueue    = Module(new MissQueue(edge, MissReqPortCount))
  val probeQueue   = Module(new ProbeQueue(edge))
  val wb           = Module(new WritebackQueue(edge))

  missQueue.io.lqEmpty := io.lqEmpty
  missQueue.io.hartId := io.hartId
  missQueue.io.l2PfStoreOnly := RegNext(io.l2PfStoreOnly, false.B)
  missQueue.io.debugTopDown <> io.debugTopDown
  missQueue.io.l2Hint <> RegNext(io.l2Hint)
  missQueue.io.mainpipeInfo := mainPipe.io.mainpipeInfo
  missQueue.io.occupySet.zip(ldu.map(_.io.occupySet)).foreach { case (l, r) => l <> r }
  missQueue.io.occupyFail.zip(ldu.map(_.io.occupyFail)).foreach { case (l, r) => l <> r }
  mainPipe.io.refillInfo := missQueue.io.refillInfo
  mainPipe.io.replace <> missQueue.io.replace
  mainPipe.io.smsAgtEvictReq <> io.smsAgtEvictReq
  io.mshrStoreEmpty := missQueue.io.mshrStoreEmpty
  io.memSetPattenDetected := missQueue.io.memSetPattenDetected
  io.wfi <> missQueue.io.wfi
  io.refillTrain := missQueue.io.refillTrain
  mainPipe.io.prefetchReq <> io.prefetchReq

  // l1 dcache controller
  outer.cacheCtrlOpt.foreach {
    case mod =>
      mod.module.ioPseudoError.foreach {
        case x => x.ready := false.B
      }
  }
  ldu.foreach {
    case mod =>
      mod.io.pseudoError.valid := false.B
      mod.io.pseudoError.bits := DontCare
  }
  mainPipe.io.pseudoError.valid := false.B
  mainPipe.io.pseudoError.bits  := DontCare
  bankedDataArray.io.pseudoError.valid := false.B
  bankedDataArray.io.pseudoError.bits  := DontCare

  // pseudo tag ecc error
  if (outer.cacheCtrlOpt.nonEmpty && EnableTagEcc) {
    val ctrlUnit = outer.cacheCtrlOpt.head.module
    ldu.map(mod => mod.io.pseudoError <> ctrlUnit.ioPseudoError(0))
    mainPipe.io.pseudoError <> ctrlUnit.ioPseudoError(0)
    ctrlUnit.ioPseudoError(0).ready := mainPipe.io.pseudoTagErrorInjDone ||
                                        ldu.map(_.io.pseudoTagErrorInjDone).reduce(_|_)
  }

  // pseudo data ecc error
  if (outer.cacheCtrlOpt.nonEmpty && EnableDataEcc) {
    val ctrlUnit = outer.cacheCtrlOpt.head.module
    bankedDataArray.io.pseudoError <> ctrlUnit.ioPseudoError(1)
    ctrlUnit.ioPseudoError(1).ready := bankedDataArray.io.pseudoError.ready &&
                                        (mainPipe.io.pseudoDataErrorInjDone ||
                                         ldu.map(_.io.pseudoDataErrorInjDone).reduce(_|_))
  }

  val errors = Seq(mainPipe.io.error) ++ // store / misc error
        ldu.map(_.io.error)// load error
  val errorValid = errors.map(e => e.valid).reduce(_|_)
  io.error.bits <> RegEnable(
    ParallelPriorityMux(errors.map(e => RegNext(e.valid) -> RegEnable(e.bits, e.valid))),
    RegNext(errorValid))
  io.error.valid := RegNext(RegNext(errorValid, init = false.B), init = false.B)

  //----------------------------------------
  // meta array
  // read / write coh meta
  val metaReadPorts = ldu.map(_.io.metaRead).take(LoadPipelineWidth) ++
    Seq(mainPipe.io.metaRead) ++
    stu.map(_.io.metaRead).take(LoadPipelineWidth)

  val metaRespPorts = ldu.map(_.io.metaResp).take(LoadPipelineWidth) ++
    Seq(mainPipe.io.metaResp) ++
    stu.map(_.io.metaResp).take(LoadPipelineWidth)

  val metaWritePorts = Seq(
    mainPipe.io.metaWrite
    // refillPipe.io.meta_write
  )
  if(StorePrefetchL1Enabled) {
    metaReadPorts.zip(metaArray.io.read).foreach { case (p, r) => r <> p }
    metaRespPorts.zip(metaArray.io.resp).foreach { case (p, r) => p := r }
  } else {
    (metaReadPorts.take(LoadPipelineWidth + 1)).zip(metaArray.io.read).foreach { case (p, r) => r <> p }
    (metaRespPorts.take(LoadPipelineWidth + 1)).zip(metaArray.io.resp).foreach { case (p, r) => p := r }

    metaReadPorts.drop(LoadPipelineWidth + 1).take(LoadPipelineWidth).foreach { case p => p.ready := false.B }
    metaRespPorts.drop(LoadPipelineWidth + 1).take(LoadPipelineWidth).foreach { case p => p := 0.U.asTypeOf(p) }
  }
  metaWritePorts.zip(metaArray.io.write).foreach { case (p, w) => w <> p }

  // read extra meta (exclude stu)
  (metaReadPorts.take(LoadPipelineWidth + 1)).zip(errorArray.io.read).foreach { case (p, r) => r <> p }
  (metaReadPorts.take(LoadPipelineWidth + 1)).zip(prefetchArray.io.read).foreach { case (p, r) => r <> p }
  (metaReadPorts.take(LoadPipelineWidth + 1)).zip(accessArray.io.read).foreach { case (p, r) => r <> p }
  val extraMetaRespPorts = ldu.map(_.io.extraMetaResp).take(LoadPipelineWidth) ++
    Seq(mainPipe.io.extraMetaResp)
  extraMetaRespPorts.zip(errorArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).error := r(i) })
  }}
  extraMetaRespPorts.zip(prefetchArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).prefetch := r(i) })
  }}
  extraMetaRespPorts.zip(accessArray.io.resp).foreach { case (p, r) => {
    (0 until nWays).map(i => { p(i).access := r(i) })
  }}
  if (GenLatencyArray) {
    (metaReadPorts.take(LoadPipelineWidth + 1)).zip(latencyArray.get.io.read).foreach { case (p, r) => r <> p }
    extraMetaRespPorts.zip(latencyArray.get.io.resp).foreach { case (p, r) => {
      (0 until nWays).map(i => { p(i).latency := r(i) })
    }}
  } else {
    (metaReadPorts.take(LoadPipelineWidth + 1)).foreach { case p => p.ready := true.B}
    extraMetaRespPorts.foreach { case p => {
      (0 until nWays).map(i => { p(i).latency := 0.U })
    }}
  }

  if(LoadPrefetchL1Enabled) {
    // use last port to read prefetch and access flag
//    prefetchArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
//    prefetchArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
//    prefetchArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en
//
//    accessArray.io.read.last.valid := refillPipe.io.prefetch_flag_write.valid
//    accessArray.io.read.last.bits.idx := refillPipe.io.prefetch_flag_write.bits.idx
//    accessArray.io.read.last.bits.way_en := refillPipe.io.prefetch_flag_write.bits.way_en
    prefetchArray.io.read.last.valid := mainPipe.io.prefetchFlagWrite.valid
    prefetchArray.io.read.last.bits.idx := mainPipe.io.prefetchFlagWrite.bits.idx
    prefetchArray.io.read.last.bits.wayEn := mainPipe.io.prefetchFlagWrite.bits.wayEn

    if(GenLatencyArray) {
      latencyArray.get.io.read.last.valid := mainPipe.io.prefetchFlagWrite.valid
      latencyArray.get.io.read.last.bits.idx := mainPipe.io.prefetchFlagWrite.bits.idx
      latencyArray.get.io.read.last.bits.wayEn := mainPipe.io.prefetchFlagWrite.bits.wayEn
    }

    accessArray.io.read.last.valid := mainPipe.io.prefetchFlagWrite.valid
    accessArray.io.read.last.bits.idx := mainPipe.io.prefetchFlagWrite.bits.idx
    accessArray.io.read.last.bits.wayEn := mainPipe.io.prefetchFlagWrite.bits.wayEn

    val extraFlagValid = RegNext(mainPipe.io.prefetchFlagWrite.valid)
    val extraFlagWayEn = RegEnable(
      mainPipe.io.prefetchFlagWrite.bits.wayEn,
      mainPipe.io.prefetchFlagWrite.valid
    )
    val extraFlagPrefetch = Mux1H(extraFlagWayEn, prefetchArray.io.resp.last)
    val extraFlagAccess = Mux1H(extraFlagWayEn, accessArray.io.resp.last)

    prefetcherMonitor.io.replinfo.pf_useless := extraFlagValid && !extraFlagAccess && isFromL1Prefetch(extraFlagPrefetch)
    prefetcherMonitor.io.replinfo.pf_source_useless := extraFlagPrefetch

    prefetcherMonitor.io.replinfo.hit_pf_in_cache := extraFlagValid && extraFlagAccess && isFromL1Prefetch(extraFlagPrefetch)
    prefetcherMonitor.io.replinfo.hit_pf_source_in_cache := extraFlagPrefetch
  }

  // write extra meta
  val errorFlagWritePorts = Seq(
    mainPipe.io.errorFlagWrite // error flag generated by corrupted store
    // refillPipe.io.error_flag_write // corrupted signal from l2
  )
  errorFlagWritePorts.zip(errorArray.io.write).foreach { case (p, w) => w <> p }

  val prefetchFlagWritePorts = ldu.map(_.io.prefetchFlagWrite) ++ Seq(
    mainPipe.io.prefetchFlagWrite // set prefetch_flag to false if coh is set to Nothing
    // refillPipe.io.prefetch_flag_write // refill required by prefetch will set prefetch_flag
  )
  prefetchFlagWritePorts.zip(prefetchArray.io.write).foreach { case (p, w) => w <> p }

  val latencyFlagWritePorts = ldu.map(_.io.latencyFlagWrite) ++ Seq(
    mainPipe.io.latencyFlagWrite
  )
  if (GenLatencyArray) {
    latencyFlagWritePorts.zip(latencyArray.get.io.write).foreach { case (p, w) => w <> p }
  } else {
    latencyFlagWritePorts.foreach { case p => p.ready := true.B }
  }

  val accessFlagWritePorts = ldu.map(_.io.accessFlagWrite) ++ Seq(
    mainPipe.io.accessFlagWrite
    // refillPipe.io.access_flag_write
  )
  accessFlagWritePorts.zip(accessArray.io.write).foreach { case (p, w) => w <> p }

  //----------------------------------------
  // tag array
  if(StorePrefetchL1Enabled) {
    require(tagArray.io.read.size == (LoadPipelineWidth + StorePipelineWidth + 1))
  }else {
    require(tagArray.io.read.size == (LoadPipelineWidth + 1))
  }
  // val tag_write_intend = missQueue.io.refill_pipe_req.valid || mainPipe.io.tag_write_intend
  val tagWriteIntend = mainPipe.io.tagWriteIntend
  assert(!RegNext(!tagWriteIntend && tagArray.io.write.valid))
  ldu.take(LoadPipelineWidth).zipWithIndex.foreach {
    case (ld, i) =>
      tagArray.io.read(i) <> ld.io.tagRead
      ld.io.tagResp := tagArray.io.resp(i)
      ld.io.tagRead.ready := !tagWriteIntend
  }
  if(StorePrefetchL1Enabled) {
    stu.take(LoadPipelineWidth).zipWithIndex.foreach {
      case (st, i) =>
        tagArray.io.read(LoadPipelineWidth + i) <> st.io.tagRead
        st.io.tagResp := tagArray.io.resp(LoadPipelineWidth + i)
        st.io.tagRead.ready := !tagWriteIntend
    }
  }else {
    stu.foreach {
      case st =>
        st.io.tagRead.ready := false.B
        st.io.tagResp := 0.U.asTypeOf(st.io.tagResp)
    }
  }
  tagArray.io.read.last <> mainPipe.io.tagRead
  mainPipe.io.tagResp := tagArray.io.resp.last

  val fakeTagReadConflictThisCycle = PopCount(ldu.map(ld=> ld.io.tagRead.valid))
  XSPerfAccumulate("fake_tag_read_conflict", fakeTagReadConflictThisCycle)

  val tagWriteArb = Module(new Arbiter(new TagWriteReq, 1))
  tagWriteArb.io.in(0) <> mainPipe.io.tagWrite
  tagArray.io.write <> tagWriteArb.io.out

  ldu.map(m => {
    m.io.vtagUpdate.valid := tagArray.io.write.valid
    m.io.vtagUpdate.bits := tagArray.io.write.bits
  })

  //----------------------------------------
  // data array
  mainPipe.io.dataRead.zip(ldu).map(x => x._1 := x._2.io.lsu.req.valid)

  val dataWriteArb = Module(new Arbiter(new L1BankedDataWriteReq, 1))
  // dataWriteArb.io.in(0) <> refillPipe.io.data_write
  dataWriteArb.io.in(0) <> mainPipe.io.dataWrite

  bankedDataArray.io.write <> dataWriteArb.io.out

  for (bank <- 0 until DCacheBanks) {
    val dataWriteArbDup = Module(new Arbiter(new L1BankedDataWriteReqCtrl, 1))
    // dataWriteArb_dup.io.in(0).valid := refillPipe.io.data_write_dup(bank).valid
    // dataWriteArb_dup.io.in(0).bits := refillPipe.io.data_write_dup(bank).bits
    dataWriteArbDup.io.in(0).valid := mainPipe.io.dataWriteDup(bank).valid
    dataWriteArbDup.io.in(0).bits := mainPipe.io.dataWriteDup(bank).bits

    bankedDataArray.io.writeDup(bank) <> dataWriteArbDup.io.out
  }

  bankedDataArray.io.readline <> mainPipe.io.dataReadline
  bankedDataArray.io.readlineCanGo := mainPipe.io.dataReadlineCanGo
  bankedDataArray.io.readlineStall := mainPipe.io.dataReadlineStall
  bankedDataArray.io.readlineCanResp := mainPipe.io.dataReadlineCanResp
  bankedDataArray.io.readlineIntend := mainPipe.io.dataReadIntend
  mainPipe.io.readlineError := bankedDataArray.io.readlineError
  mainPipe.io.readlineErrorDelayed := bankedDataArray.io.readlineErrorDelayed
  mainPipe.io.dataResp := bankedDataArray.io.readlineResp

  (0 until LoadPipelineWidth).map(i => {
    bankedDataArray.io.read(i) <> ldu(i).io.bankedDataRead
    bankedDataArray.io.is128Req(i) <> ldu(i).io.is128Req
    bankedDataArray.io.readErrorDelayed(i) <> ldu(i).io.readErrorDelayed

    ldu(i).io.bankedDataResp := bankedDataArray.io.readResp(i)

    ldu(i).io.bankConflictSlow := bankedDataArray.io.bankConflictSlow(i)
  })

  io.lsu.forwardD.zipWithIndex.foreach { case (forward, i) =>
    val s0_req_valid = forward.s0_req.valid
    val s0_req = forward.s0_req.bits
    val s1_req_valid = RegNext(s0_req_valid)
    val s1_req = RegEnable(s0_req, s0_req_valid)
    val mshrId = s1_req.mshrId
    val paddr = forward.s1_req.paddr

    val (_, _, done, _) = edge.count(bus.d)
    val mshrMatch = mshrId === bus.d.bits.source
    val beatMatch = (bus.d.bits.echo.lift(IsKeywordKey).getOrElse(false.B) ^ done) === paddr(log2Up(refillBytes))
    val paddrMatch = missQueue.io.forwardS1PAddrMatch(i)
    val s1_resp_valid = s1_req_valid && bus.d.valid && bus.d.bits.opcode === TLMessages.GrantData &&
      mshrMatch && beatMatch && paddrMatch
    val s1_resp_forward_data = VecInit.tabulate(l1BusDataWidth / VLEN) { i =>
      bus.d.bits.data((i + 1) * VLEN - 1, i * VLEN)
    }(paddr(log2Up(VLEN / 8)))

    val s2_resp = forward.s2_resp
    s2_resp.valid := RegNext(s1_resp_valid)
    s2_resp.bits.matchInvalid := false.B
    s2_resp.bits.forwardData := RegEnable(s1_resp_forward_data.asTypeOf(s2_resp.bits.forwardData), s1_req_valid)
    s2_resp.bits.forwardMask := VecInit(Seq.fill(VLEN / 8)(RegNext(s1_resp_valid)))
    s2_resp.bits.denied := RegEnable(bus.d.bits.denied, s1_req_valid)
    s2_resp.bits.corrupt := RegEnable(bus.d.bits.corrupt, s1_req_valid)
  }
  // tl D channel wakeup
  io.lsu.loadWakeup.valid := (bus.d.bits.opcode === TLMessages.GrantData || bus.d.bits.opcode === TLMessages.Grant) &&
    bus.d.valid
  io.lsu.loadWakeup.bits.mshrId := bus.d.bits.source
  mainPipe.io.forceWrite <> io.forceWrite

  /** dwpu */
  if (dwpuParam.enWPU) {
    val dwpu = Module(new DCacheWpuWrapper(LoadPipelineWidth))
    for(i <- 0 until LoadPipelineWidth){
      dwpu.io.req(i) <> ldu(i).io.dwpu.req(0)
      dwpu.io.resp(i) <> ldu(i).io.dwpu.resp(0)
      dwpu.io.lookup_upd(i) <> ldu(i).io.dwpu.lookup_upd(0)
      dwpu.io.cfpred(i) <> ldu(i).io.dwpu.cfpred(0)
    }
    dwpu.io.tagwrite_upd.valid := tagArray.io.write.valid
    dwpu.io.tagwrite_upd.bits.vaddr := tagArray.io.write.bits.vaddr
    dwpu.io.tagwrite_upd.bits.s1_real_way_en := tagArray.io.write.bits.wayEn
  } else {
    for(i <- 0 until LoadPipelineWidth){
      ldu(i).io.dwpu.req(0).ready := true.B
      ldu(i).io.dwpu.resp(0).valid := false.B
      ldu(i).io.dwpu.resp(0).bits := DontCare
    }
  }

  //----------------------------------------
  // load pipe
  // the s1 kill signal
  // only lsu uses this, replay never kills
  for (w <- 0 until LoadPipelineWidth) {
    ldu(w).io.lsu <> io.lsu.load(w)

    // TODO:when have load128Req
    ldu(w).io.load128Req := io.lsu.load(w).is128Req

    // replay and nack not needed anymore
    // TODO: remove replay and nack
    ldu(w).io.nack := false.B

    ldu(w).io.disableLdFastWakeup :=
      bankedDataArray.io.disableLdFastWakeup(w) // load pipe fast wake up should be disabled when bank conflict
  }

  val clearFlag = Wire(Vec(LoadPipelineWidth, Bool()))
  clearFlag(0) := false.B
  for (i <- 1 until LoadPipelineWidth) {
    val conflictWithEarlier = (0 until i).map { j =>
      (ldu(i).io.prefetchFlagWrite.bits.idx === ldu(j).io.prefetchFlagWrite.bits.idx) &&
      (ldu(i).io.prefetchFlagWrite.bits.wayEn === ldu(j).io.prefetchFlagWrite.bits.wayEn)
    }.reduce(_ || _)
    clearFlag(i) := conflictWithEarlier
  }

  for (w <- 0 until LoadPipelineWidth) {
    prefetcherMonitor.io.loadinfo(w) := ldu(w).io.prefetchStat
  }
  prefetcherMonitor.io.maininfo := mainPipe.io.prefetchStat
  prefetcherMonitor.io.missinfo := missQueue.io.prefetchStat
  prefetcherMonitor.io.debugRolling := io.debugRolling
  prefetcherMonitor.io.clear_flag := clearFlag
  io.pfCtrl <> prefetcherMonitor.io.pf_ctrl

  /** LoadMissDB: record load miss state */
  val hartId = p(XSCoreParamsKey).HartId
  val isWriteLoadMissTable = Constantin.createRecord(s"isWriteLoadMissTable$hartId")
  val isFirstHitWrite = Constantin.createRecord(s"isFirstHitWrite$hartId")
  val tableName = s"LoadMissDB$hartId"
  val siteName = s"DcacheWrapper$hartId"
  val loadMissTable = ChiselDB.createTable(tableName, new LoadMissEntry)
  for( i <- 0 until LoadPipelineWidth){
    val loadMissEntry = Wire(new LoadMissEntry)
    val loadMissWriteEn =
      (!ldu(i).io.lsu.resp.bits.replay && ldu(i).io.missReq.fire) ||
      (ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid && isFirstHitWrite.orR)
    loadMissEntry.timeCnt := GTimer()
    loadMissEntry.robIdx := ldu(i).io.lsu.resp.bits.debugRobIdx
    loadMissEntry.paddr := ldu(i).io.missReq.bits.addr
    loadMissEntry.vaddr := ldu(i).io.missReq.bits.vaddr
    loadMissEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.missReq.fire & ldu(i).io.missResp.merged,
      ldu(i).io.missReq.fire & !ldu(i).io.missResp.merged,
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

  val isWriteLoadAccessTable = Constantin.createRecord(s"isWriteLoadAccessTable$hartId")
  val loadAccessTable = ChiselDB.createTable(s"LoadAccessDB$hartId", new LoadAccessEntry)
  for (i <- 0 until LoadPipelineWidth) {
    val loadAccessEntry = Wire(new LoadAccessEntry)
    loadAccessEntry.timeCnt := GTimer()
    loadAccessEntry.robIdx := ldu(i).io.lsu.resp.bits.debugRobIdx
    loadAccessEntry.paddr := ldu(i).io.missReq.bits.addr
    loadAccessEntry.vaddr := ldu(i).io.missReq.bits.vaddr
    loadAccessEntry.missState := OHToUInt(Cat(Seq(
      ldu(i).io.missReq.fire & ldu(i).io.missResp.merged,
      ldu(i).io.missReq.fire & !ldu(i).io.missResp.merged,
      ldu(i).io.lsu.s2_first_hit && ldu(i).io.lsu.resp.valid
    )))
    loadAccessEntry.pred_way_num := ldu(i).io.lsu.debugS2PredWayNum
    loadAccessEntry.real_way_num := ldu(i).io.lsu.debugS2RealWayNum
    loadAccessEntry.dm_way_num := ldu(i).io.lsu.debugS2DmWayNum
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
  val atomicRespValid = mainPipe.io.atomicResp.valid && mainPipe.io.atomicResp.bits.isAMO
  io.lsu.atomics.resp.valid := RegNext(atomicRespValid)
  io.lsu.atomics.resp.bits := RegEnable(mainPipe.io.atomicResp.bits, atomicRespValid)
  io.lsu.atomics.blockLr := mainPipe.io.blockLr

  // Request
  // seperately generating miss queue enq ready for better timeing
  val missReadyGen = Module(new MissReadyGen(MissReqPortCount))

  missReadyGen.io.in(MainPipeMissReqPort) <> mainPipe.io.missReq
  for (w <- 0 until backendParams.LduCnt) {
    missReadyGen.io.in(w + 1) <> ldu(w).io.missReq
  }

  mainPipe.io.missResp := missQueue.io.resp(0)
  for (w <- 0 until LoadPipelineWidth) { ldu(w).io.missResp := missQueue.io.resp(w + 1) }

  if(StorePrefetchL1Enabled) {
    for (w <- 0 until backendParams.StaCnt) {
      missReadyGen.io.in(1 + backendParams.LduCnt + w) <> stu(w).io.missReq
    }
  }else {
    for (w <- 0 until backendParams.StaCnt) { stu(w).io.missReq.ready := false.B }
  }

  wb.io.missReqConflictCheck(MainPipeMissReqPort) := mainPipe.io.wbqConflictCheck
  mainPipe.io.wbqBlockMissReq   := wb.io.blockMissReq(MainPipeMissReqPort)
  for(w <- 0 until LoadPipelineWidth) {
    wb.io.missReqConflictCheck(w+1) := ldu(w).io.wbqConflictCheck
    ldu(w).io.wbqBlockMissReq     := wb.io.blockMissReq(w+1)
  }

  if(StorePrefetchL1Enabled) {
    for (w <- 0 until backendParams.StaCnt) {
      wb.io.missReqConflictCheck(1 + backendParams.LduCnt + w).valid := stu(w).io.missReq.valid
      wb.io.missReqConflictCheck(1 + backendParams.LduCnt + w).bits := stu(w).io.missReq.bits.addr
    }
  }

  missQueue.io.wbqBlockMissReq := wb.io.blockMissReq

  missReadyGen.io.queryMQ <> missQueue.io.queryMQ
  io.cmoOpReq <> missQueue.io.cmoReq
  io.cmoOpResp <> missQueue.io.cmoResp

  val missQueueEnqValidVec = VecInit(missReadyGen.io.queryMQ.map(_.req.valid))
  val missQueueEnqFireVec = VecInit(missReadyGen.io.queryMQ.map(q => q.req.valid && q.ready))

  XSPerfAccumulate("miss_queue_fire", PopCount(missQueueEnqFireVec) >= 1.U)
  XSPerfAccumulate("miss_queue_muti_fire", PopCount(missQueueEnqFireVec) > 1.U)

  XSPerfAccumulate("miss_queue_has_enq_req", PopCount(missQueueEnqValidVec) >= 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_req", PopCount(missQueueEnqValidVec) > 1.U)
  XSPerfAccumulate("miss_queue_has_muti_enq_but_not_fire", PopCount(missQueueEnqValidVec) > 1.U && PopCount(missQueueEnqFireVec) === 0.U)
  // forward missqueue
  missQueue.io.forward <> io.lsu.forwardMshr
  // If a store is miss and accepted by mshr, Sbuffer releases the entry and mshr provides corresponding st-ld forwarding data.
  missQueue.io.forwardStData := io.lsu.forwardMshrStData

  // refill to load queue
 // io.lsu.lsq <> missQueue.io.refill_to_ldq

  // tilelink stuff
  bus.a <> missQueue.io.memAcquire
  bus.e <> missQueue.io.memFinish
  missQueue.io.evictSet := mainPipe.io.evictSet
  missQueue.io.btotWaysForSet <> mainPipe.io.btotWaysForSet
  missQueue.io.replace <> mainPipe.io.replace
  missQueue.io.probe.req.valid := bus.b.valid
  missQueue.io.probe.req.bits.addr := bus.b.bits.address
  if(DCacheAboveIndexOffset > DCacheTagOffset) {
    // have alias problem, extra alias bits needed for index
    val aliasAddrFrag = bus.b.bits.data(2, 1)
    missQueue.io.probe.req.bits.vaddr := Cat(
      0.U(PAddrBits - 1, DCacheAboveIndexOffset), // dontcare
      aliasAddrFrag(DCacheAboveIndexOffset - DCacheTagOffset - 1, 0), // index
      bus.b.bits.address(DCacheTagOffset - 1, 0)                 // index & others
    )
  } else { // no alias problem
    missQueue.io.probe.req.bits.vaddr := bus.b.bits.address
  }

  missQueue.io.mainPipeResp.valid := RegNext(mainPipe.io.atomicResp.valid)
  missQueue.io.mainPipeResp.bits := RegEnable(mainPipe.io.atomicResp.bits, mainPipe.io.atomicResp.valid)

  //----------------------------------------
  // probe
  // probeQueue.io.mem_probe <> bus.b
  blockDecoupled(bus.b, probeQueue.io.memProbe, missQueue.io.probe.block)
  probeQueue.io.lrscLockedBlock <> mainPipe.io.lrscLockedBlock
  probeQueue.io.updateResvSet <> mainPipe.io.updateResvSet

  val refillReq = RegNext(missQueue.io.mainPipeReq.valid && ((missQueue.io.mainPipeReq.bits.isLoad) | (missQueue.io.mainPipeReq.bits.isStore)))
  //----------------------------------------
  // mainPipe
  // when a req enters main pipe, if it is set-conflict with replace pipe or refill pipe,
  // block the req in main pipe
  probeQueue.io.pipeReq <> mainPipe.io.probeReq
  io.lsu.store.req <> mainPipe.io.storeReq

  io.lsu.store.replayResp.valid := RegNext(mainPipe.io.storeReplayResp.valid)
  io.lsu.store.replayResp.bits := RegEnable(mainPipe.io.storeReplayResp.bits, mainPipe.io.storeReplayResp.valid)
  io.lsu.store.mainPipeHitResp := mainPipe.io.storeHitResp

  mainPipe.io.atomicReq <> io.lsu.atomics.req

  mainPipe.io.invalidResvSet := RegNext(
    wb.io.req.fire &&
    wb.io.req.bits.addr === mainPipe.io.lrscLockedBlock.bits &&
    mainPipe.io.lrscLockedBlock.valid
  )

  //----------------------------------------
  // replace (main pipe)
  val mpStatus = mainPipe.io.status
  mainPipe.io.refillReq <> missQueue.io.mainPipeReq

  mainPipe.io.dataWriteReadyDup := VecInit(Seq.fill(nDupDataWriteReady)(true.B))
  mainPipe.io.tagWriteReadyDup := VecInit(Seq.fill(nDupDataWriteReady)(true.B))
  mainPipe.io.wbReadyDup := wb.io.reqReadyDup

  //----------------------------------------
  // wb
  // add a queue between MainPipe and WritebackUnit to reduce MainPipe stalls due to WritebackUnit busy
  wb.io.req <> mainPipe.io.wb
  bus.c     <> wb.io.memRelease
  // wb.io.release_wakeup := refillPipe.io.release_wakeup
  // wb.io.release_update := mainPipe.io.release_update
  //wb.io.probe_ttob_check_req <> mainPipe.io.probe_ttob_check_req
  //wb.io.probe_ttob_check_resp <> mainPipe.io.probe_ttob_check_resp

  io.lsu.release.valid := RegNext(wb.io.req.fire)
  io.lsu.release.bits.paddr := RegEnable(wb.io.req.bits.addr, wb.io.req.fire)
  // Note: RegNext() is required by:
  // * load queue released flag update logic
  // * load / load violation check logic
  // * and timing requirements
  // CHANGE IT WITH CARE

  // connect bus d
  missQueue.io.memGrant.valid := false.B
  missQueue.io.memGrant.bits  := DontCare

  wb.io.memGrant.valid := false.B
  wb.io.memGrant.bits  := DontCare

  // in L1DCache, we ony expect Grant[Data] and ReleaseAck
  bus.d.ready := false.B
  when (bus.d.bits.opcode === TLMessages.Grant || bus.d.bits.opcode === TLMessages.GrantData || bus.d.bits.opcode === TLMessages.CBOAck) {
    missQueue.io.memGrant <> bus.d
  } .elsewhen (bus.d.bits.opcode === TLMessages.ReleaseAck) {
    wb.io.memGrant <> bus.d
  } .otherwise {
    assert (!bus.d.fire)
  }

  //----------------------------------------
  // Bloom Filter
  // bloomFilter.io.set <> missQueue.io.bloom_filter_query.set
  // bloomFilter.io.clr <> missQueue.io.bloom_filter_query.clr
  bloomFilter.io.set <> mainPipe.io.bloomFilterQuery.set
  bloomFilter.io.clr <> mainPipe.io.bloomFilterQuery.clr

  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.query(w) <> ldu(w).io.bloomFilterQuery.query }
  for (w <- 0 until LoadPipelineWidth)  { bloomFilter.io.resp(w) <> ldu(w).io.bloomFilterQuery.resp }

  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.ld_in(w) <> ldu(w).io.counterFilterEnq }
  for (w <- 0 until LoadPipelineWidth)  { counterFilter.io.query(w) <> ldu(w).io.counterFilterQuery }

  //----------------------------------------
  // replacement algorithm
  val replacer = ReplacementPolicy.fromString(cacheParams.replacer, nWays, nSets)
  val replWayReqs = ldu.map(_.io.replaceWay) ++ Seq(mainPipe.io.replaceWay) ++ stu.map(_.io.replaceWay)

  if (dwpuParam.enCfPred) {
    val victimList = VictimList(nSets)
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

  val replAccessReqs = ldu.map(_.io.replaceAccess) ++ Seq(
    mainPipe.io.replaceAccess
  ) ++ stu.map(_.io.replaceAccess)
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
    assert(PmemRanges.map(_.cover(bus.a.bits.address)).reduce(_ || _))
  }
  when (bus.b.fire) {
    assert(PmemRanges.map(_.cover(bus.b.bits.address)).reduce(_ || _))
  }
  when (bus.c.fire) {
    assert(PmemRanges.map(_.cover(bus.c.bits.address)).reduce(_ || _))
  }

  //----------------------------------------
  // utility functions
  def blockDecoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }

  //----------------------------------------
  // performance counters
  val numLoads = PopCount(ldu.map(e => e.io.lsu.req.fire))
  XSPerfAccumulate("num_loads", numLoads)

  io.mshrFull := missQueue.io.full
  io.l1Miss := missQueue.io.l1Miss

  // performance counter
  // val ld_access = Wire(Vec(LoadPipelineWidth, missQueue.io.debug_early_replace.last.cloneType))
  // val st_access = Wire(ld_access.last.cloneType)
  // ld_access.zip(ldu).foreach {
  //   case (a, u) =>
  //     a.valid := RegNext(u.io.lsu.req.fire) && !u.io.lsu.s1_kill
  //     a.bits.idx := RegEnable(get_idx(u.io.lsu.req.bits.vaddr), u.io.lsu.req.fire)
  //     a.bits.tag := get_tag(u.io.lsu.s1_paddr_dup_dcache)
  // }
  // st_access.valid := RegNext(mainPipe.io.store_req.fire)
  // st_access.bits.idx := RegEnable(get_idx(mainPipe.io.store_req.bits.vaddr), mainPipe.io.store_req.fire)
  // st_access.bits.tag := RegEnable(get_tag(mainPipe.io.store_req.bits.addr), mainPipe.io.store_req.fire)
  // val access_info = ld_access.toSeq ++ Seq(st_access)
  // val early_replace = RegNext(missQueue.io.debug_early_replace) // TODO: clock gate
  // val access_early_replace = access_info.map {
  //   case acc =>
  //     Cat(early_replace.map {
  //       case r =>
  //         acc.valid && r.valid &&
  //           acc.bits.tag === r.bits.tag &&
  //           acc.bits.idx === r.bits.idx
  //     })
  // }
  // XSPerfAccumulate("access_early_replace", PopCount(Cat(access_early_replace)))
  val grantDataFire = {
    val (first, last, done, count) = edge.count(bus.d)
    bus.d.fire && first && bus.d.bits.opcode === GrantData
  }
  XSPerfAccumulate("grant_data_fire", grantDataFire)

  val hintSource = io.l2Hint.bits.sourceId

  val grantDataSource = bus.d.bits.source

  val hintPipe2 = Module(new Pipeline(UInt(32.W), 3))
  hintPipe2.io.in.valid := io.l2Hint.valid
  hintPipe2.io.in.bits := hintSource
  hintPipe2.io.out.ready := true.B

  val hintPipe1 = Module(new Pipeline(UInt(32.W), 2))
  hintPipe1.io.in.valid := io.l2Hint.valid
  hintPipe1.io.in.bits := hintSource
  hintPipe1.io.out.ready := true.B

  val accurateHint = grantDataFire && hintPipe2.io.out.valid && hintPipe2.io.out.bits === grantDataSource
  XSPerfAccumulate("accurate3Hints", accurateHint)

  val okHint = grantDataFire && hintPipe1.io.out.valid && hintPipe1.io.out.bits === grantDataSource
  XSPerfAccumulate("ok2Hints", okHint)
  val hintWithoutGrant = hintPipe2.io.out.valid && !grantDataFire
  val grantWithoutHint = !hintPipe2.io.out.valid && grantDataFire
  val hintGrantUnmatch = hintPipe2.io.out.valid && grantDataFire && (hintPipe2.io.out.bits =/= grantDataSource)
  XSPerfAccumulate("hint_without_grant", hintWithoutGrant)
  XSPerfAccumulate("grant_without_hint", grantWithoutHint)
  XSPerfAccumulate("hint_grant_unmatch", hintGrantUnmatch)


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

class DCacheWrapper()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasDCacheParameters
{
  override def shouldBeInlined: Boolean = false

  val useDcache = coreParams.dcacheParametersOpt.nonEmpty
  val clientNode = if (useDcache) TLIdentityNode() else null
  val dcache = if (useDcache) LazyModule(new DCache()) else null
  if (useDcache) {
    clientNode := dcache.clientNode
  }
  val uncacheNode = OptionWrapper(cacheCtrlParamsOpt.isDefined, TLIdentityNode())
  require(
    (uncacheNode.isDefined && dcache.cacheCtrlOpt.isDefined) ||
    (!uncacheNode.isDefined && !dcache.cacheCtrlOpt.isDefined), "uncacheNode and ctrlUnitOpt are not connected!")
  if (uncacheNode.isDefined && dcache.cacheCtrlOpt.isDefined) {
    dcache.cacheCtrlOpt.get.node := uncacheNode.get
  }

  class DCacheWrapperImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) with HasPerfEvents {
    val io = IO(new DCacheIO)
    val perfEvents = if (!useDcache) {
      // a fake dcache which uses dpi-c to access memory, only for debug usage!
      val fakeDcache = Module(new FakeDCache())
      io <> fakeDcache.io
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
