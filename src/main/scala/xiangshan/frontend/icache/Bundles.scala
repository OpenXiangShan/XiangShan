// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.tilelink.TLEdgeOut
import org.chipsalliance.cde.config.Parameters
import xiangshan.SoftIfetchPrefetchBundle
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.cache.mmu.Pbmt
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FtqICacheInfo
import xiangshan.frontend.FtqPtr
import xiangshan.frontend.FtqToICacheRequestBundle
import xiangshan.frontend.PrunedAddr

// meta
class ICacheMetadata(implicit p: Parameters) extends ICacheBundle {
  val tag: UInt = UInt(tagBits.W)
}

object ICacheMetadata {
  def apply(tag: Bits)(implicit p: Parameters): ICacheMetadata = {
    val meta = Wire(new ICacheMetadata)
    meta.tag := tag
    meta
  }
}

/* ***** Array write ***** */
// ICacheMissUnit -> ICacheMetaArray
class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val phyTag:  UInt = UInt(tagBits.W)
  val waymask: UInt = UInt(nWays.W)
  val bankIdx: Bool = Bool()
  val poison:  Bool = Bool()

  def generate(tag: UInt, idx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
    this.virIdx  := idx
    this.phyTag  := tag
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.poison  := poison
  }
}

// ICacheMissUnit -> ICacheDataArray
class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val data:    UInt = UInt(blockBits.W)
  val waymask: UInt = UInt(nWays.W)
  val bankIdx: Bool = Bool()
  val poison:  Bool = Bool()

  def generate(data: UInt, idx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
    this.virIdx  := idx
    this.data    := data
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.poison  := poison
  }
}

/* ***** Array flush ***** */
// ICacheMainPipe -> ICacheMetaArray
class ICacheMetaFlushBundle(implicit p: Parameters) extends ICacheBundle {
  val virIdx:  UInt = UInt(idxBits.W)
  val waymask: UInt = UInt(nWays.W)
}

/* ***** Array read ***** */
// ICachePrefetchPipe / ICacheMainPipe -> ICacheMetaArray / ICacheDataArray
class ICacheReadBundle(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:      Vec[UInt]      = Vec(2, UInt(idxBits.W))
  val waymask:      Vec[Vec[Bool]] = Vec(2, Vec(nWays, Bool()))
  val blkOffset:    UInt           = UInt(log2Ceil(blockBytes).W)
  val isDoubleLine: Bool           = Bool()
}

//class ICacheMetaReadBundle(implicit p: Parameters) extends ICacheReadBundle
class ICacheMetaRespBundle(implicit p: Parameters) extends ICacheBundle {
  val metas:      Vec[Vec[ICacheMetadata]] = Vec(PortNumber, Vec(nWays, new ICacheMetadata))
  val codes:      Vec[Vec[UInt]]           = Vec(PortNumber, Vec(nWays, UInt(ICacheMetaCodeBits.W)))
  val entryValid: Vec[Vec[Bool]]           = Vec(PortNumber, Vec(nWays, Bool()))
  // for compatibility
  def tags: Vec[Vec[UInt]] = VecInit(metas.map(port => VecInit(port.map(way => way.tag))))
}

class ICacheMetaReqBundle(implicit p: Parameters) extends ICacheBundle {
  val toIMeta:   DecoupledIO[ICacheReadBundle] = DecoupledIO(new ICacheReadBundle)
  val fromIMeta: ICacheMetaRespBundle          = Input(new ICacheMetaRespBundle)
}

//class ICacheDataReadBundle(implicit p: Parameters) extends ICacheReadBundle
class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle {
  val datas: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataBits.W))
  val codes: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataCodeBits.W))
}

class ICacheDataReqBundle(implicit p: Parameters) extends ICacheBundle {
  val toIData:   Vec[DecoupledIO[ICacheReadBundle]] = Vec(partWayNum, DecoupledIO(new ICacheReadBundle))
  val fromIData: ICacheDataRespBundle               = Input(new ICacheDataRespBundle)
}

/* ***** Replacer ***** */
// ICacheMainPipe -> ICacheReplacer
class ReplacerTouch(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx: UInt = UInt(idxBits.W)
  val way:     UInt = UInt(wayBits.W)
}
// ICacheReplacer -> ICacheMissUnit
class ReplacerVictim(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx: Valid[UInt] = ValidIO(UInt(idxBits.W))
  val way:     UInt        = Input(UInt(wayBits.W))
}

/* ***** MainPipe ***** */
// ICache(MainPipe) -> IFU
class ICacheMainPipeResp(implicit p: Parameters) extends ICacheBundle {
  val doubleline:       Bool            = Bool()
  val vaddr:            Vec[PrunedAddr] = Vec(PortNumber, PrunedAddr(VAddrBits))
  val data:             UInt            = UInt(blockBits.W)
  val paddr:            Vec[PrunedAddr] = Vec(PortNumber, PrunedAddr(PAddrBits))
  val exception:        Vec[UInt]       = Vec(PortNumber, UInt(ExceptionType.width.W))
  val pmp_mmio:         Vec[Bool]       = Vec(PortNumber, Bool())
  val itlb_pbmt:        Vec[UInt]       = Vec(PortNumber, UInt(Pbmt.width.W))
  val backendException: Bool            = Bool()
  /* NOTE: GPAddrBits(=50bit) is not enough for gpaddr here, refer to PR#3795
   * Sv48*4 only allows 50bit gpaddr, when software violates this requirement
   * it needs to fill the mtval2 register with the full XLEN(=64bit) gpaddr,
   * PAddrBitsMax(=56bit currently) is required for the frontend datapath due to the itlb ppn length limitation
   * (cases 56<x<=64 are handled by the backend datapath)
   */
  val gpaddr:            PrunedAddr = PrunedAddr(PAddrBitsMax)
  val isForVSnonLeafPTE: Bool       = Bool()
}
class ICacheMainPipeBundle(implicit p: Parameters) extends ICacheBundle {
  val req:               DecoupledIO[FtqToICacheRequestBundle] = Flipped(DecoupledIO(new FtqToICacheRequestBundle))
  val resp:              Valid[ICacheMainPipeResp]             = ValidIO(new ICacheMainPipeResp)
  val topdownIcacheMiss: Bool                                  = Output(Bool())
  val topdownItlbMiss:   Bool                                  = Output(Bool())
}

/* ***** PrefetchPipe ***** */
class IPrefetchReq(implicit p: Parameters) extends ICacheBundle {
  val startAddr:        PrunedAddr = PrunedAddr(VAddrBits)
  val nextlineStart:    PrunedAddr = PrunedAddr(VAddrBits)
  val ftqIdx:           FtqPtr     = new FtqPtr
  val isSoftPrefetch:   Bool       = Bool()
  val backendException: UInt       = UInt(ExceptionType.width.W)
  def crossCacheline:   Bool       = startAddr(blockOffBits - 1) === 1.U

  def fromFtqICacheInfo(info: FtqICacheInfo): IPrefetchReq = {
    this.startAddr      := info.startAddr
    this.nextlineStart  := info.nextlineStart
    this.ftqIdx         := info.ftqIdx
    this.isSoftPrefetch := false.B
    this
  }

  def fromSoftPrefetch(req: SoftIfetchPrefetchBundle): IPrefetchReq = {
    this.startAddr      := req.vaddr
    this.nextlineStart  := req.vaddr + (1 << blockOffBits).U
    this.ftqIdx         := DontCare
    this.isSoftPrefetch := true.B
    this
  }
}

/* ***** WayLookup ***** */
/* WayLookupEntry is for internal storage, while WayLookupInfo is for interface
 * Notes:
 *   1. there must be a flush (caused by guest page fault) after excp_tlb_gpf === true.B,
 *      so, we need only the first excp_tlb_gpf and the corresponding gpaddr.
 *      to save area, we separate those signals from WayLookupEntry and store only once.
 */
class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:        Vec[UInt] = Vec(PortNumber, UInt(idxBits.W))
  val waymask:        Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
  val ptag:           Vec[UInt] = Vec(PortNumber, UInt(tagBits.W))
  val itlb_exception: Vec[UInt] = Vec(PortNumber, UInt(ExceptionType.width.W))
  val itlb_pbmt:      Vec[UInt] = Vec(PortNumber, UInt(Pbmt.width.W))
  val meta_codes:     Vec[UInt] = Vec(PortNumber, UInt(ICacheMetaCodeBits.W))
}

class WayLookupGPFEntry(implicit p: Parameters) extends ICacheBundle {
  // NOTE: we don't use GPAddrBits here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  val gpaddr:            PrunedAddr = PrunedAddr(PAddrBitsMax)
  val isForVSnonLeafPTE: Bool       = Bool()
}

class WayLookupInfo(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  val gpf   = new WayLookupGPFEntry

  // for compatibility
  def vSetIdx:           Vec[UInt]  = entry.vSetIdx
  def waymask:           Vec[UInt]  = entry.waymask
  def ptag:              Vec[UInt]  = entry.ptag
  def itlb_exception:    Vec[UInt]  = entry.itlb_exception
  def itlb_pbmt:         Vec[UInt]  = entry.itlb_pbmt
  def meta_codes:        Vec[UInt]  = entry.meta_codes
  def gpaddr:            PrunedAddr = gpf.gpaddr
  def isForVSnonLeafPTE: Bool       = gpf.isForVSnonLeafPTE
}

/* ***** Miss ***** */
// ICacheMainPipe / ICachePrefetchPipe -> MissUnit
class ICacheMissReq(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
}
// MissUnit -> ICacheMainPipe / ICachePrefetchPipe / WayLookup
class ICacheMissResp(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val waymask:  UInt = UInt(nWays.W)
  val data:     UInt = UInt(blockBits.W)
  val corrupt:  Bool = Bool()
}
// ICacheMainPipe <-> ICacheMissUnit
class ICacheMSHRBundle(implicit p: Parameters) extends ICacheBundle {
  val req:  DecoupledIO[ICacheMissReq] = DecoupledIO(new ICacheMissReq)
  val resp: Valid[ICacheMissResp]      = Flipped(ValidIO(new ICacheMissResp))
}

/* ***** MSHR ***** */
// ICacheMainPipe -> MSHR
class MSHRLookup(implicit p: Parameters) extends ICacheBundle {
  val info: Valid[ICacheMissReq] = ValidIO(new ICacheMissReq)
  val hit:  Bool                 = Input(Bool())
}

// MSHR -> ICacheMissUnit
class MSHRResp(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val way:      UInt = UInt(wayBits.W)
}

// MSHR -> tilelink bus
class MSHRAcquire(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  val acquire: TLBundleA = new TLBundleA(edge.bundle)
  val vSetIdx: UInt      = UInt(idxBits.W)
}

/* ***** PMP ***** */
// ICacheMainPipe / ICachePrefetchPipe <-> PMP
class ICachePMPBundle(implicit p: Parameters) extends ICacheBundle {
  val req:  Valid[PMPReqBundle] = ValidIO(new PMPReqBundle)
  val resp: PMPRespBundle       = Input(new PMPRespBundle)
}

/* ***** Perf ***** */
class ICachePerfInfo(implicit p: Parameters) extends ICacheBundle {
  val only_0_hit:      Bool      = Bool()
  val only_0_miss:     Bool      = Bool()
  val hit_0_hit_1:     Bool      = Bool()
  val hit_0_miss_1:    Bool      = Bool()
  val miss_0_hit_1:    Bool      = Bool()
  val miss_0_miss_1:   Bool      = Bool()
  val hit_0_except_1:  Bool      = Bool()
  val miss_0_except_1: Bool      = Bool()
  val except_0:        Bool      = Bool()
  val bank_hit:        Vec[Bool] = Vec(PortNumber, Bool())
  val hit:             Bool      = Bool()
}
