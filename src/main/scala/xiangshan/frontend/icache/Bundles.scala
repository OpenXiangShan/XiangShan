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

/* ***
 * Naming:
 * - I/O:
 *   - ICache inner use only: xxxBundle
 *   - Other modules use: ICacheXxxBundle, consider move to FrontendBundle.scala
 * - Sram/register: xxxEntry
 *
 * Try avoiding directed Bundle, unless it's req-resp pair
 * *** */

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
// ICacheMissUnit <-> ICacheMetaArray
class MetaWriteBundle(implicit p: Parameters) extends ICacheBundle {
  class MetaWriteReqBundle(implicit p: Parameters) extends ICacheBundle {
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
  val req: DecoupledIO[MetaWriteReqBundle] = DecoupledIO(new MetaWriteReqBundle)
}

// ICacheMissUnit <-> ICacheDataArray
class DataWriteBundle(implicit p: Parameters) extends ICacheBundle {
  class DataWriteReqBundle(implicit p: Parameters) extends ICacheBundle {
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
  val req: DecoupledIO[DataWriteReqBundle] = DecoupledIO(new DataWriteReqBundle)
}

/* ***** Array flush ***** */
// ICacheMainPipe <-> ICacheMetaArray
class MetaFlushBundle(implicit p: Parameters) extends ICacheBundle {
  class MetaFlushReqBundle(implicit p: Parameters) extends ICacheBundle {
    val virIdx:  UInt = UInt(idxBits.W)
    val waymask: UInt = UInt(nWays.W)
  }
  val req: Vec[Valid[MetaFlushReqBundle]] = Vec(PortNumber, ValidIO(new MetaFlushReqBundle))
}

/* ***** Array read ***** */
class ArrayReadReqBundle(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:      Vec[UInt]      = Vec(PortNumber, UInt(idxBits.W))
  val waymask:      Vec[Vec[Bool]] = Vec(PortNumber, Vec(nWays, Bool()))
  val blkOffset:    UInt           = UInt(log2Ceil(blockBytes).W)
  val isDoubleLine: Bool           = Bool()
}

// ICachePrefetchPipe / ICacheCtrlUnit <-> ICacheMetaArray
class MetaReadBundle(implicit p: Parameters) extends ICacheBundle {
  class MetaReadReqBundle(implicit p: Parameters) extends ArrayReadReqBundle
  class MetaReadRespBundle(implicit p: Parameters) extends ICacheBundle {
    val metas:      Vec[Vec[ICacheMetadata]] = Vec(PortNumber, Vec(nWays, new ICacheMetadata))
    val codes:      Vec[Vec[UInt]]           = Vec(PortNumber, Vec(nWays, UInt(ICacheMetaCodeBits.W)))
    val entryValid: Vec[Vec[Bool]]           = Vec(PortNumber, Vec(nWays, Bool()))
    // for compatibility
    def tags: Vec[Vec[UInt]] = VecInit(metas.map(port => VecInit(port.map(way => way.tag))))
  }
  val req:  DecoupledIO[MetaReadReqBundle] = DecoupledIO(new MetaReadReqBundle)
  val resp: MetaReadRespBundle             = Input(new MetaReadRespBundle)
}

// ICacheMainPipe -> ICacheDataArray
class DataReadBundle(implicit p: Parameters) extends ICacheBundle {
  class DataReadReqBundle(implicit p: Parameters) extends ArrayReadReqBundle
  class DataReadRespBundle(implicit p: Parameters) extends ICacheBundle {
    val datas: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataBits.W))
    val codes: Vec[UInt] = Vec(ICacheDataBanks, UInt(ICacheDataCodeBits.W))
  }
  val req:  Vec[DecoupledIO[DataReadReqBundle]] = Vec(partWayNum, DecoupledIO(new DataReadReqBundle))
  val resp: DataReadRespBundle                  = Input(new DataReadRespBundle)
}

/* ***** Replacer ***** */
// ICacheMainPipe <-> ICacheReplacer
class ReplacerTouchBundle(implicit p: Parameters) extends ICacheBundle {
  class ReplacerTouchReqBundle(implicit p: Parameters) extends ICacheBundle {
    val vSetIdx: UInt = UInt(idxBits.W)
    val way:     UInt = UInt(wayBits.W)
  }
  val req: Vec[Valid[ReplacerTouchReqBundle]] = Vec(PortNumber, ValidIO(new ReplacerTouchReqBundle))
}

// ICacheMissUnit <-> ICacheReplacer
class ReplacerVictimBundle(implicit p: Parameters) extends ICacheBundle {
  class ReplacerVictimReqBundle(implicit p: Parameters) extends ICacheBundle {
    val vSetIdx: UInt = UInt(idxBits.W)
  }
  class ReplacerVictimRespBundle(implicit p: Parameters) extends ICacheBundle {
    val way: UInt = UInt(wayBits.W)
  }
  val req:  Valid[ReplacerVictimReqBundle] = ValidIO(new ReplacerVictimReqBundle)
  val resp: ReplacerVictimRespBundle       = Input(new ReplacerVictimRespBundle)
}

/* ***** MainPipe ***** */
// ICache(MainPipe) -> IFU
class ICacheRespBundle(implicit p: Parameters) extends ICacheBundle {
  val doubleline:         Bool      = Bool()
  val vaddr:              Vec[UInt] = Vec(PortNumber, UInt(VAddrBits.W))
  val data:               UInt      = UInt(blockBits.W)
  val paddr:              Vec[UInt] = Vec(PortNumber, UInt(PAddrBits.W))
  val exception:          Vec[UInt] = Vec(PortNumber, ExceptionType())
  val pmp_mmio:           Vec[Bool] = Vec(PortNumber, Bool())
  val itlb_pbmt:          Vec[UInt] = Vec(PortNumber, UInt(Pbmt.width.W))
  val isBackendException: Bool      = Bool()
  /* NOTE: GPAddrBits(=50bit) is not enough for gpaddr here, refer to PR#3795
   * Sv48*4 only allows 50bit gpaddr, when software violates this requirement
   * it needs to fill the mtval2 register with the full XLEN(=64bit) gpaddr,
   * PAddrBitsMax(=56bit currently) is required for the frontend datapath due to the itlb ppn length limitation
   * (cases 56<x<=64 are handled by the backend datapath)
   */
  val gpaddr:            UInt = UInt(PAddrBitsMax.W)
  val isForVSnonLeafPTE: Bool = Bool()
}

/* ***** PrefetchPipe ***** */
class PrefetchReqBundle(implicit p: Parameters) extends ICacheBundle {
  val startAddr:        UInt   = UInt(VAddrBits.W)
  val nextlineStart:    UInt   = UInt(VAddrBits.W)
  val ftqIdx:           FtqPtr = new FtqPtr
  val isSoftPrefetch:   Bool   = Bool()
  val backendException: UInt   = ExceptionType()
  def crossCacheline:   Bool   = startAddr(blockOffBits - 1) === 1.U

  def fromFtqICacheInfo(info: FtqICacheInfo): PrefetchReqBundle = {
    this.startAddr      := info.startAddr
    this.nextlineStart  := info.nextlineStart
    this.ftqIdx         := info.ftqIdx
    this.isSoftPrefetch := false.B
    this
  }

  def fromSoftPrefetch(req: SoftIfetchPrefetchBundle): PrefetchReqBundle = {
    this.startAddr      := req.vaddr
    this.nextlineStart  := req.vaddr + (1 << blockOffBits).U
    this.ftqIdx         := DontCare
    this.isSoftPrefetch := true.B
    this
  }
}

/* ***** ICacheWayLookup ***** */
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
  val itlb_exception: Vec[UInt] = Vec(PortNumber, ExceptionType())
  val itlb_pbmt:      Vec[UInt] = Vec(PortNumber, UInt(Pbmt.width.W))
  val meta_codes:     Vec[UInt] = Vec(PortNumber, UInt(ICacheMetaCodeBits.W))
}

class WayLookupGpfEntry(implicit p: Parameters) extends ICacheBundle {
  // NOTE: we don't use GPAddrBits here, refer to ICacheMainPipe.scala L43-48 and PR#3795
  val gpaddr:            UInt = UInt(PAddrBitsMax.W)
  val isForVSnonLeafPTE: Bool = Bool()
}

class WayLookupBundle(implicit p: Parameters) extends ICacheBundle {
  val entry = new WayLookupEntry
  val gpf   = new WayLookupGpfEntry

  // for compatibility
  def vSetIdx:           Vec[UInt] = entry.vSetIdx
  def waymask:           Vec[UInt] = entry.waymask
  def ptag:              Vec[UInt] = entry.ptag
  def itlb_exception:    Vec[UInt] = entry.itlb_exception
  def itlb_pbmt:         Vec[UInt] = entry.itlb_pbmt
  def meta_codes:        Vec[UInt] = entry.meta_codes
  def gpaddr:            UInt      = gpf.gpaddr
  def isForVSnonLeafPTE: Bool      = gpf.isForVSnonLeafPTE
}

/* ***** Miss ***** */
// ICacheMainPipe / ICachePrefetchPipe -> MissUnit
class MissReqBundle(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
}
// MissUnit -> ICacheMainPipe / ICachePrefetchPipe / ICacheWayLookup
class MissRespBundle(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val waymask:  UInt = UInt(nWays.W)
  val data:     UInt = UInt(blockBits.W)
  val corrupt:  Bool = Bool()
}

/* ***** Mshr ***** */
// ICacheMissUnit <-> Mshr
class MshrLookupBundle(implicit p: Parameters) extends ICacheBundle {
  class MshrLookupReqBundle(implicit p: Parameters) extends MissReqBundle
  class MshrLookupRespBundle(implicit p: Parameters) extends ICacheBundle {
    val hit: Bool = Bool()
  }
  val req:  Valid[MshrLookupReqBundle] = ValidIO(new MshrLookupReqBundle)
  val resp: MshrLookupRespBundle       = Input(new MshrLookupRespBundle)
}

// Mshr -> ICacheMissUnit
class MshrInfoBundle(implicit p: Parameters) extends ICacheBundle {
  val blkPaddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
  val way:      UInt = UInt(wayBits.W)
}

// Mshr -> tilelink bus
class MshrAcquireBundle(edge: TLEdgeOut)(implicit p: Parameters) extends ICacheBundle {
  val acquire: TLBundleA = new TLBundleA(edge.bundle)
  val vSetIdx: UInt      = UInt(idxBits.W)
}

/* ***** Pmp ***** */
// ICacheMainPipe / ICachePrefetchPipe <-> Pmp
class PmpCheckBundle(implicit p: Parameters) extends ICacheBundle {
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

class ICacheTopdownInfo(implicit p: Parameters) extends ICacheBundle {
  val icacheMiss: Bool = Output(Bool())
  val itlbMiss:   Bool = Output(Bool())
}
