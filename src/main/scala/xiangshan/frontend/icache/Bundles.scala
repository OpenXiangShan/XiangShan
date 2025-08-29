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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.ftq.FtqPtr

/* ***
 * Naming:
 * - I/O:
 *   - ICache inner use only: xxxBundle
 *   - Other modules use: ICacheXxxBundle, consider move to Bundles.scala
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
    val phyTag:  UInt = UInt(tagBits.W)
    val vSetIdx: UInt = UInt(idxBits.W)
    val waymask: UInt = UInt(nWays.W)
    val bankIdx: Bool = Bool()
    val poison:  Bool = Bool()

    def generate(phyTag: UInt, vSetIdx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
      this.phyTag  := phyTag
      this.vSetIdx := vSetIdx
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
    val vSetIdx: UInt = UInt(idxBits.W)
    val data:    UInt = UInt(blockBits.W)
    val waymask: UInt = UInt(nWays.W)
    val bankIdx: Bool = Bool()
    val poison:  Bool = Bool()

    def generate(data: UInt, vSetIdx: UInt, waymask: UInt, bankIdx: Bool, poison: Bool): Unit = {
      this.data    := data
      this.vSetIdx := vSetIdx
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
    val vSetIdx: UInt = UInt(idxBits.W)
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
    val codes:      Vec[Vec[UInt]]           = Vec(PortNumber, Vec(nWays, UInt(MetaEccBits.W)))
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
    val datas: Vec[UInt] = Vec(DataBanks, UInt(ICacheDataBits.W))
    val codes: Vec[UInt] = Vec(DataBanks, UInt(DataEccBits.W))
  }
  val req:  DecoupledIO[DataReadReqBundle] = DecoupledIO(new DataReadReqBundle)
  val resp: DataReadRespBundle             = Input(new DataReadRespBundle)
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
  val doubleline:         Bool            = Bool()
  val vAddr:              Vec[PrunedAddr] = Vec(PortNumber, PrunedAddr(VAddrBits))
  val data:               UInt            = UInt(blockBits.W)
  val pAddr:              PrunedAddr      = PrunedAddr(PAddrBits)
  val exception:          ExceptionType   = new ExceptionType
  val pmpMmio:            Bool            = Bool()
  val itlbPbmt:           UInt            = UInt(Pbmt.width.W)
  val isBackendException: Bool            = Bool()
  /* NOTE: GPAddrBits(=50bit) is not enough for gpAddr here, refer to PR#3795
   * Sv48*4 only allows 50bit gpAddr, when software violates this requirement
   * it needs to fill the mtval2 register with the full XLEN(=64bit) gpAddr,
   * PAddrBitsMax(=56bit currently) is required for the frontend datapath due to the itlb ppn length limitation
   * (cases 56<x<=64 are handled by the backend datapath)
   */
  val gpAddr:            PrunedAddr = PrunedAddr(PAddrBitsMax)
  val isForVSnonLeafPTE: Bool       = Bool()
}

/* ***** PrefetchPipe ***** */
class PrefetchReqBundle(implicit p: Parameters) extends ICacheBundle {
  val startAddr:        PrunedAddr    = PrunedAddr(VAddrBits)
  val nextlineStart:    PrunedAddr    = PrunedAddr(VAddrBits)
  val ftqIdx:           FtqPtr        = new FtqPtr
  val isSoftPrefetch:   Bool          = Bool()
  val backendException: ExceptionType = new ExceptionType

  def crossCacheline: Bool = startAddr(blockOffBits - 1) === 1.U

  def fromFtqICacheInfo(info: FtqICacheInfo): PrefetchReqBundle = {
    this.startAddr      := info.startVAddr
    this.nextlineStart  := info.nextCachelineVAddr
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
/* WayLookupEntry is for internal storage, while WayLookupBundle is for interface
 * Notes:
 *   1. There must be a flush (caused by exception) after itlbException.hasException,
 *      so, we need only the first exception and the corresponding gpAddr / related info.
 *      To save area, we separate those signals from WayLookupEntry and store only once.
 */
class WayLookupEntry(implicit p: Parameters) extends ICacheBundle {
  val vSetIdx:   Vec[UInt] = Vec(PortNumber, UInt(idxBits.W))
  val waymask:   Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
  val metaCodes: Vec[UInt] = Vec(PortNumber, UInt(MetaEccBits.W))
  val pTag:      UInt      = UInt(tagBits.W)
  val itlbPbmt:  UInt      = UInt(Pbmt.width.W)
}

class WayLookupExceptionEntry(implicit p: Parameters) extends ICacheBundle {
  val itlbException:     ExceptionType = new ExceptionType
  val gpAddr:            PrunedAddr    = PrunedAddr(PAddrBitsMax)
  val isForVSnonLeafPTE: Bool          = Bool()
}

class WayLookupBundle(implicit p: Parameters) extends ICacheBundle {
  val entry          = new WayLookupEntry
  val exceptionEntry = new WayLookupExceptionEntry

  // for compatibility
  def vSetIdx:           Vec[UInt]     = entry.vSetIdx
  def waymask:           Vec[UInt]     = entry.waymask
  def metaCodes:         Vec[UInt]     = entry.metaCodes
  def pTag:              UInt          = entry.pTag
  def itlbPbmt:          UInt          = entry.itlbPbmt
  def itlbException:     ExceptionType = exceptionEntry.itlbException
  def gpAddr:            PrunedAddr    = exceptionEntry.gpAddr
  def isForVSnonLeafPTE: Bool          = exceptionEntry.isForVSnonLeafPTE
}

/* ***** Miss ***** */
// ICacheMainPipe / ICachePrefetchPipe -> MissUnit
class MissReqBundle(implicit p: Parameters) extends ICacheBundle {
  val blkPAddr: UInt = UInt((PAddrBits - blockOffBits).W)
  val vSetIdx:  UInt = UInt(idxBits.W)
}
// MissUnit -> ICacheMainPipe / ICachePrefetchPipe / ICacheWayLookup
class MissRespBundle(implicit p: Parameters) extends ICacheBundle {
  val blkPAddr: UInt = UInt((PAddrBits - blockOffBits).W)
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
  val blkPAddr: UInt = UInt((PAddrBits - blockOffBits).W)
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
// to Ifu
class ICachePerfInfo(implicit p: Parameters) extends ICacheBundle {
  val hits:         Vec[Bool]     = Vec(PortNumber, Bool())
  val isDoubleLine: Bool          = Bool()
  val exception:    ExceptionType = new ExceptionType

  // single line request, port 0 is hit/miss
  def hit0NoReq1:  Bool = hits(0) && !isDoubleLine
  def miss0NoReq1: Bool = !hits(0) && !isDoubleLine
  // double line request, port 0/1 is hit/miss
  def hit0Hit1:   Bool = hits(0) && hits(1) && isDoubleLine
  def hit0Miss1:  Bool = hits(0) && !hits(1) && isDoubleLine
  def miss0Hit1:  Bool = !hits(0) && hits(1) && isDoubleLine
  def miss0Miss1: Bool = !hits(0) && !hits(1) && isDoubleLine
  // has exception
  def except: Bool = exception.hasException

  def hit0: Bool = hits(0)
  def hit1: Bool = hits(1) && isDoubleLine

  def hit: Bool = hits(0) && (!isDoubleLine || hits(1))
}

// to Ifu -> IBuffer -> Backend
class ICacheTopdownInfo(implicit p: Parameters) extends ICacheBundle {
  val iCacheMissBubble: Bool = Output(Bool())
  val itlbMissBubble:   Bool = Output(Bool())
}

// inner MainPipe -> top
class MainPipePerfInfo(implicit p: Parameters) extends ICacheBundle {
  val rawHits:     Vec[Bool] = Vec(PortNumber, Bool())
  val pendingMiss: Bool      = Bool() // currently handling a miss
}

// inner PrefetchPipe -> top
class PrefetchPipePerfInfo(implicit p: Parameters) extends ICacheBundle {
  val pendingItlbMiss: Bool = Bool()
}

// inner WayLookup -> top
class WayLookupPerfInfo(implicit p: Parameters) extends ICacheBundle {
  val empty: Bool = Bool()
}
