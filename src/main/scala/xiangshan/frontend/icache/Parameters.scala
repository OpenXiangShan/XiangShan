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

import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import utility.Code
import xiangshan.cache.HasL1CacheParameters
import xiangshan.cache.L1CacheParameters
import xiangshan.frontend.HasFrontendParameters

// For users: these are default ICache parameters set by dev, do not change them here,
// use top-level Parameters.scala to change them on initialization.
case class ICacheParameters(
    /* *** from trait L1CacheParameters *** */
    nSets:      Int = 256,
    nWays:      Int = 4,
    rowBits:    Int = 64, // per bank, by default we split 64B cacheline into 8 banks, so each bank has 8B (64b)
    blockBytes: Int = 64, // cacheline size
    /* *** ICache specific *** */
    PortNumber: Int = 2, // TODO: remove this when dropping cross-page fetch
    // replacer
    Replacer: String = "setplru", // "random", "setlru", "setplru"
    // missUnit
    NumFetchMshr:    Int = 4,
    NumPrefetchMshr: Int = 10,
    // wayLookup
    WayLookupSize: Int = 32,
    // ecc
    // NOTE: we call it "ecc" since it can be configured to use ecc like "secded", but by default it is parity
    // TODO: support disabling ecc completely (currently "none" will use "identity", we want to remove entire ecc logic)
    MetaEcc:     String = "parity",  // "none", "identity", "parity", "sec", "secded"
    DataEcc:     String = "parity",  // "none", "identity", "parity", "sec", "secded"
    DataEccUnit: Option[Int] = None, // if None, use blockBytes
    // data array
    // by default we store 64data + 1parity + 1padding, this is better than 65bits (from physical design)
    DataPaddingBits: Int = 1,
    /* *** submodule enable *** */
    // if not enabled, the whole CtrlUnit will not be created, and parity-fault injection will not be supported
    EnableCtrlUnit: Boolean = true,
    /* *** submodule parameters *** */
    ctrlUnitParameters: ICacheCtrlUnitParameters = ICacheCtrlUnitParameters(),
    /* *** testing *** */
    ForceMetaEccFail: Boolean = false,
    ForceDataEccFail: Boolean = false
) extends L1CacheParameters {
  require(isPow2(nSets), s"nSets($nSets) must be pow2")
  require(isPow2(nWays), s"nWays($nWays) must be pow2")
  require(isPow2(rowBits), s"rowBits($rowBits) must be pow2")
  require(isPow2(blockBytes), s"blockBytes($blockBytes) must be pow2")
  require(rowBits < blockBytes * 8, s"rowBits($rowBits) must be less than blockBits(${blockBytes * 8})")
}

trait HasICacheParameters extends HasFrontendParameters with HasL1CacheParameters {
  def icacheParameters: ICacheParameters = frontendParameters.icacheParameters

  // implement cacheParams to use HasL1CacheParameters trait
  val cacheParams: ICacheParameters = icacheParameters
  // and nSets, nWays, rowBits, blockBytes, etc. are inherited from HasL1CacheParameters

  // If untagBits(idxBits + blockOffBits) is longer than pgIdxBits, different vSets may map to a same physical page,
  // we need to give L2 cache alias tag to resolve the aliasing problem
  // e.g. By default, we have 256 sets, 64B block, so idxBits = log2(256) = 8bits, blockOffBits = log2(64) = 6bits,
  //      and, pgIdxBits = log2(4096) = 12bits,
  //      in this case, we need vAddr(8+6-1, 12) as alias tag, it's 2 bits long
  // Also refer to https://xiangshan-doc-en.readthedocs.io/en/latest/huancun/cache_alias/
  // Actually, ICache aliasing may not be a functional problem, as its cached data is never dirty,
  // and, currently ICache is using software fence.i to ensure coherency, so we can assume cached data is always newest,
  // even when we support hardware coherency later, we can simply flush all aliased sets when receiving a probe request,
  // therefore, this may not be necessary, but anyway, we still implement it to keep consistent with DCache.
  def AliasTagBits: Option[Int] = Option.when(untagBits > pgIdxBits)(untagBits - pgIdxBits)

  def PortNumber: Int = icacheParameters.PortNumber

  // maybeRvc
  def MaxInstNumPerBlock: Int = blockBytes / instBytes
  def MaxInstNumPerBank:  Int = MaxInstNumPerBlock / DataBanks

  // replacer
  def Replacer: String = icacheParameters.Replacer

  // missUnit
  def NumFetchMshr:    Int = icacheParameters.NumFetchMshr
  def NumPrefetchMshr: Int = icacheParameters.NumPrefetchMshr

  // wayLookup
  def WayLookupSize: Int = icacheParameters.WayLookupSize

  // metaArray w/ parity
  def MetaEcc:       String = icacheParameters.MetaEcc
  def MetaCode:      Code   = Code.fromString(MetaEcc)
  def MetaBits:      Int    = (new ICacheMetadata).getWidth
  def MetaEntryBits: Int    = MetaCode.width(MetaBits)
  def MetaEccBits:   Int    = MetaEntryBits - MetaBits

  // dataArray w/ parity
  def DataEcc:         String = icacheParameters.DataEcc
  def DataEccUnit:     Int    = icacheParameters.DataEccUnit.getOrElse(blockBytes)
  def DataPaddingBits: Int    = icacheParameters.DataPaddingBits
  def DataBanks:       Int    = blockBits / rowBits
  def DataCode:        Code   = Code.fromString(DataEcc)
  def ICacheDataBits:  Int    = rowBits // FIXME: this should be "DataBits", but it's already declared in XSParameters
  def DataEccSegments: Int    = math.ceil(ICacheDataBits / DataEccUnit).toInt
  def DataEccBitsPerSegment: Int = DataCode.width(DataEccUnit) - DataEccUnit // ecc bits per segment
  def DataEccBits:           Int = DataEccSegments * DataEccBitsPerSegment
  def DataEntryBits:         Int = ICacheDataBits + DataEccBits
  def DataSramWidth:         Int = DataEntryBits + DataPaddingBits

  // submodule enable
  def EnableCtrlUnit: Boolean = icacheParameters.EnableCtrlUnit

  // testing
  def ForceMetaEccFail: Boolean = icacheParameters.ForceMetaEccFail
  def ForceDataEccFail: Boolean = icacheParameters.ForceDataEccFail
}

// For users: these are default ICache parameters set by dev, do not change them here,
// use top-level Parameters.scala to change them on initialization.
case class ICacheCtrlUnitParameters(
    Address:   AddressSet = AddressSet(0x38022080, 0x7f),
    BeatBytes: Int = 8 // by default, we can transfer entire 8B data of one ctrlUnit register in one beat
) {
  require(BeatBytes > 0 && isPow2(BeatBytes), s"BeatBytes($BeatBytes) must be pow2 and > 0")
}

trait HasICacheCtrlUnitParameters extends HasICacheParameters {
  def ctrlUnitParameters: ICacheCtrlUnitParameters = icacheParameters.ctrlUnitParameters

  def Address:   AddressSet = ctrlUnitParameters.Address
  def BeatBytes: Int        = ctrlUnitParameters.BeatBytes
  def RegWidth:  Int        = XLEN // 64 bits
  def RegBytes:  Int        = RegWidth / 8

  def EccCtrlOffset:  Int = 0
  def EccIAddrOffset: Int = EccCtrlOffset + RegBytes
}
