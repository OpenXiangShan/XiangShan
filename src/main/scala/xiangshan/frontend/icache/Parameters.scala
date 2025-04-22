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
import freechips.rocketchip.util.BundleFieldBase
import freechips.rocketchip.util.SetAssocReplacementPolicy
import huancun.AliasField
import huancun.PrefetchField
import utility.Code
import utility.ReplacementPolicy
import utility.ReqSourceField
import utils.OptionWrapper
import xiangshan.cache.HasL1CacheParameters
import xiangshan.cache.L1CacheParameters

case class ICacheParameters(
    nSets:               Int = 256,
    nWays:               Int = 4,
    rowBits:             Int = 64,
    nTLBEntries:         Int = 32,
    tagECC:              Option[String] = None,
    dataECC:             Option[String] = None,
    replacer:            Option[String] = Option("random"),
    PortNumber:          Int = 2,
    nFetchMshr:          Int = 4,
    nPrefetchMshr:       Int = 10,
    nWayLookupSize:      Int = 32,
    DataCodeUnit:        Int = 64,
    ICacheDataBanks:     Int = 8,
    ICacheDataSRAMWidth: Int = 66,
    // TODO: hard code, need delete
    partWayNum:          Int = 4,
    blockBytes:          Int = 64,
    cacheCtrlAddressOpt: Option[AddressSet] = None
) extends L1CacheParameters {

  val setBytes:     Int         = nSets * blockBytes
  val aliasBitsOpt: Option[Int] = Option.when(setBytes > pageSize)(log2Ceil(setBytes / pageSize))
  val reqFields: Seq[BundleFieldBase] = Seq(
    PrefetchField(),
    ReqSourceField()
  ) ++ aliasBitsOpt.map(AliasField)
  val echoFields:  Seq[BundleFieldBase]      = Nil
  def tagCode:     Code                      = Code.fromString(tagECC)
  def dataCode:    Code                      = Code.fromString(dataECC)
  def replacement: SetAssocReplacementPolicy = ReplacementPolicy.fromString(replacer, nWays, nSets)
}

trait HasICacheParameters extends HasL1CacheParameters {
  val cacheParams: ICacheParameters = icacheParameters

  def ctrlUnitParamsOpt: Option[ICacheCtrlUnitParameters] = OptionWrapper(
    cacheParams.cacheCtrlAddressOpt.nonEmpty,
    ICacheCtrlUnitParameters(
      address = cacheParams.cacheCtrlAddressOpt.get,
      regWidth = XLEN
    )
  )

  def ICacheSets:          Int = cacheParams.nSets
  def ICacheWays:          Int = cacheParams.nWays
  def PortNumber:          Int = cacheParams.PortNumber
  def nFetchMshr:          Int = cacheParams.nFetchMshr
  def nPrefetchMshr:       Int = cacheParams.nPrefetchMshr
  def nWayLookupSize:      Int = cacheParams.nWayLookupSize
  def DataCodeUnit:        Int = cacheParams.DataCodeUnit
  def ICacheDataBanks:     Int = cacheParams.ICacheDataBanks
  def ICacheDataSRAMWidth: Int = cacheParams.ICacheDataSRAMWidth
  def partWayNum:          Int = cacheParams.partWayNum

  def ICacheMetaBits:      Int = (new ICacheMetadata).getWidth
  def ICacheMetaCodeBits:  Int = 1 // FIXME: unportable: maybe use cacheParams.tagCode.somemethod to get width
  def ICacheMetaEntryBits: Int = ICacheMetaBits + ICacheMetaCodeBits

  def ICacheDataBits: Int = blockBits / ICacheDataBanks
  def ICacheDataCodeSegs: Int =
    math.ceil(ICacheDataBits / DataCodeUnit).toInt // split data to segments for ECC checking
  def ICacheDataCodeBits: Int =
    ICacheDataCodeSegs * 1 // FIXME: unportable: maybe use cacheParams.dataCode.somemethod to get width
  def ICacheDataEntryBits: Int = ICacheDataBits + ICacheDataCodeBits
  def ICacheBankVisitNum:  Int = 32 * 8 / ICacheDataBits + 1
  def highestIdxBit:       Int = log2Ceil(nSets) - 1

  def MaxInstNumPerBlock: Int = blockBytes / instBytes

  require((ICacheDataBanks >= 2) && isPow2(ICacheDataBanks))
  require(ICacheDataSRAMWidth >= ICacheDataEntryBits)
  require(isPow2(ICacheSets), s"nSets($ICacheSets) must be pow2")
  require(isPow2(ICacheWays), s"nWays($ICacheWays) must be pow2")
}

// for ICacheCtrlUnit
case class ICacheCtrlUnitParameters(
    address:   AddressSet,
    regWidth:  Int,
    beatBytes: Int = 8
) {
  def RegBytes: Int = regWidth / 8

  def EccCtrlOffset:  Int = 0
  def EccIAddrOffset: Int = EccCtrlOffset + RegBytes
}
