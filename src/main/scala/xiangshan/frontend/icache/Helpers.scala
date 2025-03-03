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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

trait ICacheEccHelper extends HasICacheParameters {
  def encodeMetaEcc(meta: UInt, poison: Bool = false.B): UInt = {
    require(meta.getWidth == ICacheMetaBits)
    val code = cacheParams.tagCode.encode(meta, poison) >> ICacheMetaBits
    code.asTypeOf(UInt(ICacheMetaCodeBits.W))
  }

  def encodeDataEcc(data: UInt, poison: Bool = false.B): UInt = {
    require(data.getWidth == ICacheDataBits)
    val datas = data.asTypeOf(Vec(ICacheDataCodeSegs, UInt((ICacheDataBits / ICacheDataCodeSegs).W)))
    val codes = VecInit(datas.map(cacheParams.dataCode.encode(_, poison) >> (ICacheDataBits / ICacheDataCodeSegs)))
    codes.asTypeOf(UInt(ICacheDataCodeBits.W))
  }
}

trait ICacheDataSelHelper extends HasICacheParameters {
  def getBankSel(blkOffset: UInt, valid: Bool = true.B): Vec[UInt] = {
    val bankIdxLow  = (Cat(0.U(1.W), blkOffset) >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
    val bankIdxHigh = ((Cat(0.U(1.W), blkOffset) + 32.U) >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
    val bankSel     = VecInit((0 until ICacheDataBanks * 2).map(i => (i.U >= bankIdxLow) && (i.U <= bankIdxHigh)))
    assert(
      !valid || PopCount(bankSel) === ICacheBankVisitNum.U,
      "The number of bank visits must be %d, but bankSel=0x%x",
      ICacheBankVisitNum.U,
      bankSel.asUInt
    )
    bankSel.asTypeOf(UInt((ICacheDataBanks * 2).W)).asTypeOf(Vec(2, UInt(ICacheDataBanks.W)))
  }

  def getLineSel(blkOffset: UInt): Vec[Bool] = {
    val bankIdxLow = (blkOffset >> log2Ceil(blockBytes / ICacheDataBanks)).asUInt
    val lineSel    = VecInit((0 until ICacheDataBanks).map(i => i.U < bankIdxLow))
    lineSel
  }
}

trait ICacheAddrHelper extends HasICacheParameters {
  def getBlkAddr(addr: PrunedAddr): UInt =
    (addr >> blockOffBits).asUInt

  def getPTagFromBlk(blkAddr: UInt): UInt =
    (blkAddr >> (pgUntagBits - blockOffBits)).asUInt

  def getIdxFromBlk(blkAddr: UInt): UInt =
    blkAddr(idxBits - 1, 0)

  def getPAddrFromPTag(vAddr: PrunedAddr, pTag: UInt): PrunedAddr =
    PrunedAddrInit(Cat(pTag, vAddr(pgUntagBits - 1, 0)))

  def getPAddrFromPTag(vAddrVec: Vec[PrunedAddr], pTagVec: Vec[UInt]): Vec[PrunedAddr] =
    VecInit((vAddrVec zip pTagVec).map { case (vAddr, pTag) => getPAddrFromPTag(vAddr, pTag) })
}

trait ICacheMissUpdateHelper extends HasICacheParameters with ICacheEccHelper with ICacheAddrHelper {
  def updateMetaInfo(
      update:  Valid[MissRespBundle],
      waymask: UInt,
      vSetIdx: UInt,
      pTag:    UInt,
      code:    UInt
  ): (Bool, UInt, UInt) = {
    require(waymask.getWidth == nWays)
    val newMask  = WireInit(waymask)
    val newCode  = WireInit(code)
    val valid    = update.valid && !update.bits.corrupt
    val vSetSame = update.bits.vSetIdx === vSetIdx
    val pTagSame = getPTagFromBlk(update.bits.blkPAddr) === pTag
    val waySame  = update.bits.waymask === waymask
    when(valid && vSetSame) {
      when(pTagSame) {
        // vSetIdx & pTag match => update has newer data
        newMask := update.bits.waymask
        // also update meta_codes
        // we have getPhyTagFromBlk(fromMSHR.bits.blkPAddr) === pTag, so we can use pTag directly for better timing
        newCode := encodeMetaEcc(pTag)
      }.elsewhen(waySame) {
        // vSetIdx & way match, but pTag not match => older hit data has been replaced, treat as a miss
        newMask := 0.U
        // we don't care about newCode, since it's not used for a missed request
      }
      // otherwise is an irrelevant update, ignore it
    }
    val updated = valid && vSetSame && (pTagSame || waySame)
    (updated, newMask, newCode)
  }
}
