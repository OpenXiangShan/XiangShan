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
  // per-port
  def encodeMetaEccByPort(meta: ICacheMetadata, poison: Bool = false.B): UInt = {
    val code = MetaCode.encode(meta.asUInt, poison) >> MetaBits
    code.asTypeOf(UInt(MetaEccBits.W))
  }

  // per-port
  def checkMetaEccByPort(meta: ICacheMetadata, code: UInt, waymask: Vec[Bool], enable: Bool): Bool = {
    require(code.getWidth == MetaEccBits)
    val hitNum = PopCount(waymask)
    // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
    // hit one way, but parity code does not match => ECC failure
    val corrupt = encodeMetaEccByPort(meta) =/= code && hitNum === 1.U
    // hit multi-way => must be an ECC failure
    val multiHit = hitNum > 1.U
    enable && (corrupt || multiHit)
  }

  // all ports
  def checkMetaEcc(
      metaVec:    Vec[ICacheMetadata],
      codeVec:    Vec[UInt],
      waymaskVec: Vec[Vec[Bool]],
      enable:     Bool,
      doubleline: Bool
  ): Vec[Bool] = {
    require(metaVec.length == PortNumber)
    require(codeVec.length == PortNumber)
    require(waymaskVec.length == PortNumber)
    require(waymaskVec.head.length == nWays)
    VecInit((metaVec zip codeVec zip waymaskVec).zipWithIndex.map { case (((meta, code), mask), i) =>
      val needThisLine = if (i == 0) true.B else doubleline
      checkMetaEccByPort(meta, code, mask, enable) && needThisLine
    })
  }

  // per-bank
  def encodeDataEccByBank(data: UInt, poison: Bool = false.B): UInt = {
    require(data.getWidth == ICacheDataBits)
    val datas = data.asTypeOf(Vec(DataEccSegments, UInt((ICacheDataBits / DataEccSegments).W)))
    val codes = VecInit(datas.map(DataCode.encode(_, poison) >> (ICacheDataBits / DataEccSegments)))
    codes.asTypeOf(UInt(DataEccBits.W))
  }

  def checkDataEccByBank(data: UInt, code: UInt, enable: Bool): Bool = {
    require(data.getWidth == ICacheDataBits)
    require(code.getWidth == DataEccBits)
    enable && (encodeDataEccByBank(data) =/= code)
  }

  // all banks
  def checkDataEcc(
      data:      Vec[UInt],
      code:      Vec[UInt],
      enable:    Bool,
      bankSel:   Vec[Vec[Bool]],
      bankValid: Vec[Bool],
      portHit:   Vec[Bool]
  ): Vec[Bool] = {
    require(data.length == DataBanks)
    require(code.length == DataBanks)
    require(bankSel.length == PortNumber)
    require(bankSel.head.length == DataBanks)
    require(bankValid.length == DataBanks)
    require(portHit.length == PortNumber)

    val bankCorrupt = VecInit((data zip code).map { case (d, c) =>
      checkDataEccByBank(d, c, enable)
    })

    VecInit((bankSel zip portHit).map { case (bs, h) =>
      // port is corrupted iff: any bank:
      //   is corrupted && is selected in this port && is valid (not from Mshr)
      // && port is hit
      VecInit((bankCorrupt zip bs zip bankValid).map { case ((c, s), v) =>
        c && s && v
      }).reduce(_ || _) && h
    })
  }
}

trait ICacheMetaHelper extends HasICacheParameters {
  def getWaymask(reqPTag: UInt, metaPTag: Vec[UInt], metaValid: Vec[Bool]): UInt = {
    require(metaPTag.length == nWays)
    require(metaValid.length == nWays)
    VecInit((metaPTag zip metaValid).map { case (wayPTag, wayValid) =>
      wayValid && (wayPTag === reqPTag)
    }).asUInt
  }

  def getWaymask(reqPTagVec: Vec[UInt], metaPTagVec: Vec[Vec[UInt]], metaValidVec: Vec[Vec[Bool]]): Vec[UInt] =
    VecInit((reqPTagVec zip metaPTagVec zip metaValidVec).map { case ((reqPTag, metaPTag), metaValid) =>
      getWaymask(reqPTag, metaPTag, metaValid)
    })
}

trait ICacheDataHelper extends HasICacheParameters {
  def getBankIdx(blkOffset: UInt): UInt =
    (blkOffset >> rowOffBits).asUInt

  def getBankValid(portValid: Vec[Bool], blkOffset: UInt): Vec[Bool] = {
    require(portValid.length == PortNumber)
    val bankIdxLow = getBankIdx(blkOffset)
    VecInit((0 until DataBanks).map(i => (i.U >= bankIdxLow) && portValid(0) || (i.U < bankIdxLow) && portValid(1)))
  }

  def getBankSel(blkOffset: UInt, blkEndOffset: UInt, crossLine: Bool): Vec[Vec[Bool]] = {
    val bankIdxLow  = getBankIdx(blkOffset)
    val bankIdxHigh = getBankIdx(blkEndOffset)
    VecInit(
      // first line: if in same line, select [low, high], else select [low, end]
      VecInit((0 until DataBanks).map(i => (i.U >= bankIdxLow) && (crossLine || i.U <= bankIdxHigh))),
      // second line: if in same line, select nothing, else select [start, high]
      VecInit((0 until DataBanks).map(i => (i.U <= bankIdxHigh) && crossLine))
    )
  }

  def getLineSel(blkOffset: UInt): Vec[Bool] = {
    val bankIdxLow = getBankIdx(blkOffset)
    VecInit((0 until DataBanks).map(i => i.U < bankIdxLow))
  }
}

trait ICacheAddrHelper extends HasICacheParameters {
  def getBlkAddrFromPTag(vAddr: PrunedAddr, pTag: UInt): UInt =
    Cat(pTag, vAddr(pgUntagBits - 1, blockOffBits))

  def getPTagFromBlk(blkAddr: UInt): UInt =
    (blkAddr >> (pgUntagBits - blockOffBits)).asUInt

  def getIdxFromBlk(blkAddr: UInt): UInt =
    blkAddr(idxBits - 1, 0)

  def getPAddrFromPTag(vAddr: PrunedAddr, pTag: UInt): PrunedAddr =
    PrunedAddrInit(Cat(pTag, vAddr(pgUntagBits - 1, 0)))
}

trait ICacheMissUpdateHelper extends HasICacheParameters with ICacheEccHelper with ICacheAddrHelper {
  def updateMetaInfo(
      update:      Valid[MissRespBundle],
      waymask:     UInt,
      vSetIdx:     UInt,
      pTag:        UInt,
      maybeRvcMap: UInt,
      code:        UInt
  ): (Bool, UInt, UInt, UInt) = {
    require(waymask.getWidth == nWays)
    val newMask        = WireInit(waymask)
    val newMaybeRvcMap = WireInit(maybeRvcMap)
    val newCode        = WireInit(code)
    val valid          = update.valid && !update.bits.corrupt
    val vSetSame       = update.bits.vSetIdx === vSetIdx
    val pTagSame       = getPTagFromBlk(update.bits.blkPAddr) === pTag
    val waySame        = update.bits.waymask === waymask
    when(valid && vSetSame) {
      when(pTagSame) {
        // vSetIdx & pTag match => update has newer data
        newMask := update.bits.waymask
        // also update maybeRvcMap and ecc code
        newMaybeRvcMap := update.bits.maybeRvcMap
        // we have getPhyTagFromBlk(fromMSHR.bits.blkPAddr) === pTag, so we can use pTag directly for better timing
        newCode := encodeMetaEccByPort(ICacheMetadata(pTag, update.bits.maybeRvcMap))
      }.elsewhen(waySame) {
        // vSetIdx & way match, but pTag not match => older hit data has been replaced, treat as a miss
        newMask := 0.U
        // we don't care about maybeRvcMap/code, since it's not used for a missed request
      }
      // otherwise is an irrelevant update, ignore it
    }
    val updated = valid && vSetSame && (pTagSame || waySame)
    (updated, newMask, newMaybeRvcMap, newCode)
  }

  def checkMshrHit(
      update:       Valid[MissRespBundle],
      vSetIdx:      UInt,
      pTag:         UInt,
      valid:        Bool,
      allowCorrupt: Boolean = false
  ): Bool =
    valid &&
      update.valid &&
      vSetIdx === update.bits.vSetIdx &&
      pTag === getPTagFromBlk(update.bits.blkPAddr) &&
      (if (allowCorrupt) true.B else !update.bits.corrupt)

  def checkMshrHitVec(
      update:       Valid[MissRespBundle],
      vSetIdxVec:   Vec[UInt],
      pTag:         UInt,
      validVec:     Vec[Bool],
      allowCorrupt: Boolean = false
  ): Vec[Bool] =
    VecInit((vSetIdxVec zip validVec).map { case (vs, v) =>
      checkMshrHit(update, vs, pTag, v, allowCorrupt)
    })
}

trait ICacheCacheLineHelper extends HasICacheParameters {
  def isCrossLine(startVAddr: PrunedAddr, takenCfiOffset: UInt): Bool = {
    require(FetchBlockSize <= blockBytes, "Cannot fetch more than one cache line in a fetch block")
    val startBlockOffset = startVAddr(blockOffBits - 1, instOffsetBits)
    val endBlockOffset   = startBlockOffset +& takenCfiOffset
    // if overflow, must be cross line
    endBlockOffset(blockOffBits - instOffsetBits)
  }
}
