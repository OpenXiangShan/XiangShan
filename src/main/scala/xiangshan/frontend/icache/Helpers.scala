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
import utility.UIntToMask
import xiangshan.frontend.FtqFetchRequest
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.HasBpuParameters

trait ICacheEccHelper extends HasICacheParameters {
  // per-port
  def encodeMetaEccByPort(meta: ICacheMetadata, poison: Bool = false.B): UInt = {
    val code = MetaCode.encode(meta.asUInt, poison) >> MetaBits
    code.asTypeOf(UInt(MetaEccBits.W))
  }

  // per-port
  def checkMetaEccByPort(meta: ICacheMetadata, code: UInt, waymask: UInt, enable: Bool): Bool = {
    require(code.getWidth == MetaEccBits)
    require(waymask.getWidth == nWays)
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
      waymaskVec: Vec[UInt],
      enable:     Bool,
      doubleline: Bool
  ): Vec[Bool] = {
    require(metaVec.length == PortNumber)
    require(codeVec.length == PortNumber)
    require(waymaskVec.length == PortNumber)
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
  def getWaymask(reqPTag: UInt, pTags: Vec[UInt], valids: Vec[Bool]): UInt =
    VecInit((pTags zip valids).map { case (pt, v) => v && pt === reqPTag }).asUInt

  def getWaymask(reqPTag: UInt, entries: Vec[Valid[ICacheMetaEntry]]): UInt =
    getWaymask(reqPTag, VecInit(entries.map(_.bits.meta.phyTag)), VecInit(entries.map(_.valid)))
}

trait ICacheDataHelper extends HasICacheParameters with ICacheCacheLineHelper {
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

  def getBankSel(startPc: PrunedAddr, endPosition: UInt): Vec[UInt] = {
    val bankIdxLow = startPc(blockOffBits - 1, rowOffBits)
    val (isCrossLine, bankIdxHigh) =
      if (useHalfAlignFastPath) {
        (
          super.isCrossLine(startPc, endPosition),
          Cat(
            startPc(blockOffBits - 1) ^ endPosition(CfiPositionWidth - 1),
            endPosition(CfiPositionWidth - 2, rowOffBits - instOffsetBits)
          )
        )
      } else {
        val (crossLine, endLineOffset) = getFetchBlockEndLineOffset(startPc, endPosition)
        (
          crossLine,
          endLineOffset(blockOffBits - instOffsetBits - 1, rowOffBits - instOffsetBits)
        )
      }
    VecInit(
      // first line: if in same line, select [low, high], else select [low, end]
      VecInit((0 until DataBanks).map(i => (i.U >= bankIdxLow) && (isCrossLine || i.U <= bankIdxHigh))).asUInt,
      // second line: if in same line, select nothing, else select [start, high]
      VecInit((0 until DataBanks).map(i => (i.U <= bankIdxHigh) && isCrossLine)).asUInt
    )
  }

  def getLineSel(blkOffset: UInt): Vec[Bool] = {
    val bankIdxLow = getBankIdx(blkOffset)
    VecInit((0 until DataBanks).map(i => i.U < bankIdxLow))
  }

  def getLineSel(startPc: PrunedAddr): Vec[Bool] = {
    val blockOffset = startPc(blockOffBits - 1, 0)
    val bankIdxLow  = getBankIdx(blockOffset)
    VecInit((0 until DataBanks).map(i => i.U < bankIdxLow))
  }
}

trait ICacheAddrHelper extends HasICacheParameters {
  def getBlkAddrFromPTag(vAddr: PrunedAddr, pTag: UInt): UInt =
    Cat(pTag, vAddr(pgUntagBits - 1, blockOffBits))

  def getGPAddr(gpAddrFromItlb: UInt, vAddr: PrunedAddr): UInt =
    Cat(gpAddrFromItlb(PAddrBitsMax - 1, PageOffsetWidth), vAddr(PageOffsetWidth - 1, 0))

  def getPTagFromBlk(blkAddr: UInt): UInt =
    (blkAddr >> (pgUntagBits - blockOffBits)).asUInt

  def getIdxFromBlk(blkAddr: UInt): UInt =
    blkAddr(idxBits - 1, 0)

  def getPAddrFromPTag(vAddr: PrunedAddr, pTag: UInt): PrunedAddr =
    PrunedAddrInit(Cat(pTag, vAddr(pgUntagBits - 1, 0)))

  def getInterleavedBankIdx(vSetIdx: UInt): UInt =
    vSetIdx(InterleavedBankIdxBits - 1, 0)

  def getInterleavedSetIdx(vSetIdx: UInt): UInt =
    vSetIdx(idxBits - 1, InterleavedBankIdxBits)
}

trait ICacheMissUpdateHelper extends HasICacheParameters with ICacheEccHelper with ICacheAddrHelper {
  def updateMetaInfo(
      update:  Valid[MissRespBundle],
      vSetIdx: UInt,
      pTag:    UInt,
      info:    MetaInfo
  ): (Bool, MetaInfo) = {
    val newInfo  = WireInit(info)
    val valid    = update.valid && !update.bits.corrupt
    val vSetSame = update.bits.vSetIdx === vSetIdx
    val pTagSame = getPTagFromBlk(update.bits.blkPAddr) === pTag
    val waySame  = update.bits.waymask === info.waymask
    when(valid && vSetSame) {
      when(pTagSame) {
        // vSetIdx & pTag match => update has newer data
        newInfo.waymask := update.bits.waymask
        // also update maybeRvcMap and ecc code
        newInfo.maybeRvcMap := update.bits.maybeRvcMap
        // we have getPhyTagFromBlk(fromMSHR.bits.blkPAddr) === pTag, so we can use pTag directly for better timing
        newInfo.metaCodes := encodeMetaEccByPort(ICacheMetadata(pTag, update.bits.maybeRvcMap))
      }.elsewhen(waySame) {
        // vSetIdx & way match, but pTag not match => older hit data has been replaced, treat as a miss
        newInfo.waymask := 0.U
        // we don't care about maybeRvcMap/code, since it's not used for a missed request
      }
      // otherwise is an irrelevant update, ignore it
    }
    val updated = valid && vSetSame && (pTagSame || waySame)
    (updated, newInfo)
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

trait ICacheCacheLineHelper extends HasICacheParameters with HasBpuParameters {
  protected def useHalfAlignFastPath: Boolean =
    FetchBlockSize == blockBytes && FetchBlockAlignSize == FetchBlockSize / 2

  def getFetchBlockEndLineOffset(startPc: PrunedAddr, endPosition: UInt): (Bool, UInt) = {
    require(FetchBlockSize <= blockBytes, "Cannot fetch more than one cache line in a fetch block")
    val endOffset              = endPosition - startPc(FetchBlockAlignWidth - 1, instOffsetBits)
    val startLineOffset        = startPc(blockOffBits - 1, instOffsetBits)
    val endLineOffsetWithCarry = startLineOffset +& endOffset
    val isCrossLine            = endLineOffsetWithCarry(blockOffBits - instOffsetBits)
    val endLineOffset          = endLineOffsetWithCarry(blockOffBits - instOffsetBits - 1, 0)
    (isCrossLine, endLineOffset)
  }

  def isCrossLine(startPc: PrunedAddr, endPosition: UInt): Bool =
    // Fast path for the default configuration: 64B fetch block, 32B half-align.
    if (useHalfAlignFastPath) {
      startPc(blockOffBits - 1) && endPosition(CfiPositionWidth - 1)
    } else {
      // Generic fallback for other align sizes.
      getFetchBlockEndLineOffset(startPc, endPosition)._1
    }
}

trait ICacheMaybeRvcHelper extends HasICacheParameters with ICacheCacheLineHelper with HalfAlignHelper {
  def shiftMaybeRvc(
      maybeRvcMap: UInt,
      shiftNum:    UInt,
      leftShift:   Bool
  ): UInt = Mux(leftShift, maybeRvcMap << shiftNum, maybeRvcMap >> shiftNum)(MaxInstNumPerBlock - 1, 0)

  def genInstRange(size: UInt): UInt =
    Mux(size >= MaxInstNumPerBlock.U, ~0.U(MaxInstNumPerBlock.W), UIntToMask(size, MaxInstNumPerBlock))

  def genMaybeRvcShiftInfo(
      req:            Vec[FtqFetchRequest],
      wayLookupEntry: Vec[WayLookupEntry]
  ): MaybeRvcShiftInfo = {
    val info = Wire(new MaybeRvcShiftInfo)

    val reqStart         = VecInit(req.map(_.startVAddr(log2Ceil(MaxInstNumPerBlock), 1)))
    val takenCfiOffset   = VecInit(req.map(req => getFtqOffset(req.startVAddr, req.endPosition)))
    val blockIsCrossLine = VecInit(req.map(req => isCrossLine(req.startVAddr, req.endPosition)))
    val fetchSize        = VecInit(takenCfiOffset.map(_ +& 1.U))
    val firstFetchSize   = fetchSize(0)
    val secondFetchSize  = fetchSize(1)
    val totalFetchSize   = firstFetchSize +& secondFetchSize
    val firstBlockRange  = genInstRange(firstFetchSize)
    val totalBlockRange  = Mux(req(1).valid, genInstRange(totalFetchSize), firstBlockRange)

    info.firstBlockRange := firstBlockRange
    info.totalBlockRange := totalBlockRange
    info.takenCfiOffset  := takenCfiOffset

    // Line 0 starts at req0.start, so shift right to align its first valid bit to bit 0.
    info.shiftNum(0) := reqStart(0)

    // Line 1 is the cross-line tail of req0. Its valid bits start at bit 0 and are placed
    // after the line-0 fragment. The extra +1 shift is encoded by Cat(map, 0) below.
    info.shiftNum(1) := ~reqStart(0)

    // Line 2 belongs to req1's first cache line. It may be before or after the end of req0,
    // so shiftFlag selects whether it should move right or left.
    info.shiftFlag   := reqStart(1) > firstFetchSize
    info.shiftNum(2) := Mux(info.shiftFlag, reqStart(1) - firstFetchSize, firstFetchSize - reqStart(1))

    // Line 3 is the cross-line tail of req1. The extra +2 shift is encoded by Cat(map, 0.U(2.W)) below.
    info.shiftNum(3) := ~reqStart(1) + takenCfiOffset(0)

    // Apply the low bits in s0 so the registered map only needs coarse shifts in s1.
    info.sramShiftMaybeRvc(0)(0) := shiftMaybeRvc(
      wayLookupEntry(0).maybeRvcMap(0),
      info.shiftNum(0),
      leftShift = false.B
    )
    info.sramShiftMaybeRvc(0)(1) :=
      shiftMaybeRvc(
        Cat(wayLookupEntry(0).maybeRvcMap(1), 0.U(1.W)),
        info.shiftNum(1),
        leftShift = true.B
      )
    info.sramShiftMaybeRvc(1)(0) :=
      shiftMaybeRvc(
        wayLookupEntry(1).maybeRvcMap(0),
        info.shiftNum(2),
        leftShift = !info.shiftFlag
      )
    info.sramShiftMaybeRvc(1)(1) :=
      shiftMaybeRvc(
        Cat(wayLookupEntry(1).maybeRvcMap(1), 0.U(2.W)),
        info.shiftNum(3),
        leftShift = true.B
      )

    info.maybeRvcMaskVec(0)(0) := genInstRange(
      Mux(
        blockIsCrossLine(0),
        MaxInstNumPerBlock.U - info.shiftNum(0),
        firstFetchSize
      )
    )
    info.maybeRvcMaskVec(0)(1) := firstBlockRange & ~info.maybeRvcMaskVec(0)(0)
    info.maybeRvcMaskVec(1)(0) := Mux(
      req(1).valid,
      genInstRange(
        firstFetchSize +& Mux(
          blockIsCrossLine(1),
          MaxInstNumPerBlock.U - reqStart(1),
          secondFetchSize
        )
      ) & ~firstBlockRange,
      0.U
    )
    info.maybeRvcMaskVec(1)(1) := Mux(
      req(1).valid,
      totalBlockRange & ~(firstBlockRange | info.maybeRvcMaskVec(1)(0)),
      0.U
    )
    info
  }
}
