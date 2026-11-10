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

  def genInstRange(size: UInt): UInt = {
    // For max width = 2^N, shift amount bit-width (N+1) causes extra Mux stage.
    // Since any value >= 2^N saturates to all-1, we check the MSB (size[N]) first.
    // If set -> output all ones; else use lower N bits for UIntToMask.
    // This reduces shifter width from (N+1) to N, improving timing.
    require(isPow2(MaxInstNumPerBlock), s"MaxInstNumPerBlock ($MaxInstNumPerBlock) must be a power of two")
    val sizeExt = size.pad(log2Ceil(MaxInstNumPerBlock) + 1) // Adapt to varying FetchSize values
    val range = Mux(
      sizeExt(log2Ceil(MaxInstNumPerBlock)),
      Fill(MaxInstNumPerBlock, true.B),
      UIntToMask(sizeExt(log2Ceil(MaxInstNumPerBlock) - 1, 0), MaxInstNumPerBlock)
    )
    range
  }
  def genMaybeRvcAlignInfo(
      req:            Vec[FtqFetchRequest],
      wayLookupEntry: Vec[WayLookupEntry]
  ): MaybeRvcAlignInfo = {
    val info = Wire(new MaybeRvcAlignInfo)

    val reqStart        = VecInit(req.map(_.startVAddr(log2Ceil(MaxInstNumPerBlock), 1)))
    val takenCfiOffset  = VecInit(req.map(req => getFtqOffset(req.startVAddr, req.endPosition)))
    val fetchSize       = VecInit(takenCfiOffset.map(_ +& 1.U))
    val totalFetchSize  = fetchSize(0) +& fetchSize(1)
    val firstBlockRange = genInstRange(fetchSize(0))
    // Keep req(1).valid out of this s0 timing path. totalBlockRange is selected
    // with the registered twoFetchValid in MainPipe before being sent to IFU.
    val totalBlockRange = genInstRange(totalFetchSize)

    info.firstBlockRange := firstBlockRange
    info.totalBlockRange := totalBlockRange
    info.takenCfiOffset  := takenCfiOffset

    // Line 0 starts at req0.start, so shift right to align its first valid bit to bit 0.
    info.shiftNum(0) := reqStart(0)

    // Line 1 is the cross-line tail of req0. Its valid bits start at bit 0 and are placed
    // after the line-0 fragment. The extra +1 shift is encoded by Cat(map, 0) below.
    info.shiftNum(1) := ~reqStart(0)

    // Line 2 belongs to req1's first cache line. It may be before or after the end of req0,
    // so shouldShiftRight selects whether it should move right or left.
    info.shouldShiftRight := reqStart(1) > fetchSize(0)
    info.shiftNum(2)      := Mux(info.shouldShiftRight, reqStart(1) - fetchSize(0), fetchSize(0) - reqStart(1))

    // Line 3 is the cross-line tail of req1. The extra +2 shift is encoded by Cat(map, 0.U(2.W)) below.
    info.shiftNum(3) := ~reqStart(1) + takenCfiOffset(0)

    // Pre-align each raw SRAM per-line map into the fetch coordinate here in s0
    // (the SRAM path is fully aligned in s0 and registered; the MSHR path reuses
    // the same shiftNum/shouldShiftRight to align the missUnit map in s1).
    // shiftConfig describes each maybeRvcMap: (extraShiftNum, shiftLeft).
    info.shiftConfig.zipWithIndex.foreach { case (c, i) =>
      val reqIdx  = i / PortNumber
      val portIdx = i % PortNumber
      info.sramAlignedMaybeRvcMap(reqIdx)(portIdx) := shiftMaybeRvc(
        Cat(wayLookupEntry(reqIdx).maybeRvcMap(portIdx), 0.U(c._1.W)),
        info.shiftNum(i),
        leftShift = c._2
      )
    }

    // The following masks are all in the "combined coordinate" space:
    // bit 0 corresponds to the first valid instruction slot of req0,
    // consistent with the coordinate after aligning by sramAlignedMaybeRvcMap / mshrAlignedMaybeRvcMap.
    //
    // (0)(0): instructions of req0 in line0.
    //   genInstRange(N - shiftNum(0)): all slots from req0 start to the end of line0.
    //   & firstBlockRange clamps to req0's actual fetch range, i.e., min(N - reqStart(0), fetchSize(0)).
    info.alignedMaybeRvcMaskVec(0)(0) := genInstRange(MaxInstNumPerBlock.U - info.shiftNum(0)) & firstBlockRange
    // (0)(1): req0's tail across line1 = the part of firstBlockRange not covered by line0.
    info.alignedMaybeRvcMaskVec(0)(1) := firstBlockRange & ~info.alignedMaybeRvcMaskVec(0)(0)

    // req1 starts fetching at combined coordinate fetchSize(0). Its line0 can occupy at most N - reqStart(1) slots,
    // so the upper bound for line0 mask is fetchSize(0) + (N - reqStart(1)).
    // By the invariant fetchSize(0) + fetchSize(1) <= N (see constraints in Ftq), we have:
    //   1) This upper bound <= 2*N, truncating to log2Ceil(N)+1 bits covers all valid values.
    //   2) The only value that gets truncated is 2*N, which occurs iff fetchSize(0)=N and reqStart(1)=0,
    //      in which case fetchSize(1)=0 and both masks are already zero. Thus truncation is safe,
    //      and also reduces shifter width to improve timing.
    val req1Line0End = (MaxInstNumPerBlock.U - reqStart(1) + fetchSize(0))(log2Ceil(MaxInstNumPerBlock), 0)
    // (1)(0): instructions of req1 in line0 = [fetchSize(0), min(req1Line0End, fetchSize(0)+fetchSize(1))-1].
    //   genInstRange(req1Line0End) provides the upper bound; & totalBlockRange clamps to the total fetch range.
    //   & ~firstBlockRange removes req0's part.
    info.alignedMaybeRvcMaskVec(1)(0) := genInstRange(req1Line0End) & totalBlockRange & ~firstBlockRange
    // (1)(1): req1's tail in line1 = the part of totalBlockRange not covered by firstBlockRange and (1)(0).
    info.alignedMaybeRvcMaskVec(1)(1) := totalBlockRange & ~(firstBlockRange | info.alignedMaybeRvcMaskVec(1)(0))
    info
  }
}
