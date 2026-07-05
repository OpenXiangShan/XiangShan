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

trait ICacheDataHelper extends HasICacheParameters {
  def shiftSramMaybeRvc(
      maybeRvcMap: UInt,
      shiftNum: UInt,
      leftShift: Bool,
      secondShift: Boolean
  ): UInt = {
    val realShiftNum =
      if (secondShift) Cat(shiftNum(log2Ceil(MaxInstNumPerBlock) - 1, 2), 0.U(2.W)) else shiftNum(1, 0)
    val shifted = Mux(leftShift, maybeRvcMap << realShiftNum, maybeRvcMap >> realShiftNum)
    if (secondShift) shifted else shifted(MaxInstNumPerBlock - 1, 0)
  }

  def shiftSramMaybeRvcVec(
      maybeRvcMap: Vec[UInt],
      shiftNum: Vec[UInt],
      shiftFlag: Bool,
      secondShift: Boolean
  ): Vec[Vec[UInt]] = VecInit(
    VecInit(
      shiftSramMaybeRvc(maybeRvcMap(0), shiftNum(0), leftShift = false.B, secondShift = secondShift),
      shiftSramMaybeRvc(maybeRvcMap(1), shiftNum(1), leftShift = true.B, secondShift = secondShift)
    ),
    VecInit(
      shiftSramMaybeRvc(maybeRvcMap(2), shiftNum(2), leftShift = !shiftFlag, secondShift = secondShift),
      shiftSramMaybeRvc(maybeRvcMap(3), shiftNum(3), leftShift = true.B, secondShift = secondShift)
    )
  )

  def genMaybeRvcShiftInfo(
      req: Vec[FtqFetchRequest],
      wayLookupEntry: Vec[WayLookupEntry],
      firstFetchSize: UInt
  ): MaybeRvcShiftInfo = {
    val info = Wire(new MaybeRvcShiftInfo)

    val req0Start = req(0).startVAddr(log2Ceil(MaxInstNumPerBlock), 1)
    val req1Start = req(1).startVAddr(log2Ceil(MaxInstNumPerBlock), 1)
    val secondFetchSize = req(1).takenCfiOffset.bits +& 1.U
    val totalFetchSize  = secondFetchSize +& firstFetchSize
    val firstBlockRange = genInstRange(firstFetchSize)
    val totalBlockRange = Mux(req(1).valid, genInstRange(totalFetchSize), firstBlockRange)

    info.shiftNum(0)  := req0Start
    info.shiftNum(1)  := ~info.shiftNum(0)
    info.shiftFlag    := req1Start > firstFetchSize
    info.shiftNum(2)  := Mux(info.shiftFlag, req1Start - firstFetchSize, firstFetchSize - req1Start)
    info.shiftNum(3)  := ~req1Start + req(0).takenCfiOffset.bits

    info.shiftMaybeRvcMap(0) :=
      shiftSramMaybeRvc(
        wayLookupEntry(0).maybeRvcMap(0),
        info.shiftNum(0),
        leftShift = false.B,
        secondShift = false
      )
    info.shiftMaybeRvcMap(1) :=
      shiftSramMaybeRvc(
        Cat(wayLookupEntry(0).maybeRvcMap(1), 0.U(1.W)),
        info.shiftNum(1),
        leftShift = true.B,
        secondShift = false
      )
    info.shiftMaybeRvcMap(2) :=
      shiftSramMaybeRvc(
        wayLookupEntry(1).maybeRvcMap(0),
        info.shiftNum(2),
        leftShift = !info.shiftFlag,
        secondShift = false
      )
    info.shiftMaybeRvcMap(3) :=
      shiftSramMaybeRvc(
        Cat(wayLookupEntry(1).maybeRvcMap(1), 0.U(2.W)),
        info.shiftNum(3),
        leftShift = true.B,
        secondShift = false
      )

    info.rangeVec(0) := genInstRange(
      Mux(
        req(0).isCrossLine,
        MaxInstNumPerBlock.U - info.shiftNum(0),
        firstFetchSize
      )
    )
    info.rangeVec(1) := firstBlockRange & ~info.rangeVec(0)
    info.rangeVec(2) := Mux(
      req(1).valid,
      genInstRange(
        firstFetchSize +& Mux(
          req(1).isCrossLine,
          MaxInstNumPerBlock.U - req1Start,
          secondFetchSize
        )
      ) & ~firstBlockRange,
      0.U
    )
    info.rangeVec(3) := Mux(req(1).valid, totalBlockRange & ~(firstBlockRange | info.rangeVec(2)), 0.U)

    info
  }

  def genInstRange(size: UInt): UInt =
    Mux(size >= MaxInstNumPerBlock.U, (~0.U(MaxInstNumPerBlock.W)), UIntToMask(size, MaxInstNumPerBlock))

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

  def getBankSel(startPc: PrunedAddr, takenCfiOffset: UInt): (Bool, Vec[UInt]) = {
    val blockOffset        = startPc(blockOffBits - 1, 0)
    val blockEndOffsetTemp = blockOffset +& Cat(takenCfiOffset, 0.U(instOffsetBits.W))
    val blockEndOffset     = blockEndOffsetTemp(blockOffBits - 1, 0)
    val isCrossLine        = blockEndOffsetTemp(blockOffBits)
    val bankIdxLow         = getBankIdx(blockOffset)
    val bankIdxHigh        = getBankIdx(blockEndOffset)
    val bankSel = VecInit(
      // first line: if in same line, select [low, high], else select [low, end]
      VecInit((0 until DataBanks).map(i => (i.U >= bankIdxLow) && (isCrossLine || i.U <= bankIdxHigh))).asUInt,
      // second line: if in same line, select nothing, else select [start, high]
      VecInit((0 until DataBanks).map(i => (i.U <= bankIdxHigh) && isCrossLine)).asUInt
    )
    (isCrossLine, bankSel)
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

trait ICacheCacheLineHelper extends HasICacheParameters {
  def isCrossLine(startVAddr: PrunedAddr, takenCfiOffset: UInt): Bool = {
    require(FetchBlockSize <= blockBytes, "Cannot fetch more than one cache line in a fetch block")
    val startBlockOffset = startVAddr(blockOffBits - 1, instOffsetBits)
    val endBlockOffset   = startBlockOffset +& takenCfiOffset
    // if overflow, must be cross line
    endBlockOffset(blockOffBits - instOffsetBits)
  }
}
