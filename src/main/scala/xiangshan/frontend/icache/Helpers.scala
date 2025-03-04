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
  def encodeMetaEccByPort(meta: UInt, poison: Bool = false.B): UInt = {
    require(meta.getWidth == ICacheMetaBits)
    val code = cacheParams.tagCode.encode(meta, poison) >> ICacheMetaBits
    code.asTypeOf(UInt(ICacheMetaCodeBits.W))
  }

  // per-port
  def checkMetaEccByPort(meta: UInt, code: UInt, waymask: Vec[Bool], enable: Bool): Bool = {
    require(meta.getWidth == ICacheMetaBits)
    require(code.getWidth == ICacheMetaCodeBits)
    val hitNum = PopCount(waymask)
    // NOTE: if not hit, encodeMetaECC(meta) =/= code can also be true, but we don't care about it
    // hit one way, but parity code does not match => ECC failure
    val corrupt = encodeMetaEccByPort(meta) =/= code && hitNum === 1.U
    // hit multi-way => must be an ECC failure
    val multiHit = hitNum > 1.U
    enable && (corrupt || multiHit)
  }

  // all ports
  def checkMetaEcc(metaVec: Vec[UInt], codeVec: Vec[UInt], waymaskVec: Vec[Vec[Bool]], enable: Bool): Vec[Bool] = {
    require(metaVec.length == PortNumber)
    require(codeVec.length == PortNumber)
    require(waymaskVec.length == PortNumber)
    require(waymaskVec.head.length == nWays)
    VecInit((metaVec zip codeVec zip waymaskVec).map { case ((meta, code), mask) =>
      checkMetaEccByPort(meta, code, mask, enable)
    })
  }

  // per-bank
  def encodeDataEccByBank(data: UInt, poison: Bool = false.B): UInt = {
    require(data.getWidth == ICacheDataBits)
    val datas = data.asTypeOf(Vec(ICacheDataCodeSegs, UInt((ICacheDataBits / ICacheDataCodeSegs).W)))
    val codes = VecInit(datas.map(cacheParams.dataCode.encode(_, poison) >> (ICacheDataBits / ICacheDataCodeSegs)))
    codes.asTypeOf(UInt(ICacheDataCodeBits.W))
  }

  def checkDataEccByBank(data: UInt, code: UInt, enable: Bool): Bool = {
    require(data.getWidth == ICacheDataBits)
    require(code.getWidth == ICacheDataCodeBits)
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
    require(data.length == ICacheDataBanks)
    require(code.length == ICacheDataBanks)
    require(bankSel.length == PortNumber)
    require(bankSel.head.length == ICacheDataBanks)
    require(bankValid.length == ICacheDataBanks)
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

trait ICacheDataSelHelper extends HasICacheParameters {
  def getBankSel(blkOffset: UInt, valid: Bool = true.B): Vec[Vec[Bool]] = {
    val bankOffBits = log2Ceil(blockBytes / ICacheDataBanks)
    val bankIdxLow  = (Cat(0.U(1.W), blkOffset) >> bankOffBits).asUInt
    val bankIdxHigh = ((Cat(0.U(1.W), blkOffset) + 32.U) >> bankOffBits).asUInt
    val bankSel     = VecInit((0 until ICacheDataBanks * 2).map(i => (i.U >= bankIdxLow) && (i.U <= bankIdxHigh)))
    assert(
      !valid || PopCount(bankSel) === ICacheBankVisitNum.U,
      "The number of bank visits must be %d, but bankSel=0x%x",
      ICacheBankVisitNum.U,
      bankSel.asUInt
    )
    bankSel.asTypeOf(UInt((ICacheDataBanks * 2).W)).asTypeOf(Vec(2, Vec(ICacheDataBanks, Bool())))
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
        newCode := encodeMetaEccByPort(pTag)
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
