// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import utility.SignExt
import xiangshan.backend.decode.isa.predecode.PreDecodeInst
import xiangshan.frontend.BrType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.icache.ICacheRespBundle

trait PreDecodeHelper extends HasIfuParameters {
  def isRVC(inst: UInt): Bool = inst(1, 0) =/= 3.U

  def isLink(reg: UInt): Bool = reg === 1.U || reg === 5.U

  def getBrType(inst: UInt): UInt =
    ListLookup(inst, List(BrType.NotCfi), PreDecodeInst.brTable).head

  def getBrInfo(inst: UInt): (UInt, Bool, Bool) = {
    val brType = getBrType(inst)
    val rd     = Mux(isRVC(inst), inst(12), inst(11, 7))
    val rs     = Mux(isRVC(inst), Mux(brType === BrType.Jal, 0.U, inst(11, 7)), inst(19, 15))
    val isCall = (brType === BrType.Jal && !isRVC(inst) || brType === BrType.Jalr) && isLink(rd) // Only for RV64
    val isRet  = brType === BrType.Jalr && isLink(rs) && !isCall
    (brType, isCall, isRet)
  }

  def getJalOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), VAddrBits))
  }

  def getBrOffset(inst: UInt, isRvc: Bool): PrunedAddr = {
    val rvcOffset = Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W))
    val rviOffset = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W))
    val maxWidth  = rviOffset.getWidth
    PrunedAddrInit(SignExt(Mux(isRvc, SignExt(rvcOffset, maxWidth), SignExt(rviOffset, maxWidth)), VAddrBits))
  }
}

trait FetchBlockHelper extends HasIfuParameters {
  def getBasicBlockIdx(pc: PrunedAddr, start: PrunedAddr): UInt = {
    val byteOffset = (pc - start).toUInt
    (byteOffset - instBytes.U)(FetchBlockInstOffsetWidth, instOffsetBits)
  }
}

trait IfuHelper extends HasIfuParameters with FetchBlockHelper {
  private object ShiftType {
    val NoShift     = 0.U(2.W)
    val ShiftRight1 = 1.U(2.W)
    val ShiftRight2 = 2.U(2.W)
    val ShiftRight3 = 3.U(2.W)
  }

  def genFetchBlockInfo(ftqFetch: Vec[FetchRequestBundle]): Vec[FetchBlockInfo] = {
    val fetchBlock = WireDefault(VecInit.fill(FetchPorts)(0.U.asTypeOf(new FetchBlockInfo)))
    for (i <- 0 until FetchPorts) {
      val cfiOffset = ftqFetch(i).takenCfiOffset.bits
      val taken     = ftqFetch(i).takenCfiOffset.valid
      val jumpRange = Fill(FetchBlockInstNum, !taken) | Fill(FetchBlockInstNum, 1.U(1.W)) >> ~cfiOffset
      val ftrRange = Fill(FetchBlockInstNum, taken) |
        Fill(FetchBlockInstNum, 1.U(1.W)) >> ~getBasicBlockIdx(ftqFetch(i).nextStartVAddr, ftqFetch(i).startVAddr)
      val instrRange = jumpRange & ftrRange
      val fetchSize = Mux(
        taken,
        cfiOffset + 1.U(log2Ceil(FetchBlockInstNum + 1).W),
        (ftqFetch(i).nextStartVAddr - ftqFetch(i).startVAddr)(log2Ceil(FetchBlockInstNum + 1) + 1, 1)
      )
      fetchBlock(i).ftqIdx             := ftqFetch(i).ftqIdx
      fetchBlock(i).doubleline         := ftqFetch(i).crossCacheline
      fetchBlock(i).takenCfiOffset     := ftqFetch(i).takenCfiOffset
      fetchBlock(i).instrRange         := instrRange
      fetchBlock(i).startVAddr         := ftqFetch(i).startVAddr
      fetchBlock(i).target             := ftqFetch(i).nextStartVAddr
      fetchBlock(i).pcHigh             := ftqFetch(i).startVAddr(VAddrBits - 1, PcCutPoint)
      fetchBlock(i).pcHighPlus1        := ftqFetch(i).startVAddr(VAddrBits - 1, PcCutPoint) + 1.U
      fetchBlock(i).fetchSize          := fetchSize
      fetchBlock(i).identifiedCfi      := ftqFetch(i).identifiedCfi
      fetchBlock(i).nextCachelineVAddr := ftqFetch(i).nextCachelineVAddr
    }
    fetchBlock
  }

  def isICacheMatch(fromICache: ICacheRespBundle, fetchBlock: Vec[FetchBlockInfo]): Bool =
    fromICache.vAddr(0) === fetchBlock(0).startVAddr &&
      (fromICache.doubleline && fromICache.vAddr(1) === fetchBlock(0).nextCachelineVAddr ||
        !fetchBlock(0).doubleline)

  def genICacheMeta(fromICache: ICacheRespBundle): ICacheMeta = {
    val cacheMeta = WireDefault(0.U.asTypeOf(new ICacheMeta))
    cacheMeta.exception          := fromICache.exception
    cacheMeta.pmpMmio            := fromICache.pmpMmio
    cacheMeta.itlbPbmt           := fromICache.itlbPbmt
    cacheMeta.isBackendException := fromICache.isBackendException
    cacheMeta.pAddr              := fromICache.pAddr
    cacheMeta.gpAddr             := fromICache.gpAddr
    cacheMeta.isForVSnonLeafPTE  := fromICache.isForVSnonLeafPTE
    cacheMeta
  }

  def mergeInstrRange(needMerge: Bool, firstRange: UInt, secondRange: UInt, firstSize: UInt): UInt = {
    val totalRange = Mux(needMerge, (secondRange << firstSize) | firstRange, firstRange)
    totalRange
  }

  def genPredMask(
      firstFlag:  Bool,
      firstIdx:   UInt,
      secondFlag: Bool,
      secondIdx:  UInt,
      select:     Vec[Bool]
  ): Vec[Bool] =
    VecInit.tabulate(FetchBlockInstNum) { i =>
      ((firstIdx === i.U) && !select(i) && firstFlag) || ((secondIdx === i.U) && select(i) && secondFlag)
    }

  def bitMask(index: UInt, blockSize: Int, numBlocks: Int): UInt = {
    val selectOH = UIntToOH(index)
    val blocks   = VecInit((0 until numBlocks).map(i => Mux(selectOH(i), Fill(blockSize, 1.U(1.W)), 0.U(blockSize.W))))
    Cat(blocks.reverse)
  }
  def catPC(low: UInt, high: UInt, high1: UInt): PrunedAddr =
    PrunedAddrInit(Mux(
      low(PcCutPoint),
      Cat(high1, low(PcCutPoint - 1, 0)),
      Cat(high, low(PcCutPoint - 1, 0))
    ))

  def catPC(lowVec: Vec[UInt], high: UInt, high1: UInt): Vec[PrunedAddr] =
    VecInit(lowVec.map(catPC(_, high, high1)))

  def cutICacheData(cacheline: UInt): Vec[UInt] = {
    // FIXME: !HasCExtension
    require(HasCExtension)
    val result  = Wire(Vec(ICacheLineBytes / 2, UInt(32.W)))
    val dataVec = cacheline.asTypeOf(Vec(ICacheLineBytes / 2, UInt(16.W)))
    (0 until ICacheLineBytes / 2 - 1).foreach(i =>
      result(i) := Cat(dataVec(i + 1), dataVec(i))
    )
    result(ICacheLineBytes / 2 - 1) := Cat(dataVec(0), dataVec(ICacheLineBytes / 2 - 1))
    result
  }

  def alignData[T <: Data](indataVec: Vec[T], shiftNum: UInt, default: T): Vec[T] = {
    require(shiftNum.getWidth == 2)
    val dataVec = VecInit((0 until IBufferEnqueueWidth).map(i =>
      if (i < indataVec.length) indataVec(i) else 0.U.asTypeOf(default)
    ))
    VecInit((0 until IBufferEnqueueWidth).map { i =>
      MuxLookup(shiftNum, 0.U.asTypeOf(default))(Seq(
        ShiftType.NoShift     -> dataVec(i),
        ShiftType.ShiftRight1 -> (if (i == 0) 0.U.asTypeOf(default) else dataVec(i - 1)),
        ShiftType.ShiftRight2 -> (if (i < 2) 0.U.asTypeOf(default) else dataVec(i - 2)),
        ShiftType.ShiftRight3 -> (if (i < 3) 0.U.asTypeOf(default) else dataVec(i - 3))
      ))
    })
  }

  def alignInstrCompact(indata: InstrCompactBundle, shiftNum: UInt): InstrCompactBundle = {
    val out = Wire(new InstrCompactBundle(IBufEnqWidth))
    out.instrIndex     := alignData(indata.instrIndex, shiftNum, 0.U.asTypeOf(new InstrIndexEntry))
    out.instrIsRvc     := alignData(indata.instrIsRvc, shiftNum, false.B)
    out.selectBlock    := alignData(indata.selectBlock, shiftNum, false.B)
    out.instrPcLower   := alignData(indata.instrPcLower, shiftNum, 0.U((PcCutPoint + 1).W))
    out.instrEndOffset := alignData(indata.instrEndOffset, shiftNum, 0.U(log2Ceil(FetchBlockInstNum).W))
    out.identifiedCfi  := alignData(indata.identifiedCfi, shiftNum, false.B)
    out
  }
}
