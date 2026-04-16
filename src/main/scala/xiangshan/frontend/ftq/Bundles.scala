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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utils.EnumUInt
import xiangshan.frontend.FtqFetchRequest
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPerfMeta
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.icache.{MetaInfo => ICacheMetaInfo}
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.ICacheCacheLineHelper
import xiangshan.frontend.icache.ICacheDataHelper
import xiangshan.frontend.icache.PrefetchReqBundle

class FtqEntry(implicit p: Parameters) extends FtqBundle {
  val startPc:        PrunedAddr  = PrunedAddr(VAddrBits)
  val takenCfiOffset: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
}

class MetaEntry(implicit p: Parameters) extends FtqBundle {
  val meta        = new BpuMeta
  val paddingBits = if (meta.getWidth % 4 != 0) Some(UInt((4 - meta.getWidth % 4).W)) else None
}

class ResolveEntry(implicit p: Parameters) extends FtqBundle {
  val ftqIdx:  FtqPtr     = new FtqPtr
  val flushed: Bool       = Bool()
  val startPc: PrunedAddr = PrunedAddr(VAddrBits)
  // TODO: Reconsider branch number
  val branches: Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
}

class FtqRead[T <: Data](private val gen: T)(implicit p: Parameters) extends FtqBundle {
  val valid  = Output(Bool())
  val ptr    = Output(new FtqPtr)
  val offset = Output(UInt(FetchBlockInstOffsetWidth.W))
  val data   = Input(gen)
  def apply(valid: Bool, ptr: FtqPtr, offset: UInt) = {
    this.valid  := valid
    this.ptr    := ptr
    this.offset := offset
    this.data
  }
}

class BpuFlushInfo(implicit p: Parameters) extends FtqBundle with HasCircularQueuePtrHelper {
  val s3 = Valid(new FtqPtr)

  def stage(idx: Int): Valid[FtqPtr] = {
    require(idx >= 3 && idx <= 3)
    idx match {
      case 3 => s3
    }
  }

  private def shouldFlushBy(src: Valid[FtqPtr], idxToFlush: FtqPtr, valid: Bool): Bool =
    valid && src.valid && !isAfter(src.bits, idxToFlush)

  def shouldFlushByStage3(idx: FtqPtr, valid: Bool): Bool = shouldFlushBy(s3, idx, valid)
}

class FtqToCtrlIO(implicit p: Parameters) extends FtqBundle {
  // write to backend pc mem
  val wen:     Bool       = Output(Bool())
  val ftqIdx:  UInt       = Output(UInt(FtqPtr.width.W))
  val startPc: PrunedAddr = Output(PrunedAddr(VAddrBits))
}

class PerfMeta(implicit p: Parameters) extends FtqBundle {
  val bpuPerf: BpuPerfMeta = new BpuPerfMeta

  // Whether a position is a Control-Flow Instruction
  val isCfi: Vec[Bool] = Vec(FetchBlockInstNum, Bool())

  // This block mispredicted
  // no matter how many mispredictions happened before, count correct-path only
  val mispredict:           Bool       = Bool()
  val mispredictBranchInfo: BranchInfo = new BranchInfo()
}

class FtqToPrefetchBundle(implicit p: Parameters) extends FtqBundle {
  val req:             Vec[PrefetchReqBundle] = Vec(MaxPrefetchReqNum, new PrefetchReqBundle)
  val twoPrefetchCase: TwoPrefetchCase        = new TwoPrefetchCase
}

class FtqToMainPipeBundle(implicit p: Parameters) extends FtqBundle {
  val req: Vec[FtqFetchRequest] = Vec(MaxFetchReqNum, new FtqFetchRequest)
}

class FtqPrefetchReq(implicit p: Parameters) extends FtqBundle with ICacheCacheLineHelper {
  val startVAddr:     PrunedAddr = PrunedAddr(VAddrBits)
  val nextLineVAddr:  PrunedAddr = PrunedAddr(VAddrBits)
  val takenCfiOffset: UInt       = UInt(CfiPositionWidth.W)
  val isCrossLine:    Bool       = Bool()
  val vSetIdx:        Vec[UInt]  = Vec(MaxPrefetchReqNum, UInt(idxBits.W))
  val vPageNumber:    UInt       = UInt((VAddrBits - PageOffsetWidth).W)

  def fromFtqEntry(entry: FtqEntry): FtqPrefetchReq = {
    startVAddr     := entry.startPc
    nextLineVAddr  := entry.startPc + blockBytes.U
    takenCfiOffset := entry.takenCfiOffset.bits
    isCrossLine    := isCrossLine(startVAddr, takenCfiOffset)
    vSetIdx        := VecInit(get_idx(startVAddr), get_idx(nextLineVAddr))
    vPageNumber    := entry.startPc(VAddrBits - 1, PageOffsetWidth)
    this
  }
}

class FtqFetchReq(implicit p: Parameters) extends FtqBundle with ICacheDataHelper with ICacheCacheLineHelper {
  val startVAddr:     PrunedAddr = PrunedAddr(VAddrBits)
  val nextLineVAddr:  PrunedAddr = PrunedAddr(VAddrBits)
  val takenCfiOffset: UInt       = UInt(CfiPositionWidth.W)
  val isCrossLine:    Bool       = Bool()
  val bankSel:        Vec[UInt]  = Vec(PortNumber, UInt(DataBanks.W))
  val vSetIdx:        Vec[UInt]  = Vec(PortNumber, UInt(idxBits.W))
  val wayMask:        Vec[UInt]  = Vec(PortNumber, UInt(nWays.W))
  val isMmio:         Bool       = Bool()
  val size:           UInt       = UInt((log2Ceil(FetchBlockSize) + 1).W)
  val vPageNumber:    UInt       = UInt((VAddrBits - PageOffsetWidth).W)

  def fromFtqEntry(entry: FtqEntry, twoFetchInfo: TwoFetchInfo): FtqFetchReq = {
    val (isCrossLine, bankSel) = getBankSel(startVAddr, takenCfiOffset)
    startVAddr       := entry.startPc
    nextLineVAddr    := entry.startPc + blockBytes.U
    takenCfiOffset   := entry.takenCfiOffset.bits
    this.isCrossLine := isCrossLine
    this.bankSel     := bankSel
    vSetIdx          := VecInit(get_idx(startVAddr), get_idx(nextLineVAddr))
    wayMask          := twoFetchInfo.wayMask
    isMmio           := twoFetchInfo.isMmio
    size             := (entry.takenCfiOffset.bits +& 1.U) << 1
    vPageNumber      := entry.startPc(VAddrBits - 1, PageOffsetWidth)
    this
  }
}

class TwoPrefetchCase extends Bundle {
  val value: UInt = TwoPrefetchCase.Value()

  def valid: Bool = value.orR

  // select 2 vaddr to read ICacheMetaArray
  def selectMetaVAddr(reqVec: Vec[PrefetchReqBundle]): Vec[PrunedAddr] =
    MuxCase(
      // unable to do 2-prefetch, or isSameLine or isOverlap1, both use req1's start and nextLine
      VecInit(reqVec(0).startVAddr, reqVec(0).nextLineVAddr),
      Seq(
        isOverlap2   -> VecInit(reqVec(1).startVAddr, reqVec(1).nextLineVAddr),
        isInterleave -> VecInit(reqVec(0).startVAddr, reqVec(1).startVAddr)
      )
    )

  // select isCrossLine flag to read ICacheMetaArray
  def selectIsCrossLine(reqVec: Vec[PrefetchReqBundle]): Bool =
    MuxCase(
      // unable to do 2-prefetch, use req1.isCrossLine
      reqVec(0).isCrossLine,
      Seq(
        // if 2 fb are in the same line, read 2 if one of them crosses cacheline
        isSameLine -> (reqVec(0).isCrossLine || reqVec(1).isCrossLine),
        // otherwise, we must read 2 cacheline
        (isOverlap1 || isOverlap2 || isInterleave) -> true.B
      )
    )

  // NOTE: refer to object TwoPrefetchCase.Value for explanation
  def isSameLine: Bool = value(0)

  def isOverlap1: Bool = value(1)

  def isOverlap2: Bool = value(2)

  def isInterleave: Bool = value(3)

  // after read 2 (at most) metaInfo from ICacheMetaArray, broadcast to 2 fetch blocks
  def generateReqMetaInfo(readInfoVec: Vec[ICacheMetaInfo]): Vec[Vec[ICacheMetaInfo]] =
    VecInit(
      VecInit(
        // if isOverlap2, fb1's first line is fb2's second line and is from port 2, otherwise from port 1
        Mux(isOverlap2, readInfoVec(1), readInfoVec(0)),
        // if isOverlap2 or isInterleave, fb1 does not have a second line, otherwise from port 1
        Mux(isOverlap2 || isInterleave, 0.U.asTypeOf(readInfoVec(0)), readInfoVec(1))
      ),
      VecInit(
        // if isSameLine or isOverlap2, fb2's first line is from port 1, otherwise from port 2
        Mux(isSameLine || isOverlap2, readInfoVec(0), readInfoVec(1)),
        // if isOverlap1 or isInterleave, fb2 does not have a second line, otherwise from port 2
        Mux(isOverlap1 || isInterleave, 0.U.asTypeOf(readInfoVec(0)), readInfoVec(1))
      )
    )
}

object TwoPrefetchCase {
  def Unable: TwoPrefetchCase = apply(Value.Unable)

  def apply(that: UInt, canAssert: Bool = true.B): TwoPrefetchCase = {
    when(canAssert) {
      Value.assertLegal(that)
    }
    val twoPrefetchCase = Wire(new TwoPrefetchCase)
    twoPrefetchCase.value := that
    twoPrefetchCase
  }

  def SameLine: TwoPrefetchCase = apply(Value.SameLine)

  def Overlap1: TwoPrefetchCase = apply(Value.Overlap1)

  def Overlap2: TwoPrefetchCase = apply(Value.Overlap2)

  def Interleave: TwoPrefetchCase = apply(Value.Interleave)

  def apply(reqVec: Vec[FtqPrefetchReq], canAssert: Bool): TwoPrefetchCase =
    TwoPrefetchCase(
      reqVec(0).vSetIdx(0) === reqVec(1).vSetIdx(0),                                                    // sameLine
      reqVec(0).isCrossLine && !reqVec(1).isCrossLine && reqVec(0).vSetIdx(1) === reqVec(1).vSetIdx(0), // overlap1
      !reqVec(0).isCrossLine && reqVec(1).isCrossLine && reqVec(1).vSetIdx(1) === reqVec(0).vSetIdx(0), // overlap2
      !reqVec(0).isCrossLine && !reqVec(1).isCrossLine && reqVec(0).vSetIdx(0)(0) =/= reqVec(1).vSetIdx(0)(0), // inter
      canAssert
    )

  def apply(sameLine: Bool, overlap1: Bool, overlap2: Bool, interleave: Bool, canAssert: Bool): TwoPrefetchCase =
    apply(VecInit(sameLine, overlap1, overlap2, interleave).asUInt, canAssert)

  private object Value extends EnumUInt(5, useOneHot = true, allowZeroForOneHot = true) {
    // cannot do 2-prefetch due to SRAM read port conflict
    def Unable: UInt = 0.U(width.W)

    /* SameLine: 2 fetch block in the same cacheline(s)
     * |    cacheline0    |    cacheline1    |
     *      |  fb1             |
     *          | fb2 |
     */
    def SameLine: UInt = 1.U(width.W)

    /* Overlap1: fb2 is in fb1's next line
     * |    cacheline0    |    cacheline1    |
     *      |  fb1             |
     *                       | fb2 |
     *                       | fb2                | // bad: fb2 cannot cross cacheline
     */
    def Overlap1: UInt = 2.U(width.W)

    /* Overlap2: reverse of Overlap1, i.e. fb1 is in fb2's next line
     */
    def Overlap2: UInt = 4.U(width.W)

    /* Interleave: 2 fetch block in interleaved cachelines
     *  |    cacheline(2n)    | ... |    cacheline(2n+1)    |
     *        | fb1 |
     *                                   | fb2 |
     *                                   | fb2                 | // bad: both fb1 and fb2 cannot cross cacheline
     */
    def Interleave: UInt = 8.U(width.W)
  }
}

class TwoFetchInfo(implicit p: Parameters) extends FtqBundle with HasICacheParameters {
  val isMmio:  Bool      = Bool()
  val wayMask: Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
}
