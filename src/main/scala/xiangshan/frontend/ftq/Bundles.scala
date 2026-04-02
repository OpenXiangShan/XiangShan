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
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.icache.ICacheBundle
import xiangshan.frontend.icache.ICacheCacheLineHelper
import xiangshan.frontend.icache.ICacheDataHelper
import xiangshan.frontend.icache.ICacheParameters
import xiangshan.frontend.icache.PrefetchReqBundle
import xiangshan.frontend.icache.WayLookupEntry

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

  def valid:   Bool = value.orR
  def isCase1: Bool = value(0)
  def isCase2: Bool = value(1)
  def isCase3: Bool = value(2)
  def isCase4: Bool = value(3)
}

object TwoPrefetchCase {
  private object Value extends EnumUInt(5, useOneHot = true, allowZeroForOneHot = true) {
    def None:  UInt = 0.U(width.W)
    def Case1: UInt = 1.U(width.W)
    def Case2: UInt = 2.U(width.W)
    def Case3: UInt = 4.U(width.W)
    def Case4: UInt = 8.U(width.W)
  }

  def apply(that: UInt, canAssert: Bool = true.B): TwoPrefetchCase = {
    when(canAssert) {
      Value.assertLegal(that)
    }
    val twoPrefetchCase = Wire(new TwoPrefetchCase)
    twoPrefetchCase.value := that
    twoPrefetchCase
  }

  def None:  TwoPrefetchCase = apply(Value.None)
  def Case1: TwoPrefetchCase = apply(Value.Case1)
  def Case2: TwoPrefetchCase = apply(Value.Case2)
  def Case3: TwoPrefetchCase = apply(Value.Case3)
  def Case4: TwoPrefetchCase = apply(Value.Case4)

  def apply(firstReq: FtqPrefetchReq, secondReq: FtqPrefetchReq, canAssert: Bool): TwoPrefetchCase = {
    // case1: two blocks' startPc are in the same cache line
    val case1 = firstReq.vSetIdx(0) === secondReq.vSetIdx(0)

    // case2: the first block is cross line,
    // and the second block's startPc is in the same cache line with the first block's second line
    val case2 = firstReq.isCrossLine && firstReq.vSetIdx(1) === secondReq.vSetIdx(0) && !secondReq.isCrossLine

    // case3: the second block is cross line,
    // and the first block's startPc is in the same cache line with the second block's second line
    val case3 = secondReq.isCrossLine && secondReq.vSetIdx(1) === firstReq.vSetIdx(0) && !firstReq.isCrossLine

    // case4: the two blocks' startPc are in different cache lines (one even, one odd),
    // and both of them are not cross line
    val case4 = (firstReq.vSetIdx(0)(0) ^ secondReq.vSetIdx(0)(0)) && !firstReq.isCrossLine && !secondReq.isCrossLine

    // TODO: remove them
    dontTouch(firstReq)
    dontTouch(secondReq)
    dontTouch(case1)
    dontTouch(case2)
    dontTouch(case3)
    dontTouch(case4)

    TwoPrefetchCase(VecInit(case1, case2, case3, case4).asUInt, canAssert)
  }
}

class TwoFetchInfo(implicit p: Parameters) extends FtqBundle with HasICacheParameters {
  val isMmio:  Bool      = Bool()
  val wayMask: Vec[UInt] = Vec(PortNumber, UInt(nWays.W))
}
