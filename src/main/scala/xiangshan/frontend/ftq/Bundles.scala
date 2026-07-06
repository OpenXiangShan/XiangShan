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
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPerfMeta
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo
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

object ResolveSource extends EnumUInt(2) {
  def Backend: UInt = 0.U(width.W)
  def Ifu:     UInt = 1.U(width.W)
}

class ResolveEntry(implicit p: Parameters) extends FtqBundle {
  val ftqIdx:  FtqPtr     = new FtqPtr
  val flushed: Bool       = Bool()
  val startPc: PrunedAddr = PrunedAddr(VAddrBits)
  // TODO: Reconsider branch number
  val branches: Vec[Valid[BranchInfo]] = Vec(ResolveEntryBranchNumber, Valid(new BranchInfo))
  // used for bptrace & other debug proposes
  val debug_source: UInt = ResolveSource()
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
  val isCfi:   Vec[Bool]            = Vec(FetchBlockInstNum, Bool())
  val cfiAttr: Vec[BranchAttribute] = Vec(FetchBlockInstNum, new BranchAttribute)

  // This block mispredicted
  // no matter how many mispredictions happened before, count correct-path only
  val mispredict:           Bool       = Bool()
  val mispredictBranchInfo: BranchInfo = new BranchInfo()
}

class FtqToPrefetchBundle(implicit p: Parameters) extends FtqBundle {
  val req:             Vec[PrefetchReqBundle] = Vec(MaxPrefetchReqNum, new PrefetchReqBundle)
  val twoPrefetchCase: TwoPrefetchCase        = new TwoPrefetchCase
}

class FtqToWayLookupBundle(implicit p: Parameters) extends FtqBundle {
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
  val size:           UInt       = UInt((log2Ceil(FetchBlockInstNum) + 1).W)
  val vPageNumber:    UInt       = UInt((VAddrBits - PageOffsetWidth).W)

  def fromFtqEntry(entry: FtqEntry): FtqFetchReq = {
    val (isCrossLine, bankSel) = getBankSel(startVAddr, takenCfiOffset)
    startVAddr       := entry.startPc
    nextLineVAddr    := entry.startPc + blockBytes.U
    takenCfiOffset   := entry.takenCfiOffset.bits
    this.isCrossLine := isCrossLine
    this.bankSel     := bankSel
    vSetIdx          := VecInit(get_idx(startVAddr), get_idx(nextLineVAddr))
    size             := entry.takenCfiOffset.bits +& 1.U
    vPageNumber      := entry.startPc(VAddrBits - 1, PageOffsetWidth)
    this
  }
}
