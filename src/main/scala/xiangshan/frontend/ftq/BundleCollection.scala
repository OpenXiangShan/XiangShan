// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
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
import utility.ParallelPriorityEncoder
import utility.ParallelPriorityMux
import xiangshan.ValidUndirectioned
import xiangshan.XSBundle
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.BranchPredictionBundle
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.BranchPredictionUpdate
import xiangshan.frontend.CGHPtr
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PredecodeWritebackBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.RasSpeculativeInfo
import xiangshan.frontend.bpu.BPUUtils
import xiangshan.frontend.bpu.FTBEntry
import xiangshan.frontend.bpu.FullBranchPrediction
import xiangshan.frontend.bpu.HasBPUConst
import xiangshan.frontend.bpu.NewPredictorMeta
import xiangshan.frontend.bpu.PredictorMeta
import xiangshan.frontend.bpu.abtb.AheadBtbTrain

class FtqDebugBundle(implicit p: Parameters) extends FtqBundle {
  val pc        = PrunedAddr(VAddrBits)
  val target    = PrunedAddr(VAddrBits)
  val isBr      = Bool()
  val isJmp     = Bool()
  val isCall    = Bool()
  val isRet     = Bool()
  val misPred   = Bool()
  val isTaken   = Bool()
  val predStage = UInt(2.W)
}

class FtqRfComponents(implicit p: Parameters) extends FtqBundle with BPUUtils {
  val startAddr     = PrunedAddr(VAddrBits)
  val nextLineAddr  = PrunedAddr(VAddrBits)
  val isNextMask    = Vec(PredictWidth, Bool())
  val fallThruError = Bool()
  // val carry = Bool()
  def getPc(offset: UInt): UInt = {
    def getHigher(pc: PrunedAddr): UInt = pc(VAddrBits - 1, log2Ceil(PredictWidth) + instOffsetBits + 1)
    def getOffset(pc: PrunedAddr): UInt = pc(log2Ceil(PredictWidth) + instOffsetBits, instOffsetBits)
    Cat(
      getHigher(Mux(isNextMask(offset) && startAddr(log2Ceil(PredictWidth) + instOffsetBits), nextLineAddr, startAddr)),
      getOffset(startAddr) + offset,
      0.U(instOffsetBits.W)
    )
  }
  def fromBranchPrediction(resp: BranchPredictionBundle): Unit = {
    def carryPos(addr: UInt) = addr(instOffsetBits + log2Ceil(PredictWidth) + 1)
    this.startAddr    := resp.pc
    this.nextLineAddr := resp.pc + (FetchWidth * 4 * 2).U // may be broken on other configs
    this.isNextMask := VecInit((0 until PredictWidth).map(i =>
      (resp.pc(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool
    ))
    this.fallThruError := resp.fallThruError
  }

  def fromPrediction(pred: FullBranchPrediction): Unit = {
    this.startAddr    := pred.startVAddr
    this.nextLineAddr := pred.startVAddr + (FetchWidth * 4 * 2).U // may be broken on other configs
    this.isNextMask := VecInit((0 until PredictWidth).map(i =>
      (pred.startVAddr(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool
    ))
    this.fallThruError := false.B // FIXME
  }

  override def toPrintable: Printable =
    p"startAddr:${Hexadecimal(startAddr.toUInt)}"
}

class FtqPdEntry(implicit p: Parameters) extends FtqBundle {
  val brMask    = Vec(PredictWidth, Bool())
  val jmpInfo   = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jalTarget = PrunedAddr(VAddrBits)
  val rvcMask   = Vec(PredictWidth, Bool())
  def hasJal    = jmpInfo.valid && !jmpInfo.bits(0)
  def hasJalr   = jmpInfo.valid && jmpInfo.bits(0)
  def hasCall   = jmpInfo.valid && jmpInfo.bits(1)
  def hasRet    = jmpInfo.valid && jmpInfo.bits(2)

  def fromPdWb(pdWb: PredecodeWritebackBundle) = {
    val pds = pdWb.pd
    this.brMask        := VecInit(pds.map(pd => pd.isBr && pd.valid))
    this.jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
    this.jmpInfo.bits := ParallelPriorityMux(
      pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
      pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet))
    )
    this.jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
    this.rvcMask   := VecInit(pds.map(pd => pd.isRVC))
    this.jalTarget := pdWb.jalTarget
  }

  def toPd(offset: UInt) = {
    require(offset.getWidth == log2Ceil(PredictWidth))
    val pd = Wire(new PreDecodeInfo)
    pd.valid := true.B
    pd.isRVC := rvcMask(offset)
    val isBr   = brMask(offset)
    val isJalr = offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(0)
    pd.brType := Cat(offset === jmpOffset && jmpInfo.valid, isJalr || isBr)
    pd.isCall := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(1)
    pd.isRet  := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(2)
    pd
  }
}

class PrefetchPtrDb(implicit p: Parameters) extends Bundle {
  val fromFtqPtr = UInt(log2Up(p(XSCoreParamsKey).FtqSize).W)
  val fromIfuPtr = UInt(log2Up(p(XSCoreParamsKey).FtqSize).W)
}

class FtqRedirectSramEntry(implicit p: Parameters) extends XSBundle {
  val histPtr = new CGHPtr
//  val sc_disagree = if (!env.FPGAPlatform) Some(Vec(numBr, Bool())) else None
  val rasSpecInfo = new RasSpeculativeInfo
}

class Ftq_1R_SRAMEntry(implicit p: Parameters) extends FtqBundle with HasBPUConst {
  val meta       = new PredictorMeta
  val newMeta    = new NewPredictorMeta
  val ftb_entry  = new FTBEntry
  val paddingBit = if ((meta.getWidth + newMeta.getWidth + ftb_entry.getWidth) % 2 != 0) Some(UInt(1.W)) else None
}

class Ftq_Pred_Info(implicit p: Parameters) extends FtqBundle {
  val target   = PrunedAddr(VAddrBits)
  val cfiIndex = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
}

class FtqRead[T <: Data](private val gen: T)(implicit p: Parameters) extends FtqBundle {
  val valid  = Output(Bool())
  val ptr    = Output(new FtqPtr)
  val offset = Output(UInt(log2Ceil(PredictWidth).W))
  val data   = Input(gen)
  def apply(valid: Bool, ptr: FtqPtr, offset: UInt) = {
    this.valid  := valid
    this.ptr    := ptr
    this.offset := offset
    this.data
  }
}

class FtqToBpuIO(implicit p: Parameters) extends FtqBundle {
  val redirect       = Valid(new BranchPredictionRedirect)
  val update         = Valid(new BranchPredictionUpdate)
  val enq_ptr        = Output(new FtqPtr)
  val redirctFromIFU = Output(Bool())
}

class BpuFlushInfo(implicit p: Parameters) extends FtqBundle with HasCircularQueuePtrHelper {
  // when ifu pipeline is not stalled,
  // a packet from bpu s3 can reach f1 at most
  val s2 = Valid(new FtqPtr)
  val s3 = Valid(new FtqPtr)
  def shouldFlushBy(src: Valid[FtqPtr], idx_to_flush: FtqPtr) =
    src.valid && !isAfter(src.bits, idx_to_flush)
  def shouldFlushByStage2(idx: FtqPtr) = shouldFlushBy(s2, idx)
  def shouldFlushByStage3(idx: FtqPtr) = shouldFlushBy(s3, idx)
}

class FtqToCtrlIO(implicit p: Parameters) extends FtqBundle {
  // write to backend pc mem
  val pc_mem_wen   = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(new FtqRfComponents)
  // newest target
  val newest_entry_en     = Output(Bool())
  val newest_entry_target = Output(UInt(VAddrBits.W))
  val newest_entry_ptr    = Output(new FtqPtr)
}
