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
import xiangshan.frontend.BranchPredictionRedirect
import xiangshan.frontend.BranchPredictionUpdate
import xiangshan.frontend.CGHPtr
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.RasSpeculativeInfo
import xiangshan.frontend.bpu.FTBEntry
import xiangshan.frontend.bpu.HasBPUConst
import xiangshan.frontend.bpu.NewPredictorMeta
import xiangshan.frontend.bpu.PredictorMeta
import xiangshan.frontend.bpu.abtb.AheadBtbUpdate

class FtqRedirectSramEntry(implicit p: Parameters) extends FtqBundle {
  val histPtr     = new CGHPtr
  val rasSpecInfo = new RasSpeculativeInfo
}

class MetaEntry(implicit p: Parameters) extends FtqBundle with HasBPUConst {
  val meta       = new PredictorMeta
  val newMeta    = new NewPredictorMeta
  val ftb_entry  = new FTBEntry
  val paddingBit = if ((meta.getWidth + newMeta.getWidth + ftb_entry.getWidth) % 2 != 0) Some(UInt(1.W)) else None
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
  val redirect:        Valid[BranchPredictionRedirect] = Valid(new BranchPredictionRedirect)
  val update:          Valid[BranchPredictionUpdate]   = Valid(new BranchPredictionUpdate)
  val newUpdate:       Valid[AheadBtbUpdate]           = Valid(new AheadBtbUpdate) // FIXME
  val bpuPtr:          FtqPtr                          = Output(new FtqPtr)
  val redirectFromIFU: Bool                            = Output(Bool())
}

class BpuFlushInfo(implicit p: Parameters) extends FtqBundle with HasCircularQueuePtrHelper {
  // when ifu pipeline is not stalled,
  // a packet from bpu s3 can reach f1 at most
  val s2 = Valid(new FtqPtr)
  val s3 = Valid(new FtqPtr)

  def stage(idx: Int): Valid[FtqPtr] = {
    require(idx >= 2 && idx <= 3)
    idx match {
      case 2 => s2
      case 3 => s3
    }
  }

  def shouldFlushBy(src: Valid[FtqPtr], idx_to_flush: FtqPtr) =
    src.valid && !isAfter(src.bits, idx_to_flush)
  def shouldFlushByStage2(idx: FtqPtr) = shouldFlushBy(s2, idx)
  def shouldFlushByStage3(idx: FtqPtr) = shouldFlushBy(s3, idx)
}

class FtqToCtrlIO(implicit p: Parameters) extends FtqBundle {
  // write to backend pc mem
  val pc_mem_wen   = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(PrunedAddr(VAddrBits))
  // newest target
  val newest_entry_en     = Output(Bool())
  val newest_entry_target = Output(UInt(VAddrBits.W))
  val newest_entry_ptr    = Output(new FtqPtr)
}
