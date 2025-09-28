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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo

class FtqEntry(implicit p: Parameters) extends FtqBundle {
  val startVAddr:     PrunedAddr  = PrunedAddr(VAddrBits)
  val takenCfiOffset: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
  val identifiedCfi:  Vec[Bool]   = Vec(FetchBlockInstNum, Bool())
}

class MetaEntry(implicit p: Parameters) extends FtqBundle {
  val meta       = new BpuMeta
  val paddingBit = if (meta.getWidth % 2 != 0) Some(UInt(1.W)) else None
}

class ResolveEntry(implicit p: Parameters) extends FtqBundle {
  val ftqIdx:     FtqPtr     = new FtqPtr
  val startVAddr: PrunedAddr = PrunedAddr(VAddrBits)
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
  val pc_mem_wen   = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(PrunedAddr(VAddrBits))
  // newest target
  val newest_entry_en     = Output(Bool())
  val newest_entry_target = Output(UInt(VAddrBits.W))
  val newest_entry_ptr    = Output(new FtqPtr)
}
