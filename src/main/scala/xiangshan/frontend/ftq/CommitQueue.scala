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
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.CallRetCommit
import xiangshan.frontend.bpu.BpuCommitMeta
import xiangshan.frontend.bpu.BranchAttribute

class CommitQueue(implicit p: Parameters) extends FtqModule with HasCircularQueuePtrHelper {

  class CommitQueueIO extends FtqBundle {
    class BpuCommitInfo extends FtqBundle {
      val ftqPtr:     FtqPtr        = new FtqPtr
      val commitMeta: BpuCommitMeta = new BpuCommitMeta
    }
    val bpuWrite: Valid[BpuCommitInfo] = Input(Valid(new BpuCommitInfo))
    val reWrite:  Valid[CallRetCommit] = Input(Valid(new CallRetCommit))
    val commit:   Bool                 = Input(Bool())
    val bpuTrain: Valid[BpuCommitMeta] = Output(Valid(new BpuCommitMeta))
  }

  val io: CommitQueueIO = IO(new CommitQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(CommitQueueSize, Valid(new BpuCommitMeta))))

  private val deqPtr       = RegInit(CommitQueuePtr(false.B, 0.U))
  private val enqIndex     = io.bpuWrite.bits.ftqPtr.value
  private val reWriteIndex = io.reWrite.bits.ftqPtr.value

  when(io.bpuWrite.valid) {
    mem(enqIndex).valid := true.B
    mem(enqIndex).bits  := io.bpuWrite.bits.commitMeta
  }
  when(io.reWrite.valid) {
    mem(reWriteIndex).valid                    := true.B
    mem(reWriteIndex).bits.attribute.rasAction := io.reWrite.bits.rasAction
  }

  when(io.commit) {
    mem(deqPtr.value).valid := false.B
    deqPtr                  := deqPtr + 1.U
  }

  io.bpuTrain.valid := mem(deqPtr.value).valid && io.commit
  io.bpuTrain.bits  := mem(deqPtr.value).bits
}
