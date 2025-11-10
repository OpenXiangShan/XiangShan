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
import xiangshan.frontend.bpu.BranchAttribute

class CommitQueue(implicit p: Parameters) extends FtqModule with HasCircularQueuePtrHelper {

  class CommitQueueIO extends FtqBundle {
    val backendCommit: Vec[Valid[CallRetCommit]] = Input(Vec(CommitWidth, Valid(new CallRetCommit)))
    val bpuTrain:      Valid[CallRetCommit]      = Output(Valid(new CallRetCommit))
  }

  val io: CommitQueueIO = IO(new CommitQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(CommitQueueSize, Valid(new CallRetCommit))))

  private val enqPtr = RegInit(CommitQueuePtr(false.B, 0.U))
  private val deqPtr = RegInit(CommitQueuePtr(false.B, 0.U))

  private val full = distanceBetween(enqPtr, deqPtr) >= (CommitQueueSize - 8).U

  private val isCallRet = io.backendCommit.map(instr =>
    instr.valid && instr.bits.rasAction =/= BranchAttribute.RasAction.None
  )

  private val enqIndex = VecInit((0 until CommitWidth).map(i => (enqPtr + PopCount(isCallRet.take(i))).value))

  when(!full)(enqPtr := enqPtr + PopCount(isCallRet))

  io.backendCommit.zipWithIndex.foreach { case (branch, i) =>
    when(isCallRet(i) && !full) {
      mem(enqIndex(i)).valid := true.B
      mem(enqIndex(i)).bits  := branch.bits
    }
  }

  when(mem(deqPtr.value).valid) {
    deqPtr := deqPtr + 1.U

    mem(deqPtr.value).valid := false.B
  }

  io.bpuTrain.valid := mem(deqPtr.value).valid
  io.bpuTrain.bits  := mem(deqPtr.value).bits

  XSError(deqPtr > enqPtr, "Dequeue pointer exceeds enqueue pointer in Commit Queue")

  // We do not expect the commit queue to be full often
  XSPerfAccumulate("commitQueueFull", full)
}
