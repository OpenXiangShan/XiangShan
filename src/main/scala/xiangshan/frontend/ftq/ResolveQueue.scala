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
import utility.XSError
import xiangshan.Resolve
import xiangshan.frontend.bpu.HalfAlignHelper

class ResolveQueue(implicit p: Parameters) extends FtqModule with HalfAlignHelper {

  class ResolveQueueIO extends Bundle {
    val backendResolve:     Vec[Valid[Resolve]]       = Input(Vec(backendParams.BrhCnt, Valid(new Resolve)))
    val backendRedirect:    Bool                      = Input(Bool())
    val backendRedirectPtr: FtqPtr                    = Input(new FtqPtr)
    val bpuTrain:           DecoupledIO[ResolveEntry] = Decoupled(new ResolveEntry)
  }

  val io: ResolveQueueIO = IO(new ResolveQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(ResolveQueueSize, Valid(new ResolveEntry))))

  private val enqPtr = RegInit(ResolveQueuePtr(false.B, 0.U))
  private val deqPtr = RegInit(ResolveQueuePtr(false.B, 0.U))

  private val hit = io.backendResolve.map { branch =>
    mem.map(entry =>
      branch.valid && entry.valid && !entry.bits.flushed && entry.bits.ftqIdx === branch.bits.ftqIdx
    ).reduce(_ || _)
  }
  private val hitIndex = io.backendResolve.map { branch =>
    mem.indexWhere(entry =>
      branch.valid && entry.valid && !entry.bits.flushed && entry.bits.ftqIdx === branch.bits.ftqIdx
    )
  }
  private val hitPrevious = io.backendResolve.zipWithIndex.map { case (branch, i) =>
    io.backendResolve.take(i).map(previousBranch =>
      previousBranch.valid && branch.valid && previousBranch.bits.ftqIdx === branch.bits.ftqIdx
    )
  }
  private val needNewEntry = io.backendResolve.zipWithIndex.map { case (branch, i) =>
    branch.valid && !hit(i) && !hitPrevious(i).fold(false.B)(_ || _)
  }

  private val enqIndex = WireDefault(VecInit.fill(backendParams.BrhCnt)(0.U(log2Ceil(ResolveQueueSize).W)))
  enqIndex := VecInit((0 until backendParams.BrhCnt).map { i =>
    val newIndex = MuxCase(
      (enqPtr + PopCount(needNewEntry.take(i))).value,
      hitPrevious(i).zipWithIndex.map { case (hit, j) => (hit, enqIndex(j)) }
    )

    Mux(hit(i), hitIndex(i), newIndex)
  })
  enqPtr := enqPtr + PopCount(needNewEntry)

  io.backendResolve.zipWithIndex.foreach { case (branch, i) =>
    when(branch.valid) {
      mem(enqIndex(i)).valid           := true.B
      mem(enqIndex(i)).bits.ftqIdx     := branch.bits.ftqIdx
      mem(enqIndex(i)).bits.startVAddr := branch.bits.pc

      val firstEmpty = mem(enqIndex(i)).bits.branches.indexWhere(!_.valid)
      val branchSlot = mem(enqIndex(i)).bits.branches(firstEmpty + PopCount(hitPrevious(i)))
      branchSlot.valid            := true.B
      branchSlot.bits.target      := branch.bits.target
      branchSlot.bits.taken       := branch.bits.taken
      branchSlot.bits.cfiPosition := getAlignedPosition(branch.bits.pc, branch.bits.ftqOffset)._1
      branchSlot.bits.attribute   := branch.bits.attribute
      branchSlot.bits.mispredict  := branch.bits.mispredict
    }
  }

  when(io.backendRedirect) {
    mem.foreach(entry =>
      when(entry.valid)(entry.bits.flushed := entry.bits.flushed || entry.bits.ftqIdx > io.backendRedirectPtr)
    )
  }

  private val deqValid = mem(deqPtr.value).valid && !io.backendResolve.map(branch =>
    branch.valid && branch.bits.ftqIdx === mem(deqPtr.value).bits.ftqIdx
  ).reduce(_ || _)

  io.bpuTrain.valid := deqValid && !mem(deqPtr.value).bits.flushed
  io.bpuTrain.bits  := mem(deqPtr.value).bits

  when(io.bpuTrain.fire || mem(deqPtr.value).valid && mem(deqPtr.value).bits.flushed) {
    deqPtr := deqPtr + 1.U

    mem(deqPtr.value).valid        := false.B
    mem(deqPtr.value).bits.flushed := false.B
    mem(deqPtr.value).bits.branches.foreach(_.valid := false.B)
  }

  XSError(deqPtr > enqPtr, "Dequeue pointer exceeds enqueue pointer in Resolve Queue")
}
