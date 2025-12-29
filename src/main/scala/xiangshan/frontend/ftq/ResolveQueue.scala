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
import utility.DataHoldBypass
import utility.HasCircularQueuePtrHelper
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.Resolve
import xiangshan.frontend.bpu.HalfAlignHelper

class ResolveQueue(implicit p: Parameters) extends FtqModule with HalfAlignHelper with HasCircularQueuePtrHelper {

  class ResolveQueueIO extends Bundle {
    val backendResolve: Vec[Valid[Resolve]]       = Input(Vec(backendParams.BrhCnt, Valid(new Resolve)))
    val bpuTrain:       DecoupledIO[ResolveEntry] = Decoupled(new ResolveEntry)

    val backendRedirect:    Bool   = Input(Bool())
    val backendRedirectPtr: FtqPtr = Input(new FtqPtr)
    val bpuEnqueue:         Bool   = Input(Bool())
    val bpuEnqueuePtr:      FtqPtr = Input(new FtqPtr)
  }

  val io: ResolveQueueIO = IO(new ResolveQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(ResolveQueueSize, Valid(new ResolveEntry))))

  private val enqPtr = RegInit(ResolveQueuePtr(false.B, 0.U))
  private val deqPtr = RegInit(ResolveQueuePtr(false.B, 0.U))

  private val full = distanceBetween(enqPtr, deqPtr) >= (ResolveQueueSize - 4).U

  // Branches that have been issued to functional units cannot be flushed by redirects. Therefore, these branches will
  // be resolved. However, the meta of these branches may already be overwritten by new branches, which means they
  // cannot be updated by BPU. To handle this case, backend redirect will be propagated for several cycles to make sure
  // newly resolved branches can be flushed correctly.
  // 3 cycles may be enough, we'll see in practice.
  private def RedirectDelay   = 3
  private val backendRedirect = WireDefault(VecInit.fill(RedirectDelay)(false.B))
  backendRedirect := VecInit((0 until RedirectDelay).map(i =>
    if (i == 0) io.backendRedirect else RegNext(backendRedirect(i - 1))
  ))
  private val backendRedirectPtr = DataHoldBypass(io.backendRedirectPtr, FtqPtr(false.B, 0.U), io.backendRedirect)
  XSError(
    RegNext(backendRedirect(RedirectDelay - 1)) && !backendRedirect.reduce(_ || _) && io.backendResolve.map(branch =>
      branch.valid && branch.bits.ftqIdx > backendRedirectPtr
    ).reduce(_ || _),
    "Backend resolves branches that should have been flushed\n"
  )

  private val resolve = io.backendResolve.map { backendResolve =>
    val filteredResolve = Wire(Valid(new Resolve))
    filteredResolve.valid := backendResolve.valid &&
      !(backendRedirect.reduce(_ || _) && backendResolve.bits.ftqIdx > backendRedirectPtr)
    filteredResolve.bits := backendResolve.bits
    filteredResolve
  }

  private val hit = resolve.map { branch =>
    mem.map(entry =>
      branch.valid && entry.valid && !entry.bits.flushed && entry.bits.ftqIdx === branch.bits.ftqIdx
    ).reduce(_ || _)
  }
  private val hitIndex = resolve.map { branch =>
    mem.indexWhere(entry =>
      branch.valid && entry.valid && !entry.bits.flushed && entry.bits.ftqIdx === branch.bits.ftqIdx
    )
  }
  private val hitPrevious = resolve.zipWithIndex.map { case (branch, i) =>
    resolve.take(i).map(previousBranch =>
      previousBranch.valid && branch.valid && previousBranch.bits.ftqIdx === branch.bits.ftqIdx
    )
  }
  private val needNewEntry = resolve.zipWithIndex.map { case (branch, i) =>
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
  when(!full)(enqPtr := enqPtr + PopCount(needNewEntry))

  resolve.zipWithIndex.foreach { case (branch, i) =>
    when(branch.valid && !full) {
      mem(enqIndex(i)).valid        := true.B
      mem(enqIndex(i)).bits.ftqIdx  := branch.bits.ftqIdx
      mem(enqIndex(i)).bits.startPc := branch.bits.pc

      val firstEmpty = mem(enqIndex(i)).bits.branches.indexWhere(!_.valid)
      val branchSlot = mem(enqIndex(i)).bits.branches(firstEmpty + PopCount(hitPrevious(i)))
      branchSlot.valid            := true.B
      branchSlot.bits.target      := branch.bits.target
      branchSlot.bits.taken       := branch.bits.taken
      branchSlot.bits.cfiPosition := getAlignedPosition(branch.bits.pc, branch.bits.ftqOffset)._1
      branchSlot.bits.attribute   := branch.bits.attribute
      branchSlot.bits.mispredict  := branch.bits.mispredict
      if (branchSlot.bits.debug_realCfiPc.isDefined) {
        branchSlot.bits.debug_realCfiPc.get := getRealCfiPcFromOffset(
          branch.bits.pc,
          branch.bits.ftqOffset,
          branch.bits.debug_isRVC.get
        )
      }
    }
  }

  mem.foreach { entry =>
    when(entry.valid &&
      (backendRedirect.reduce(_ || _) && entry.bits.ftqIdx > backendRedirectPtr ||
        io.bpuEnqueue && entry.bits.ftqIdx.value === io.bpuEnqueuePtr.value)) {
      entry.bits.flushed := true.B
    }
  }

  private val deqValid = mem(deqPtr.value).valid && !resolve.map(branch =>
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

  // We currently do not want resolve queue to be full as it has already been unacceptably large. Now it can be full
  // because it is a sequential queue, which results in multiple flushed entries staying in the queue and blocking new
  // entries from being enqueued. More sophisticated designs should be considered.
  XSPerfAccumulate("resolveQueueFull", full)
  XSPerfAccumulate("dropResolve", PopCount(io.backendResolve.map(_.valid && full)))
}
