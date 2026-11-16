// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
import utility.UIntToMask
import xiangshan.Redirect
import xiangshan.Resolve
import xiangshan.frontend.FrontendRedirect
import xiangshan.frontend.bpu.BpuPerfMeta
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.HalfAlignHelper

/** Resolve-based statistics for FTQ entries, with backend redirect recovery. */
class PerfQueue(implicit p: Parameters) extends FtqModule with HalfAlignHelper {
  val io = IO(new Bundle {
    val prediction = Flipped(Valid(new Bundle {
      val ftqPtr = new FtqPtr
      val meta   = new BpuPerfMeta
    }))
    val resolve         = Input(Vec(backendParams.BrhCnt, Valid(new Resolve)))
    val backendRedirect = Flipped(Valid(new Redirect))
    val ifuRedirect     = Flipped(Valid(new FrontendRedirect))
    val ifuCorrection   = Input(Bool())
    val commitPtr       = Input(new FtqPtr)
    val commitValid     = Output(Bool())
    val entries         = Output(Vec(FtqSize, new PerfMeta))
  })

  private val entries     = Reg(Vec(FtqSize, new PerfMeta))
  private val entryValid  = RegInit(VecInit.fill(FtqSize)(false.B))
  private val entryPtr    = Reg(Vec(FtqSize, new FtqPtr))
  private val allowedCfi  = Reg(Vec(FtqSize, UInt(FetchBlockInstNum.W)))
  private val nextEntries = WireInit(entries)
  private val nextValid   = WireInit(entryValid)
  private val nextPtr     = WireInit(entryPtr)
  private val nextAllowed = WireInit(allowedCfi)

  when(io.prediction.valid) {
    val idx = io.prediction.bits.ftqPtr.value
    nextEntries(idx).bpuPerf := io.prediction.bits.meta
    nextEntries(idx).isCfi.foreach(_ := false.B)
    nextEntries(idx).mispredict          := false.B
    nextEntries(idx).ifuRedirect.valid   := false.B
    nextEntries(idx).ifuCorrection.valid := false.B
    nextEntries(idx).backendTruncated    := false.B
    nextValid(idx)                       := true.B
    nextPtr(idx)                         := io.prediction.bits.ftqPtr
    nextAllowed(idx)                     := Fill(FetchBlockInstNum, true.B)
  }

  // A non-branch redirect can squash branches that have already resolved in its own block.
  // Keep the surviving prefix as a persistent mask, so late resolves cannot restore the suffix.
  // Use the saved start PC: redirect.pc is already offset for replays and unspecified for ROB flushes.
  private val redirectPosition = getAlignedPosition(
    nextEntries(io.backendRedirect.bits.ftqIdx.value).bpuPerf.startPc,
    io.backendRedirect.bits.ftqOffset
  )._1
  // Keep only positions that survive the redirect, excluding the redirecting instruction for flushItself.
  // This mask removes squashed CFIs already recorded and prevents late resolves from restoring them.
  private val redirectMask = UIntToMask(
    redirectPosition +& !io.backendRedirect.bits.flushItself(),
    FetchBlockInstNum
  )
  // recovered* is the S3-updated state after redirect recovery: younger entries are invalidated,
  // and the redirecting entry retains only its surviving prefix before resolve updates are merged.
  private val recoveredEntries = WireInit(nextEntries)
  private val recoveredValid   = WireInit(nextValid)
  private val recoveredAllowed = WireInit(nextAllowed)
  for (i <- 0 until FtqSize) {
    when(io.backendRedirect.valid && nextValid(i)) {
      when(nextPtr(i) > io.backendRedirect.bits.ftqIdx) {
        recoveredEntries(i).isCfi.foreach(_ := false.B)
        recoveredEntries(i).mispredict          := false.B
        recoveredEntries(i).ifuRedirect.valid   := false.B
        recoveredEntries(i).ifuCorrection.valid := false.B
        recoveredValid(i)                       := false.B
        recoveredAllowed(i)                     := 0.U
      }.elsewhen(nextPtr(i) === io.backendRedirect.bits.ftqIdx) {
        recoveredEntries(i).isCfi := (nextEntries(i).isCfi.asUInt & redirectMask).asBools
        recoveredEntries(i).mispredict := nextEntries(i).mispredict &&
          redirectMask(nextEntries(i).mispredictBranchInfo.cfiPosition)
        recoveredEntries(i).ifuRedirect.valid := nextEntries(i).ifuRedirect.valid &&
          redirectMask(nextEntries(i).ifuRedirect.bits)
        recoveredEntries(i).ifuCorrection.valid := nextEntries(i).ifuCorrection.valid &&
          redirectMask(nextEntries(i).ifuCorrection.bits)
        recoveredEntries(i).backendTruncated := nextEntries(i).backendTruncated || !io.backendRedirect.bits.isMisPred
        recoveredAllowed(i)                  := nextAllowed(i) & redirectMask
      }
    }
  }

  // Match ResolveQueue's three-cycle redirect window: issued branches may still resolve after
  // their entries were flushed, including after the physical FTQ slot has been allocated again.
  private val redirect1       = RegNext(io.backendRedirect, 0.U.asTypeOf(Valid(new Redirect)))
  private val redirect2       = RegNext(redirect1, 0.U.asTypeOf(Valid(new Redirect)))
  private val recentRedirects = Seq(io.backendRedirect, redirect1, redirect2)
  private val accepted = io.resolve.map { branch =>
    val idx      = branch.bits.ftqIdx.value
    val idxOH    = UIntToOH(idx, FtqSize)
    val position = getAlignedPosition(branch.bits.pc, branch.bits.ftqOffset)._1
    val flushed = recentRedirects.map { redirect =>
      redirect.valid && (branch.bits.ftqIdx > redirect.bits.ftqIdx ||
        branch.bits.ftqIdx === redirect.bits.ftqIdx &&
        (branch.bits.ftqOffset > redirect.bits.ftqOffset ||
          branch.bits.ftqOffset === redirect.bits.ftqOffset && redirect.bits.flushItself()))
    }.reduce(_ || _)
    // Explicit read muxes avoid stale index dependencies in GSIM's array alias optimization.
    branch.valid && Mux1H(idxOH, recoveredValid) && Mux1H(idxOH, nextPtr) === branch.bits.ftqIdx &&
    Mux1H(idxOH, recoveredAllowed)(position) && !flushed
  }

  // IFU corrections are counted separately. They must not make a corrected backend execution
  // appear to validate the original BPU prediction. canTrain is not a correction-valid flag.
  private val observedEntries = WireInit(recoveredEntries)
  private val ifuIdx          = io.ifuRedirect.bits.ftqIdx.value
  private val ifuPosition = getAlignedPosition(
    recoveredEntries(ifuIdx).bpuPerf.startPc,
    io.ifuRedirect.bits.ftqOffset
  )._1
  private val ifuFlushed = recentRedirects.map { redirect =>
    redirect.valid && (io.ifuRedirect.bits.ftqIdx > redirect.bits.ftqIdx ||
      io.ifuRedirect.bits.ftqIdx === redirect.bits.ftqIdx &&
      (io.ifuRedirect.bits.ftqOffset > redirect.bits.ftqOffset ||
        io.ifuRedirect.bits.ftqOffset === redirect.bits.ftqOffset && redirect.bits.flushItself()))
  }.reduce(_ || _)
  when(io.ifuRedirect.valid && recoveredValid(ifuIdx) && nextPtr(ifuIdx) === io.ifuRedirect.bits.ftqIdx &&
    recoveredAllowed(ifuIdx)(ifuPosition) && !ifuFlushed) {
    when(!recoveredEntries(ifuIdx).ifuRedirect.valid || ifuPosition < recoveredEntries(ifuIdx).ifuRedirect.bits) {
      observedEntries(ifuIdx).ifuRedirect.valid := true.B
      observedEntries(ifuIdx).ifuRedirect.bits  := ifuPosition
    }
    when(io.ifuCorrection &&
      (!recoveredEntries(ifuIdx).ifuCorrection.valid || ifuPosition < recoveredEntries(ifuIdx).ifuCorrection.bits)) {
      observedEntries(ifuIdx).ifuCorrection.valid := true.B
      observedEntries(ifuIdx).ifuCorrection.bits  := ifuPosition
    }
  }

  entries    := observedEntries
  entryValid := recoveredValid
  entryPtr   := nextPtr
  allowedCfi := recoveredAllowed

  // Merge resolve ports in the same cycle so updates to one FTQ entry are not overwritten.
  // mergedPerfMetas(i) holds the accumulated state after processing port i, for use by later ports.
  // Each port starts from the nearest preceding accepted port with the same full FTQ pointer,
  // or from observedEntries when no preceding port matches. This is a combinational forwarding chain.
  private val mergedPerfMetas = Wire(Vec(backendParams.BrhCnt, new PerfMeta))
  io.resolve.zipWithIndex.foreach { case (branch, i) =>
    val idx = branch.bits.ftqIdx.value
    val basePerfMeta = WireInit(MuxCase(
      Mux1H(UIntToOH(idx, FtqSize), observedEntries),
      (0 until i).reverse.map { j =>
        (accepted(j) && io.resolve(j).bits.ftqIdx === branch.bits.ftqIdx) -> mergedPerfMetas(j)
      }
    ))
    val curPerfMeta   = WireInit(basePerfMeta)
    val newBranchInfo = Wire(new BranchInfo)
    newBranchInfo.fromResolve(branch.bits)
    val curOH     = UIntToOH(newBranchInfo.cfiPosition, FetchBlockInstNum)
    val validMask = UIntToMask(newBranchInfo.cfiPosition +& 1.U, FetchBlockInstNum)
    val beforeKnownMispredict = !basePerfMeta.mispredict ||
      newBranchInfo.cfiPosition < basePerfMeta.mispredictBranchInfo.cfiPosition

    when(accepted(i)) {
      when(beforeKnownMispredict) {
        curPerfMeta.isCfi := (basePerfMeta.isCfi.asUInt | curOH).asBools
        curPerfMeta.cfiAttr := VecInit(basePerfMeta.cfiAttr.zipWithIndex.map { case (attr, pos) =>
          Mux(curOH(pos), newBranchInfo.attribute, attr)
        })
        when(branch.bits.mispredict) {
          curPerfMeta.mispredict           := true.B
          curPerfMeta.mispredictBranchInfo := newBranchInfo
          curPerfMeta.isCfi                := ((basePerfMeta.isCfi.asUInt | curOH) & validMask).asBools
        }
      }
      entries(idx) := curPerfMeta
    }
    mergedPerfMetas(i) := curPerfMeta
  }

  io.entries     := entries
  io.commitValid := entryValid(io.commitPtr.value) && entryPtr(io.commitPtr.value) === io.commitPtr
}
