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
    nextEntries(idx).mispredict := false.B
    nextValid(idx)              := true.B
    nextPtr(idx)                := io.prediction.bits.ftqPtr
    nextAllowed(idx)            := Fill(FetchBlockInstNum, true.B)
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
        recoveredEntries(i).mispredict := false.B
        recoveredValid(i)              := false.B
        recoveredAllowed(i)            := 0.U
      }.elsewhen(nextPtr(i) === io.backendRedirect.bits.ftqIdx) {
        recoveredEntries(i).isCfi := (nextEntries(i).isCfi.asUInt & redirectMask).asBools
        recoveredEntries(i).mispredict := nextEntries(i).mispredict &&
          redirectMask(nextEntries(i).mispredictBranchInfo.cfiPosition)
        recoveredAllowed(i) := nextAllowed(i) & redirectMask
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

  entries    := recoveredEntries
  entryValid := recoveredValid
  entryPtr   := nextPtr
  allowedCfi := recoveredAllowed

  // Merge resolve ports in the same cycle so updates to one FTQ entry are not overwritten.
  // mergedPerfMetas(i) holds the accumulated state after processing port i, for use by later ports.
  // Each port starts from the nearest preceding accepted port with the same full FTQ pointer,
  // or from recoveredEntries when no preceding port matches. This is a combinational forwarding chain.
  private val mergedPerfMetas = Wire(Vec(backendParams.BrhCnt, new PerfMeta))
  io.resolve.zipWithIndex.foreach { case (branch, i) =>
    val idx = branch.bits.ftqIdx.value
    val basePerfMeta = WireInit(MuxCase(
      Mux1H(UIntToOH(idx, FtqSize), recoveredEntries),
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

  io.entries := entries
}
