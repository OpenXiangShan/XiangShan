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

  // Next-state calculation within one cycle: registers -> prediction -> redirect -> resolve merge -> writeback.
  // 1. Current register state.
  private val entries            = Reg(Vec(FtqSize, new PerfMeta))
  private val entryValid         = RegInit(VecInit.fill(FtqSize)(false.B))
  private val entryPtr           = Reg(Vec(FtqSize, new FtqPtr))
  private val resolveAllowedMask = Reg(Vec(FtqSize, UInt(FetchBlockInstNum.W)))

  // 2. Apply the S3 prediction to the current state.
  private val entriesAfterPrediction            = WireInit(entries)
  private val entryValidAfterPrediction         = WireInit(entryValid)
  private val entryPtrAfterPrediction           = WireInit(entryPtr)
  private val resolveAllowedMaskAfterPrediction = WireInit(resolveAllowedMask)

  when(io.prediction.valid) {
    val idx = io.prediction.bits.ftqPtr.value
    entryValidAfterPrediction(idx)      := true.B
    entriesAfterPrediction(idx).bpuPerf := io.prediction.bits.meta
    entriesAfterPrediction(idx).isCfi.foreach(_ := false.B)
    entriesAfterPrediction(idx).mispredict := false.B
    entryPtrAfterPrediction(idx)           := io.prediction.bits.ftqPtr
    resolveAllowedMaskAfterPrediction(idx) := Fill(FetchBlockInstNum, true.B)
  }

  // 3. Apply redirect recovery to the state after prediction.
  private val redirectPosition = getAlignedPosition(
    entriesAfterPrediction(io.backendRedirect.bits.ftqIdx.value).bpuPerf.startPc,
    io.backendRedirect.bits.ftqOffset
  )._1
  // Keep only positions that survive the redirect, excluding the redirecting instruction for flushItself.
  // This mask removes squashed CFIs already recorded and prevents late resolves from restoring them.
  private val survivingMask = UIntToMask(
    redirectPosition +& !io.backendRedirect.bits.flushItself(),
    FetchBlockInstNum
  )
  // State after redirect recovery: younger entries are invalidated,
  // and the redirecting entry retains only its surviving prefix before resolve updates are merged.
  private val entriesAfterRedirect            = WireInit(entriesAfterPrediction)
  private val entryValidAfterRedirect         = WireInit(entryValidAfterPrediction)
  private val resolveAllowedMaskAfterRedirect = WireInit(resolveAllowedMaskAfterPrediction)

  for (i <- 0 until FtqSize) {
    when(io.backendRedirect.valid && entryValidAfterPrediction(i)) {
      when(entryPtrAfterPrediction(i) > io.backendRedirect.bits.ftqIdx) {
        entryValidAfterRedirect(i) := false.B
        entriesAfterRedirect(i).isCfi.foreach(_ := false.B)
        entriesAfterRedirect(i).mispredict := false.B
        resolveAllowedMaskAfterRedirect(i) := 0.U
      }.elsewhen(entryPtrAfterPrediction(i) === io.backendRedirect.bits.ftqIdx) {
        entriesAfterRedirect(i).isCfi := (entriesAfterPrediction(i).isCfi.asUInt & survivingMask).asBools
        entriesAfterRedirect(i).mispredict := entriesAfterPrediction(i).mispredict &&
          survivingMask(entriesAfterPrediction(i).mispredictBranchInfo.cfiPosition)
        resolveAllowedMaskAfterRedirect(i) := resolveAllowedMaskAfterPrediction(i) & survivingMask
      }
    }
  }

  // 4. Filter and merge resolve updates into the state after redirect recovery.
  // Match ResolveQueue's three-cycle redirect window: issued branches may still resolve after
  // their entries were flushed, including after the physical FTQ slot has been allocated again.
  private val redirect1       = RegNext(io.backendRedirect, 0.U.asTypeOf(Valid(new Redirect)))
  private val redirect2       = RegNext(redirect1, 0.U.asTypeOf(Valid(new Redirect)))
  private val recentRedirects = Seq(io.backendRedirect, redirect1, redirect2)
  private val resolveAccepted = io.resolve.map { branch =>
    val idx          = branch.bits.ftqIdx.value
    val idxOH        = UIntToOH(idx, FtqSize)
    val entryIsValid = Mux1H(idxOH, entryValidAfterRedirect)
    val hitEntry     = Mux1H(idxOH, entryPtrAfterPrediction) === branch.bits.ftqIdx
    val position     = getAlignedPosition(branch.bits.pc, branch.bits.ftqOffset)._1
    val allowed      = Mux1H(idxOH, resolveAllowedMaskAfterRedirect)(position)
    val flushed = recentRedirects.map { redirect =>
      redirect.valid && (branch.bits.ftqIdx > redirect.bits.ftqIdx ||
        branch.bits.ftqIdx === redirect.bits.ftqIdx &&
        (branch.bits.ftqOffset > redirect.bits.ftqOffset ||
          branch.bits.ftqOffset === redirect.bits.ftqOffset && redirect.bits.flushItself()))
    }.reduce(_ || _)
    branch.valid && entryIsValid && hitEntry && allowed && !flushed
  }

  // Merge resolve ports in the same cycle so updates to one FTQ entry are not overwritten.
  // mergedMetaByPort(i) holds the accumulated state after processing port i, for use by later ports.
  // Each port starts from the nearest preceding accepted port with the same full FTQ pointer,
  // or from entriesAfterRedirect when no preceding port matches. This is a combinational forwarding chain.
  private val entriesAfterResolve = WireInit(entriesAfterRedirect)
  private val mergedMetaByPort    = Wire(Vec(backendParams.BrhCnt, new PerfMeta))
  io.resolve.zipWithIndex.foreach { case (branch, i) =>
    val idx = branch.bits.ftqIdx.value
    val basePerfMeta = WireInit(MuxCase(
      Mux1H(UIntToOH(idx, FtqSize), entriesAfterRedirect),
      (0 until i).reverse.map { j =>
        (resolveAccepted(j) && io.resolve(j).bits.ftqIdx === branch.bits.ftqIdx) -> mergedMetaByPort(j)
      }
    ))
    val updatedPerfMeta = WireInit(basePerfMeta)
    val newBranchInfo   = Wire(new BranchInfo)
    newBranchInfo.fromResolve(branch.bits)
    val cfiPositionOH        = UIntToOH(newBranchInfo.cfiPosition, FetchBlockInstNum)
    val mispredictPrefixMask = UIntToMask(newBranchInfo.cfiPosition +& 1.U, FetchBlockInstNum)
    val beforeKnownMispredict = !basePerfMeta.mispredict ||
      newBranchInfo.cfiPosition < basePerfMeta.mispredictBranchInfo.cfiPosition

    when(resolveAccepted(i)) {
      when(beforeKnownMispredict) {
        updatedPerfMeta.isCfi := (basePerfMeta.isCfi.asUInt | cfiPositionOH).asBools
        updatedPerfMeta.cfiAttr := VecInit(basePerfMeta.cfiAttr.zipWithIndex.map { case (attr, pos) =>
          Mux(cfiPositionOH(pos), newBranchInfo.attribute, attr)
        })
        when(branch.bits.mispredict) {
          updatedPerfMeta.mispredict           := true.B
          updatedPerfMeta.mispredictBranchInfo := newBranchInfo
          updatedPerfMeta.isCfi := ((basePerfMeta.isCfi.asUInt | cfiPositionOH) & mispredictPrefixMask).asBools
        }
      }
      entriesAfterResolve(idx) := updatedPerfMeta
    }
    mergedMetaByPort(i) := updatedPerfMeta
  }

  // 5. Write back the final state. Resolve updates only change performance metadata.
  entries            := entriesAfterResolve
  entryValid         := entryValidAfterRedirect
  entryPtr           := entryPtrAfterPrediction
  resolveAllowedMask := resolveAllowedMaskAfterRedirect

  io.entries := entries
}
