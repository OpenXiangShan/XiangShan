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

package xiangshan.frontend.bpu.history.fastphr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

// external recovery: reload the cached window from the big Phr's reconstructed history
class FastPhrRedirect(implicit p: Parameters) extends FastPhrBundle with HasFastPhrParameters {
  val valid: Bool = Bool()
  val phr:   UInt = UInt(WindowLength.W)
}

// paired (phr, foldedHist) snapshot, used by the override recovery's 2-deep pre-update pipeline
class FastPhrSnapshot(implicit p: Parameters) extends FastPhrBundle with HasFastPhrParameters {
  val phr:    UInt                  = UInt(WindowLength.W)
  val folded: PhrAllFoldedHistories = new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum)
}

// steady-state IO: advance the cached window by 0/1/2 blocks per cycle and read back the
// resulting resident folded histories. two recovery paths sit above the steady advance, taken
// in priority order redirect > override > steady:
//   - redirect: reload from an externally reconstructed window (the big Phr's corrected history)
//   - override: internally re-apply a corrected group from the snapshot that group started at
class FastPhrIO(implicit p: Parameters) extends FastPhrBundle with HasFastPhrParameters {
  val valid:       Bool      = Input(Bool())
  val token:       UInt      = Input(UInt(MaxUpdateNum.W))
  val numBlocksOH: Vec[Bool] = Input(Vec(3, Bool())) // one-hot, index i means advance i blocks

  // carries a snapshot from s2 to s3 alongside the group it belongs to, so an override recovers the state that group
  // started from however many bubbles separated the two advances
  val s2Fire: Bool = Input(Bool())

  val redirect: FastPhrRedirect = Input(new FastPhrRedirect)

  val overrideValid:       Bool      = Input(Bool())
  val overrideToken:       UInt      = Input(UInt(MaxUpdateNum.W))
  val overrideNumBlocksOH: Vec[Bool] = Input(Vec(3, Bool())) // one-hot, corrected group's block count

  val foldedHist: PhrAllFoldedHistories = Output(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum))
}
