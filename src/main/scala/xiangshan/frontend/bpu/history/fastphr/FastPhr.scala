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

// FastPhr: a fast short shift-register cache of the predicted path history, giving pTAGE
// resident folded-history registers. two recovery paths sit above the steady advance, taken
// in priority order redirect > override > steady; see FastPhrIO for the recovery contract.
class FastPhr(implicit p: Parameters) extends FastPhrModule with HasFastPhrParameters with Helpers {
  val io: FastPhrIO = IO(new FastPhrIO)

  private val phr = RegInit(0.U(WindowLength.W)) // bit0 is newest
  private val foldedHist =
    RegInit(0.U.asTypeOf(new PhrAllFoldedHistories(FastFoldedHistoryInfo, MaxUpdateNum)))

  // The advance mirrors Phr's own update, so this window stays a true cache of it: shift the window by one Shamt per
  // block and XOR the group's token over the newest bits. The token carries both halves of each block's path hash,
  // which is why this is an XOR and not an OR -- the hash's high half lands on bits the shift already moved there.
  // insertNum only ever takes 3 discrete values (0, Shamt, 2*Shamt), so the shift is realized as a select over 3
  // fixed-shamt shifters instead of a runtime barrel shift. shared by the steady advance and an override's corrected
  // replay, which apply the identical shift to different base windows
  private def advance(base: UInt, token: UInt, numBlocksOH: Vec[Bool]): UInt = Mux1H(
    numBlocksOH,
    Seq(
      base,
      ((base << Shamt).asUInt ^ token(PathHashWidth - 1, 0))(WindowLength - 1, 0),
      ((base << (2 * Shamt)).asUInt ^ token)(WindowLength - 1, 0)
    )
  )

  private def steadyPhrNext:    UInt                  = advance(phr, io.token, io.numBlocksOH)
  private def steadyFoldedNext: PhrAllFoldedHistories = foldStep(foldedHist, phr, io.token, io.numBlocksOH)

  // Pipeline of the pre-update {phr, foldedHist}, following the group it was captured for down the predictor
  // pipeline: captured when a group advances out of s1, carried to s3 when that group leaves s2. Each stage's
  // snapshot is therefore the state the group now in that stage started from, which is what an override at that
  // stage has to replay from. Gating both stages on the s1 advance instead would count advances rather than track
  // the group, and a bubble between the two would leave snapS3 one group too old.
  private val snapS2 = RegInit(0.U.asTypeOf(new FastPhrSnapshot))
  private val snapS3 = RegInit(0.U.asTypeOf(new FastPhrSnapshot))
  when(io.valid) {
    snapS2.phr    := phr
    snapS2.folded := foldedHist
  }
  when(io.s2Fire) {
    snapS3 := snapS2
  }

  private val overrideSnap    = Mux(io.overrideFromS3, snapS3, snapS2)
  private val overridePhrNext = advance(overrideSnap.phr, io.overrideToken, io.overrideNumBlocksOH)
  private val overrideFoldedNext =
    foldStep(overrideSnap.folded, overrideSnap.phr, io.overrideToken, io.overrideNumBlocksOH)

  phr := MuxCase(
    phr,
    Seq(
      io.redirect.valid -> io.redirect.phr,
      io.overrideValid  -> overridePhrNext,
      io.valid          -> steadyPhrNext
    )
  )
  foldedHist := MuxCase(
    foldedHist,
    Seq(
      io.redirect.valid -> refold(io.redirect.phr),
      io.overrideValid  -> overrideFoldedNext,
      io.valid          -> steadyFoldedNext
    )
  )

  // The ahead read that consumes this indexes a table with it, so the value has to be a function of the path and
  // nothing else. The register alone is not: it takes a group's contribution the cycle after that group passes s1,
  // so whether the most recent group is in it depends on whether the pipeline happened to bubble. The same point in
  // the program would then index two different sets, and an entry trained under one would never be found under the
  // other. Hand out the value the register is about to take instead, which is the same bypass the architectural
  // history does for its own pending write.
  io.foldedHist := foldedHist
  // What an ahead-indexed read must use. The register above takes a group's contribution the cycle after that group
  // passes s1, so whether the most recent group is in it depends on whether the pipeline happened to bubble -- the
  // same point in the program would index two different sets, and an entry trained under one would never be found
  // under the other. This is the value the register is about to take, the same bypass the architectural history does
  // for its own pending write. It is deliberately not io.foldedHist, which stays equal to a fold of the cached window
  // so that the window check keeps meaning what it says.
  io.foldedHistAhead := MuxCase(
    foldedHist,
    Seq(
      io.redirect.valid -> refold(io.redirect.phr),
      // The snapshot, not the corrected group folded into it. A read that indexes a cycle ahead keys an entry on the
      // group before the one it answers for, paired with the history that group started from, so a correction hands
      // back the state the corrected group started from -- which is what the snapshot holds. Folding the correction
      // in as well would key the replayed read one group further on than every other read, and the entries the two
      // build would never be found by each other.
      io.overrideValid -> overrideSnap.folded,
      io.valid         -> steadyFoldedNext
    )
  )
  io.debug_phr := phr
}
