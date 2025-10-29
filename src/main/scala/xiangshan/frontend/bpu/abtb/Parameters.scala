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

package xiangshan.frontend.bpu.abtb

import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters

case class AheadBtbParameters(
    NumEntries:           Int = 1024,
    NumBanks:             Int = 4,
    NumWays:              Int = 8,
    TagWidth:             Int = 24,
    TargetLowerBitsWidth: Int = 22,
    WriteBufferSize:      Int = 4,
    TakenCounterWidth:    Int = 2,
    // enable carry and borrow fix for target, so jumps around 2^(TargetWidth+1) boundary will not cause misprediction
    // mainBtb should handle this case, so performance affect should be slight, and, bad for timing
    EnableTargetFix: Boolean = false
) {}

trait HasAheadBtbParameters extends HasBpuParameters {
  def abtbParameters: AheadBtbParameters = bpuParameters.abtbParameters

  def NumEntries:           Int = abtbParameters.NumEntries
  def NumBanks:             Int = abtbParameters.NumBanks
  def NumWays:              Int = abtbParameters.NumWays
  def NumSets:              Int = NumEntries / NumWays / NumBanks
  def TagWidth:             Int = abtbParameters.TagWidth
  def TargetLowerBitsWidth: Int = abtbParameters.TargetLowerBitsWidth
  def SetIdxWidth:          Int = log2Ceil(NumSets)
  def WayIdxWidth:          Int = log2Ceil(NumWays)
  def BankIdxWidth:         Int = log2Ceil(NumBanks)
  def WriteBufferSize:      Int = abtbParameters.WriteBufferSize
  def TakenCounterWidth:    Int = abtbParameters.TakenCounterWidth

  def EnableTargetFix: Boolean = abtbParameters.EnableTargetFix
}
