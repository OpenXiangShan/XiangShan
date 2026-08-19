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

package xiangshan.frontend.bpu.ptage

import chisel3.util._
import xiangshan.frontend.bpu.history.fastphr.HasFastPhrParameters

// default pTAGE parameters, do not change here, use top-level xiangshan/Parameters.scala
case class PtageParameters(
    NumBanks: Int = 4,
    NumSets:  Int = 256,
    TagWidth: Int = 8,
    // low bits of a block's next pc kept in the entry. Read straight out and concatenated with the current pc's upper
    // bits, so the fast loop never pays for an adder; a target reaching beyond this range cannot be a pTAGE entry.
    NextPcLowWidth:  Int = 11,
    CounterWidth:    Int = 2,
    WriteBufferSize: Int = 4,
    // how many refused allocations it takes before one is allowed to evict an entry that is marked useful
    AllocRefusalLimitWidth: Int = 5
) {
  require(isPow2(NumBanks), "pTAGE banks are selected by pc bits, so the count must be a power of two")
  require(isPow2(NumSets), "pTAGE sets are selected by a folded history XOR, so the count must be a power of two")
  require(CounterWidth > 1, "a direction counter needs a strength bit as well as a direction bit")
}

trait HasPtageParameters extends HasFastPhrParameters {
  def ptageParameters: PtageParameters = bpuParameters.ptageParameters

  def NumBanks:               Int = ptageParameters.NumBanks
  def NumSets:                Int = ptageParameters.NumSets
  def TagWidth:               Int = ptageParameters.TagWidth
  def NextPcLowWidth:         Int = ptageParameters.NextPcLowWidth
  def CounterWidth:           Int = ptageParameters.CounterWidth
  def WriteBufferSize:        Int = ptageParameters.WriteBufferSize
  def AllocRefusalLimitWidth: Int = ptageParameters.AllocRefusalLimitWidth

  // One table per FastPhr history span: a table is indexed by the folded history of its own span, which is what makes
  // the geometric-history structure of TAGE. The two therefore have to be configured together.
  def NumTables:  Int = fastPhrParameters.Spans.length
  def NumEntries: Int = NumTables * NumBanks * NumSets

  def SetIdxWidth:  Int = log2Ceil(NumSets)
  def BankIdxWidth: Int = log2Ceil(NumBanks)

  // A group advances the path history once per taken block, so the token has to carry that many blocks' worth of bits.
  def GhrShamtWidth: Int = log2Ceil(MaxPredictionNum + 1)

  require(
    fastPhrParameters.IdxWidth == SetIdxWidth,
    s"FastPhr folds its index history to ${fastPhrParameters.IdxWidth} bits but pTAGE needs $SetIdxWidth to index a set"
  )
  require(
    fastPhrParameters.TagWidth == TagWidth,
    s"FastPhr folds its tag history to ${fastPhrParameters.TagWidth} bits but pTAGE tags are $TagWidth bits"
  )
}
