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

package xiangshan.frontend.bpu.history.phr

import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.bpu.HasBpuParameters

case class PhrParameters(
    Shamt:          Int = 2,         // shift amount for Phr
    EnableTwoTaken: Boolean = false, // enable two-taken support in Phr
    PathHashWidth:  Int = 15,
    // ensure history length is a multiple of this value
    // default is 4, when history value is displayed in hexadecimal, it has better readability
    HistoryAlign: Int = 4
) {}

// the same quantity as HasPhrParameters.MaxUpdateNum, reachable from a constructor argument list, where trait
// members are not yet in scope
object PhrMaxUpdateNum {
  // the most a single cycle can shift into the path history: one path hash's worth of shift bits per taken block
  def apply(p: Parameters): Int = {
    val params = p(XSCoreParamsKey).frontendParameters
    params.MaxPredictionNum * params.bpuParameters.phrParameters.Shamt
  }
}

trait HasPhrParameters extends HasBpuParameters {
  def phrParameters: PhrParameters = bpuParameters.phrParameters

  def Shamt: Int = phrParameters.Shamt

  // the most a single cycle may shift into the path history: one block's shift bits per block of a group
  def MaxUpdateNum: Int = MaxPredictionNum * Shamt

  // A group's whole contribution to the path history, as bits to XOR over the shifted window. A taken block's shift
  // bits and its hash-high overlay are the two halves of one path hash, so one block contributes a whole hash and
  // each further block's hash sits Shamt above the last.
  def TokenWidth:        Int     = PathHashWidth + (MaxPredictionNum - 1) * Shamt
  def EnableTwoTaken:    Boolean = phrParameters.EnableTwoTaken
  def PathHashWidth:     Int     = phrParameters.PathHashWidth
  def PathHashHighWidth: Int     = PathHashWidth - Shamt
  def MaxHistLens:       Int     = bpuParameters.tageParameters.TableInfos.map(_.HistoryLength).max

  // inherited from HasBpuParameters
  // def PhrHistoryLength: Int = PhrHistoryLength
}
