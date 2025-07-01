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
    NumEntries:         Int = 1024,
    NumWays:            Int = 4,
    TagLen:             Int = 24,
    TargetLowerBitsLen: Int = 22,
    NumBanks:           Int = 4,
    WriteBufferSize:    Int = 4,
    TakenCounterWidth:  Int = 2,
    UsefulCounterWidth: Int = 3
) {}

trait HasAheadBtbParameters extends HasBpuParameters {
  def aBtbParameters: AheadBtbParameters = bpuParameters.aBtbParameters

  def NumEntries:         Int = aBtbParameters.NumEntries
  def NumWays:            Int = aBtbParameters.NumWays
  def NumSets:            Int = NumEntries / NumWays
  def TagLen:             Int = aBtbParameters.TagLen
  def TargetLowerBitsLen: Int = aBtbParameters.TargetLowerBitsLen
  def NumBanks:           Int = aBtbParameters.NumBanks
  def SetIdxLen:          Int = log2Ceil(NumSets)
  def WayIdxLen:          Int = log2Ceil(NumWays)
  def BankIdxLen:         Int = log2Ceil(NumBanks)
  def WriteBufferSize:    Int = aBtbParameters.WriteBufferSize
  def TakenCounterWidth:  Int = aBtbParameters.TakenCounterWidth
  def UsefulCounterWidth: Int = aBtbParameters.UsefulCounterWidth
}
