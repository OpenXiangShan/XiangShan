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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters
import xiangshan.frontend.bpu.TageTableInfo

case class TageParameters(
    TableInfos: Seq[TageTableInfo] = Seq(
      // TageTableInfo(Size, NumWays, HistoryLength)
      new TageTableInfo(1024, 2, 4),
      new TageTableInfo(1024, 2, 9),
      new TageTableInfo(1024, 2, 17),
      new TageTableInfo(1024, 2, 29),
      new TageTableInfo(1024, 2, 56),
      new TageTableInfo(1024, 2, 109),
      new TageTableInfo(1024, 2, 211),
      new TageTableInfo(1024, 2, 397)
    ),
    NumBanks:            Int = 4, // to alleviate read-write conflicts in single-port SRAM
    TagWidth:            Int = 13,
    TakenCtrWidth:       Int = 3,
    UsefulCtrWidth:      Int = 2,
    UsefulCtrInitValue:  Int = 0,
    WriteBufferSize:     Int = 4,
    UsefulResetCtrWidth: Int = 8,
    UseAltOnNaWidth:     Int = 7,
    NumUseAltOnNa:       Int = 128,
    EnableTageTrace:     Boolean = false
) {}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def NumBanks:           Int = tageParameters.NumBanks
  def BankIdxWidth:       Int = log2Ceil(NumBanks)
  def TagWidth:           Int = tageParameters.TagWidth
  def TakenCtrWidth:      Int = tageParameters.TakenCtrWidth
  def UsefulCtrWidth:     Int = tageParameters.UsefulCtrWidth
  def UsefulCtrInitValue: Int = tageParameters.UsefulCtrInitValue
  def WriteBufferSize:    Int = tageParameters.WriteBufferSize

  def UsefulResetCtrWidth: Int = tageParameters.UsefulResetCtrWidth
  def UseAltOnNaWidth:     Int = tageParameters.UseAltOnNaWidth
  def NumUseAltOnNa:       Int = tageParameters.NumUseAltOnNa

  def TableInfos: Seq[TageTableInfo] = tageParameters.TableInfos

  def NumTables:     Int = TableInfos.length
  def TableIdxWidth: Int = log2Ceil(NumTables)

  def MaxNumSets:     Int = TableInfos.map(_.getNumSets(NumBanks)).max
  def MaxSetIdxWidth: Int = log2Ceil(MaxNumSets)

  def MaxNumWays:     Int = TableInfos.map(_.NumWays).max
  def MaxWayIdxWidth: Int = log2Ceil(MaxNumWays)

  // per table parameters
  def NumSets(implicit info:     TageTableInfo): Int = info.getNumSets(NumBanks)
  def SetIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumSets)
  def NumWays(implicit info:     TageTableInfo): Int = info.NumWays
  def WayIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumWays)

  def EnableTageTrace: Boolean = tageParameters.EnableTageTrace
}
