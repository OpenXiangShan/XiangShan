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
      // (NumSetsLog2, NumWays, HistoryLength); NumSets is the number of sets in each bank.
      new TageTableInfo(9, 2, 4),
      new TageTableInfo(9, 2, 9),
      new TageTableInfo(9, 2, 17),
      new TageTableInfo(9, 2, 29),
      new TageTableInfo(9, 2, 56),
      new TageTableInfo(9, 2, 109),
      new TageTableInfo(9, 2, 211),
      new TageTableInfo(9, 2, 397)
    ),
    NumBanks:              Int = 4,  // to alleviate read-write conflicts in single-port SRAM
    TagWidth:              Int = 13, // default active tag width
    TakenCtrWidth:         Int = 3,
    UsefulCtrWidth:        Int = 2,  // default active useful counter width
    UsefulCtrInitValue:    Int = 0,
    NumUsefulCtrSramFolds: Int = 8,
    WriteBufferSize:       Int = 4,
    UsefulResetCtrWidth:   Int = 8,
    UseAltOnNaWidth:       Int = 5,
    MinNumSetsLog2:        Int = 7,
    MaxNumSetsLog2:        Int = 11,
    MinNumWays:            Int = 2,
    MaxNumWays:            Int = 8,
    MinTagWidth:           Int = 10,
    MaxTagWidth:           Int = 15,
    MinUsefulCtrWidth:     Int = 1,
    MaxUsefulCtrWidth:     Int = 2,
    EnableTageTrace:       Boolean = false
) {
  require(TableInfos.forall(info => info.NumSetsLog2 >= MinNumSetsLog2 && info.NumSetsLog2 <= MaxNumSetsLog2))
  require(TableInfos.forall(info => info.NumWays >= MinNumWays && info.NumWays <= MaxNumWays))
  require(TagWidth >= MinTagWidth && TagWidth <= MaxTagWidth)
  require(UsefulCtrWidth >= MinUsefulCtrWidth && UsefulCtrWidth <= MaxUsefulCtrWidth)
}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def NumBanks:           Int = tageParameters.NumBanks
  def BankIdxWidth:       Int = log2Ceil(NumBanks)
  def TagWidth:           Int = tageParameters.MaxTagWidth
  def TakenCtrWidth:      Int = tageParameters.TakenCtrWidth
  def UsefulCtrWidth:     Int = tageParameters.MaxUsefulCtrWidth
  def UsefulCtrInitValue: Int = tageParameters.UsefulCtrInitValue
  def WriteBufferSize:    Int = tageParameters.WriteBufferSize

  def UsefulResetCtrWidth: Int = tageParameters.UsefulResetCtrWidth
  def UseAltOnNaWidth:     Int = tageParameters.UseAltOnNaWidth
  def NumUseAltOnNa:       Int = 2 * NumTables // each table corresponds to two counters
  def UseAltOnNaIdxWidth:  Int = log2Ceil(NumUseAltOnNa)

  def TableInfos: Seq[TageTableInfo] = tageParameters.TableInfos

  def NumTables:     Int = TableInfos.length
  def TableIdxWidth: Int = log2Ceil(NumTables)

  def MinNumSetsLog2: Int = tageParameters.MinNumSetsLog2
  def MaxNumSetsLog2: Int = tageParameters.MaxNumSetsLog2
  def MinNumSets:     Int = 1 << MinNumSetsLog2
  def MaxNumSets:     Int = 1 << MaxNumSetsLog2
  def MaxSetIdxWidth: Int = MaxNumSetsLog2

  def MinNumWays:     Int = tageParameters.MinNumWays
  def MaxNumWays:     Int = tageParameters.MaxNumWays
  def MaxWayIdxWidth: Int = log2Ceil(MaxNumWays)

  def ActiveNumSetsLog2Width:    Int = log2Ceil(MaxNumSetsLog2 + 1)
  def ActiveNumWaysWidth:        Int = log2Ceil(MaxNumWays + 1)
  def ActiveTagWidthWidth:       Int = log2Ceil(TagWidth + 1)
  def ActiveUsefulCtrWidthWidth: Int = log2Ceil(UsefulCtrWidth + 1)

  def SupportedNumSets: Seq[Int] =
    (MinNumSetsLog2 to MaxNumSetsLog2).map(1 << _)
  def SupportedTagWidths: Seq[Int] =
    tageParameters.MinTagWidth to tageParameters.MaxTagWidth

  // per table parameters
  def NumSets(implicit info:     TageTableInfo): Int = MaxNumSets
  def SetIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumSets)
  def NumWays(implicit info:     TageTableInfo): Int = MaxNumWays
  def WayIdxWidth(implicit info: TageTableInfo): Int = log2Ceil(NumWays)

  def NumUsefulCtrSramFolds: Int = tageParameters.NumUsefulCtrSramFolds

  def EnableTageTrace: Boolean = tageParameters.EnableTageTrace
}
