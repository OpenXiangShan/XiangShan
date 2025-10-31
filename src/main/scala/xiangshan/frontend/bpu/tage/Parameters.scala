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
    BaseTableSize:          Int = 1024 * 8,
    BaseTableTakenCtrWidth: Int = 2,
    TableInfos: Seq[TageTableInfo] = Seq(
      // TageTableInfo(NumSets, HistoryLength)
      new TageTableInfo(1024, 4),
      new TageTableInfo(1024, 9),
      new TageTableInfo(1024, 17),
      new TageTableInfo(1024, 31),
      new TageTableInfo(1024, 58),
      new TageTableInfo(1024, 109),
      new TageTableInfo(1024, 211),
      new TageTableInfo(1024, 407)
    ),
    NumWays:             Int = 2,
    NumBanks:            Int = 4, // to alleviate read-write conflicts in single-port SRAM
    TagWidth:            Int = 13,
    TakenCtrWidth:       Int = 3,
    UsefulCtrWidth:      Int = 2,
    UsefulCtrInitValue:  Int = 0,
    WriteBufferSize:     Int = 4,
    UsefulResetCtrWidth: Int = 7,
    UseAltCtrWidth:      Int = 7,
    NumUseAltCtrs:       Int = 128
) {}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def BaseTableNumSets:       Int = tageParameters.BaseTableSize / NumBanks / FetchBlockInstNum
  def BaseTableSetIdxWidth:   Int = log2Ceil(BaseTableNumSets)
  def BaseTableNumAlignBanks: Int = FetchBlockSize / FetchBlockAlignSize
  def BaseTableTakenCtrWidth: Int = tageParameters.BaseTableTakenCtrWidth

  def NumWays:            Int = tageParameters.NumWays
  def NumBanks:           Int = tageParameters.NumBanks
  def BankIdxWidth:       Int = log2Ceil(NumBanks)
  def TagWidth:           Int = tageParameters.TagWidth
  def TakenCtrWidth:      Int = tageParameters.TakenCtrWidth
  def UsefulCtrWidth:     Int = tageParameters.UsefulCtrWidth
  def UsefulCtrInitValue: Int = tageParameters.UsefulCtrInitValue
  def WriteBufferSize:    Int = tageParameters.WriteBufferSize

  def UsefulResetCtrWidth: Int = tageParameters.UsefulResetCtrWidth
  def UseAltCtrWidth:      Int = tageParameters.UseAltCtrWidth
  def NumUseAltCtrs:       Int = tageParameters.NumUseAltCtrs

  def TableInfos: Seq[TageTableInfo] = tageParameters.TableInfos
  def NumTables:  Int                = TableInfos.length
}
