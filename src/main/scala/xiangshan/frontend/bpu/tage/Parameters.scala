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
    BaseTableSize:          Int = 8192,
    BaseTableInternalBanks: Int = 4,
    BaseTableCtrWidth:      Int = 2,
    TableInfos: Seq[TageTableInfo] = Seq(
      // Table size, history length, NumWay
      new TageTableInfo(1024, 4, 3),
      new TageTableInfo(1024, 9, 3),
      new TageTableInfo(1024, 17, 3),
      new TageTableInfo(1024, 31, 3),
      new TageTableInfo(1024, 58, 3),
      new TageTableInfo(1024, 109, 3),
      new TageTableInfo(1024, 211, 3),
      new TageTableInfo(1024, 407, 3)
    ),
    NumInternalBanks: Int = 2,
    TagWidth:         Int = 13,
    CtrWidth:         Int = 3,
    UsefulWidth:      Int = 2,
    WriteBufferSize:  Int = 4
) {}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def BaseTableSize:                Int                = tageParameters.BaseTableSize
  def BaseTableInternalBanks:       Int                = tageParameters.BaseTableInternalBanks
  def BaseTableInternalBanksIdxLen: Int                = log2Ceil(BaseTableInternalBanks)
  def BaseTableSramSets:            Int                = BaseTableSize / BaseTableInternalBanks / FetchBlockInstNum
  def BaseTableSetIdxLen:           Int                = log2Ceil(BaseTableSramSets)
  def BaseTableNumAlignBanks:       Int                = FetchBlockSize / FetchBlockAlignSize
  def BaseTableCtrWidth:            Int                = tageParameters.BaseTableCtrWidth
  def TableInfos:                   Seq[TageTableInfo] = tageParameters.TableInfos
  def NumInternalBanks:             Int                = tageParameters.NumInternalBanks
  def TagWidth:                     Int                = tageParameters.TagWidth
  def CtrWidth:                     Int                = tageParameters.CtrWidth
  def UsefulWidth:                  Int                = tageParameters.UsefulWidth
  def WriteBufferSize:              Int                = tageParameters.WriteBufferSize

  def NumTables: Int = TableInfos.length
}
