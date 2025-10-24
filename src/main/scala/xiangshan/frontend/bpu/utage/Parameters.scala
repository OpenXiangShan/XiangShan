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

package xiangshan.frontend.bpu.utage

import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters
import xiangshan.frontend.bpu.MicroTageInfo
import xiangshan.frontend.bpu.TageTableInfo

case class MicroTageParameters(
    TableInfos: Seq[MicroTageInfo] = Seq(
      new MicroTageInfo(128, 4),
      new MicroTageInfo(64, 8),
      new MicroTageInfo(64, 13)
    ),
    NumWays:       Int = 1,
    TakenCtrWidth: Int = 3,
    TagWidth:      Int = 13,
    NumTables:     Int = 3,
    TickWidth:     Int = 3
) {}

trait HasMicroTageParameters extends HasBpuParameters {
  val utageParameters: MicroTageParameters = MicroTageParameters()
  def TableInfos:      Seq[MicroTageInfo]  = utageParameters.TableInfos
  def NumWays:         Int                 = utageParameters.NumWays
  def TakenCtrWidth:   Int                 = utageParameters.TakenCtrWidth
  def TagWidth:        Int                 = utageParameters.TagWidth
  def NumTables:       Int                 = utageParameters.NumTables
  def TickWidth:       Int                 = utageParameters.TickWidth

  def TestPredIdxWidth: Int = log2Ceil(TableInfos(0).NumSets)
  def TestPredTagWidth: Int = log2Ceil(TableInfos(0).HistoryLength)
  // abtb can only be fast-trained, we don't have continous predict block on resolve
  def EnableFastTrain: Boolean = false
}
