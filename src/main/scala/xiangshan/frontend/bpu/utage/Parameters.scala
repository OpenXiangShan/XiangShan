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
    // TODO: The length of the Tag and its alias status will need to be adjusted later. The same applies to the number of items.
    TableInfos: Seq[MicroTageInfo] = Seq(
      new MicroTageInfo(512, 6, 6, 15), // 3Taken maybe better than 2Taken
      // new MicroTageInfo(64, 16, 8, 18),
      // new MicroTageInfo(512, 18, 9, 15) // 6Taken maybe better than 4Taken
      new MicroTageInfo(512, 16, 12, 15) // follow Tage.
      // new MicroTageInfo(128, 32, 16, 24)
    ),
    TakenCtrWidth:       Int = 3,
    NumTables:           Int = 2,
    TickWidth:           Int = 10,
    UsefulWidth:         Int = 2,
    EnableTraceAndDebug: Boolean = false,
    BaseTableSize:       Int = 512 // TODO: Not necessarily required; currently unused.
) {}

trait HasMicroTageParameters extends HasBpuParameters {
  val utageParameters: MicroTageParameters = bpuParameters.utageParameters
  def TableInfos:      Seq[MicroTageInfo]  = utageParameters.TableInfos
  def TakenCtrWidth:   Int                 = utageParameters.TakenCtrWidth
  def NumTables:       Int                 = utageParameters.NumTables
  def TickWidth:       Int                 = utageParameters.TickWidth
  def UsefulWidth:     Int                 = utageParameters.UsefulWidth
  def BaseTableSize:   Int                 = utageParameters.BaseTableSize

  def MaxNumSets:        Int = 512
  def MaxTagLen:         Int = 16
  def DebugPredIdxWidth: Int = log2Ceil(TableInfos(0).NumSets)
  def DebugPredTagWidth: Int = TableInfos(0).TagWidth

  // utage can only be fast-trained, we don't have continous predict block on resolve
  def EnableFastTrain:     Boolean = true
  def EnableTraceAndDebug: Boolean = utageParameters.EnableTraceAndDebug

  // Hash PC into tag to reduce aliasing (at cost of capacity).
  def PCTagHashBitsForShortHistory:  Seq[Int] = Seq(15, 13, 11, 9, 8, 7, 5, 3, 1)
  def PCTagHashBitsForMediumHistory: Seq[Int] = Seq(16, 15, 13, 11, 10, 8, 6, 4, 2, 1)
  def PCTagHashXorPairsForLongHistory: Seq[(Int, Int)] = Seq(
    (17, 3),
    (16, 1),
    (15, 11),
    (13, 9),
    (12, 8),
    (10, 6),
    (7, 4),
    (5, 2)
  )
  def PCTagHashBitsDefault: Seq[Int] = Seq(1, 0)
}
