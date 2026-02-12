// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
      new MicroTageInfo(512, 5, 5, 15),
      new MicroTageInfo(512, 9, 9, 15), // 3Taken maybe better than 2Taken
      // new MicroTageInfo(512, 12, 12, 15),
      new MicroTageInfo(512, 16, 10, 16), // follow Tage.
      new MicroTageInfo(512, 24, 12, 16)
    ),
    TakenCtrWidth:       Int = 3,
    LowTickWidth:        Int = 7,
    HighTickWidth:       Int = 8,
    UsefulWidth:         Int = 2,
    PCHighTagStart:      Int = 7,
    EnableTraceAndDebug: Boolean = false,
    BaseTableSize:       Int = 512, // TODO: Not necessarily required; currently unused.
    NumWays:             Int = 1
) {}

trait HasMicroTageParameters extends HasBpuParameters {
  val utageParameters: MicroTageParameters = bpuParameters.utageParameters
  def TableInfos:      Seq[MicroTageInfo]  = utageParameters.TableInfos
  def TakenCtrWidth:   Int                 = utageParameters.TakenCtrWidth
  def NumTables:       Int                 = TableInfos.length
  def LowTickWidth:    Int                 = utageParameters.LowTickWidth
  def HighTickWidth:   Int                 = utageParameters.HighTickWidth
  def UsefulWidth:     Int                 = utageParameters.UsefulWidth
  def BaseTableSize:   Int                 = utageParameters.BaseTableSize
  def PCHighTagStart:  Int                 = utageParameters.PCHighTagStart
  def NumWays:         Int                 = utageParameters.NumWays

  def MaxNumSets:        Int = 512
  def MaxTagLen:         Int = 16
  def DebugPredIdxWidth: Int = log2Ceil(TableInfos(0).NumSets)
  def DebugPredTagWidth: Int = TableInfos(0).TagWidth

  // utage can only be fast-trained, we don't have continous predict block on resolve
  def EnableFastTrain:     Boolean = false
  def EnableTraceAndDebug: Boolean = utageParameters.EnableTraceAndDebug

  // Hash PC into tag to reduce aliasing (at cost of capacity).
  def PCTagHashBitsForShortHistory:  Seq[Int] = Seq(23, 20, 17, 14, 11, 9, 7, 5, 3, 2, 1)
  def PCTagHashBitsForMediumHistory: Seq[Int] = Seq(15, 13, 11, 9, 7, 6, 5, 4, 3, 2, 1)
  def PCTagHashBitsForLongHistory:   Seq[Int] = Seq(18, 16, 14, 12, 10, 6, 5, 4, 2, 1)
  def PCTagHashBitsDefault:          Seq[Int] = Seq(31, 19, 12, 7, 4, 2, 1, 0)

  // The PHR’s history generation incorporates the target address; during a branch,
  // this target address matches the starting address of the next predicted basic block.
  // Fine-grained adjustments may be beneficial during folding.
  def PCTagConcatBitsForShortHistory:    Seq[Int] = Seq(15, 13, 11, 9, 8, 7, 5, 4, 3, 2, 1)
  def PCTagConcatBitsForMediumHistory:   Seq[Int] = Seq(17, 15, 13, 11, 10, 9, 8, 5, 3, 1)
  def PCTagConcatBitsForLongHistory:     Seq[Int] = Seq(11, 10, 9, 7, 5, 3)
  def PCTagConcatBitsForVeryLongHistory: Seq[Int] = Seq(11, 7, 5, 3)

  def PCTagXorBitsForShortHistory:    Seq[Int] = Seq(12, 10, 8, 6, 4, 2)
  def PCTagXorBitsForMediumHistory:   Seq[Int] = Seq(16, 14, 12, 10, 8, 6, 4, 2, 0)
  def PCTagXorBitsForLongHistory:     Seq[Int] = Seq(18, 16, 14, 12, 10, 8, 6, 4, 2, 0)
  def PCTagXorBitsForVeryLongHistory: Seq[Int] = Seq(20, 18, 16, 14, 12, 10, 8, 6, 4, 2, 1, 0)

  def PCTagConcatBitsDefault: Seq[Int] = PCTagConcatBitsForShortHistory
  def PCTagXorBitsDefault:    Seq[Int] = PCTagXorBitsForShortHistory
}
