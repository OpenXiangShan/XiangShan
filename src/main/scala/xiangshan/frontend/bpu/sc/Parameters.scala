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

package xiangshan.frontend.bpu.sc

import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters
import xiangshan.frontend.bpu.ScTableInfo

case class ScParameters(
    PathTableInfos: Seq[ScTableInfo] = Seq(
      new ScTableInfo(1024, 8),
      new ScTableInfo(1024, 16)
    ),
    GlobalTableInfos: Seq[ScTableInfo] = Seq(
      new ScTableInfo(1024, 8),
      new ScTableInfo(1024, 16)
    ),
    biasTableSize:       Int = 1024,
    biasUseTageBitWidth: Int = 4, // use tage_taken and tage_low_conf bits as index bits
    ctrWidth:            Int = 6,
    weightCtrWidth:      Int = 6,
    thresholdThresWidth: Int = 10,
    NumTables:           Int = 2,
    NumBanks:            Int = 2,
    WriteBufferSize:     Int = 4,
    TagWidth:            Int = 12
) {}

trait HasScParameters extends HasBpuParameters {
  def scParameters:        ScParameters     = bpuParameters.scParameters
  def tageTakenCtrWidth:   Int              = bpuParameters.tageParameters.TakenCtrWidth
  def ctrWidth:            Int              = scParameters.ctrWidth
  def NumWays:             Int              = NumBtbResultEntries
  def NumBanks:            Int              = scParameters.NumBanks
  def BankWidth:           Int              = log2Ceil(NumBanks)
  def weightCtrWidth:      Int              = scParameters.weightCtrWidth
  def thresholdThresWidth: Int              = scParameters.thresholdThresWidth
  def PathTableInfos:      Seq[ScTableInfo] = scParameters.PathTableInfos
  def PathTableSize:       Int              = PathTableInfos.length
  def NumPathTables:       Int              = PathTableInfos.length
  def GlobalTableInfos:    Seq[ScTableInfo] = scParameters.GlobalTableInfos
  def GlobalTableSize:     Int              = GlobalTableInfos.length
  def BiasTableSize:       Int              = scParameters.biasTableSize
  def BiasUseTageBitWidth: Int              = scParameters.biasUseTageBitWidth
  def BiasTableNumWays: Int =
    NumWays * BiasUseTageBitWidth // add tage_taken and tage_low_conf bits as wayIdx
  def WriteBufferSize: Int = scParameters.WriteBufferSize
  def TagWidth:        Int = scParameters.TagWidth
}
