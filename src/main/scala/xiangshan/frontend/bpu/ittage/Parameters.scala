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

package xiangshan.frontend.bpu.ittage

import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters
import xiangshan.frontend.bpu.IttageTableInfo

case class IttageParameters(
    TableInfos: Seq[IttageTableInfo] = Seq(
      new IttageTableInfo(256, 4),
      new IttageTableInfo(256, 8),
      new IttageTableInfo(512, 13),
      new IttageTableInfo(512, 16),
      new IttageTableInfo(512, 32)
    ),
    TagWidth:           Int = 9,
    ConfidenceCntWidth: Int = 2,
    UsefulCntWidth:     Int = 1,
    UseAltOnNaWidth:    Int = 4,
    TargetWidth:        Int = 20,
    TickWidth:          Int = 8,
    /* *** IttageTable *** */
    TableSramSize:        Int = 128,
    TableWrBypassEntries: Int = 4,
    /* *** Region *** */
    RegionNums:     Int = 16,
    RegionPorts:    Int = 2,
    RegionReplacer: String = "plru" // "random", "plru", "lru"
) {
  require(isPow2(TableSramSize), "TableSramSize must be a power of 2")
}

// TODO: expose this to Parameters.scala / XSCore.scala
trait HasIttageParameters extends HasBpuParameters {
  def ittageParameters: IttageParameters = bpuParameters.ittageParameters

  def TableInfos: Seq[IttageTableInfo] = ittageParameters.TableInfos
  def NumTables:  Int                  = TableInfos.length

  def TagWidth:           Int = ittageParameters.TagWidth
  def ConfidenceCntWidth: Int = ittageParameters.ConfidenceCntWidth
  def UsefulCntWidth:     Int = ittageParameters.UsefulCntWidth
  def UseAltOnNaWidth:    Int = ittageParameters.UseAltOnNaWidth
  def TargetOffsetWidth:  Int = ittageParameters.TargetWidth
  def TickWidth:          Int = ittageParameters.TickWidth

  /* *** IttageTable *** */
  def TableSramSize:        Int = ittageParameters.TableSramSize
  def TableWrBypassEntries: Int = ittageParameters.TableWrBypassEntries

  /* *** Region *** */
  def RegionNums:     Int    = ittageParameters.RegionNums
  def RegionPorts:    Int    = ittageParameters.RegionPorts
  def RegionReplacer: String = ittageParameters.RegionReplacer
  def RegionBits:     Int    = VAddrBits - TargetOffsetWidth

  /* *** testing *** */
  def debug: Boolean = !env.FPGAPlatform && env.EnablePerfDebug

  // ittage cannot be fast-trained, this is required by abstract class BasePredictor
  def EnableFastTrain: Boolean = false
}
