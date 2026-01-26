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

package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters

case class PrefetchBtbParameters(
    NumEntries:      Int = 8192,
    NumWay:          Int = 4,
    NumBanks:        Int = 4,
    TagWidth:        Int = 16,
    TargetWidth:     Int = 20, // 2B aligned
    WriteBufferSize: Int = 4,
    Replacer:        String = "Lru"
) {}

// TODO: expose this to Parameters.scala / XSCore.scala
trait HasPrefetchBtbParameters extends HasBpuParameters {
  def prefetchParameters: PrefetchBtbParameters = bpuParameters.prefetchBtbParameters

  def NumEntries: Int = prefetchParameters.NumEntries
  def NumWay:     Int = prefetchParameters.NumWay
  // NumSets is the number of sets in one bank, a bank corresponds to a physical SRAM
  def NumSets:         Int    = NumEntries / NumWay / prefetchParameters.NumBanks
  def TagWidth:        Int    = prefetchParameters.TagWidth
  def TargetWidth:     Int    = prefetchParameters.TargetWidth
  def SetIdxLen:       Int    = log2Ceil(NumSets)
  def BankIdxLen:      Int    = log2Ceil(prefetchParameters.NumBanks)
  def WriteBufferSize: Int    = prefetchParameters.WriteBufferSize
  def Replacer:        String = prefetchParameters.Replacer

}
