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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters

case class MainBtbParameters(
    NumEntries: Int = 8192,
    NumWay:     Int = 4,
    // Lowest level banks, each bank is a physical SRAM
    // This banking is used to resolve read-write conflicts and reduce SRAM power
    NumInternalBanks: Int = 4,
    // Highest level banks
    // This banking is used to resolve the alignement restriction of the BTB
    // When using align banking, the BTB can provide at most banks - 1 / banks * predict width wide prediction
    NumAlignBanks:   Int = 2,
    TagWidth:        Int = 16,
    TargetWidth:     Int = 20, // 2B aligned
    WriteBufferSize: Int = 4
) {}

// TODO: expose this to Parameters.scala / XSCore.scala
trait HasMainBtbParameters extends HasBpuParameters {
  def mbtbParameters: MainBtbParameters = bpuParameters.mbtbParameters

  def NumEntries:       Int = mbtbParameters.NumEntries
  def NumWay:           Int = mbtbParameters.NumWay
  def NumInternalBanks: Int = mbtbParameters.NumInternalBanks
  def NumAlignBanks:    Int = FetchBlockSize / FetchBlockAlignSize
  // NumSets is the number of sets in one bank, a bank coresponds to a physical SRAM
  def NumSets:            Int = NumEntries / NumWay / NumInternalBanks / NumAlignBanks
  def TagWidth:           Int = mbtbParameters.TagWidth
  def TargetWidth:        Int = mbtbParameters.TargetWidth
  def SetIdxLen:          Int = log2Ceil(NumSets)
  def InternalBankIdxLen: Int = log2Ceil(NumInternalBanks)
  def WriteBufferSize:    Int = mbtbParameters.WriteBufferSize

  // Used in any aligned-addr-indexed predictor, indicates the position relative to the aligned start addr
  def CfiAlignedPositionWidth: Int = CfiPositionWidth - log2Ceil(NumAlignBanks)

  // mbtb cannot be fast-trained, this is required by abstract class BasePredictor
  def EnableFastTrain: Boolean = false
}
