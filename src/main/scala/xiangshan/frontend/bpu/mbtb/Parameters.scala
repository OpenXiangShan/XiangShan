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
    // Lowest level banks used to resolve read-write conflicts and reduce SRAM power, each bank is a physical SRAM
    NumInternalBanks: Int = 4,
    // NumAlignBanks is determined by top-level FetchBlockSize and FetchBlockAlignSize, not adjustable in mbtb
    WriteBufferSize: Int = 4,
    Replacer:        String = "Lru", // "Lru" or "Plru"
    // Entry config
    TagWidth:        Int = 16,
    EnableTargetFix: Boolean = true,
    TargetWidth:     Int = 20, // 2B aligned
    TakenCntWidth:   Int = 2,
    // Mbtb write trace
    EnableMainbtbTrace: Boolean = false
) {}

trait HasMainBtbParameters extends HasBpuParameters {
  def mbtbParameters: MainBtbParameters = bpuParameters.mbtbParameters

  def NumEntries:       Int    = mbtbParameters.NumEntries
  def NumWay:           Int    = mbtbParameters.NumWay
  def NumInternalBanks: Int    = mbtbParameters.NumInternalBanks
  def WriteBufferSize:  Int    = mbtbParameters.WriteBufferSize
  def Replacer:         String = mbtbParameters.Replacer

  def NumAlignBanks: Int = FetchBlockSize / FetchBlockAlignSize
  // NumSets is the number of sets in one bank, a bank corresponds to a physical SRAM
  def NumSets: Int = NumEntries / NumWay / NumInternalBanks / NumAlignBanks

  def SetIdxLen:          Int = log2Ceil(NumSets)
  def InternalBankIdxLen: Int = log2Ceil(NumInternalBanks)
  def AlignBankIdxLen:    Int = log2Ceil(NumAlignBanks)

  // Entry config
  def TagWidth:        Int     = mbtbParameters.TagWidth
  def TargetWidth:     Int     = mbtbParameters.TargetWidth
  def EnableTargetFix: Boolean = mbtbParameters.EnableTargetFix
  def TakenCntWidth:   Int     = mbtbParameters.TakenCntWidth

  // Used in any aligned-addr-indexed predictor, indicates the position relative to the aligned start addr
  def CfiAlignedPositionWidth: Int = CfiPositionWidth - AlignBankIdxLen

  def EnableMainbtbTrace: Boolean = mbtbParameters.EnableMainbtbTrace
}
