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
import xiangshan.frontend.bpu.TargetCarry

case class MainBtbParameters(
    NumEntries: Int = 8192,
    NumWay:     Int = 4,

    // Lowest level banks, each bank is a physical SRAM
    // This banking is used to resolve read-write conflicts and reduce SRAM power
    NumInternalBanks: Int = 4,
    // Highest level banks
    // This banking is used to resolve the alignement restriction of the BTB
    // When using align banking, the BTB can provide at most (banks-1 or banks) * predict width wide prediction
    NumAlignBanks: Int = 2,

    // Different tag width for long/short branches
    TagFullWidth:  Int = 16,
    TagLowerWidth: Int = 12,

    // We only store target offset in main table
    // The full target is reconstructed using page/region table
    TargetLowerWidth: Int = 11,
    WriteBufferSize:  Int = 4,

    // Tage Base table
    TakenCntWidth: Int = 2,

    // Page table
    VpnLowerWidth:       Int = 16,
    NumPageTableEntries: Int = 256,
    NumPageTableWay:     Int = 16,

    // Region table
    NumRegionTableWay: Int = 4,

    // Misc
    Replacer:           String = "Rrip",
    EnableMainBtbTrace: Boolean = false
) {}

// TODO: expose this to Parameters.scala / XSCore.scala
trait HasMainBtbParameters extends HasBpuParameters {
  def mbtbParameters: MainBtbParameters = bpuParameters.mbtbParameters

  def NumEntries:       Int = mbtbParameters.NumEntries
  def NumWay:           Int = mbtbParameters.NumWay
  def NumInternalBanks: Int = mbtbParameters.NumInternalBanks
  def NumAlignBanks:    Int = FetchBlockSize / FetchBlockAlignSize
  // NumSets is the number of sets in one bank, a bank corresponds to a physical SRAM
  def NumSets:       Int = NumEntries / NumWay / NumInternalBanks / NumAlignBanks
  def TagFullWidth:  Int = mbtbParameters.TagFullWidth
  def TagLowerWidth: Int = mbtbParameters.TagLowerWidth
//  def TargetWidth:        Int = mbtbParameters.TargetWidth
  def SetIdxLen:          Int = log2Ceil(NumSets)
  def InternalBankIdxLen: Int = log2Ceil(NumInternalBanks)
  def AlignBankIdxLen:    Int = log2Ceil(NumAlignBanks)
  def WriteBufferSize:    Int = mbtbParameters.WriteBufferSize

  def Replacer: String = mbtbParameters.Replacer

  // Tage Base table
  def TakenCntWidth: Int = mbtbParameters.TakenCntWidth

  // Page table
  def PageTableUseSRAM:    Boolean = NumPageTableEntries > 256
  def TargetLowerWidth:    Int     = mbtbParameters.TargetLowerWidth
  def VpnLowerWidth:       Int     = mbtbParameters.VpnLowerWidth
  def NumPageTableEntries: Int     = mbtbParameters.NumPageTableEntries
  def NumPageTableWay:     Int     = mbtbParameters.NumPageTableWay
  def NumPageTableSet:     Int     = NumPageTableEntries / NumPageTableWay
  def PageTableSetIdxLen:  Int     = log2Ceil(NumPageTableSet)
  def PageTableWayIdxLen:  Int     = log2Ceil(NumPageTableWay)
//  def vpnLowerWidth:       Int     = TargetWidth - TargetLowerWidth

  // Region table
  def NumRegionTableWay: Int = mbtbParameters.NumRegionTableWay
  def RegionWayIdxLen:   Int = log2Ceil(NumRegionTableWay)
  def VpnUpperWidth:     Int = VAddrBits - instOffsetBits - TargetLowerWidth - VpnLowerWidth

  // Used in any aligned-addr-indexed predictor, indicates the position relative to the aligned start addr
  def CfiAlignedPositionWidth: Int = CfiPositionWidth - AlignBankIdxLen

  def EnableMainBtbTrace: Boolean = mbtbParameters.EnableMainBtbTrace
}
