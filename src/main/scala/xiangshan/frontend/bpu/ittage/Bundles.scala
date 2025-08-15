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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr

class IttageOffset(implicit p: Parameters) extends IttageBundle {
  val offset:      PrunedAddr = PrunedAddr(TargetOffsetWidth)
  val pointer:     UInt       = UInt(log2Ceil(RegionNums).W)
  val usePcRegion: Bool       = Bool()
}

class IttageMeta(implicit p: Parameters) extends IttageBundle {
  val provider:          Valid[UInt] = Valid(UInt(log2Ceil(NumTables).W))
  val altProvider:       Valid[UInt] = Valid(UInt(log2Ceil(NumTables).W))
  val altDiffers:        Bool        = Bool()
  val providerUsefulCnt: Bool        = Bool()
  val providerCnt:       UInt        = UInt(ConfidenceCntWidth.W)
  val altProviderCnt:    UInt        = UInt(ConfidenceCntWidth.W)
  val allocate:          Valid[UInt] = Valid(UInt(log2Ceil(NumTables).W))
  val providerTarget:    PrunedAddr  = PrunedAddr(VAddrBits)
  val altProviderTarget: PrunedAddr  = PrunedAddr(VAddrBits)

  val debug_predCycle: Option[UInt] = if (!env.FPGAPlatform) Some(UInt(XLEN.W)) else None

  override def toPrintable =
    p"pvdr(v:${provider.valid} num:${provider.bits} ctr:$providerCnt u:$providerUsefulCnt tar:${Hexadecimal(providerTarget.toUInt)}), " +
      p"altpvdr(v:${altProvider.valid} num:${altProvider.bits}, ctr:$altProviderCnt, tar:${Hexadecimal(altProviderTarget.toUInt)})"
}
