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
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.WriteReqBundle

object ConfidenceCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ittageParameters.ConfidenceCntWidth
}

object UsefulCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ittageParameters.UsefulCntWidth
}

object TickCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ittageParameters.TickWidth
}

class IttageEntry(tagLen: Int)(implicit p: Parameters) extends IttageBundle {
  val valid:         Bool            = Bool()
  val tag:           UInt            = UInt(tagLen.W)
  val confidenceCnt: SaturateCounter = ConfidenceCounter()
  val targetOffset:  IttageOffset    = new IttageOffset()
  val usefulCnt:  SaturateCounter = UsefulCounter() // Due to the bitMask the useful bit needs to be at the lowest bit
  val paddingBit: UInt            = UInt(1.W)
}

class IttageOffset(implicit p: Parameters) extends IttageBundle {
  val offset:      PrunedAddr = PrunedAddr(TargetOffsetWidth)
  val pointer:     UInt       = UInt(log2Ceil(RegionNums).W)
  val usePcRegion: Bool       = Bool()
}

class IttagePrediction(implicit p: Parameters) extends IttageBundle {
  val hit:    Bool       = Bool()
  val target: PrunedAddr = PrunedAddr(VAddrBits)
}

class IttageMeta(implicit p: Parameters) extends IttageBundle {
  val valid: Bool = Bool()

  val provider:          Valid[UInt]     = Valid(UInt(log2Ceil(NumTables).W))
  val altProvider:       Valid[UInt]     = Valid(UInt(log2Ceil(NumTables).W))
  val altDiffers:        Bool            = Bool()
  val providerUsefulCnt: SaturateCounter = UsefulCounter()
  val providerCnt:       SaturateCounter = ConfidenceCounter()
  val altProviderCnt:    SaturateCounter = ConfidenceCounter()
  val allocate:          Valid[UInt]     = Valid(UInt(log2Ceil(NumTables).W))
  val providerTarget:    PrunedAddr      = PrunedAddr(VAddrBits)
  val altProviderTarget: PrunedAddr      = PrunedAddr(VAddrBits)

  val debug_predCycle: Option[UInt] = if (!env.FPGAPlatform) Some(UInt(XLEN.W)) else None

  override def toPrintable =
    p"pvdr(v:${provider.valid} num:${provider.bits} ctr:$providerCnt u:$providerUsefulCnt tar:${Hexadecimal(providerTarget.toUInt)}), " +
      p"altpvdr(v:${altProvider.valid} num:${altProvider.bits}, ctr:$altProviderCnt, tar:${Hexadecimal(altProviderTarget.toUInt)})"
}

class IttageWriteReq(tagLen: Int, nRows: Int, ittageEntrySz: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasIttageParameters {
  val entry:   IttageEntry = new IttageEntry(tagLen)
  val setIdx:  UInt        = UInt(log2Ceil(nRows).W)
  val bitmask: UInt        = UInt(ittageEntrySz.W)
}
