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
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.TargetCarry
import xiangshan.frontend.bpu.WriteReqBundle

object TakenCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.mbtbParameters.TakenCntWidth
}

class MainBtbEntry(implicit p: Parameters) extends MainBtbBundle {
  // whether the entry is valid
  val valid: Bool = Bool()

  // Short branch appears more frequently than long branch
  // So we use full tag for short branch to reduce conflict miss
  // and use short/lower tag for long branch to save area
  // TODO: use folded tag
  val tagLower: UInt = UInt(TagLowerWidth.W)

  val attribute: BranchAttribute = new BranchAttribute

  // Whether a branch is bias toward a single target
  // For conditional branch, this means bias toward same direction
  // For indirect branch, this means bias toward single target
//  val stronglyBiased: Bool = Bool() // TODO

  // Relative position to the aligned start addr
  val position: UInt = UInt(CfiAlignedPositionWidth.W)

  val targetLower: UInt = UInt(TargetLowerWidth.W)

  // Whether is a long branch, see below for details
  val targetCrossPage: Bool = Bool()

//  val replaceCnt: UInt = UInt(2.W) // TODO: not used for now
}
// Shared Info
// short branch :
// - need target carry to reconstruct full target
// - appears more frequently, so store full tag to reduce conflict miss
//
// long branch :
// - always taken, no counter and carry needed, but need to access page table
//
// Therefore, the static info has two different formats, store them together to save area
//

class ShortSharedInfo(implicit p: Parameters) extends MainBtbBundle {
  val tagUpper:    UInt            = UInt((TagFullWidth - TagLowerWidth).W)
  val targetCarry: TargetCarry     = new TargetCarry
  val counter:     SaturateCounter = TakenCounter()
}

class LongSharedInfo(implicit p: Parameters) extends MainBtbBundle {
  val pageIdx: UInt = UInt(log2Ceil(NumPageTableEntries).W)
}

class MainBtbSharedInfo(implicit p: Parameters) extends MainBtbBundle {
  private def SharedInfoWidth: Int = math.max(
    new ShortSharedInfo().getWidth,
    new LongSharedInfo().getWidth
  )

  val bits: UInt = UInt(SharedInfoWidth.W)

  def asShort: ShortSharedInfo = bits.asTypeOf(new ShortSharedInfo)
  def asLong:  LongSharedInfo  = bits.asTypeOf(new LongSharedInfo)
}

class MainBtbPageTableEntry(implicit p: Parameters) extends MainBtbBundle {
  val vpnLower:  UInt = UInt(VpnLowerWidth.W)
  val regionWay: UInt = UInt(RegionWayIdxLen.W)
  // TODO: move RRPV here
}

class MainBtbRegionTableEntry(implicit p: Parameters) extends MainBtbBundle {
  val vpnUpper: UInt = UInt(VpnUpperWidth.W)
}

class MainBtbEntrySramWriteReq(implicit p: Parameters) extends WriteReqBundle with HasMainBtbParameters {
  val setIdx: UInt              = UInt(SetIdxLen.W)
  val entry:  MainBtbEntry      = new MainBtbEntry
  val shared: MainBtbSharedInfo = new MainBtbSharedInfo
  override def tag: Option[UInt] =
    Some(Cat(
      // if not cross page, i.e. a short branch, use full tag
      Mux(!entry.targetCrossPage, shared.asShort.tagUpper, 0.U),
      entry.tagLower,
      entry.position
    ))
}

class MainBtbSharedSramWriteReq(implicit p: Parameters) extends MainBtbBundle {
  val setIdx: UInt              = UInt(SetIdxLen.W)
  val shared: MainBtbSharedInfo = new MainBtbSharedInfo
}

class MainBtbPageTableEntrySramWriteReq(implicit p: Parameters) extends MainBtbBundle {
  val setIdx: UInt                  = UInt(PageTableSetIdxLen.W)
  val entry:  MainBtbPageTableEntry = new MainBtbPageTableEntry
}

class MainBtbMetaEntry(implicit p: Parameters) extends MainBtbBundle {
  val rawHit:    Bool              = Bool()
  val position:  UInt              = UInt(CfiPositionWidth.W)
  val attribute: BranchAttribute   = new BranchAttribute
  val shared:    MainBtbSharedInfo = new MainBtbSharedInfo

  def hit(branch: BranchInfo): Bool = rawHit && position === branch.cfiPosition
}

class MainBtbMeta(implicit p: Parameters) extends MainBtbBundle {
  val entries: Vec[Vec[MainBtbMetaEntry]] = Vec(NumAlignBanks, Vec(NumWay, new MainBtbMetaEntry))
}

class MainBtbTrace(implicit p: Parameters) extends MainBtbBundle {
  val startPc:     PrunedAddr      = PrunedAddr(VAddrBits)
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val attribute:   BranchAttribute = new BranchAttribute

  val setIdx:       UInt = UInt(SetIdxLen.W)
  val internalIdx:  UInt = UInt(InternalBankIdxLen.W)
  val alignBankIdx: UInt = UInt(AlignBankIdxLen.W)
  val wayIdx:       UInt = UInt(NumWay.W)
}
