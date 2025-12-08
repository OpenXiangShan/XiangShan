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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.WriteReqBundle

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:    Bool            = Bool()
  val tag:      UInt            = UInt(TagWidth.W)
  val takenCtr: SaturateCounter = new SaturateCounter(TakenCtrWidth)
}

class BaseTableSramWriteReq(implicit p: Parameters) extends TageBundle {
  val setIdx:    UInt                 = UInt(BaseTableSetIdxWidth.W)
  val wayMask:   UInt                 = UInt(FetchBlockAlignInstNum.W)
  val takenCtrs: Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
}

class TableReadReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:   UInt = UInt(SetIdxWidth.W)
  val bankMask: UInt = UInt(NumBanks.W)
}

class TableReadResp(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, new SaturateCounter(UsefulCtrWidth))
}

class EntrySramWriteReq(implicit p: Parameters, info: TageTableInfo) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:         UInt                    = UInt(SetIdxWidth.W)
  val entry:          TageEntry               = new TageEntry
  val usefulCtr:      SaturateCounter         = new SaturateCounter(UsefulCtrWidth)
  override def tag:   Option[UInt]            = Some(entry.tag)
  override def cnt:   Option[SaturateCounter] = Some(entry.takenCtr)
  override def taken: Option[Bool]            = Some(entry.takenCtr.isPositive) // FIXME: use actualTaken
}

class TableWriteReq(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val setIdx:     UInt                 = UInt(SetIdxWidth.W)
  val bankMask:   UInt                 = UInt(NumBanks.W)
  val wayMask:    UInt                 = UInt(NumWays.W)
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, new SaturateCounter(UsefulCtrWidth))
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val baseTableCtrs: Vec[SaturateCounter] = Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
  val debug_setIdx:  Vec[UInt]            = Vec(NumTables, UInt(8.W))        // TODO: remove it
  val debug_tempTag: Vec[UInt]            = Vec(NumTables, UInt(TagWidth.W)) // TODO: remove it
}

class TageFoldedHist(implicit p: Parameters, info: TageTableInfo) extends TageBundle {
  val forIdx: UInt = UInt(SetIdxWidth.W)
  val forTag: UInt = UInt(TagWidth.W)
}

class TagMatchResult(implicit p: Parameters) extends TageBundle {
  val hit:          Bool            = Bool()
  val hitWayMaskOH: UInt            = UInt(MaxNumWays.W)
  val entry:        TageEntry       = new TageEntry
  val usefulCtr:    SaturateCounter = new SaturateCounter(UsefulCtrWidth)
}

class UpdateInfo(implicit p: Parameters) extends TageBundle {
  val providerTableOH:      UInt            = UInt(NumTables.W)
  val providerWayOH:        UInt            = UInt(MaxNumWays.W)
  val providerEntry:        TageEntry       = new TageEntry
  val providerOldUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
  val providerNewUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)

  val altTableOH:      UInt            = UInt(NumTables.W)
  val altWayOH:        UInt            = UInt(MaxNumWays.W)
  val altEntry:        TageEntry       = new TageEntry
  val altOldUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)

  val useAlt:    Bool = Bool()
  val finalPred: Bool = Bool()

  val increaseUseAlt: Bool = Bool()
  val decreaseUseAlt: Bool = Bool()

  val needAllocate: Bool = Bool()
}

class ConditionalBranchTrace(implicit p: Parameters) extends TageBundle {
  val startVAddr:  PrunedAddr = PrunedAddr(VAddrBits)
  val branchVAddr: PrunedAddr = PrunedAddr(VAddrBits)

  val hasProvider:       Bool            = Bool()
  val providerTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val providerSetIdx:    UInt            = UInt(16.W)
  val providerWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val providerTakenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val providerUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)

  val hasAlt:       Bool            = Bool()
  val altTableIdx:  UInt            = UInt(TableIdxWidth.W)
  val altSetIdx:    UInt            = UInt(16.W)
  val altWayIdx:    UInt            = UInt(MaxWayIdxWidth.W)
  val altTakenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val altUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)

  val baseTableCtr: SaturateCounter = new SaturateCounter(BaseTableTakenCtrWidth)

  val useAlt:         Bool = Bool()
  val finalPred:      Bool = Bool()
  val actualTaken:    Bool = Bool()
  val allocSuccess:   Bool = Bool()
  val allocTableIdx:  UInt = UInt(TableIdxWidth.W)
  val allocateSetIdx: UInt = UInt(16.W)
  val allocWayIdx:    UInt = UInt(MaxWayIdxWidth.W)
}
