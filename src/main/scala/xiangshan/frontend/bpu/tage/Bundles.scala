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
import xiangshan.frontend.bpu.SaturateCounter
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

class TableReadReq(numSets: Int)(implicit p: Parameters) extends TageBundle {
  val setIdx:   UInt = UInt(log2Ceil(numSets / NumBanks).W)
  val bankMask: UInt = UInt(NumBanks.W)
}

class TableReadResp(implicit p: Parameters) extends TageBundle {
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, new SaturateCounter(UsefulCtrWidth))
}

class EntrySramWriteReq(numSets: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:         UInt                    = UInt(log2Ceil(numSets / NumBanks).W)
  val entry:          TageEntry               = new TageEntry
  val usefulCtr:      SaturateCounter         = new SaturateCounter(UsefulCtrWidth)
  override def tag:   Option[UInt]            = Some(entry.tag)
  override def cnt:   Option[SaturateCounter] = Some(entry.takenCtr)
  override def taken: Option[Bool]            = Some(entry.takenCtr.isPositive)
}

class TableWriteReq(numSets: Int)(implicit p: Parameters) extends TageBundle {
  val setIdx:     UInt                 = UInt(log2Ceil(numSets / NumBanks).W)
  val bankMask:   UInt                 = UInt(NumBanks.W)
  val wayMask:    UInt                 = UInt(NumWays.W)
  val entries:    Vec[TageEntry]       = Vec(NumWays, new TageEntry)
  val usefulCtrs: Vec[SaturateCounter] = Vec(NumWays, new SaturateCounter(UsefulCtrWidth))
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val baseTableCtrs: Vec[SaturateCounter] = Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
  val debug_setIdx:  Vec[UInt]            = Vec(NumTables, UInt(8.W)) // TODO
  val debug_tempTag: Vec[UInt]            = Vec(NumTables, UInt(TagWidth.W))
}

class TagMatchResultPerTable(implicit p: Parameters) extends TageBundle {
  val hit:       Bool            = Bool()
  val takenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val usefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)

  // only used for train
  val hitWayMaskOH: UInt = UInt(NumWays.W)
  val tag:          UInt = UInt(TagWidth.W)
}

class PredictionPerBranch(implicit p: Parameters) extends TageBundle {
  val hasProvider:    Bool = Bool()
  val providerIsWeak: Bool = Bool()
  val pred:           Bool = Bool()
  val hasAlt:         Bool = Bool()
  val altPred:        Bool = Bool()

  // only used for debug
  val debug_providerOH:   UInt = UInt(NumTables.W)
  val debug_altOH:        UInt = UInt(NumTables.W)
  val debug_hitTableMask: UInt = UInt(NumTables.W)
}

class UpdateInfoPerBranch(implicit p: Parameters) extends TageBundle {
  val providerOH:           UInt            = UInt(NumTables.W)
  val providerHitWayMaskOH: UInt            = UInt(NumWays.W)
  val providerNewTakenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val providerNewUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
  val providerTag:          UInt            = UInt(TagWidth.W)

  val altOH:           UInt            = UInt(NumTables.W)
  val altHitWayMaskOH: UInt            = UInt(NumWays.W)
  val altNewTakenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val altOldUsefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
  val altTag:          UInt            = UInt(TagWidth.W)

  val increaseUseAlt: Bool = Bool()
  val decreaseUseAlt: Bool = Bool()

  val needAllocate: Bool = Bool()
}
