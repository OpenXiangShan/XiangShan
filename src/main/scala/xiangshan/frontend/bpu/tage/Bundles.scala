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
  val valid:     Bool            = Bool()
  val tag:       UInt            = UInt(TagWidth.W)
  val takenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val usefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
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
  val entries:      Vec[TageEntry]  = Vec(NumWays, new TageEntry)
  val allocFailCtr: SaturateCounter = new SaturateCounter(AllocFailCtrWidth)
}

class EntrySramWriteReq(numSets: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:         UInt                    = UInt(log2Ceil(numSets / NumBanks).W)
  val entry:          TageEntry               = new TageEntry
  override def tag:   Option[UInt]            = Some(entry.tag)
  override def cnt:   Option[SaturateCounter] = Some(entry.takenCtr)
  override def taken: Option[Bool]            = Some(entry.takenCtr.isPositive)
}
class AllocFailCtrSramWriteReq(numSets: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:       UInt            = UInt(log2Ceil(numSets / NumBanks).W)
  val allocFailCtr: SaturateCounter = new SaturateCounter(AllocFailCtrWidth)
}

class TableUpdateEntriesReq(implicit p: Parameters) extends TageBundle {
  val wayMask: UInt           = UInt(NumWays.W)
  val entries: Vec[TageEntry] = Vec(NumWays, new TageEntry)
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val baseTableCtrs: Vec[SaturateCounter] = Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
}
