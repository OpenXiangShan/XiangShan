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
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteReqBundle

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:     Bool            = Bool()
  val tag:       UInt            = UInt(TagWidth.W)
  val takenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val usefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
}

class BaseTableSramWriteReq(implicit p: Parameters) extends WriteReqBundle with HasTageParameters {
  val setIdx:  UInt                 = UInt(BaseTableSetIdxWidth.W)
  val ctrs:    Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
  val wayMask: Vec[Bool]            = Vec(FetchBlockAlignInstNum, Bool())
}

class TableSramWriteReq(numSets: Int, numWays: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val setIdx: UInt      = UInt(log2Ceil(numSets / NumBanks).W)
  val wayIdx: UInt      = UInt(log2Ceil(numWays).W)
  val entry:  TageEntry = new TageEntry

  override def tag: Option[UInt] = Some(entry.tag)
}

class TageTableResult(numWays: Int)(implicit p: Parameters) extends TageBundle {
  val hit:              Bool            = Bool()
  val hitWayMask:       Vec[Bool]       = Vec(numWays, Bool())
  val takenCtr:         SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val usefulCtr:        SaturateCounter = new SaturateCounter(UsefulCtrWidth)
  val notUsefulWayMask: Vec[Bool]       = Vec(numWays, Bool())
}

class TageTablePrediction(implicit p: Parameters) extends TageBundle {
  val hasProvider:      Bool = Bool()
  val providerTableIdx: UInt = UInt(log2Ceil(NumTables).W)
  // FIXME: currently we only support a fixed way number for all tables
  val providerWayMask:    Vec[Bool]       = Vec(3, Bool())
  val providerTakenCtr:   SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val providerUsefulCtr:  SaturateCounter = new SaturateCounter(UsefulCtrWidth)
  val hasHcProvider:      Bool            = Bool()
  val hcProviderIdx:      UInt            = UInt(log2Ceil(NumTables).W)
  val hcProviderTakenCtr: SaturateCounter = new SaturateCounter(TakenCtrWidth)
  // FIXME: currently we only support a fixed way number for all tables
  val allTableNotUsefulWayMask: Vec[Vec[Bool]] = Vec(NumTables, Vec(3, Bool()))
}

class TageMeta(implicit p: Parameters) extends TageBundle {
  val baseTableCtrs:    Vec[SaturateCounter]     = Vec(FetchBlockInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
  val tablePredictions: Vec[TageTablePrediction] = Vec(MainBtbResultNumEntries, new TageTablePrediction)
}
