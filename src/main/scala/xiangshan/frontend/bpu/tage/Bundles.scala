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
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.WriteReqBundle

class TageEntry(implicit p: Parameters) extends TageBundle {
  val valid:     Bool            = Bool()
  val tag:       UInt            = UInt(TagWidth.W)
  val takenCtr:  SaturateCounter = new SaturateCounter(TakenCtrWidth)
  val usefulCtr: SaturateCounter = new SaturateCounter(UsefulCtrWidth)
}

class BaseTableSramWriteReq(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val setIdx:    UInt                 = UInt(BaseTableSetIdxWidth.W)
  val wayMask:   UInt                 = UInt(FetchBlockAlignInstNum.W)
  val takenCtrs: Vec[SaturateCounter] = Vec(FetchBlockAlignInstNum, new SaturateCounter(BaseTableTakenCtrWidth))
}

class TableSramWriteReq(numSets: Int)(implicit p: Parameters) extends WriteReqBundle
    with HasTageParameters {
  val needResetUsefulCtr: Bool           = Bool()
  val setIdx:             UInt           = UInt(log2Ceil(numSets / NumBanks).W)
  val wayMask:            UInt           = UInt(NumWays.W)
  val data:               Vec[TageEntry] = Vec(NumWays, new TageEntry)
}

class TableResult(implicit p: Parameters) extends TageBundle {
  val hasProvider:      Bool            = Bool()
  val providerTakenCtr: SaturateCounter = new SaturateCounter(TakenCtrWidth)
}

class TableData(implicit p: Parameters) extends TageBundle {
  val hitWayMask: Vec[Bool] = Vec(NumWays, Bool())
  val takenCtr:   Vec[UInt] = Vec(NumWays, UInt(TakenCtrWidth.W))
  val usefulCtr:  Vec[UInt] = Vec(NumWays, UInt(UsefulCtrWidth.W))
}

// TODO: temp, remove it
class BranchInfo(implicit p: Parameters) extends TageBundle {
  val taken:      Bool            = Bool()
  val target:     PrunedAddr      = PrunedAddr(VAddrBits)
  val position:   UInt            = UInt(CfiPositionWidth.W)
  val attribute:  BranchAttribute = new BranchAttribute
  val mispredict: Bool            = Bool()
}

// TODO: temp, remove it
class ResolveTrain(implicit p: Parameters) extends TageBundle {
  val startVAddr: PrunedAddr             = PrunedAddr(VAddrBits)
  val branches:   Vec[Valid[BranchInfo]] = Vec(NumResolveBranches, Valid(new BranchInfo))
}
