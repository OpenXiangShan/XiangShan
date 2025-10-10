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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SignedSaturateCounter
import xiangshan.frontend.bpu.WriteReqBundle

class ScEntry(implicit p: Parameters) extends ScBundle {
  val ctrs: SignedSaturateCounter = new SignedSaturateCounter(ctrWidth)
}

class ScThreshold(implicit p: Parameters) extends ScBundle {
  val ctr: SaturateCounter = new SaturateCounter(thresholdCtrWidth)
  val thres = UInt(thresholdThresWidth.W)
  def satPos(ctr: UInt = this.ctr.value): Bool = ctr === ((1.U << thresholdCtrWidth) - 1.U)
  def satNeg(ctr: UInt = this.ctr.value): Bool = ctr === 0.U
  def neutralVal: UInt = (1 << (thresholdCtrWidth - 1)).U
  def initVal:    UInt = 6.U
  def minThres:   UInt = 6.U
  def maxThres:   UInt = 31.U
  def update(cause: Bool): ScThreshold = {
    val res    = Wire(new ScThreshold())
    val newCtr = this.ctr.getUpdate(cause)
    val newThres = Mux(
      res.satPos(newCtr) && this.thres <= maxThres,
      this.thres + 2.U,
      Mux(res.satNeg(newCtr) && this.thres >= minThres, this.thres - 2.U, this.thres)
    )
    res.thres     := newThres
    res.ctr.value := Mux(res.satPos(newCtr) || res.satNeg(newCtr), res.neutralVal, newCtr)
    res
  }
}

object ScThreshold {
  def apply(implicit p: Parameters): ScThreshold = {
    val t = Wire(new ScThreshold())
    t.ctr.value := t.neutralVal
    t.thres     := t.initVal
    t
  }
}

class PathTableSramWriteReq(val numSets: Int)(implicit p: Parameters) extends WriteReqBundle with HasScParameters {
  val setIdx:    UInt         = UInt(log2Ceil(numSets).W)
  val wayIdxVec: Vec[UInt]    = Vec(ResolveEntryBranchNumber, UInt(log2Ceil(NumWays).W))
  val entryVec:  Vec[ScEntry] = Vec(ResolveEntryBranchNumber, new ScEntry())
}

class PathTableTrain(val numSets: Int)(implicit p: Parameters) extends ScBundle {
  val valid:     Bool         = Bool()
  val setIdx:    UInt         = UInt(log2Ceil(numSets / NumBanks).W)
  val wayIdxVec: Vec[UInt]    = Vec(ResolveEntryBranchNumber, UInt(log2Ceil(NumWays).W))
  val entryVec:  Vec[ScEntry] = Vec(ResolveEntryBranchNumber, new ScEntry())
}

class ScMeta(implicit p: Parameters) extends ScBundle with HasScParameters {
  val scResp:    Vec[Vec[ScEntry]] = Vec(PathTableSize, Vec(NumWays, new ScEntry()))
  val scPred:    Vec[Bool]         = Vec(NumWays, Bool())
  val useScPred: Vec[Bool]         = Vec(NumWays, Bool())
}
