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
  val ctr: SignedSaturateCounter = new SignedSaturateCounter(ctrWidth)
}

class ScThreshold(implicit p: Parameters) extends ScBundle {
  val thres: SaturateCounter = new SaturateCounter(thresholdThresWidth)

  def initVal: UInt = 6.U

  def update(cause: Bool): ScThreshold = {
    val res = Wire(new ScThreshold())
    res.thres.value := this.thres.getUpdate(cause)
    res
  }
}

object ScThreshold {
  def apply(implicit p: Parameters): ScThreshold = {
    val t = Wire(new ScThreshold())
    t.thres.value := t.initVal
    t
  }
}

class PathTableSramWriteReq(val numSets: Int)(implicit p: Parameters) extends WriteReqBundle with HasScParameters {
  val setIdx:   UInt         = UInt(log2Ceil(numSets).W)
  val wayMask:  Vec[Bool]    = Vec(NumWays, Bool())
  val entryVec: Vec[ScEntry] = Vec(NumWays, new ScEntry())
}

class PathTableTrain(val numSets: Int)(implicit p: Parameters) extends ScBundle {
  val valid:    Bool         = Bool()
  val setIdx:   UInt         = UInt(log2Ceil(numSets / NumBanks).W)
  val wayMask:  Vec[Bool]    = Vec(NumWays, Bool())
  val entryVec: Vec[ScEntry] = Vec(NumWays, new ScEntry())
}

class ScMeta(implicit p: Parameters) extends ScBundle with HasScParameters {
  // NOTE: Seems ChiselDB has problem dealing with SInt, so we do not use ScEntry for scResp here
  // FIXME: is there a better way to do this?
  private def ScEntryWidth = (new ScEntry).getWidth
  val scPathResp:   Vec[Vec[UInt]] = Vec(PathTableSize, Vec(NumWays, UInt(ScEntryWidth.W)))
  val scGlobleResp: Vec[Vec[UInt]] = Vec(GlobalTableSize, Vec(NumWays, UInt(ScEntryWidth.W)))
  val scGhr:        UInt           = UInt(GhrHistoryLength.W)
  val scPred:       Vec[Bool]      = Vec(NumWays, Bool())
  val useScPred:    Vec[Bool]      = Vec(NumWays, Bool())
}
