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

package xiangshan.frontend.bpu.history.ghr

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.frontend.PrunedAddr

class GhrEntry(implicit p: Parameters) extends GhrBundle {
  val valid: Bool = Bool()
  val ghr:   UInt = UInt(GhrHistoryLength.W)
  val imli:  UInt = UInt(ImliHistoryLength.W)
}
class GhrUpdate(implicit p: Parameters) extends GhrBundle {
  val taken:        Bool       = Bool()
  val hitMask:      Vec[Bool]  = Vec(NumBtbResultEntries, Bool())
  val position:     Vec[UInt]  = Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W))
  val firstTakenOH: Vec[Bool]  = Vec(NumBtbResultEntries, Bool())
  val startPc:      PrunedAddr = PrunedAddr(VAddrBits)
  val target:       PrunedAddr = PrunedAddr(VAddrBits)
}

class GhrMeta(implicit p: Parameters) extends GhrBundle {
  val ghr:      UInt      = UInt(GhrHistoryLength.W)
  val imli:     UInt      = UInt(ImliHistoryLength.W)
  val hitMask:  Vec[Bool] = Vec(NumBtbResultEntries, Bool())
  val position: Vec[UInt] = Vec(NumBtbResultEntries, UInt(CfiPositionWidth.W))
}

class GhrRedirect(implicit p: Parameters) extends GhrBundle {
  val valid:  Bool       = Bool()
  val cfiPc:  PrunedAddr = PrunedAddr(VAddrBits)
  val taken:  Bool       = Bool()
  val target: PrunedAddr = PrunedAddr(VAddrBits)
  val meta:   GhrMeta    = new GhrMeta
}
