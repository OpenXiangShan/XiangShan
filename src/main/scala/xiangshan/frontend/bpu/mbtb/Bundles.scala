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

  val tag:       UInt            = UInt(TagWidth.W)
  val attribute: BranchAttribute = new BranchAttribute

  // Whether a branch is bias toward a single target
  // For conditional branch, this means bias toward same direction
  // For indirect branch, this means bias toward single target
//  val stronglyBiased: Bool = Bool() // TODO

  // Relative position to the aligned start addr
  val position: UInt = UInt(CfiAlignedPositionWidth.W)

  //  Branch target info
  val targetCarry:     TargetCarry = new TargetCarry
  val targetLowerBits: UInt        = UInt(TargetWidth.W)

//  val replaceCnt: UInt = UInt(2.W) // TODO: not used for now
}

class MainBtbEntrySramWriteReq(implicit p: Parameters) extends WriteReqBundle with HasMainBtbParameters {
  val setIdx:       UInt         = UInt(SetIdxLen.W)
  val entry:        MainBtbEntry = new MainBtbEntry
  override def tag: Option[UInt] = Some(Cat(entry.tag, entry.position)) // use entry's tag directly
}

class MainBtbCounterSramWriteReq(implicit p: Parameters) extends MainBtbBundle {
  val setIdx:   UInt                 = UInt(SetIdxLen.W)
  val wayMask:  UInt                 = UInt(NumWay.W)
  val counters: Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
}

class MainBtbMetaEntry(implicit p: Parameters) extends MainBtbBundle {
  val rawHit:    Bool            = Bool()
  val position:  UInt            = UInt(CfiPositionWidth.W)
  val attribute: BranchAttribute = new BranchAttribute
  val counter:   SaturateCounter = TakenCounter()

  def hit(branch: BranchInfo): Bool = rawHit && position === branch.cfiPosition
}

class MainBtbMeta(implicit p: Parameters) extends MainBtbBundle {
  val entries: Vec[Vec[MainBtbMetaEntry]] = Vec(NumAlignBanks, Vec(NumWay, new MainBtbMetaEntry))
}
