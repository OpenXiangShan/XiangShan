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
import utils.EnumUInt
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.TargetCarry
import xiangshan.frontend.bpu.WriteReqBundle

class MainBtbEntry(implicit p: Parameters) extends MainBtbBundle {

  // whether the entry is valid
  val valid: Bool = Bool()

  val tag:       UInt            = UInt(TagWidth.W)
  val attribute: BranchAttribute = new BranchAttribute

  // Whether a branch is bias toward a single target
  // For conditional branch, this means bias toward same direction
  // For indirect branch, this means bias toward single target
  val stronglyBiased: Bool = Bool()

  // Relative position to the aligned start addr
  val position: UInt = UInt(CfiAlignedPositionWidth.W)

  //  Branch target info
  val targetCarry: EnumUInt = TargetCarry
  val target:      UInt     = UInt(TargetWidth.W)

  val replaceCnt: UInt = UInt(2.W) // FIXME: not used for now
}

class MainBtbSramWriteReq(implicit p: Parameters) extends WriteReqBundle with HasMainBtbParameters {
  val setIdx: UInt         = UInt(SetIdxLen.W)
  val entry:  MainBtbEntry = new MainBtbEntry
  def tag:    UInt         = entry.tag // use entry's tag directly
}

class MainBtbMeta(implicit p: Parameters) extends MainBtbBundle {
  val valid              = Bool()
  val hitMask            = Vec(NumAlignBanks * NumWay, Bool())
  val stronglyBiasedMask = Vec(NumAlignBanks * NumWay, Bool())
  val positions          = Vec(NumAlignBanks * NumWay, UInt(CfiPositionWidth.W)) // FIXME: use correct one
}

class MainBtbTrain(implicit p: Parameters) extends MainBtbBundle {
  val startVAddr:  PrunedAddr      = Input(PrunedAddr(VAddrBits))
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val target:      PrunedAddr      = PrunedAddr(VAddrBits)
  val attribute:   BranchAttribute = new BranchAttribute
  val meta:        MainBtbMeta     = new MainBtbMeta
}
