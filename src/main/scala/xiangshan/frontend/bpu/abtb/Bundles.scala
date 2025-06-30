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

package xiangshan.frontend.bpu.abtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuBundle
import xiangshan.frontend.bpu.BranchAttribute

class TargetState extends Bundle {
  val value: UInt = TargetState.Value()

  def NoCarryAndBorrow: Bool = value === TargetState.Value.NoCarryAndBorrow
  def Carry:            Bool = value === TargetState.Value.Carry
  def Borrow:           Bool = value === TargetState.Value.Borrow
}

object TargetState {
  object Value extends EnumUInt(3, useOneHot = true) {
    def NoCarryAndBorrow: UInt = 1.U(width.W)
    def Carry:            UInt = 2.U(width.W)
    def Borrow:           UInt = 4.U(width.W)
  }
}

class AheadBtbMeta(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val valid         = Bool()
  val taken         = Bool()
  val takenPosition = UInt(log2Ceil(PredictWidth).W)
  val takenWayIdx   = UInt(WayIdxLen.W)
  val hitMask       = Vec(NumWays, Bool())
  val positions     = Vec(NumWays, UInt(log2Ceil(PredictWidth).W))
}

class AheadBtbEntry(implicit p: Parameters) extends BpuBundle with HasAheadBtbParameters {
  val valid           = Bool()
  val tag             = UInt(TagLen.W)
  val position        = UInt(log2Ceil(PredictWidth).W)
  val attribute       = new BranchAttribute
  val targetState     = new TargetState
  val targetLowerBits = UInt(TargetLowerBitsLen.W)
  val isStaticTarget  = Bool()
}

class AheadBtbUpdate(implicit p: Parameters) extends BpuBundle {
  val startVAddr    = PrunedAddr(VAddrBits)
  val target        = PrunedAddr(VAddrBits)
  val hasMispredict = Bool()
  val taken         = Bool()
  val cfiPosition   = UInt(log2Ceil(PredictWidth).W)
  val cfiAttribute  = new BranchAttribute
  val aBtbMeta      = new AheadBtbMeta
}
