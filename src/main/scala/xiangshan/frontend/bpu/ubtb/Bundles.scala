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

package xiangshan.frontend.bpu.ubtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.TargetCarry

object UsefulCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.ubtbParameters.UsefulCntWidth
}

class MicroBtbEntry(implicit p: Parameters) extends MicroBtbBundle {
  class SlotBase extends Bundle {
    // branch position: at fetchBlockVAddr + position
    val position: UInt = UInt(CfiPositionWidth.W)
    // branch attribute
    val attribute: BranchAttribute = new BranchAttribute
    // partial target: full target = Cat(fetchBlockVAddr(VAddrBits-1, TargetWidth), target)
    val target: UInt = UInt(TargetWidth.W)

    // used for target fix, see comment in Parameters.scala
    val targetCarry: Option[TargetCarry] = if (EnableTargetFix) Option(new TargetCarry) else None
  }

  class Slot1 extends SlotBase {
    // whether branch in slot 1 has a static target
    val isStaticTarget: Bool = Bool()
  }

  class Slot2 extends SlotBase {
    // whether branch in slot 2 is valid
    val valid: Bool = Bool()
    // whether branch in slot 2 is predicted as taken
    val taken: Bool = Bool()
  }

  // we consider an entry is valid if it has usefulCnt > 0
  def valid: Bool = !usefulCnt.isSaturateNegative
  // partial vTag = fetchBlockVAddr(TagWidth, 1)
  val tag: UInt = UInt(TagWidth.W)
  // saturate counter indicating how useful is this entry
  val usefulCnt: SaturateCounter = UsefulCounter()

  val slot1: Slot1 = new Slot1
  val slot2: Slot2 = new Slot2
}

class MicroBtbMeta(implicit p: Parameters) extends MicroBtbBundle {
  // seems no meta is needed now, reserved for future use
}

class ReplacerPerfInfo(implicit p: Parameters) extends MicroBtbBundle {
  val replaceNotUseful: Bool = Bool() // if not, replacePlru
}
