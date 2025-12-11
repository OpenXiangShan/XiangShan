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
import xiangshan.XSCoreParamsKey
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounterFactory
import xiangshan.frontend.bpu.TargetCarry
import xiangshan.frontend.bpu.WriteReqBundle

object TakenCounter extends SaturateCounterFactory {
  def width(implicit p: Parameters): Int =
    p(XSCoreParamsKey).frontendParameters.bpuParameters.abtbParameters.TakenCounterWidth
}

class BankReadReq(implicit p: Parameters) extends AheadBtbBundle {
  val setIdx: UInt = UInt(SetIdxWidth.W)
}

class BankReadResp(implicit p: Parameters) extends AheadBtbBundle {
  val entries: Vec[AheadBtbEntry] = Vec(NumWays, new AheadBtbEntry)
}

class BankWriteReq(implicit p: Parameters) extends WriteReqBundle with HasAheadBtbParameters {
  val needResetCtr: Bool          = Bool()
  val setIdx:       UInt          = UInt(SetIdxWidth.W)
  val wayIdx:       UInt          = UInt(WayIdxWidth.W)
  val entry:        AheadBtbEntry = new AheadBtbEntry

  override def tag: Option[UInt] = Some(entry.tag)
}

class BankWriteResp(implicit p: Parameters) extends AheadBtbBundle {
  val needResetCtr: Bool = Bool()
  val setIdx:       UInt = UInt(SetIdxWidth.W)
  val wayIdx:       UInt = UInt(WayIdxWidth.W)
}

class ReplacerIO(implicit p: Parameters) extends AheadBtbBundle {
  val readValid:   Bool      = Input(Bool())
  val readSetIdx:  UInt      = Input(UInt(SetIdxWidth.W))
  val readWayMask: Vec[Bool] = Input(Vec(NumWays, Bool()))

  val writeValid:  Bool = Input(Bool())
  val writeSetIdx: UInt = Input(UInt(SetIdxWidth.W))
  val writeWayIdx: UInt = Input(UInt(WayIdxWidth.W))

  val replaceSetIdx: UInt = Input(UInt(SetIdxWidth.W))
  val victimWayIdx:  UInt = Output(UInt(WayIdxWidth.W))
}

class AheadBtbMeta(implicit p: Parameters) extends AheadBtbBundle {
  val valid:           Bool                 = Bool()
  val hitMask:         Vec[Bool]            = Vec(NumWays, Bool())
  val attributes:      Vec[BranchAttribute] = Vec(NumWays, new BranchAttribute)
  val positions:       Vec[UInt]            = Vec(NumWays, UInt(CfiPositionWidth.W))
  val taken:           Bool                 = Bool()
  val takenMaskOH:     Vec[Bool]            = Vec(NumWays, Bool())
  val targetLowerBits: UInt                 = UInt(TargetLowerBitsWidth.W)
}

class AheadBtbEntry(implicit p: Parameters) extends AheadBtbBundle {
  val valid:           Bool            = Bool()
  val tag:             UInt            = UInt(TagWidth.W)
  val position:        UInt            = UInt(CfiPositionWidth.W)
  val attribute:       BranchAttribute = new BranchAttribute
  val targetLowerBits: UInt            = UInt(TargetLowerBitsWidth.W)
  // target fix, see comment in Parameters.scala
  val targetCarry: Option[TargetCarry] = if (EnableTargetFix) Option(new TargetCarry) else None
}
