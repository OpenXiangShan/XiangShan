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
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.TargetState

class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO {
  val redirectValid: Bool                 = Input(Bool())
  val overrideValid: Bool                 = Input(Bool())
  val train:         Valid[AheadBtbTrain] = Flipped(Valid(new AheadBtbTrain))

  val meta:             AheadBtbMeta = Output(new AheadBtbMeta)
  val debug_startVaddr: PrunedAddr   = Output(PrunedAddr(VAddrBits))
}

class BankReadReq(implicit p: Parameters) extends AheadBtbBundle {
  val setIdx: UInt = UInt(SetIdxWidth.W)
}

class BankReadResp(implicit p: Parameters) extends AheadBtbBundle {
  val entries: Vec[AheadBtbEntry] = Vec(NumWays, new AheadBtbEntry)
}

class BankWriteReq(implicit p: Parameters) extends AheadBtbBundle {
  val needResetCtr: Bool          = Bool()
  val setIdx:       UInt          = UInt(SetIdxWidth.W)
  val wayIdx:       UInt          = UInt(WayIdxWidth.W)
  val entry:        AheadBtbEntry = new AheadBtbEntry
}

class BankWriteResp(implicit p: Parameters) extends AheadBtbBundle {
  val needResetCtr: Bool = Bool()
  val setIdx:       UInt = UInt(SetIdxWidth.W)
  val wayIdx:       UInt = UInt(WayIdxWidth.W)
}

class BankIO(implicit p: Parameters) extends AheadBtbBundle {
  val readReq:  DecoupledIO[BankReadReq] = Flipped(Decoupled(new BankReadReq))
  val readResp: BankReadResp             = Output(new BankReadResp)

  val writeReq:  Valid[BankWriteReq]  = Flipped(Valid(new BankWriteReq))
  val writeResp: Valid[BankWriteResp] = Valid(new BankWriteResp)
}

class ReplacerIO(implicit p: Parameters) extends AheadBtbBundle {
  val readValid:   Bool      = Input(Bool())
  val readSetIdx:  UInt      = Input(UInt(SetIdxWidth.W))
  val readWayMask: Vec[Bool] = Input(Vec(NumWays, Bool()))

  val writeValid:  Bool = Input(Bool())
  val writeSetIdx: UInt = Input(UInt(SetIdxWidth.W))
  val writeWayIdx: UInt = Input(UInt(WayIdxWidth.W))

//  val usefulCounter:     Vec[SaturateCounter] = Input(Vec(NumWays, new SaturateCounter(UsefulCounterWidth)))
  val needReplaceSetIdx: UInt = Input(UInt(SetIdxWidth.W))
  val victimWayIdx:      UInt = Output(UInt(WayIdxWidth.W))
}

class AheadBtbMeta(implicit p: Parameters) extends AheadBtbBundle {
  val valid: Bool = Bool()
//  val previousPc:    PrunedAddr           = PrunedAddr(VAddrBits) // TODO: update after execution will need it
  val hitMask:     Vec[Bool]            = Vec(NumWays, Bool())
  val taken:       Bool                 = Bool()
  val takenWayIdx: UInt                 = UInt(WayIdxWidth.W)
  val attributes:  Vec[BranchAttribute] = Vec(NumWays, new BranchAttribute)
  val positions:   Vec[UInt]            = Vec(NumWays, UInt(CfiPositionWidth.W))
  // TODO: remove it, we need backend send one bit to indicate whether the target is right or wrong
  val target: PrunedAddr = PrunedAddr(VAddrBits)
}

class AheadBtbEntry(implicit p: Parameters) extends AheadBtbBundle {
  val valid:           Bool            = Bool()
  val tag:             UInt            = UInt(TagWidth.W)
  val position:        UInt            = UInt(CfiPositionWidth.W)
  val attribute:       BranchAttribute = new BranchAttribute
  val targetState:     TargetState     = new TargetState
  val targetLowerBits: UInt            = UInt(TargetLowerBitsWidth.W)
//  val isStaticTarget:  Bool            = Bool() // TODO: abtb really need it?
}

class AheadBtbTrain(implicit p: Parameters) extends AheadBtbBundle {
  val startPc:   PrunedAddr      = PrunedAddr(VAddrBits)
  val target:    PrunedAddr      = PrunedAddr(VAddrBits)
  val taken:     Bool            = Bool()
  val position:  UInt            = UInt(CfiPositionWidth.W)
  val attribute: BranchAttribute = new BranchAttribute
  val abtbMeta:  AheadBtbMeta    = new AheadBtbMeta
}
