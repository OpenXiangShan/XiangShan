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
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.TargetState
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.WriteReqBundle

class AheadBtbIO(implicit p: Parameters) extends BasePredictorIO {
  val redirectValid: Bool                  = Input(Bool())
  val overrideValid: Bool                  = Input(Bool())
  val update:        Valid[AheadBtbUpdate] = Flipped(Valid(new AheadBtbUpdate))

  val meta:             AheadBtbMeta = Output(new AheadBtbMeta)
  val debug_startVaddr: PrunedAddr   = Output(PrunedAddr(VAddrBits))
}

class BankReadReq(implicit p: Parameters) extends AheadBtbBundle {
  val setIdx: UInt = UInt(SetIdxLen.W)
}

class BankReadResp(implicit p: Parameters) extends AheadBtbBundle {
  val entries: Vec[AheadBtbEntry] = Vec(NumWays, new AheadBtbEntry)
}

// class BankWriteReq(implicit p: Parameters) extends AheadBtbBundle {
//   val isNewEntry: Bool          = Bool()
//   val setIdx:     UInt          = UInt(SetIdxLen.W)
//   val wayIdx:     UInt          = UInt(WayIdxLen.W)
//   val entry:      AheadBtbEntry = new AheadBtbEntry
// }

class BankWriteReq(implicit p: Parameters) extends WriteReqBundle with HasAheadBtbParameters {
  val setIdx:     UInt          = UInt(SetIdxLen.W)
  val isNewEntry: Bool          = Bool()
  val wayIdx:     UInt          = UInt(WayIdxLen.W)
  val entry:      AheadBtbEntry = new AheadBtbEntry
  // This is not needed since we use setIdx directly in the write buffer
  def tag: UInt = entry.tag // use entry's tag directly
}

class BankWriteResp(implicit p: Parameters) extends AheadBtbBundle {
  val setIdx: UInt = UInt(SetIdxLen.W)
  val wayIdx: UInt = UInt(log2Ceil(NumWays).W)
}

class BankIO(implicit p: Parameters) extends AheadBtbBundle {
  val readReq:  DecoupledIO[BankReadReq] = Flipped(Decoupled(new BankReadReq))
  val readResp: BankReadResp             = Output(new BankReadResp)

  val writeReq:  Valid[BankWriteReq]  = Flipped(Valid(new BankWriteReq))
  val writeResp: Valid[BankWriteResp] = Valid(new BankWriteResp)
}

class ReplacerIO(implicit p: Parameters) extends AheadBtbBundle {
  val readValid:   Bool      = Input(Bool())
  val readSetIdx:  UInt      = Input(UInt(SetIdxLen.W))
  val readHitMask: Vec[Bool] = Input(Vec(NumWays, Bool()))

  val writeValid:  Bool = Input(Bool())
  val writeSetIdx: UInt = Input(UInt(SetIdxLen.W))
  val writeWayIdx: UInt = Input(UInt(WayIdxLen.W))

  val usefulCounter:     Vec[SaturateCounter] = Input(Vec(NumWays, new SaturateCounter(UsefulCounterWidth)))
  val needReplaceSetIdx: UInt                 = Input(UInt(SetIdxLen.W))
  val victimWayIdx:      UInt                 = Output(UInt(WayIdxLen.W))
}

class AheadBtbMeta(implicit p: Parameters) extends AheadBtbBundle {
  val valid:         Bool      = Bool()
  val taken:         Bool      = Bool()
  val takenPosition: UInt      = UInt(log2Ceil(PredictWidth).W)
  val takenWayIdx:   UInt      = UInt(WayIdxLen.W)
  val hitMask:       Vec[Bool] = Vec(NumWays, Bool())
  val positions:     Vec[UInt] = Vec(NumWays, UInt(log2Ceil(PredictWidth).W))
}

class AheadBtbEntry(implicit p: Parameters) extends AheadBtbBundle {
  val valid:           Bool            = Bool()
  val tag:             UInt            = UInt(TagLen.W)
  val position:        UInt            = UInt(log2Ceil(PredictWidth).W)
  val attribute:       BranchAttribute = new BranchAttribute
  val targetState:     TargetState     = new TargetState
  val targetLowerBits: UInt            = UInt(TargetLowerBitsLen.W)
  val isStaticTarget:  Bool            = Bool()
}

class AheadBtbUpdate(implicit p: Parameters) extends AheadBtbBundle {
  val startVAddr:    PrunedAddr      = PrunedAddr(VAddrBits)
  val target:        PrunedAddr      = PrunedAddr(VAddrBits)
  val hasMispredict: Bool            = Bool()
  val taken:         Bool            = Bool()
  val cfiPosition:   UInt            = UInt(log2Ceil(PredictWidth).W)
  val cfiAttribute:  BranchAttribute = new BranchAttribute
  val aBtbMeta:      AheadBtbMeta    = new AheadBtbMeta
}
