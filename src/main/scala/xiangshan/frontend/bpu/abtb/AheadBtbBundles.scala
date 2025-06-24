// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
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
import xiangshan.HasXSParameter
import xiangshan.XSBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute

trait HasAheadBtbParams extends HasXSParameter {
  val FetchBlockSize     = 32 // Byte TODO: 64
  val FetchAddrAlignSize = 32 // Byte
  val NumEntries         = 1024
  val NumWays            = 4
  val NumSets            = NumEntries / NumWays
  val TagLen             = 24
  val TargetLowerBitsLen = 22 // Note: The LSB (bit 0) of the target address is excluded.
  val NumBanks           = 4
  val SetIndexLen        = log2Ceil(NumSets)
  val WayIdxLen          = log2Ceil(NumWays)
  val BankIdxLen         = log2Ceil(NumBanks)
  val WriteBufferSize    = 4
  val TakenCounterWidth  = 2

  require(isPow2(FetchBlockSize))
  require(isPow2(FetchAddrAlignSize))
  require(isPow2(NumWays))
  require(isPow2(NumSets))
  require(isPow2(NumBanks))
}

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

class AheadBtbMeta(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val hit           = Bool()
  val hitMask       = Vec(NumWays, Bool())
  val taken         = Bool()
  val takenMask     = Vec(NumWays, Bool())
  val takenWayIdx   = UInt(WayIdxLen.W)
  val takenPosition = UInt(log2Ceil(PredictWidth).W)
  val positions     = Vec(NumWays, UInt(log2Ceil(PredictWidth).W))
}

class AheadBtbEntry(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val valid           = Bool()
  val tag             = UInt(TagLen.W)
  val isStaticTarget  = Bool()
  val position        = UInt(log2Ceil(PredictWidth).W)
  val branchAttribute = new BranchAttribute
  val targetState     = new TargetState
  val targetLowerBits = UInt(TargetLowerBitsLen.W)
}

class AheadBtbUpdateBundle(implicit p: Parameters) extends XSBundle with HasAheadBtbParams {
  val valid          = Bool()
  val position       = UInt(log2Ceil(FetchBlockSize).W)
  val startPc        = PrunedAddr(VAddrBits)
  val target         = PrunedAddr(VAddrBits)
  val taken          = Bool()
  val hasMispredict  = Bool()
  val meta           = new AheadBtbMeta
  val mispredictMask = Vec(PredictWidth, Bool())
  val cfiPosition    = Valid(UInt(log2Ceil(PredictWidth).W)) // control flow instruction position
  val cfiAttribute   = new BranchAttribute
  val notCfiMask     = Vec(PredictWidth, Bool())

}

class BpuToAheadBtbIO(implicit p: Parameters) extends XSBundle {
  val startPc       = Flipped(Valid(PrunedAddr(VAddrBits)))
  val bpuOverride   = Input(Bool())
  val redirectValid = Input(Bool())
//  val update        = Flipped(Valid(new AheadBtbUpdateBundle))
}

class AheadBtbToBpuIO(implicit p: Parameters) extends XSBundle {
  val prediction = Valid(new Prediction)
}

class AheadBtbIO(implicit p: Parameters) extends XSBundle {
  val fromBpu = new BpuToAheadBtbIO
  val toBpu   = new AheadBtbToBpuIO
}

class Prediction(implicit p: Parameters) extends XSBundle {
  val startPc          = PrunedAddr(VAddrBits)
  val target           = PrunedAddr(VAddrBits)
  val takenPosition    = Valid(UInt(log2Ceil(PredictWidth).W))
  val fallThroughError = Bool()
}
