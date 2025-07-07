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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils.EnumUInt
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.abtb.AheadBtbMeta
import xiangshan.frontend.ftq.FtqPtr

class BranchAttribute extends Bundle {
  val branchType: UInt = BranchAttribute.BranchType()
  val rasAction:  UInt = BranchAttribute.RasAction()

  def isNone:        Bool = branchType === BranchAttribute.BranchType.None
  def isConditional: Bool = branchType === BranchAttribute.BranchType.Conditional
  def isDirect:      Bool = branchType === BranchAttribute.BranchType.Direct
  def isIndirect:    Bool = branchType === BranchAttribute.BranchType.Indirect

  // NOTE: maybe we should check branchType === BranchAttribute.BranchType.Direct/Indirect,
  //       but as BranchAttribute.BranchType is declared as private,
  //       we should not able to create attribute with (Conditional, Push) or something like that.
  //       So, just check rasAction should be enough.
  def isCall:          Bool = rasAction === BranchAttribute.RasAction.Push
  def isReturn:        Bool = rasAction === BranchAttribute.RasAction.Pop
  def isReturnAndCall: Bool = rasAction === BranchAttribute.RasAction.PopAndPush

  // hasPop = isPop || isPushAndPop, hasPush = isPush || isPushAndPop
  def hasPop:  Bool = rasAction(BranchAttribute.RasAction.popBit)
  def hasPush: Bool = rasAction(BranchAttribute.RasAction.pushBit)
}

object BranchAttribute {
  private object BranchType extends EnumUInt(4) {
    // no branch
    def None: UInt = 0.U(width.W)
    // conditional branches: beq, bne, blt, bge, bltu, bgeu
    def Conditional: UInt = 1.U(width.W)
    // direct branches: j, jal
    def Direct: UInt = 2.U(width.W)
    // indirect branches: jr, jalr
    def Indirect: UInt = 3.U(width.W)
  }
  private object RasAction extends EnumUInt(4) {
    def popBit:  Int = 0
    def pushBit: Int = 1
    // no action
    def None: UInt = 0.U(width.W)
    // special indirect branches: return/call, refer to risc-v spec Table3. Return-address stack prediction hints
    // return: jalr with rs1=x1/x5 and rd!=x1/x5
    def Pop: UInt = (1 << popBit).U(width.W)
    // call: jalr with rd=x1/x5 and rs1!=x1/x5; or rd=x1/x5 and rs1=rd. Or jal with rd=x1/x5
    def Push: UInt = (1 << pushBit).U(width.W)
    // return & call: jalr with rd=x1/x5 and rs1=x1/x5 and rs1!=rd
    def PopAndPush: UInt = ((1 << popBit) | (1 << pushBit)).U(width.W)
  }

  def apply(branchType: UInt, rasAction: UInt): BranchAttribute = {
    BranchType.assertLegal(branchType)
    RasAction.assertLegal(rasAction)
    val e = Wire(new BranchAttribute)
    e.branchType := branchType
    e.rasAction  := rasAction
    e
  }

  def None:          BranchAttribute = apply(BranchType.None, RasAction.None)
  def Conditional:   BranchAttribute = apply(BranchType.Conditional, RasAction.None)
  def DirectCall:    BranchAttribute = apply(BranchType.Direct, RasAction.Push)
  def IndirectCall:  BranchAttribute = apply(BranchType.Indirect, RasAction.Push)
  def Return:        BranchAttribute = apply(BranchType.Indirect, RasAction.Pop)
  def ReturnAndCall: BranchAttribute = apply(BranchType.Indirect, RasAction.PopAndPush)
  def OtherDirect:   BranchAttribute = apply(BranchType.Direct, RasAction.None)
  def OtherIndirect: BranchAttribute = apply(BranchType.Indirect, RasAction.None)
}

// used to sync sub-predictors
class StageCtrl(implicit p: Parameters) extends BpuBundle {
  // TODO: do we need ready / valid of each stage?
  val s0_fire: Bool = Bool()
  val s1_fire: Bool = Bool()
  val s2_fire: Bool = Bool()
  val s3_fire: Bool = Bool()
}

// sub predictors -> Bpu top
class BranchPrediction(implicit p: Parameters) extends BpuBundle {
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val target:      PrunedAddr      = PrunedAddr(VAddrBits)
  val attribute:   BranchAttribute = new BranchAttribute
  // TODO: what else do we need?
}

class OverrideBranchPrediction(implicit p: Parameters) extends BpuBundle {
  val ftqPtr: FtqPtr = new FtqPtr
}

// Bpu -> Ftq
class FullBranchPrediction(implicit p: Parameters) extends BpuBundle {
  val startVAddr: PrunedAddr = PrunedAddr(VAddrBits)
  // FIXME: do not use Valid[UInt] for cfiPosition, currently keeping it for Ftq compatibility
//  val taken:       Bool       = Bool()
  val cfiPosition: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
  val target:      PrunedAddr  = PrunedAddr(VAddrBits)
  // override valid
  val s2Override: Valid[OverrideBranchPrediction] = Valid(new OverrideBranchPrediction)
  val s3Override: Valid[OverrideBranchPrediction] = Valid(new OverrideBranchPrediction)

  def fromStage(pc: PrunedAddr, prediction: BranchPrediction): Unit = {
    this.startVAddr        := pc
    this.cfiPosition.valid := prediction.taken
    this.cfiPosition.bits  := prediction.cfiPosition
    this.target            := prediction.target
  }

  def overrideStage(idx: Int): Valid[OverrideBranchPrediction] = {
    require(idx >= 2 && idx <= 3)
    idx match {
      case 2 => s2Override
      case 3 => s3Override
    }
  }

  // TODO: what else do we need?
}

class NewPredictorMeta(implicit p: Parameters) extends BpuBundle {
  val aBtbMeta: AheadBtbMeta = new AheadBtbMeta
  // TODO: other meta
}

class TargetState extends Bundle {
  val value: UInt = TargetState.Value()

  def noCarryAndBorrow: Bool = value === TargetState.Value.NoCarryAndBorrow
  def isCarry:          Bool = value === TargetState.Value.Carry
  def isBorrow:         Bool = value === TargetState.Value.Borrow
}

object TargetState {
  private object Value extends EnumUInt(3) {
    def NoCarryAndBorrow: UInt = 0.U(width.W)
    def Carry:            UInt = 1.U(width.W)
    def Borrow:           UInt = 2.U(width.W)
  }

  def apply(value: UInt): TargetState = {
    Value.assertLegal(value)
    val e = Wire(new TargetState)
    e.value := value
    e
  }

  def NoCarryAndBorrow: TargetState = apply(Value.NoCarryAndBorrow)
  def Carry:            TargetState = apply(Value.Carry)
  def Borrow:           TargetState = apply(Value.Borrow)
}
