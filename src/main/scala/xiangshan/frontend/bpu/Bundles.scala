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
import xiangshan.Redirect
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.abtb.AheadBtbMeta
import xiangshan.frontend.bpu.ittage.IttageMeta
import xiangshan.frontend.bpu.mbtb.MainBtbMeta
import xiangshan.frontend.bpu.phr.PhrPtr
import xiangshan.frontend.bpu.ras.RasInternalMeta
import xiangshan.frontend.bpu.ras.RasMeta
import xiangshan.frontend.bpu.tage.TageMeta

/* *** public const & type *** */
class BranchAttribute extends Bundle {
  val branchType: UInt = BranchAttribute.BranchType()
  val rasAction:  UInt = BranchAttribute.RasAction()

  def isNone:        Bool = branchType === BranchAttribute.BranchType.None
  def isConditional: Bool = branchType === BranchAttribute.BranchType.Conditional
  def isDirect:      Bool = branchType === BranchAttribute.BranchType.Direct
  def isIndirect:    Bool = branchType === BranchAttribute.BranchType.Indirect

  def isOtherIndirect: Bool =
    branchType === BranchAttribute.BranchType.Indirect && rasAction === BranchAttribute.RasAction.None

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

/* *** public *** */
// Csr -> Bpu
class BpuCtrl extends Bundle {
  // s1 predictor enable
  val ubtbEnable: Bool = Bool()
  val abtbEnable: Bool = Bool()
  // s3 predictor enable
  val mbtbEnable:   Bool = Bool()
  val tageEnable:   Bool = Bool()
  val scEnable:     Bool = Bool() // depends on tageEnable
  val ittageEnable: Bool = Bool()
  val rasEnable:    Bool = Bool()
}

// Bpu -> Ftq
class BpuPrediction(implicit p: Parameters) extends BpuBundle with HalfAlignHelper {
  val startVAddr:     PrunedAddr  = PrunedAddr(VAddrBits)
  val target:         PrunedAddr  = PrunedAddr(VAddrBits)
  val takenCfiOffset: Valid[UInt] = Valid(UInt(CfiPositionWidth.W))
  val identifiedCfi:  Vec[Bool]   = Vec(FetchBlockInstNum, Bool())
  // override valid
  val s3Override: Bool = Bool()

  def fromStage(pc: PrunedAddr, prediction: Prediction): Unit = {
    this.startVAddr           := pc
    this.takenCfiOffset.valid := prediction.taken
    this.takenCfiOffset.bits  := getFtqOffset(pc, prediction.cfiPosition)
    this.target               := prediction.target
    // FIXME: BP should mark all branches identified by itself
    this.identifiedCfi := (0 until FetchBlockInstNum).map(i => i.U === this.takenCfiOffset.bits)
  }
  // TODO: what else do we need?
}

// Backend & Ftq -> Bpu
class BpuRedirect(implicit p: Parameters) extends BpuBundle {
  val startVAddr:      PrunedAddr         = PrunedAddr(VAddrBits)
  val target:          PrunedAddr         = PrunedAddr(VAddrBits)
  val isRvc:           Bool               = Bool()
  val taken:           Bool               = Bool()
  val speculationMeta: BpuSpeculationMeta = new BpuSpeculationMeta
  val attribute:       BranchAttribute    = new BranchAttribute
}

class BranchInfo(implicit p: Parameters) extends BpuBundle {
  val target:      PrunedAddr      = PrunedAddr(VAddrBits)
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val attribute:   BranchAttribute = new BranchAttribute
  val mispredict:  Bool            = Bool()
}

// Backend & Ftq -> Bpu
class BpuTrain(implicit p: Parameters) extends BpuBundle with HalfAlignHelper {
  val meta:       BpuMeta                = new BpuMeta
  val startVAddr: PrunedAddr             = PrunedAddr(VAddrBits)
  val branches:   Vec[Valid[BranchInfo]] = Vec(backendParams.BrhCnt, Valid(new BranchInfo))
}

// metadata for redirect (e.g. speculative state recovery) & training (e.g. rasPtr, phr)
class BpuSpeculationMeta(implicit p: Parameters) extends BpuBundle {
  val phrHistPtr: PhrPtr          = new PhrPtr
  val rasMeta:    RasInternalMeta = new RasInternalMeta
  val topRetAddr: PrunedAddr      = PrunedAddr(VAddrBits)
  // TODO: rasPtr for recovery
  // TODO: and maybe more
}

// metadata for training (e.g. aheadBtb, mainBtb-specific)
class BpuMeta(implicit p: Parameters) extends BpuBundle {
  val abtb:   AheadBtbMeta = new AheadBtbMeta
  val mbtb:   MainBtbMeta  = new MainBtbMeta
  val ras:    RasMeta      = new RasMeta
  val phr:    PhrPtr       = new PhrPtr
  val tage:   TageMeta     = new TageMeta
  val ittage: IttageMeta   = new IttageMeta
}

/* *** internal const & type *** */
// TargetCarry is an attribute of partial target
// While lower part of target is recorded in predictor structure,
// Some more bits are need when a branch target is crossing the boundary of what lower partial target bits can record.
class TargetCarry extends Bundle {
  val value: UInt = TargetCarry.Value()

  def isFit:       Bool = value === TargetCarry.Value.Fit
  def isOverflow:  Bool = value === TargetCarry.Value.Overflow
  def isUnderflow: Bool = value === TargetCarry.Value.Underflow
}

object TargetCarry {
  private object Value extends EnumUInt(3) {
    def Fit:       UInt = 0.U(width.W)
    def Overflow:  UInt = 1.U(width.W)
    def Underflow: UInt = 2.U(width.W)
  }

  def apply(value: UInt): TargetCarry = {
    Value.assertLegal(value)
    val e = Wire(new TargetCarry)
    e.value := value
    e
  }

  def Fit:       TargetCarry = apply(Value.Fit)
  def Overflow:  TargetCarry = apply(Value.Overflow)
  def Underflow: TargetCarry = apply(Value.Underflow)
}

/* *** internal *** */
// used to sync sub-predictors
class StageCtrl(implicit p: Parameters) extends BpuBundle {
  // TODO: do we need ready / valid of each stage?
  val s0_fire: Bool = Bool()
  val s1_fire: Bool = Bool()
  val s2_fire: Bool = Bool()
  val s3_fire: Bool = Bool()
}

// sub predictors -> Bpu top
class Prediction(implicit p: Parameters) extends BpuBundle {
  val taken:       Bool            = Bool()
  val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
  val target:      PrunedAddr      = PrunedAddr(VAddrBits)
  val attribute:   BranchAttribute = new BranchAttribute
  // TODO: what else do we need?
}

// Bpu top -> sub predictors
class Train(implicit p: Parameters) extends BpuTrain {
  // selected from BpuTrain.branches in Bpu top
  val firstMispredict: Valid[BranchInfo] = Valid(new BranchInfo)
}
