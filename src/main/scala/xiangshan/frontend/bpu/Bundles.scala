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
import utils.EnumUInt

class BranchAttribute extends Bundle {
  val value: UInt = BranchAttribute.Value()

  def isNone:        Bool = value === BranchAttribute.Value.None
  def isPop:         Bool = value === BranchAttribute.Value.Pop
  def isPush:        Bool = value === BranchAttribute.Value.Push
  def isPopAndPush:  Bool = value === BranchAttribute.Value.PopAndPush
  def isConditional: Bool = value === BranchAttribute.Value.Conditional
  def isIndirect:    Bool = value === BranchAttribute.Value.Indirect

  def hasPop:  Bool = value === BranchAttribute.Value.Pop || value === BranchAttribute.Value.PopAndPush
  def hasPush: Bool = value === BranchAttribute.Value.Push || value === BranchAttribute.Value.PopAndPush
}

object BranchAttribute {
  private object Value extends EnumUInt(6) {
    // no branch
    def None: UInt = 0.U(width.W)
    // special indirect branches: return/call, refer to risc-v spec Table3. Return-address stack prediction hints
    // NOTE: do not change this encoding, this actually ensures `Pop | Push = PopAndPush`, so area can be better
    def Pop:        UInt = 1.U(width.W) // return: rs1 is x1/x5 and rd is not
    def Push:       UInt = 2.U(width.W) // call: rd is x1/x5 and rs1 is not; or rd is x1/x5 and rs1 == rd
    def PopAndPush: UInt = 3.U(width.W) // return & call: both rd and rs1 is x1/x5, and rs1 != rd
    // conditional branches: beq, bne, blt, bge, bltu, bgeu
    def Conditional: UInt = 4.U(width.W)
    // indirect branches, other than return/call
    def Indirect: UInt = 5.U(width.W)
  }

  def apply(that: UInt): BranchAttribute = {
    Value.assertLegal(that)
    val e = Wire(new BranchAttribute)
    e.value := that
    e
  }

  def None:        BranchAttribute = apply(Value.None)
  def Pop:         BranchAttribute = apply(Value.Pop)
  def Push:        BranchAttribute = apply(Value.Push)
  def PopAndPush:  BranchAttribute = apply(Value.PopAndPush)
  def Conditional: BranchAttribute = apply(Value.Conditional)
  def Indirect:    BranchAttribute = apply(Value.Indirect)
}
