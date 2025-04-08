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
import utils.NamedUInt

object BranchAttribute extends NamedUInt(3) {
  // no branch
  def None: UInt = 0.U(width.W)
  // special indirect branches: return/call, refer to risc-v spec Table3. Return-address stack prediction hints
  def Pop:        UInt = 1.U(width.W) // return: rs1 is x1/x5 and rd is not
  def Push:       UInt = 2.U(width.W) // call: rd is x1/x5 and rs1 is not; or rd is x1/x5 and rs1 == rd
  def PopAndPush: UInt = 3.U(width.W) // return & call: both rd and rs1 is x1/x5, and rs1 != rd
  // conditional branches: beq, bne, blt, bge, bltu, bgeu
  def Conditional: UInt = 4.U(width.W)
  // indirect branches, other than return/call
  def Indirect: UInt = 5.U(width.W)
}
