/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait FreeListBaseIO {

  // control signals from CtrlBlock
  def flush: Bool
  def redirect: Bool
  def walk: Bool

  // allocate physical registers (rename)
  def allocateReq: Vec[Bool] // need allocating phy reg (may be refused due to lacking of free reg)
  def allocatePhyReg: Vec[UInt] // phy dest response according to allocateReq
  def canAllocate: Bool // free list can allocate new phy registers
  def doAllocate: Bool // actually do the allocation (given by rename)

  // free old physical registers (commit)
  def freeReq: Vec[Bool] // need to free phy reg
  def freePhyReg: Vec[UInt] // free old p_dest reg

  // walk recovery
  def stepBack: UInt
}
