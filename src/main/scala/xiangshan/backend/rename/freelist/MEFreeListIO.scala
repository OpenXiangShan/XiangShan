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

trait MEFreeListIO extends FreeListBaseIO {
  // psrc of move instructions ready for elimination
  def psrcOfMove: Vec[Valid[UInt]]

  // instruction fits move elimination
  def eliminatedMove: Vec[Bool]
  // for eliminated move instruction, increase arch ref count of (new) p_dest reg
  def multiRefPhyReg: Vec[UInt]

  // max vector from speculative reference counter
  def maxVec: Vec[Bool]
}
