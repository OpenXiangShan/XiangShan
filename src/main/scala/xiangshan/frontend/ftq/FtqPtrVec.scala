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

package xiangshan.frontend.ftq

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

class FtqPtrVec(val num: Int = 1)(implicit p: Parameters) extends FtqBundle {
  val ptrs: Vec[FtqPtr] = Vec(num, new FtqPtr)

  def :=(basePtr: FtqPtr): Unit = ptrs.zipWithIndex.foreach { case (ptr, offset) =>
    ptr := basePtr + offset.U
  }

  def +(offset: UInt): FtqPtr = ptrs(0) + offset

  def ===(that: FtqPtr): Bool = ptrs(0) === that

  def <(that: FtqPtrVec): Bool = ptrs(0) < that.ptrs(0)

  def <(that: FtqPtr): Bool = ptrs(0) < that

  def <=(that: FtqPtr): Bool = ptrs(0) <= that

  def >(that: FtqPtr): Bool = ptrs(0) > that

  def >=(that: FtqPtr): Bool = ptrs(0) >= that

  def apply(offset: Int): FtqPtr = {
    require(offset < num)
    ptrs(offset)
  }
}

object FtqPtrVec {
  def apply(num: Int = 1)(implicit p: Parameters): FtqPtrVec = {
    require(num < p(XSCoreParamsKey).frontendParameters.ftqParameters.FtqSize)
    val ptrVec = Wire(new FtqPtrVec(num))
    // We don't use ptrVec := FtqPtr(false, 0.U) because FtqPtr + involves a Mux,
    // which makes Chisel unable to infer that the initial value is a constant, which will fail with async reset.
    ptrVec.ptrs.zipWithIndex.foreach { case (ptr, offset) => ptr := FtqPtr(false.B, offset.U) }
    ptrVec
  }
}
