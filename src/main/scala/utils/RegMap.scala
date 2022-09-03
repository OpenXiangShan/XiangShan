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

package utils

import chisel3._
import chisel3.util._

object RegMap {
  def Unwritable = null
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, w) => (a, r) })
    chiselMapping.map { case (a, r, w) =>
      if (w != null) when (wen && waddr === a) { r := w(MaskData(r, wdata, wmask)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata, wmask)
}

object MaskedRegMap { // TODO: add read mask
  def Unwritable = null
  def NoSideEffect: UInt => UInt = (x=>x)
  def WritableMask = Fill(64, true.B)
  def UnwritableMask = 0.U(64.W)
  def apply(addr: Int, reg: UInt,
            wmask: UInt = WritableMask, wfn: UInt => UInt = (x => x),
            rmask: UInt = WritableMask, rfn: UInt => UInt = x=>x
           ): (Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)) = (addr, (reg, wmask, wfn, rmask, rfn))
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, wm, w, rm, rfn)) => (a.U, r, wm, w, rm, rfn) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, _, _, rm, rfn) => (a, rfn(r & rm)) })
    val wdata_reg = RegEnable(wdata, wen)
    chiselMapping.foreach { case (a, r, wm, w, _, _) =>
      if (w != null && wm != UnwritableMask) {
        // Warning: this RegMap adds a RegNext for write to reduce fanout
        // the w must be pure function without side effects
        val wen_reg = RegNext(wen && waddr === a)
        when (wen_reg) { r := w(MaskData(r, wdata_reg, wm)) }
      }
    }
  }
  def isIllegalAddr(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt):Bool = {
    val illegalAddr = Wire(Bool())
    illegalAddr := LookupTreeDefault(addr, true.B, mapping.toSeq.sortBy(_._1).map { case (a, _) => (a.U, false.B) })
    illegalAddr
  }
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt, UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata)
}
