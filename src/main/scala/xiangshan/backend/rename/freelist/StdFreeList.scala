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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._


class StdFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) {

  val freeList = RegInit(VecInit(Seq.tabulate(size)( i => (i + 32).U(PhyRegIdxWidth.W) )))
  val headPtr  = RegInit(FreeListPtr(false.B, 0.U))
  val tailPtr  = RegInit(FreeListPtr(true.B,  0.U))

  //
  // free committed instructions' `old_pdest` reg
  //
  for (i <- 0 until CommitWidth) {
    val offset = if (i == 0) 0.U else PopCount(io.freeReq.take(i))
    val ptr = tailPtr + offset
    val idx = ptr.value

    when (io.freeReq(i)) {
      freeList(idx) := io.freePhyReg(i)
      XSDebug(p"req#$i free physical reg: ${io.freePhyReg(i)}\n")
    }
  }

  val tailPtrNext = tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  //
  // allocate new physical registers for instructions at rename stage
  //
  val freeRegCnt = Wire(UInt()) // number of free registers in free list
  io.canAllocate := RegNext(freeRegCnt >= RenameWidth.U) // use RegNext for better timing
  XSDebug(p"freeRegCnt: $freeRegCnt\n")

  val allocatePtr = (0 until RenameWidth).map(i => headPtr + i.U)
  val phyRegCandidates = VecInit(allocatePtr.map(ptr => freeList(ptr.value)))

  for(i <- 0 until RenameWidth){
    io.allocatePhyReg(i) := phyRegCandidates(/* if (i == 0) 0.U else */PopCount(io.allocateReq.take(i)))
    XSDebug(p"req:${io.allocateReq(i)} canAllocate:${io.canAllocate} pdest:${io.allocatePhyReg(i)}\n")
  }
  val headPtrAllocate = headPtr + PopCount(io.allocateReq)
  val headPtrNext = Mux(io.canAllocate && io.doAllocate, headPtrAllocate, headPtr)
  freeRegCnt := distanceBetween(tailPtr, headPtrNext)

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  headPtr := Mux(io.walk,
    headPtr - io.stepBack,
    Mux(io.redirect, headPtr, headPtrNext))

  XSDebug(p"head:$headPtr tail:$tailPtr\n")


  val enableFreeListCheck = false
  if (enableFreeListCheck) {
    for (i <- 0 until size) {
      for (j <- i+1 until size) {
        XSError(freeList(i) === freeList(j), s"Found same entry in free list! (i=$i j=$j)\n")
      }
    }
  }

  XSPerfAccumulate("utilization", freeRegCnt)
  XSPerfAccumulate("allocation_blocked", !io.canAllocate)
  XSPerfAccumulate("can_alloc_wrong", !io.canAllocate && freeRegCnt >= RenameWidth.U)
}
