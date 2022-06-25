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


class StdFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {

  val freeList = RegInit(VecInit(Seq.tabulate(size)( i => (i + 32).U(PhyRegIdxWidth.W) )))
  val headPtr  = RegInit(FreeListPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  val headPtrOHShift = CircularShift(headPtrOH)
  // may shift [0, RenameWidth] steps
  val headPtrOHVec = VecInit.tabulate(RenameWidth + 1)(headPtrOHShift.left)
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val fakeTailPtr = RegInit(FreeListPtr(true, 0)) // tailPtr in the last cycle (need to add freeReqReg)
  val tailPtr = Wire(new FreeListPtr) // this is the real tailPtr
  val tailPtrOHReg = RegInit(0.U(size.W))

  //
  // free committed instructions' `old_pdest` reg
  //
  val freeReqReg = RegNext(io.freeReq)
  for (i <- 0 until CommitWidth) {
    val offset = if (i == 0) 0.U else PopCount(io.freeReq.take(i))
    val ptr = tailPtr + offset
    val idx = ptr.value

    // Why RegNext: for better timing
    // Why we can RegNext: these free registers won't be used in the next cycle,
    // since we set canAllocate only when the current free regs > RenameWidth.
    when (freeReqReg(i)) {
      freeList(RegNext(idx)) := RegNext(io.freePhyReg(i))
    }
    XSDebug(io.freeReq(i), p"req#$i free physical reg: ${io.freePhyReg(i)}\n")
  }

  tailPtr := fakeTailPtr + PopCount(freeReqReg)
  fakeTailPtr := tailPtr

  //
  // allocate new physical registers for instructions at rename stage
  //
  val freeRegCnt = Wire(UInt()) // number of free registers in free list
  io.canAllocate := RegNext(freeRegCnt >= RenameWidth.U) // use RegNext for better timing
  XSDebug(p"freeRegCnt: $freeRegCnt\n")

  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList)))

  for(i <- 0 until RenameWidth){
    io.allocatePhyReg(i) := phyRegCandidates(/* if (i == 0) 0.U else */PopCount(io.allocateReq.take(i)))
    XSDebug(p"req:${io.allocateReq(i)} canAllocate:${io.canAllocate} pdest:${io.allocatePhyReg(i)}\n")
  }
  val numAllocate = PopCount(io.allocateReq)
  val headPtrAllocate = headPtr + numAllocate
  val headPtrNext = Mux(io.canAllocate && io.doAllocate, headPtrAllocate, headPtr)
  freeRegCnt := distanceBetween(tailPtr, headPtrNext)

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  val realDoAllocate = !io.redirect && io.canAllocate && io.doAllocate
  headPtr := Mux(io.walk,
    headPtr - io.stepBack,
    Mux(realDoAllocate, headPtrAllocate, headPtr))

  // Since the update of headPtr should have a good timing,
  // we calculate the OH index here to optimize the freelist read timing.
  // may shift [0, RenameWidth] steps
  val stepBackHeadPtrOHVec = VecInit.tabulate(RenameWidth + 1)(headPtrOHShift.right)
  val stepBackHeadPtrOH = stepBackHeadPtrOHVec(io.stepBack)
  headPtrOH := Mux(io.walk, stepBackHeadPtrOH,
    Mux(realDoAllocate, headPtrOHVec(numAllocate), headPtrOH))

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

  val freeRegCntReg = RegNext(freeRegCnt)
  val perfEvents = Seq(
    ("std_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                    ),
    ("std_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg < (size / 2).U    ),
    ("std_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg < (size * 3 / 4).U),
    ("std_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                )
  )
  generatePerfEvent()
}
