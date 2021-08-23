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
import chipsalliance.rocketchip.config

class StdFreeList(implicit val p: config.Parameters) extends MultiIOModule with FreeListBaseIO with HasXSParameter with HasCircularQueuePtrHelper {
  val flush = IO(Input(Bool()))
  val redirect = IO(Input(Bool()))
  val walk = IO(Input(Bool()))

  val allocateReq = IO(Input(Vec(RenameWidth, Bool())))
  val allocatePhyReg = IO(Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W))))
  val canAllocate = IO(Output(Bool()))
  val doAllocate = IO(Input(Bool()))

  val freeReq = IO(Input(Vec(CommitWidth, Bool())))
  val freePhyReg = IO(Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W))))

  val stepBack = IO(Input(UInt(log2Up(CommitWidth + 1).W)))


  class FreeListPtr extends CircularQueuePtr[FreeListPtr](StdFreeListSize)

  object FreeListPtr {
    def apply(f: Bool, v: UInt): FreeListPtr = {
      val ptr = Wire(new FreeListPtr)
      ptr.flag := f
      ptr.value := v
      ptr
    }
  }


  val freeList = RegInit(VecInit(Seq.tabulate(StdFreeListSize)( i => (i + 32).U(PhyRegIdxWidth.W) )))
  val headPtr  = RegInit(FreeListPtr(false.B, 0.U))
  val tailPtr  = RegInit(FreeListPtr(true.B,  0.U))

  //
  // free committed instructions' `old_pdest` reg
  //
  for (i <- 0 until CommitWidth) {
    val offset = if (i == 0) 0.U else PopCount(freeReq.take(i))
    val ptr = tailPtr + offset
    val idx = ptr.value

    when (freeReq(i)) {
      freeList(idx) := freePhyReg(i)
      XSDebug(p"req#$i free physical reg: ${freePhyReg(i)}\n")
    }
  }

  val tailPtrNext = tailPtr + PopCount(freeReq)
  tailPtr := tailPtrNext

  //
  // allocate new physical registers for instructions at rename stage
  //
  val freeRegCnt = Wire(UInt()) // number of free registers in free list
  canAllocate := RegNext(freeRegCnt >= RenameWidth.U) // use RegNext for better timing
  XSDebug(p"freeRegCnt: $freeRegCnt\n")

  val allocatePtr = (0 until RenameWidth).map(i => headPtr + i.U)
  val phyRegCandidates = VecInit(allocatePtr.map(ptr => freeList(ptr.value)))

  for(i <- 0 until RenameWidth){
    allocatePhyReg(i) := phyRegCandidates(/* if (i == 0) 0.U else */PopCount(allocateReq.take(i)))
    XSDebug(p"req:${allocateReq(i)} canAllocate:${canAllocate} pdest:${allocatePhyReg(i)}\n")
  }
  val headPtrAllocate = headPtr + PopCount(allocateReq)
  val headPtrNext = Mux(canAllocate && doAllocate, headPtrAllocate, headPtr)
  freeRegCnt := distanceBetween(tailPtr, headPtrNext)

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  headPtr := Mux(flush,
    FreeListPtr(!tailPtrNext.flag, tailPtrNext.value),
    Mux(walk,
      headPtr - stepBack,
      Mux(redirect, headPtr, headPtrNext))
  )

  XSDebug(p"head:$headPtr tail:$tailPtr\n")


  val enableFreeListCheck = false
  if (enableFreeListCheck) {
    for (i <- 0 until StdFreeListSize) {
      for (j <- i+1 until StdFreeListSize) {
        XSError(freeList(i) === freeList(j), s"Found same entry in free list! (i=$i j=$j)\n")
      }
    }
  }

  XSPerfAccumulate("utilization", freeRegCnt)
  XSPerfAccumulate("allocation_blocked", !canAllocate)
  XSPerfAccumulate("can_alloc_wrong", !canAllocate && freeRegCnt >= RenameWidth.U)
}
