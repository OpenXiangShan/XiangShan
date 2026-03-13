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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.rename._
import utils._
import utility._


class StdFreeList(
  freeListSize: Int,
  numLogicRegs: Int,
  regType: RegType,
  commitWidth: Int,
  realNumLogicRegs: Int = 32,
)(implicit p: Parameters) extends BaseFreeList(freeListSize, commitWidth, realNumLogicRegs) with HasPerfEvents {

  val freeList = RegInit(VecInit(Seq.tabulate(freeListSize)( i => (i + numLogicRegs).U(PhyRegIdxWidth.W) )))
  val tailPtr = RegInit(FreeListPtr(true, 0)) // tailPtr in the last cycle (need to add freeReqReg)
  val tailPtrNext = Wire(new FreeListPtr) // this is the real tailPtr

  //
  // free committed instructions' `old_pdest` reg
  //
  val freePtr = VecInit(Seq.tabulate(commitWidth)(i => tailPtr + PopCount(io.freeReq.take(i))))
  for (i <- 0 until freeListSize) {
    val freeReqOH = VecInit(io.freeReq.zipWithIndex.map { case (w, idx) =>
      w && freePtr(idx).value === i.U
    })
    val freePhyReg = Mux1H(freeReqOH, io.freePhyReg)
    when(freeReqOH.asUInt.orR) {
      freeList(i) := freePhyReg
    }
  }
  for (i <- 0 until commitWidth) {
    // Why RegNext (from RAT and Rename): for better timing
    // Why we can RegNext: these free registers won't be used in the next cycle,
    // since we set canAllocate only when the current free regs > RenameWidth.
    XSDebug(io.freeReq(i), p"req#$i free physical reg: ${io.freePhyReg(i)}\n")
  }

  tailPtrNext := tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  //
  // allocate new physical registers for instructions at rename stage
  //
  val freeRegCnt = Wire(UInt()) // number of free registers in free list
  io.canAllocate := GatedValidRegNext(freeRegCnt >= RenameWidth.U) // use RegNext for better timing
  XSDebug(p"freeRegCnt: $freeRegCnt\n")

  val freeListVec = Wire(Vec(freeListSize, Vec(RenameWidth, UInt(PhyRegIdxWidth.W))))
  for (i <- 0 until freeListSize) {
    for (j <- 0 until RenameWidth) {
      if (i + j > (freeListSize - 1)) {
        freeListVec(i)(j) := freeList(i + j - freeListSize)
      } else {
        freeListVec(i)(j) := freeList(i + j)
      }
    }
  }

  val phyRegCandidates = Mux1H(headPtrOHVec(0), freeListVec)

  for(i <- 0 until RenameWidth) {
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
    XSDebug(p"req:${io.allocateReq(i)} canAllocate:${io.canAllocate} pdest:${io.allocatePhyReg(i)}\n")
  }
  val doCommit = io.commit.doCommit
  val archAlloc = io.commit.archAlloc
  val numArchAllocate = PopCount(archAlloc)
  val archHeadPtrNew  = archHeadPtr + numArchAllocate
  val archHeadPtrNext = Mux(doCommit, archHeadPtrNew, archHeadPtr)
  archHeadPtr := archHeadPtrNext

  val isWalkAlloc = io.walk && io.doAllocate
  val isNormalAlloc = io.canAllocate && io.doAllocate
  val isAllocate = isWalkAlloc || isNormalAlloc
  val numAllocate = Mux(io.walk, PopCount(io.walkReq), PopCount(io.allocateReq))
  val headPtrAllocate = Mux(lastCycleRedirect, redirectedHeadPtr, headPtr + numAllocate)
  val headPtrOHAllocate = Mux(lastCycleRedirect, redirectedHeadPtrOH, headPtrOHVec(numAllocate))
  freeRegCnt := Mux(isWalkAlloc && !lastCycleRedirect, distanceBetween(tailPtrNext, headPtr) - PopCount(io.walkReq),
                Mux(isNormalAlloc,                     distanceBetween(tailPtrNext, headPtr) - PopCount(io.allocateReq),
                                                       distanceBetween(tailPtrNext, headPtr)))

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  val realDoAllocate = !io.redirect && isAllocate
  headPtr := Mux(realDoAllocate, headPtrAllocate, headPtr)
  headPtrOH := Mux(realDoAllocate, headPtrOHAllocate, headPtrOH)

  XSDebug(p"head:$headPtr tail:$tailPtrNext\n")

  XSError(!isFull(tailPtrNext, archHeadPtr), s"${regType}ArchFreeList should always be full\n")

  val enableFreeListCheck = false
  if (enableFreeListCheck) {
    for (i <- 0 until freeListSize) {
      for (j <- i+1 until freeListSize) {
        XSError(freeList(i) === freeList(j), s"Found same entry in free list! (i=$i j=$j)\n")
      }
    }
  }

  XSPerfAccumulate("utilization", PopCount(io.allocateReq))
  XSPerfAccumulate("allocation_blocked_cycle", !io.canAllocate)
  XSPerfAccumulate("can_alloc_wrong", !io.canAllocate && freeRegCnt >= RenameWidth.U)

  val freeRegCntReg = RegNext(freeRegCnt)
  val perfEvents = Seq(
    ("std_freelist_1_4_valid", freeRegCntReg <  (freeListSize / 4).U                                            ),
    ("std_freelist_2_4_valid", freeRegCntReg >= (freeListSize / 4).U && freeRegCntReg < (freeListSize / 2).U    ),
    ("std_freelist_3_4_valid", freeRegCntReg >= (freeListSize / 2).U && freeRegCntReg < (freeListSize * 3 / 4).U),
    ("std_freelist_4_4_valid", freeRegCntReg >= (freeListSize * 3 / 4).U                                        )
  )

  QueuePerf(size = freeListSize, utilization = freeRegCntReg, full = freeRegCntReg === 0.U)

  generatePerfEvent()
}
