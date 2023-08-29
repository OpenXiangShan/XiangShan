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
import utility._


class MEFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {
  val freeList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W)))

  val tailPtr = RegInit(FreeListPtr(false, size - 1))

  val doWalkRename = io.walk && io.doAllocate && !io.redirect
  val doNormalRename = io.canAllocate && io.doAllocate && !io.redirect
  val doRename = doWalkRename || doNormalRename
  val doCommit = io.commit.isCommit

  /**
    * Allocation: from freelist (same as StdFreelist)
    */
  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList)))
  for (i <- 0 until RenameWidth) {
    // enqueue instr, is move elimination
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
  }
  // update arch head pointer
  val archAlloc = io.commit.commitValid zip io.commit.info map {
    case (valid, info) => valid && info.rfWen && !info.isMove && info.ldest =/= 0.U
  }
  val numArchAllocate = PopCount(archAlloc)
  val archHeadPtrNew  = archHeadPtr + numArchAllocate
  val archHeadPtrNext = Mux(doCommit, archHeadPtrNew, archHeadPtr)
  archHeadPtr := archHeadPtrNext

  // update head pointer
  val numAllocate = Mux(io.walk, PopCount(io.walkReq), PopCount(io.allocateReq))
  val headPtrNew   = Mux(lastCycleRedirect, redirectedHeadPtr, headPtr + numAllocate)
  val headPtrOHNew = Mux(lastCycleRedirect, redirectedHeadPtrOH, headPtrOHVec(numAllocate))
  val headPtrNext   = Mux(doRename, headPtrNew, headPtr)
  val headPtrOHNext = Mux(doRename, headPtrOHNew, headPtrOH)
  headPtr   := headPtrNext
  headPtrOH := headPtrOHNext

  /**
    * Deallocation: when refCounter becomes zero, the register can be released to freelist
    */
  for (i <- 0 until CommitWidth) {
    when (io.freeReq(i)) {
      val freePtr = tailPtr + PopCount(io.freeReq.take(i))
      freeList(freePtr.value) := io.freePhyReg(i)
    }
  }

  // update tail pointer
  val tailPtrNext = tailPtr + PopCount(io.freeReq)
  tailPtr := tailPtrNext

  val freeRegCnt = Mux(doWalkRename && !lastCycleRedirect, distanceBetween(tailPtrNext, headPtr) - PopCount(io.walkReq),
                   Mux(doNormalRename,                     distanceBetween(tailPtrNext, headPtr) - PopCount(io.allocateReq),
                                                           distanceBetween(tailPtrNext, headPtr)))
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= RenameWidth.U

  val debugArchHeadPtr = RegNext(RegNext(archHeadPtr, FreeListPtr(false, 0)), FreeListPtr(false, 0)) // two-cycle delay from refCounter
  val debugArchRAT = RegNext(RegNext(io.debug_rat, VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W)))), VecInit(Seq.fill(32)(0.U(PhyRegIdxWidth.W))))
  val debugUniqPR = Seq.tabulate(32)(i => i match {
    case 0 => true.B
    case _ => !debugArchRAT.take(i).map(_ === debugArchRAT(i)).reduce(_ || _)
  })
  XSError(distanceBetween(tailPtr, debugArchHeadPtr) +& PopCount(debugUniqPR) =/= NRPhyRegs.U, "Integer physical register should be in either arch RAT or arch free list\n")

  val perfEvents = Seq(
    ("me_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                     ),
    ("me_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg <= (size / 2).U    ),
    ("me_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg <= (size * 3 / 4).U),
    ("me_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                 ),
  )
  generatePerfEvent()
}
