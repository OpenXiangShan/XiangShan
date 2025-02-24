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
import utils._


class MEFreeList(size: Int)(implicit p: Parameters) extends BaseFreeList(size) with HasPerfEvents {
  val freeList = RegInit(VecInit(
    // originally {1, 2, ..., size - 1} are free. Register 0-31 are mapped to x0.
    Seq.tabulate(size - 1)(i => (i + 1).U(PhyRegIdxWidth.W)) :+ 0.U(PhyRegIdxWidth.W)))

  // head and tail pointer
  val headPtr = RegInit(FreeListPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val headPtrOHShift = ResizeCircularShift(headPtrOH)
  // may shift [0, RenameWidth] steps
  val headPtrOHVec = VecInit.tabulate(RenameWidth + 1)(i =>
    headPtrOHShift.leftWithWidth(i, io.psize)
  )
  val tailPtr = RegInit(FreeListPtr(false, size - 1))

  val doRename = io.canAllocate && io.doAllocate && !io.redirect && !io.walk
  val doResize = io.psize =/= headPtr.psize.bits || io.psize =/= tailPtr.psize.bits

  /**
    * Allocation: from freelist (same as StdFreelist)
    */
  val phyRegCandidates = VecInit(headPtrOHVec.map(sel => Mux1H(sel, freeList)))
  for (i <- 0 until RenameWidth) {
    // enqueue instr, is move elimination
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
  }
  // update head pointer
  val numAllocate = PopCount(io.allocateReq)
  val headPtrNext = headPtr + numAllocate
  headPtr := Mux(doResize, headPtr.resize(0.U, io.psize), Mux(doRename, headPtrNext, headPtr))

  headPtrOH := Mux(doResize, 1.U, Mux(doRename, headPtrOHVec(numAllocate), headPtrOH))


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
  tailPtr := Mux(doResize, tailPtr.resize(io.psize - 1.U, io.psize), tailPtrNext)

  val freeRegCnt = Mux(doRename, distanceBetween(tailPtrNext, headPtrNext), distanceBetween(tailPtrNext, headPtr))
  val freeRegCntReg = RegNext(freeRegCnt)
  io.canAllocate := freeRegCntReg >= RenameWidth.U

  val perfEvents = Seq(
    ("me_freelist_1_4_valid", freeRegCntReg <  (size / 4).U                                     ),
    ("me_freelist_2_4_valid", freeRegCntReg >= (size / 4).U && freeRegCntReg <= (size / 2).U    ),
    ("me_freelist_3_4_valid", freeRegCntReg >= (size / 2).U && freeRegCntReg <= (size * 3 / 4).U),
    ("me_freelist_4_4_valid", freeRegCntReg >= (size * 3 / 4).U                                 ),
  )
  generatePerfEvent()
}
