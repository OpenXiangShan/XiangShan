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
import xiangshan.backend.rename.SnapshotGenerator
import utils._
import utility._


abstract class BaseFreeList(size: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val walk = Input(Bool())

    val allocateReq = Input(Vec(RenameWidth, Bool()))
    val walkReq = Input(Vec(CommitWidth, Bool()))
    val allocatePhyReg = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val canAllocate = Output(Bool())
    val doAllocate = Input(Bool())

    val freeReq = Input(Vec(CommitWidth, Bool()))
    val freePhyReg = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))

    val commit = Input(new RobCommitIO)

    val snpt = Input(new SnapshotPort)

    val debug_rat = if(backendParams.debugEn) Some(Vec(32, Input(UInt(PhyRegIdxWidth.W)))) else None
  })

  class FreeListPtr extends CircularQueuePtr[FreeListPtr](size)

  object FreeListPtr {
    def apply(f: Boolean, v: Int): FreeListPtr = {
      val ptr = Wire(new FreeListPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }

  val lastCycleRedirect = RegNext(io.redirect, false.B)
  val lastCycleSnpt = RegNext(io.snpt, 0.U.asTypeOf(io.snpt))

  val headPtr = RegInit(FreeListPtr(false, 0))
  val headPtrOH = RegInit(1.U(size.W))
  val archHeadPtr = RegInit(FreeListPtr(false, 0))
  XSError(headPtr.toOH =/= headPtrOH, p"wrong one-hot reg between $headPtr and $headPtrOH")
  val headPtrOHShift = CircularShift(headPtrOH)
  // may shift [0, RenameWidth] steps
  val headPtrOHVec = VecInit.tabulate(RenameWidth + 1)(headPtrOHShift.left)

  val snapshots = SnapshotGenerator(headPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect)

  val redirectedHeadPtr = Mux(
    lastCycleSnpt.useSnpt,
    snapshots(lastCycleSnpt.snptSelect) + PopCount(io.walkReq),
    archHeadPtr + PopCount(io.walkReq)
  )
  val redirectedHeadPtrOH = Mux(
    lastCycleSnpt.useSnpt,
    (snapshots(lastCycleSnpt.snptSelect) + PopCount(io.walkReq)).toOH,
    (archHeadPtr + PopCount(io.walkReq)).toOH
  )
}
