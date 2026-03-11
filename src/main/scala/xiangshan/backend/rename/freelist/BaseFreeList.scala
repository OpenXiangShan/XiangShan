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


abstract class BaseFreeList(
  size: Int,
  commitWidth: Int,
  numLogicRegs:Int = 32,
)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val walk = Input(Bool())

    val allocateReq = Input(Vec(RenameWidth, Bool()))
    val walkReq = Input(Vec(commitWidth, Bool()))
    val allocatePhyReg = Output(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
    val canAllocate = Output(Bool())
    val doAllocate = Input(Bool())

    val freeReq = Input(Vec(commitWidth, Bool()))
    val freePhyReg = Input(Vec(commitWidth, UInt(PhyRegIdxWidth.W)))

    val commit = Input(new FreeListCommitBundle(commitWidth))

    val snpt = Input(new SnapshotPort)

    val debug_rat = if(backendParams.debugEn) Some(Vec(numLogicRegs, Input(UInt(PhyRegIdxWidth.W)))) else None
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

  val lastCycleRedirect = RegNext(RegNext(io.redirect))
  val lastCycleSnpt = RegNext(RegNext(io.snpt, 0.U.asTypeOf(io.snpt)))

  val headPtrVec = RegInit(VecInit(Seq.tabulate(RenameWidth)(i => FreeListPtr(false, i))))

  val headPtr = headPtrVec(0)

  val archHeadPtr = RegInit(FreeListPtr(false, 0))

  val snapshots = SnapshotGenerator(headPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect, io.snpt.flushVec)

  val redirectedHeadPtr = Mux(
    lastCycleSnpt.useSnpt,
    snapshots(lastCycleSnpt.snptSelect) + PopCount(io.walkReq),
    archHeadPtr + PopCount(io.walkReq)
  )
}

class FreeListCommitBundle(commitWidth: Int) extends Bundle {
  val doCommit = Bool()
  val archAlloc = Vec(commitWidth, Bool())
}
