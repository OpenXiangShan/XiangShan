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

package xiangshan.backend.rename

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FreeListPtr(implicit val p: Parameters) extends CircularQueuePtr[FreeListPtr](
  p => p(XSCoreParamsKey).NRPhyRegs - 32
)

object FreeListPtr {
  def apply(f: Bool, v:UInt)(implicit p: Parameters): FreeListPtr = {
    val ptr = Wire(new FreeListPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class FreeList(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  val io = IO(new Bundle() {
    val redirect = Input(Bool())
    val flush = Input(Bool())

    val req = new Bundle {
      // need to alloc (not actually do the allocation)
      val allocReqs = Vec(RenameWidth, Input(Bool()))
      // response pdest according to alloc
      val pdests = Vec(RenameWidth, Output(UInt(PhyRegIdxWidth.W)))
      // alloc new phy regs// freelist can alloc
      val canAlloc = Output(Bool())
      // actually do the allocation
      val doAlloc = Input(Bool())
    }

    // do checkpoints
    // val cpReqs = Vec(RenameWidth, Flipped(ValidIO(new BrqPtr)))
    val walk = Flipped(ValidIO(UInt(log2Up(CommitWidth + 1).W)))

    // dealloc phy regs
    val deallocReqs = Input(Vec(CommitWidth, Bool()))
    val deallocPregs = Input(Vec(CommitWidth, UInt(PhyRegIdxWidth.W)))
  })

  val FL_SIZE = NRPhyRegs - 32

  // init: [32, 127]
  val freeList = RegInit(VecInit(Seq.tabulate(FL_SIZE)(i => (i+32).U(PhyRegIdxWidth.W))))
  val headPtr = RegInit(FreeListPtr(false.B, 0.U))
  val tailPtr = RegInit(FreeListPtr(true.B, 0.U))

  // val checkPoints = Reg(Vec(BrqSize, new FreeListPtr()))

  // dealloc: commited instructions's 'old_pdest' enqueue
  for(i <- 0 until CommitWidth){
    val offset = if(i == 0) 0.U else PopCount(io.deallocReqs.take(i))
    val ptr = tailPtr + offset
    val idx = ptr.value
    when(io.deallocReqs(i)){
      freeList(idx) := io.deallocPregs(i)
      XSDebug(p"dealloc preg: ${io.deallocPregs(i)}\n")
    }
  }
  val tailPtrNext = tailPtr + PopCount(io.deallocReqs)
  tailPtr := tailPtrNext

  // allocate new pregs to rename instructions

  // number of free regs in freelist
  val freeRegs = Wire(UInt())
  // use RegNext for better timing
  io.req.canAlloc := RegNext(freeRegs >= RenameWidth.U)
  XSDebug(p"free regs: $freeRegs\n")

  val allocatePtrs = (0 until RenameWidth).map(i => headPtr + i.U)
  val allocatePdests = VecInit(allocatePtrs.map(ptr => freeList(ptr.value)))

  for(i <- 0 until RenameWidth){
    io.req.pdests(i) := allocatePdests(/*if (i == 0) 0.U else */PopCount(io.req.allocReqs.take(i)))
    // when(io.cpReqs(i).valid){
    //   checkPoints(io.cpReqs(i).bits.value) := newHeadPtrs(i+1)
    //   XSDebug(p"do checkPt at BrqIdx=${io.cpReqs(i).bits.value} ${newHeadPtrs(i+1)}\n")
    // }
    XSDebug(p"req:${io.req.allocReqs(i)} canAlloc:${io.req.canAlloc} pdest:${io.req.pdests(i)}\n")
  }
  val headPtrAllocate = headPtr + PopCount(io.req.allocReqs)
  val headPtrNext = Mux(io.req.canAlloc && io.req.doAlloc, headPtrAllocate, headPtr)
  freeRegs := distanceBetween(tailPtr, headPtrNext)

  // priority: (1) exception and flushPipe; (2) walking; (3) mis-prediction; (4) normal dequeue
  headPtr := Mux(io.flush,
    FreeListPtr(!tailPtrNext.flag, tailPtrNext.value),
    Mux(io.walk.valid,
      headPtr - io.walk.bits,
      Mux(io.redirect, headPtr, headPtrNext))
  )

  XSDebug(p"head:$headPtr tail:$tailPtr\n")


  val enableFreelistCheck = false
  if (enableFreelistCheck) {
    for (i <- 0 until FL_SIZE) {
      for (j <- i+1 until FL_SIZE) {
        XSError(freeList(i) === freeList(j), s"Found same entry in freelist! (i=$i j=$j)")
      }
    }
  }

  XSPerfAccumulate("utilization", freeRegs)
  XSPerfAccumulate("allocation_blocked", !io.req.canAlloc)
  XSPerfAccumulate("can_alloc_wrong", !io.req.canAlloc && freeRegs >= RenameWidth.U)
}
