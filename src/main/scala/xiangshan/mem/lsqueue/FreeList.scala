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
package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class FreeList(size: Int, allocWidth: Int, freeWidth: Int, enablePreAlloc: Boolean = false, moduleName: String = "")(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val allocateReq = Input(Vec(allocWidth, Bool()))
    val allocateSlot = Output(Vec(allocWidth, UInt()))
    val canAllocate = Output(Vec(allocWidth, Bool()))
    val doAllocate = Input(Vec(allocWidth, Bool()))

    val free = Input(UInt(size.W))

    val validCount = Output(UInt())
    val empty = Output(Bool())
  })

  println(s"FreeList: $moduleName, size " + size)

  val freeList = RegInit(VecInit(
    // originally {0, 1, ..., size - 1} are free.
    Seq.tabulate(size)(i => i.U(log2Up(size).W))
  ))

  class FreeListPtr extends CircularQueuePtr[FreeListPtr](size)
  object FreeListPtr {
    def apply(f: Boolean, v: Int): FreeListPtr = {
      val ptr = Wire(new FreeListPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }

  val headPtr  = RegInit(FreeListPtr(false, 0))
  val headPtrNext = Wire(new FreeListPtr)
  val tailPtr = RegInit(FreeListPtr(true, 0))
  val tailPtrNext = Wire(new FreeListPtr)

  // legality check
  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until size / freeWidth).map(i => { input(freeWidth * i + rem) })).asUInt
  }

  // free logic
  val freeMask = RegInit(0.U(size.W))
  val freeSelMask = Wire(UInt(size.W))
  val freeSelMaskVec = Wire(Vec(freeWidth, UInt(size.W)))

  // update freeMask
  require((size % freeWidth) == 0)
  freeSelMask := freeSelMaskVec.reduce(_|_)
  freeMask := (io.free | freeMask) & ~freeSelMask

  val remFreeSelMaskVec = VecInit(Seq.tabulate(freeWidth)(rem => getRemBits((freeMask & ~freeSelMask))(rem)))
  val remFreeSelIndexOHVec = VecInit(Seq.tabulate(freeWidth)(fport => {
    val highIndexOH = PriorityEncoderOH(remFreeSelMaskVec(fport))
    val freeIndexOHVec = Wire(Vec(size, Bool()))
    freeIndexOHVec.foreach(e => e := false.B)
    for (i <- 0 until size / freeWidth) {
      freeIndexOHVec(i * freeWidth + fport) := highIndexOH(i)
    }
    freeIndexOHVec.asUInt
  }))

  val freeReq = GatedRegNext(VecInit(remFreeSelMaskVec.map(_.asUInt.orR)))
  val freeSlotOH = GatedRegNext(remFreeSelIndexOHVec)
  val doFree = freeReq.asUInt.orR

  for (i <- 0 until freeWidth) {
    val offset = PopCount(freeReq.take(i))
    val enqPtr = tailPtr + offset

    when (freeReq(i)) {
      freeList(enqPtr.value) := OHToUInt(freeSlotOH(i))
    }

    freeSelMaskVec(i) := Mux(freeReq(i), freeSlotOH(i), 0.U)
  }

  tailPtrNext := tailPtr + PopCount(freeReq)
  tailPtr := Mux(doFree, tailPtrNext, tailPtr)

  // allocate
  val doAllocate = io.doAllocate.asUInt.orR
  val numAllocate = PopCount(io.doAllocate)
  val freeSlotCnt = RegInit(size.U(log2Up(size + 1).W))
  val allocateWindow = Reg(Vec(allocWidth * 2, UInt(log2Up(size).W)))

  if (enablePreAlloc) {
    // use allocWidth * 2 request to align timing of allocate.
    /*
    *  initial status: allocateNumReg = 0, headPtr = 0, allocateWindow[0, 2N - 1] = freeList[headPtr, headPtr + 2N - 1].
    *
    *  T0: allocate N entries, allocateWindow[0, N - 1] was be allocated, allocateNumReg = 0, headPtr = 0.
    *  T1: allocate N entries, allocatedWindow[N, 2N - 1] was be allocated,  allocateNumReg = N, headPtr = N.
    *  T2: allocatedWindow[0, 2N - 1] = freeList[headPtr, headPtr + 2N - 1]
    * */

    for (i <- 0 until allocWidth * 2) {
      allocateWindow(i) := freeList((headPtr + i.U).value)
    }
  }

  for (i <- 0 until allocWidth) {
    val offset = PopCount(io.allocateReq.take(i))

    if (enablePreAlloc) {
      val preAllocateNum = numAllocate +& offset
      val preAllocatePtr = headPtr + preAllocateNum
      val allocateNumReg = RegNext(numAllocate)
      io.canAllocate(i) := RegEnable(isBefore(preAllocatePtr, tailPtr), enablePreAlloc.B)
      io.allocateSlot(i) := allocateWindow(allocateNumReg +& i.U)
    } else {
      val deqPtr = headPtr + offset
      io.canAllocate(i) := isBefore(deqPtr, tailPtr)
      io.allocateSlot(i) := freeList(deqPtr.value)
    }

  }

  headPtrNext := headPtr + numAllocate
  headPtr := Mux(doAllocate, headPtrNext, headPtr)
  freeSlotCnt := freeSlotCnt + PopCount(freeReq) - numAllocate

  io.empty := freeSlotCnt === 0.U
  io.validCount := size.U - freeSlotCnt

  XSPerfAccumulate("empty", io.empty)
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("empty", io.empty)
  )
  generatePerfEvent()

  // unique check
  val enableFreeListCheck = false
  if (enableFreeListCheck) {
    val differentFlag = tailPtr.flag ^ headPtr.flag
    val headMask = UIntToMask(headPtr.value, size)
    val tailMask = UIntToMask(tailPtr.value, size)
    val validMask1 = Mux(differentFlag, ~tailMask, tailMask ^ headMask)
    val validMask2 = Mux(differentFlag, headMask, 0.U(size.W))
    val validMask = ~(validMask1 | validMask2)
    for (i <- 0 until size) {
      for (j <- i+1 until size) {
        if (i != j) {
          XSError(validMask(i) && validMask(j) && freeList(i) === freeList(j),s"Found same entry in free list! (i=$i j=$j)\n")
        }
      }
    }
  }

  // end
}

class BitMapFreeList(size: Int, allocWidth: Int)(implicit p: Parameters) extends XSModule {
  require(allocWidth > 0 && size >= allocWidth)
  private val indexWidth = log2Up(size)
  private val countWidth = log2Ceil(allocWidth + 1)

  val io = IO(new Bundle {
    val freeBitmap = Input(UInt(size.W))
    val allocateCount = Input(UInt(countWidth.W))
    val allocateSlot = Output(Vec(allocWidth, UInt(indexWidth.W)))
    val canAllocate = Output(Vec(allocWidth, Bool()))
  })

  val candidates = RegInit(VecInit(Seq.tabulate(allocWidth)(index => index.U(indexWidth.W))))
  val candidateValid = RegInit(((BigInt(1) << allocWidth) - 1).U(allocWidth.W))
  val candidateMask = (0 until allocWidth).map { rank =>
    Mux(candidateValid(rank), UIntToOH(candidates(rank), size), 0.U(size.W))
  }.reduce(_ | _)
  val refillBitmap = io.freeBitmap & ~candidateMask

  val prefixCounts = Seq.fill(log2Ceil(size) + 1)(Wire(Vec(size, UInt(countWidth.W))))
  for (entry <- 0 until size) {
    prefixCounts.head(entry) := refillBitmap(entry)
  }
  for (stage <- 0 until log2Ceil(size)) {
    val stride = 1 << stage
    for (entry <- 0 until size) {
      if (entry < stride) {
        prefixCounts(stage + 1)(entry) := prefixCounts(stage)(entry)
      } else {
        val sum = prefixCounts(stage)(entry) +& prefixCounts(stage)(entry - stride)
        prefixCounts(stage + 1)(entry) := Mux(sum > allocWidth.U, allocWidth.U, sum)
      }
    }
  }
  val refillSlots = VecInit((0 until allocWidth).map { rank =>
    val selected = VecInit((0 until size).map { entry =>
      val precedingCount = if (entry == 0) 0.U else prefixCounts.last(entry - 1)
      refillBitmap(entry) && precedingCount === rank.U
    })
    OHToUInt(selected)
  })

  val keptValid = candidateValid >> io.allocateCount
  val refillValid = UIntToMask(prefixCounts.last.last, allocWidth)
  // Decode the contiguous valid mask so refill indices are constants at elaboration time.
  val keepCountIs = (0 to allocWidth).map { kept =>
    keptValid === ((BigInt(1) << kept) - 1).U(allocWidth.W)
  }
  val nextValid = Wire(Vec(allocWidth, Bool()))
  for (rank <- 0 until allocWidth) {
    val choices = (0 to allocWidth).map { kept =>
      val slot = if (rank < kept) candidates(io.allocateCount + rank.U) else refillSlots(rank - kept)
      val valid = if (rank < kept) true.B else refillValid(rank - kept)
      (keepCountIs(kept), slot, valid)
    }
    candidates(rank) := Mux1H(choices.map { case (select, slot, _) => select -> slot })
    nextValid(rank) := Mux1H(choices.map { case (select, _, valid) => select -> valid })
    io.allocateSlot(rank) := candidates(rank)
    io.canAllocate(rank) := candidateValid(rank)
  }
  candidateValid := nextValid.asUInt

  XSError(((candidateValid >> 1) & ~candidateValid).orR, "LRQ candidate valid mask must be contiguous from bit 0")
  XSError(io.allocateCount > PopCount(candidateValid), "LRQ allocation exceeds prepared candidates")
  XSError((candidateMask & ~io.freeBitmap).orR, "LRQ candidate refers to an allocated entry")
  XSError(PopCount(candidateMask) =/= PopCount(candidateValid), "LRQ candidates are duplicated or out of range")
}
