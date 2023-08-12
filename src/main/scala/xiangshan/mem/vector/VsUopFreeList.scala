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

import chipsalliance.rocketchip.config.Parameters
import chisel3.{util, _}
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr

class VsUopFreeList(size: Int, allocWidth: Int, freeWidth: Int, moduleName: String = "")(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle(){
    val accllReq = Vec(allocWidth,Flipped(Decoupled(Bool())))
    val idxValue = Output(Vec(allocWidth,UInt(log2Ceil(size).W)))
    val free = Input(UInt(size.W))
  })

  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until size / freeWidth).map(i => {input(freeWidth * i + rem)})).asUInt
  }

  class UopFreeListPtr extends CircularQueuePtr[UopFreeListPtr](size)
  object UopFreeListPtr {
    def apply(f: Boolean, v: Int): UopFreeListPtr = {
      val ptr = Wire(new UopFreeListPtr)
      ptr.flag := f.B
      ptr.value := v.U
      ptr
    }
  }
  // uopFreeList ptr
  val headPtr = RegInit(UopFreeListPtr(false,0))
  val tailPtr = RegInit(UopFreeListPtr(true,0))

  val freeMask = RegInit(0.U(size.W))
  val freeSelMaskVec = Wire(Vec(freeWidth,UInt(size.W)))
  val freeSelMask = Wire(UInt(size.W))
  val freeListBankBool = Wire(Vec(freeWidth,Bool()))
  val IdxValueVec = Wire(Vec(freeWidth,UInt(log2Ceil(size).W)))

  //FreeList initialize
  val uopFreeList = RegInit(VecInit(Seq.tabulate(size)(i => i.U(log2Up(size).W))))

  for (i <- 0 until allocWidth) {
    io.accllReq(i).ready := distanceBetween(tailPtr,headPtr) >= 2.U
  }

  //idxValue dequeue
  for (i <- 0 until allocWidth) {
    val deqPtr = headPtr + PopCount(io.accllReq.map(_.fire).take(i))
    io.idxValue(i) := uopFreeList(deqPtr.value)
  }
  headPtr := headPtr + PopCount(io.accllReq.map(_.fire))

  //idxValue enqueue
  freeSelMask := freeSelMaskVec.reduce(_|_)
  freeMask := (io.free | freeMask) & ~freeSelMask
  val freeLiskBank = VecInit(Seq.tabulate(freeWidth)(i => getRemBits(freeMask & ~freeSelMask)(i)))
  val freeIdxValueVec = VecInit(Seq.tabulate(freeWidth)(i => {
    val value = PriorityEncoder(freeLiskBank(i))
    Cat(value,i.U(log2Up(freeWidth).W))
  }))

  for (i <- 0 until freeWidth) {
    freeListBankBool(i) := RegNext(freeLiskBank(i).orR)
    IdxValueVec(i)      := RegNext(freeIdxValueVec(i))
    freeSelMaskVec(i) := Mux(freeListBankBool(i),UIntToOH(IdxValueVec(i)),0.U)
    val enqPtr = tailPtr + PopCount(freeListBankBool.take(i))
    uopFreeList(enqPtr.value) := IdxValueVec(i)
  }
  tailPtr := tailPtr + PopCount(freeListBankBool)

}