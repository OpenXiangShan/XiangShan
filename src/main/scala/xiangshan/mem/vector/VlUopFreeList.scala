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

class VlUopFreeList(size: Int, allocWidth: Int, maxIdxNum: Int, freeWidth: Int, moduleName: String = "")(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new Bundle(){
    val accllReq = Vec(allocWidth,Flipped(Decoupled(UInt(log2Up(maxIdxNum + 1).W))))
    val idxValue = Output(Vec(allocWidth,Vec(maxIdxNum,UInt(log2Ceil(size).W))))
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
    io.accllReq(i).ready := distanceBetween(tailPtr,headPtr) >= 16.U //FIXME:maybe optimized
  }

  //idxValue dequeue
  for (i <- 0 until allocWidth) {
    io.idxValue := DontCare
    when (io.accllReq(i).fire) {
      for (j <- 0 until maxIdxNum) {
        when (j.U < io.accllReq(i).bits) {
          val deqPtr = Wire(new UopFreeListPtr)
          when (j.U === 1.U && io.accllReq(0).fire) {
            deqPtr := headPtr + io.accllReq(0).bits + j.U
          }.otherwise {
            deqPtr := headPtr + j.U
          }
          io.idxValue(i)(j) := uopFreeList(deqPtr.value)
        }
      }
    }
  }

  when (io.accllReq(0).fire && io.accllReq(1).fire) {
    headPtr := headPtr +  io.accllReq(0).bits + io.accllReq(1).bits
  }.otherwise {
    for (i <- 0 until allocWidth) {
      when (io.accllReq(i).fire) {
        headPtr := headPtr + io.accllReq(i).bits
      }
    }
  }

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