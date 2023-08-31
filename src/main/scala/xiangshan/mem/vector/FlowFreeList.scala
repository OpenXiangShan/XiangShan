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

class FlowFreeList (size: Int, freeWidth: Int, maxIdxNum: Int, moduleName: String = "")(implicit p: Parameters)
  extends VLSUModule {
  val io = IO(new Bundle(){
    val allocReq = Flipped(Decoupled(UInt(log2Up(maxIdxNum + 1).W)))
    val idxValue = Output(Vec(maxIdxNum,UInt(log2Ceil(size).W)))
    val free = Input(UInt(size.W))
  })

  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until size / freeWidth).map(i => {
      input(freeWidth * i + rem)
    })).asUInt
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

  // flowFreeList ptr
  val headPtr = RegInit(UopFreeListPtr(false, 0))
  val tailPtr = RegInit(UopFreeListPtr(true, 0))

  val freeList = RegInit(0.U(size.W))
  val freeSelMaskVec = Wire(Vec(freeWidth, UInt(size.W)))
  val freeSelMask = Wire(UInt(size.W))
  val freeListBankBool = Wire(Vec(freeWidth, Bool()))
  val IdxValueVec = Wire(Vec(freeWidth, UInt(log2Ceil(size).W)))

  val flowFreeList = RegInit(VecInit(Seq.tabulate(size)(i => i.U(log2Up(size).W))))

  io.allocReq.ready := distanceBetween(tailPtr,headPtr) >= 16.U
  io.idxValue := DontCare
  when (io.allocReq.fire) {
    for (i <- 0 until maxIdxNum) {
      when (i.U < io.allocReq.bits) {
        val deqPtr = headPtr + i.U
        io.idxValue(i) := flowFreeList(deqPtr.value)
      }
    }
  }
  headPtr := headPtr + io.allocReq.bits

  freeSelMask := freeSelMaskVec.reduce(_|_)
  freeList := (io.free | freeList) & ~freeSelMask

  val freeListBank = VecInit(Seq.tabulate(freeWidth)(i => getRemBits(freeList & ~freeSelMask)(i)))
  val freeIdxValueVec = VecInit(Seq.tabulate(freeWidth)(i => {
    val value = PriorityEncoder(freeListBank(i))
    Cat(value,i.U(log2Up(freeWidth).W))
  }))

  for (i <- 0 until freeWidth) {
    freeListBankBool(i) := RegNext(freeListBank(i).orR)
    IdxValueVec(i) := RegNext(freeIdxValueVec(i))
    freeSelMaskVec(i) := Mux(freeListBankBool(i),UIntToOH(IdxValueVec(i)),0.U)
    val enqPtr = tailPtr + PopCount(freeListBankBool.take(i))
    flowFreeList(enqPtr.value) := IdxValueVec(i)
  }
  tailPtr := tailPtr + PopCount(freeListBankBool)
}