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

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan._

class PhrPtr(entries: Int)(implicit p: Parameters) extends CircularQueuePtr[PhrPtr](entries) {}

class PhrInput(implicit p: Parameters) extends XSBundle {
  // pc or target
  val vaddr     = if (Bpu2TakenEnable) Some(Vec(2, UInt(VAddrBits.W))) else Some(UInt(VAddrBits.W))

  // update phr based on the result of uBTB
  val updateEnable = if (Bpu2TakenEnable) Some(Vec(2, Bool())) else Some(Bool())
  val updateFtqPtr = if (Bpu2TakenEnable) Some(Vec(2, new FtqPtr)) else Some(new FtqPtr)

  // BPU pipeline control signals
  val s2Fire = Bool()
  val s3Fire = Bool()

  val s2OverrideValid = Bool() // TODO: remove it in kmhv3
  val s3OverrideValid = Bool()

  val recoveryEnable = Bool()
  val recoveryFtqPtr = new FtqPtr

  val readEnable = Bool()
  val readFtqPtr = new FtqPtr
}

class PhrOutput(HighBitsLen: Int, LowBitsLen: Int)(implicit p: Parameters) extends XSBundle {
  val phr = UInt((HighBitsLen + LowBitsLen).W)
  val phrReadResp = UInt((HighBitsLen + LowBitsLen).W)
}

// Path History Register
class Phr(HighBitsLen: Int, LowBitsLen: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val in  = Input(new PhrInput)
    val out = Output(new PhrOutput(HighBitsLen, LowBitsLen))
  })
  
  def PhrLen = HighBitsLen + LowBitsLen

  def PhrShiftLen = HighBitsLen + FtqSize + 4

  val phr = RegInit(0.U(PhrLen.W))

  val phrReadResp = Wire(UInt(PhrLen.W))

  // ues a circular queue to save the MSB of low bits
  val phrHighBits = RegInit(VecInit.fill(PhrShiftLen)(false.B))

  val phrPtr = Reg(new PhrPtr(PhrShiftLen))

  // save phrPtr for recovery of redirect
  val phrPtrVec = RegInit(VecInit.fill(FtqSize)(new PhrPtr(PhrShiftLen)))

  // save the part of PHR that participates in the XOR computation
  val phrLowBitsVec = RegInit(VecInit.fill(FtqSize)(0.U(LowBitsLen.W)))

  val phrLowBits = phr(LowBitsLen - 1, 0)

  // save phr for override
  val s2Phr = RegEnable(phr, io.in.s2Fire)
  val s3Phr = RegEnable(s2Phr, io.in.s3Fire)

  val s2PhrPtr = RegEnable(phrPtr, io.in.s2Fire)
  val s3PhrPtr = RegEnable(s2PhrPtr, io.in.s3Fire)

  val recoveryIndex = io.in.recoveryFtqPtr.value

  val recoveryPhrPtr = phrPtrVec(recoveryIndex)

  val oldPhrHighBits = (Cat(phrHighBits.asUInt, phrHighBits.asUInt) >> (recoveryPhrPtr.value + 1.U))(HighBitsLen, 0)

  val oldPhr = Cat(oldPhrHighBits, phrLowBitsVec(recoveryIndex))

  if (Bpu2TakenEnable) {
    val updateEnable = io.in.updateEnable.get.asInstanceOf[Vec[Bool]]
    val updateIndex  = io.in.updateFtqPtr.get.asInstanceOf[Vec[FtqPtr]].map(ptr => ptr.value)

    val oneTakenBranch = updateEnable(0) && !updateEnable(1)
    val twoTakenBranch = updateEnable(0) && updateEnable(1)

    val vaddr = io.in.vaddr.get.asInstanceOf[Vec[UInt]]

    when(io.in.recoveryEnable) {
      phr := oldPhr
      phrPtr := recoveryPhrPtr
    }.elsewhen(io.in.s3OverrideValid) {
      phr := s3Phr
      phrPtr := s3PhrPtr
    }.elsewhen(io.in.s2OverrideValid) { // TODO: remove it in kmhv3
      phr := s2Phr
      phrPtr := s2PhrPtr
    }.elsewhen(twoTakenBranch) {
      val highBits = Cat(phr(PhrLen - 3, LowBitsLen - 1), phr(LowBitsLen - 2) ^ vaddr(0)(LowBitsLen))
      val lowBitsStep1 = (phr(LowBitsLen - 1, 0) << 1).asUInt ^ vaddr(0)(LowBitsLen, 1)
      val lowBitsStep2 = (lowBitsStep1 << 1).asUInt  ^ vaddr(1)(LowBitsLen, 1)

      phr := Cat(highBits, lowBitsStep2)
      phrPtr := phrPtr - 2.U

      // save ptr of high bits
      phrPtrVec(updateIndex(0)) := phrPtr
      phrPtrVec(updateIndex(1)) := phrPtr - 1.U

      // save old low bits
      phrLowBitsVec(updateIndex(0)) := phrLowBits
      phrLowBitsVec(updateIndex(1)) := lowBitsStep1
    }.elsewhen(oneTakenBranch) {
      val highBits = phr(HighBitsLen - 2, LowBitsLen - 1)
      val lowBits = (phr(LowBitsLen - 1, 0) << 1).asUInt ^ vaddr(0)(LowBitsLen, 1)

      phr := Cat(highBits, lowBits)
      phrPtr := phrPtr - 1.U

      phrPtrVec(updateIndex(0)) := phrPtr
      phrLowBitsVec(updateIndex(0)) := phrLowBits
    }
  } else { // Bpu2TakenEnable = false
    val updateEnable = io.in.updateEnable.get.asInstanceOf[Bool]
    val updateIndex = io.in.updateFtqPtr.get.asInstanceOf[FtqPtr].value

    val vaddr = io.in.vaddr.get.asUInt

    when(io.in.recoveryEnable) {
      phr := oldPhr
      phrPtr.value := recoveryPhrPtr
    }.elsewhen(io.in.s3OverrideValid) {
      phr := s3Phr
      phrPtr.value := s3PhrPtr
    }.elsewhen(io.in.s2OverrideValid) { // TODO: remove it in kmhv3
      phr := s2Phr
      phrPtr.value := s2PhrPtr
    }.elsewhen(updateEnable) {
      val highBits = phr(HighBitsLen - 2, LowBitsLen - 1)
      val lowBits = (phr(LowBitsLen - 1, 0) << 1).asUInt ^ vaddr(LowBitsLen, 1)

      phr := Cat(highBits, lowBits)
      phrPtr := phrPtr - 1.U

      phrPtrVec(updateIndex) := phrPtr
      phrLowBitsVec(updateIndex) := phrLowBits
    }
  }

  val readPtr = io.in.readFtqPtr
  val readPhrHighBits = (Cat(phrHighBits.asUInt, phrHighBits.asUInt) >> (readPtr.value + 1.U))(HighBitsLen, 0)

  when (io.in.readEnable) {
    phrReadResp := Cat(readPhrHighBits, phrLowBitsVec(readPtr.value))
  }

  io.out.phr := phr
  io.out.phrReadResp := phrReadResp
}
