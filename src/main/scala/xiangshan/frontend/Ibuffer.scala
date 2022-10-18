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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.ExceptionNO._

class IbufPtr(implicit p: Parameters) extends CircularQueuePtr[IbufPtr](
  p => p(XSCoreParamsKey).IBufSize
){
}

class IBufferIO(implicit p: Parameters) extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchToIBuffer))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val full = Output(Bool())
}

class Ibuffer(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new IBufferIO)

  class IBufEntry(implicit p: Parameters) extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val foldpc = UInt(MemPredPCWidth.W)
    val pd = new PreDecodeInfo
    val pred_taken = Bool()
    val ftqPtr = new FtqPtr
    val ftqOffset = UInt(log2Ceil(PredictWidth).W)
    val ipf = Bool()
    val acf = Bool()
    val crossPageIPFFix = Bool()
    val triggered = new TriggerCf
  }

  for(out <- io.out) {
    out.bits.intrVec := DontCare
  }

  val ibuf = Module(new SyncDataModuleTemplate(new IBufEntry, IBufSize, DecodeWidth, PredictWidth))
  ibuf.io.wdata.map(w => dontTouch(w.ftqOffset))
  val head_vec = RegInit(VecInit((0 until DecodeWidth).map(_.U.asTypeOf(new IbufPtr))))
  val tail_vec = RegInit(VecInit((0 until PredictWidth).map(_.U.asTypeOf(new IbufPtr))))
  val head_ptr = head_vec(0)
  val tail_ptr = tail_vec(0)

  val validEntries = distanceBetween(tail_ptr, head_ptr)
  val allowEnq = RegInit(true.B)

  val numEnq = Mux(io.in.fire, PopCount(io.in.bits.valid), 0.U)
  val numTryDeq = Mux(validEntries >= DecodeWidth.U, DecodeWidth.U, validEntries)
  val numDeq = PopCount(io.out.map(_.fire))

  val numAfterEnq = validEntries +& numEnq
  val nextValidEntries = Mux(io.out(0).ready, numAfterEnq - numTryDeq, numAfterEnq)
  allowEnq := (IBufSize - PredictWidth).U >= nextValidEntries

  // Enque
  io.in.ready := allowEnq

  val offset = Wire(Vec(PredictWidth, UInt(log2Up(PredictWidth).W)))
  for(i <- 0 until PredictWidth) {
    if (i == 0) {
      offset(i) := 0.U
    } else {
      offset(i) := PopCount(io.in.bits.valid(i-1, 0))
    }
  }

  for (i <- 0 until PredictWidth) {
    val inWire = Wire(new IBufEntry)
    inWire.inst   := io.in.bits.instrs(i)
    inWire.pc     := io.in.bits.pc(i)
    inWire.pd     := io.in.bits.pd(i)
    inWire.foldpc := io.in.bits.foldpc(i)
    inWire.pred_taken := io.in.bits.ftqOffset(i).valid
    inWire.ftqPtr := io.in.bits.ftqPtr
    inWire.ftqOffset := io.in.bits.ftqOffset(i).bits
    inWire.ipf := io.in.bits.ipf(i)
    inWire.acf := io.in.bits.acf(i)
    inWire.crossPageIPFFix := io.in.bits.crossPageIPFFix(i)

    inWire.triggered := io.in.bits.triggered(i)

//    dontTouch(inWire.triggered)

    ibuf.io.waddr(i) := tail_vec(offset(i)).value
    ibuf.io.wdata(i) := inWire
    ibuf.io.wen(i)   := io.in.bits.enqEnable(i) && io.in.fire && !io.flush
  }

  when (io.in.fire && !io.flush) {
    tail_vec := VecInit(tail_vec.map(_ + PopCount(io.in.bits.enqEnable)))
  }

  // Dequeue
  val validVec = Mux(validEntries >= DecodeWidth.U, ((1 << DecodeWidth) - 1).U, UIntToMask(validEntries, DecodeWidth))
  for (i <- 0 until DecodeWidth) {
    io.out(i).valid := validVec(i)

    val outWire = ibuf.io.rdata(i)

    io.out(i).bits.instr := outWire.inst
    io.out(i).bits.pc := outWire.pc
    io.out(i).bits.exceptionVec := 0.U.asTypeOf(Vec(16, Bool()))
    io.out(i).bits.exceptionVec(instrPageFault) := outWire.ipf
    io.out(i).bits.exceptionVec(instrAccessFault) := outWire.acf
    io.out(i).bits.pd := outWire.pd
    io.out(i).bits.pred_taken := outWire.pred_taken
    io.out(i).bits.ftqPtr := outWire.ftqPtr
    io.out(i).bits.ftqOffset := outWire.ftqOffset
    io.out(i).bits.crossPageIPFFix := outWire.crossPageIPFFix
    io.out(i).bits.foldpc := outWire.foldpc
    io.out(i).bits.loadWaitBit := DontCare
    io.out(i).bits.waitForRobIdx := DontCare
    io.out(i).bits.storeSetHit := DontCare
    io.out(i).bits.loadWaitStrict := DontCare
    io.out(i).bits.ssid := DontCare
    io.out(i).bits.replayInst := false.B
    io.out(i).bits.trigger := outWire.triggered
  }
  val next_head_vec = VecInit(head_vec.map(_ + numDeq))
  ibuf.io.raddr := VecInit(next_head_vec.map(_.value))
  head_vec := next_head_vec

  // Flush
  when (io.flush) {
    allowEnq := true.B
    head_vec := VecInit((0 until DecodeWidth).map(_.U.asTypeOf(new IbufPtr)))
    tail_vec := VecInit((0 until PredictWidth).map(_.U.asTypeOf(new IbufPtr)))
  }
  io.full := !allowEnq


  // Debug info
  XSDebug(io.flush, "IBuffer Flushed\n")

  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(p"MASK=${Binary(io.in.bits.valid)}\n")
    for(i <- 0 until PredictWidth){
      XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
    }
  }

  for (i <- 0 until DecodeWidth) {
    XSDebug(io.out(i).fire(), p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)} v=${io.out(i).valid} r=${io.out(i).ready} " +
      p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n")
  }

  XSDebug(p"ValidEntries: ${validEntries}\n")
  XSDebug(p"EnqNum: ${numEnq}\n")
  XSDebug(p"DeqNum: ${numDeq}\n")



  val afterInit = RegInit(false.B)
  val headBubble = RegInit(false.B)
  when (io.in.fire) { afterInit := true.B }
  when (io.flush) {
    headBubble := true.B
  } .elsewhen(validEntries =/= 0.U) {
    headBubble := false.B
  }
  val instrHungry = afterInit && (validEntries === 0.U) && !headBubble

  QueuePerf(IBufSize, validEntries, !allowEnq)
  XSPerfAccumulate("flush", io.flush)
  XSPerfAccumulate("hungry", instrHungry)
  if (env.EnableTopDown) {
    val ibuffer_IDWidth_hvButNotFull = afterInit && (validEntries =/= 0.U) && (validEntries < DecodeWidth.U) && !headBubble
    XSPerfAccumulate("ibuffer_IDWidth_hvButNotFull", ibuffer_IDWidth_hvButNotFull)
  }

  val perfEvents = Seq(
    ("IBuffer_Flushed  ", io.flush                                                                     ),
    ("IBuffer_hungry   ", instrHungry                                                                  ),
    ("IBuffer_1_4_valid", (validEntries >  (0*(IBufSize/4)).U) & (validEntries < (1*(IBufSize/4)).U)   ),
    ("IBuffer_2_4_valid", (validEntries >= (1*(IBufSize/4)).U) & (validEntries < (2*(IBufSize/4)).U)   ),
    ("IBuffer_3_4_valid", (validEntries >= (2*(IBufSize/4)).U) & (validEntries < (3*(IBufSize/4)).U)   ),
    ("IBuffer_4_4_valid", (validEntries >= (3*(IBufSize/4)).U) & (validEntries < (4*(IBufSize/4)).U)   ),
    ("IBuffer_full     ",  validEntries.andR                                                           ),
    ("Front_Bubble     ", PopCount((0 until DecodeWidth).map(i => io.out(i).ready && !io.out(i).valid)))
  )
  generatePerfEvent()
}
