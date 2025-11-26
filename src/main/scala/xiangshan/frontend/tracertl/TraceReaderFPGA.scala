/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, SRAMTemplate, TimeOutAssert, XSError, XSPerfAccumulate}
import xiangshan.RedirectLevel
import xiangshan.XSModule
import chisel3.util.experimental.BoringUtils

class TraceReaderFPGAIO(implicit p: Parameters) extends TraceBundle {
  // fpga platform, from host, in trace format

  // normal trace reader io
  val enable = Input(Bool())
  val instsValid = Output(Bool())
  val instsToDut = Output(Vec(trtl.TraceFetchWidth, new TraceInstrInnerBundle))
  val redirect = Input(Valid(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)))
  val workingState = Input(Bool())
}

class TraceReaderFPGAPingPongRam(EntryNum: Int)(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val readValid = Input(Bool())
    val readReady = Output(Bool())
    val readInsts = Output(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrFpgaBundle))

    val writeValid = Input(Bool())
    val writeReady = Output(Bool())
    val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrFpgaBundle))

    val last = Output(Bool())
  })

  require(trtl.TraceFpgaHugeBufferWriteWidth == trtl.TraceFpgaHugeBufferReadWidth)
  val ram = Module(new SRAMTemplate[Vec[TraceInstrFpgaBundle]](
    gen = Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrFpgaBundle),
    set = EntryNum,
    way = 1
  ))

  val s_write :: s_read :: Nil = Enum(2)
  val state = RegInit(s_write)
  val ptr = RegInit(0.U(log2Ceil(EntryNum).W))

  val isWrite = state === s_write
  val isRead = state === s_read
  ram.io.w.req.valid := io.writeValid && isWrite
  ram.io.w.req.bits.data(0) := io.writeInsts
  ram.io.w.req.bits.setIdx := ptr
  io.writeReady := ram.io.w.req.ready && isWrite

  ram.io.r.req.valid := io.readValid && isRead
  ram.io.r.req.bits.setIdx := ptr
  io.readReady := ram.io.r.req.ready && isRead

  // FIXME: data is next cycle
  io.readInsts := ram.io.r.resp.data(0)
  io.last := ptr === (EntryNum - 1).U

  when ((io.writeValid && io.writeReady) ||
    (io.readValid && io.readReady)) {
    ptr := ptr + 1.U
    when (io.last) {
      state := Mux(isWrite, s_read, s_write)
      ptr := 0.U
    }
  }
}

class TraceReaderFPGAHugeBufferIO(implicit p: Parameters) extends TraceBundle {
  val readValid = Input(Bool())
  val readReady = Output(Bool())
  val readInsts = Output(Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrFpgaBundle))

  val writeValid = Input(Bool())
  val writeReady = Output(Bool())
  val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrFpgaBundle))
}

class TraceReaderFPGAHugeBuffer(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGAHugeBufferIO)

  // ping-pong ram to enable read and write in the same cycle
  // Weakness: only when ram is fully written, the ram is readable
  val RAM_NUM = 2
  class PingPongRamPtr(entries: Int) extends CircularQueuePtr[PingPongRamPtr](entries) with HasCircularQueuePtrHelper{}
  val ram = VecInit(Seq.fill(RAM_NUM)(Module(new TraceReaderFPGAPingPongRam(trtl.TraceFpgaHugeBufferSize/RAM_NUM)).io))
  val readPtr = RegInit(0.U.asTypeOf(new PingPongRamPtr(RAM_NUM)))
  val writePtr = RegInit(0.U.asTypeOf(new PingPongRamPtr(RAM_NUM)))

  val readRam = ram(readPtr.value)
  val readRamData = ram(RegNext(readPtr.value)).readInsts
  val writeRam = ram(writePtr.value)

  val readReady = readRam.readReady
  val readToNext = readRam.last
  val readInsts = readRam.readInsts

  val writeReady = writeRam.writeReady
  val writeToNext = writeRam.last

  ram.zipWithIndex.foreach{ case (ram, i) =>
    // wen has higher priority than ren
    ram.readValid := io.readValid && (i.U === readPtr.value)
    ram.writeValid := io.writeValid && (i.U === writePtr.value)
    ram.writeInsts := io.writeInsts
  }

  when (io.readValid && readReady && readToNext) {
    readPtr := readPtr + 1.U
  }
  when (io.writeValid && writeReady && writeToNext) {
    writePtr := writePtr + 1.U
  }

  io.readInsts := readRamData
  io.writeReady := writeReady
  io.readReady := readReady

  XSPerfAccumulate("writeNotReady", !io.writeReady)
  // TimeOutAssert(!io.readReady, 5000, "TraceReaderFPGAHugeBuffer read not ready timeout")
  // TimeOutAssert(!io.readValid, 5000, "TraceReaderFPGAHugeBuffer read not valid timeout")
  // TimeOutAssert(!io.writeValid, 5000, "TraceReaderFPGAHugeBuffer write not valid timeout")

  val firstInstCheck = RegInit(true.B)
  when (io.writeValid && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.writeInsts(0).pcVA =/= 0x80000000L.U,
      "Error in Trace HugeBuffer write first: the first inst's PC is not 0x80000000")
  }

  XSPerfAccumulate("ReadValidReady", io.readValid && io.readReady)
  XSPerfAccumulate("ReadValidNotReady", io.readValid && !io.readReady)
  XSPerfAccumulate("ReadReadyNotValid", !io.readValid && io.readReady)
  XSPerfAccumulate("ReadNotValidNotReady", !io.readValid && !io.readReady)

  XSPerfAccumulate("WriteValidReady", io.writeValid && io.writeReady)
  XSPerfAccumulate("WriteValidNotReady", io.writeValid && !io.writeReady)
  XSPerfAccumulate("WriteReadyNotValid", !io.writeValid && io.writeReady)
  XSPerfAccumulate("WriteNotValidNotReady", !io.writeValid && !io.writeReady)
}

class TraceReaderFPGASmallBufferIO(implicit p: Parameters) extends TraceBundle {
  val readValid = Input(Bool())
  val readReady = Output(Bool())
  val readInsts = Output(Vec(trtl.TraceFetchWidth, new TraceInstrInnerBundle))

  val writeValid = Input(Bool())
  val writeReady = Output(Bool())
  val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrFpgaBundle))

  val redirect = Input(Valid(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)))
}

class SmallBufferPtr(entries: Int) extends CircularQueuePtr[SmallBufferPtr](entries) with HasCircularQueuePtrHelper{}

class TraceReaderFPGASmallBuffer(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGASmallBufferIO)

  val buffer = Reg(Vec(trtl.TraceFpgaSmallBufferSize, new TraceInstrInnerBundle))

  val readPtr = RegInit(0.U.asTypeOf(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)))
  val writePtr = RegInit(0.U.asTypeOf(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)))
  val commitPtr = RegInit(0.U.asTypeOf(new SmallBufferPtr(trtl.TraceFpgaSmallBufferSize)))

  io.writeReady :=  distanceBetween(writePtr, commitPtr) <= (trtl.TraceFpgaSmallBufferSize-trtl.TraceFpgaHugeBufferReadWidth).U
  val writeFire = io.writeValid && io.writeReady
  when (writeFire) {
    writePtr := writePtr + trtl.TraceFpgaHugeBufferReadWidth.U
  }
  val writePtrForWrite = RegNext(writePtr)
  val instIDCounter = RegInit(1.U(trtl.TraceInstIDWidth.W))
  when (RegNext(writeFire, init = false.B)) {
    for (i <- 0 until trtl.TraceFpgaHugeBufferReadWidth) {
      buffer((writePtrForWrite + i.U).value) := io.writeInsts(i).toInnerBundle(instIDCounter + i.U)
    }
    instIDCounter := instIDCounter + trtl.TraceFpgaHugeBufferReadWidth.U
  }

  io.readReady := distanceBetween(writePtrForWrite, readPtr) >= trtl.TraceFetchWidth.U
  (0 until trtl.TraceFetchWidth).foreach{ i =>
    io.readInsts(i) := buffer((readPtr + i.U).value)
    io.readInsts(i).sbID.map(_ := readPtr + i.U)
  }
  when (io.readValid && io.readReady) {
    readPtr := readPtr + trtl.TraceFetchWidth.U
  }

  when (io.redirect.valid) {
    readPtr := io.redirect.bits // sbID
  }

  val commitValid = WireInit(false.B)
  val commitSbID = WireInit(0.U(readPtr.getWidth.W))
  BoringUtils.addSink(commitValid, "TraceRTLFPGACommitValid")
  BoringUtils.addSink(commitSbID, "TraceRTLFPGACommitSbID")
  when (commitValid) {
    commitPtr := commitSbID.asTypeOf(commitPtr)
  }

  XSError(readPtr.value >= trtl.TraceFpgaSmallBufferSize.U,
    "SmallBuffer read error: readPtr out of range")

  val firstInstCheck = RegInit(true.B)
  when (RegNext(io.writeValid, init = false.B) && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.writeInsts(0).pcVA =/= 0x80000000L.U,
      "Error in Trace SmallBuffer write first: the first inst's PC is not 0x80000000")
  }
  when (io.readValid && io.readReady) {
    for (i <- 0 until (trtl.TraceFetchWidth - 1)) {
      XSError((io.readInsts(i).InstID + 1.U) =/= (io.readInsts(i+1).InstID),
        s"Error in Trace SmallBuffer read: the ${i}th inst's InstID is not equal to ${i+1}th")
    }
  }

  XSPerfAccumulate("ReadValidReady", io.readValid && io.readReady)
  XSPerfAccumulate("ReadValidNotReady", io.readValid && !io.readReady)
  XSPerfAccumulate("ReadReadyNotValid", !io.readValid && io.readReady)
  XSPerfAccumulate("ReadNotValidNotReady", !io.readValid && !io.readReady)

  XSPerfAccumulate("WriteValidReady", io.writeValid && io.writeReady)
  XSPerfAccumulate("WriteValidNotReady", io.writeValid && !io.writeReady)
  XSPerfAccumulate("WriteReadyNotValid", !io.writeValid && io.writeReady)
  XSPerfAccumulate("WriteNotValidNotReady", !io.writeValid && !io.writeReady)
}

class TraceReaderFPGA(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGAIO)

  val fpgaTraces = WireInit(0.U.asTypeOf(Vec(trtl.TraceFpgaRecvWidth, new TraceInstrFpgaBundle)))
  val fpgaTracesValid = WireInit(false.B)
  val fpgaTracesReady = Wire(Bool())
  val fpgaTraces_tmp = WireInit(0.U(fpgaTraces.getWidth.W))
  BoringUtils.addSink(fpgaTraces_tmp, "TraceRTLFPGATraces")
  BoringUtils.addSink(fpgaTracesValid, "TraceRTLFPGATracesValid")
  BoringUtils.addSource(fpgaTracesReady, "TraceRTLFPGATracesReady")
  for (i <- 0 until trtl.TraceFpgaRecvWidth) {
    val TraceInstWidth = (new TraceInstrFpgaBundle).getWidth
    fpgaTraces(i) := TraceInstrFpgaBundle.readRaw(
      fpgaTraces_tmp((i + 1) * TraceInstWidth - 1, i * TraceInstWidth),
      reverse = false)
  }

  val hugeBuffer = Module(new TraceReaderFPGAHugeBuffer)
  hugeBuffer.io.writeValid := fpgaTracesValid
  hugeBuffer.io.writeInsts := fpgaTraces
  fpgaTracesReady := hugeBuffer.io.writeReady

  val smallBuffer = Module(new TraceReaderFPGASmallBuffer)
  smallBuffer.io.writeValid := hugeBuffer.io.readReady
  smallBuffer.io.writeInsts := hugeBuffer.io.readInsts
  hugeBuffer.io.readValid := smallBuffer.io.writeReady

  smallBuffer.io.redirect := io.redirect
  smallBuffer.io.readValid := io.enable
  io.instsToDut := smallBuffer.io.readInsts
  io.instsValid := smallBuffer.io.readReady

  dontTouch(io.instsToDut)
  dontTouch(io.enable)
  dontTouch(io.instsValid)

  // debug
  val firstInstCheck = RegInit(true.B)
  when (io.instsValid && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.instsValid && (io.instsToDut(0).pcVA =/= 0x80000000L.U),
      "Error in TraceReaderFPGA: the first inst's PC is not 0x80000000")
    for (i <- 0 until (trtl.TraceFetchWidth - 1)) {
      XSError(io.instsToDut(i).InstID =/= (i + 1).U,
        s"Error in TraceReaderFPGA: the ${i}th inst's InstID is not ${(i + 1)}")
    }
  }
  for (i <- 0 until (trtl.TraceFetchWidth - 1)) {
    XSError(io.instsValid && ((io.instsToDut(i).InstID + 1.U) =/= io.instsToDut(i + 1).InstID),
      s"Error in TraceReaderFPGA read: the ${i}th and ${i+1}th inst's InstID are not consecutive")
    XSError(io.instsValid && ((io.instsToDut(i+1).pcVA =/= io.instsToDut(i).nextPC)),
      s"Error in TraceReaderFPGA read: the ${i}th inst's nextPC is not equal to ${i+1}th inst's PC")
  }

  when (fpgaTracesValid) {
    (0 until trtl.TraceFpgaRecvWidth-1).foreach(i => {
      XSError(fpgaTraces(i).nextPC =/= fpgaTraces(i + 1).pcVA,
        s"Error in TraceReaderFPGA. Fpga Recv: the ${i}th inst's nextPC is not equal to ${i+1}th inst's PC")
    })
  }
}
