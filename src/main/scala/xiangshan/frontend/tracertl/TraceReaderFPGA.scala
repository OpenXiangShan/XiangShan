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
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, SRAMTemplate, TimeOutAssert, XSError}
import xiangshan.RedirectLevel
import xiangshan.XSModule
import chisel3.util.experimental.BoringUtils

class TraceReaderFPGAIO(implicit p: Parameters) extends TraceBundle {
  // fpga platform, from host, in trace format
  // val tracesFromHost = Flipped(DecoupledIO((Vec(TraceRecvWidth, new TraceInstrInnerBundle()))))

  // normal trace reader io
  val enable = Input(Bool())
  val instsValid = Output(Bool())
  val instsToDut = Output(Vec(trtl.TraceFetchWidth, new TraceInstrInnerBundle))
  val redirect = Input(Valid(UInt(64.W)))
  val workingState = Input(Bool())
}

class TraceReaderFPGAPingPongRam(EntryNum: Int)(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val readValid = Input(Bool())
    val readReady = Output(Bool())
    val readInsts = Output(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrInnerBundle))

    val writeValid = Input(Bool())
    val writeReady = Output(Bool())
    val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrInnerBundle))

    val last = Output(Bool())
  })

  require(trtl.TraceFpgaHugeBufferWriteWidth == trtl.TraceFpgaHugeBufferReadWidth)
  val ram = Module(new SRAMTemplate[Vec[TraceInstrInnerBundle]](
    gen = Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrInnerBundle),
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
  val readInsts = Output(Vec(trtl.TraceFpgaHugeBufferWriteWidth, new TraceInstrInnerBundle))

  val writeValid = Input(Bool())
  val writeReady = Output(Bool())
  val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrInnerBundle))
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

  TimeOutAssert(!io.readReady, 5000, "TraceReaderFPGAHugeBuffer read not ready timeout")
  TimeOutAssert(!io.writeReady, 5000, "TraceReaderFPGAHugeBuffer write not ready timeout")
  TimeOutAssert(!io.readValid, 5000, "TraceReaderFPGAHugeBuffer read not valid timeout")
  TimeOutAssert(!io.writeValid, 5000, "TraceReaderFPGAHugeBuffer write not valid timeout")

  val firstInstCheck = RegInit(true.B)
  when (io.writeValid && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.writeInsts(0).pcVA =/= 0x80000000L.U,
      "Error in Trace HugeBuffer write first: the first inst's PC is not 0x80000000")
    for (i <- 0 until (trtl.TraceFpgaHugeBufferWriteWidth - 1)) {
      XSError(io.writeInsts(i).InstID =/= (i + 1).U,
        s"Error in Trace HugeBuffer write first: the ${i}th inst's InstID is not ${(i + 1)}")
    }
  }
  when (io.writeValid) {
    for (i <- 0 until trtl.TraceFpgaHugeBufferWriteWidth - 1) {
      XSError((io.writeInsts(i).InstID + 1.U) =/= io.writeInsts(i+1).InstID,
        s"Error in Trace HugeBuffer write: the ${i}th inst's InstID is not equal to ${i+1}th")
    }
  }
  when (RegNext(io.readValid && io.readReady, init = false.B)) {
    for (i <- 0 until trtl.TraceFpgaHugeBufferReadWidth - 1) {
      XSError((io.readInsts(i).InstID + 1.U) =/= io.readInsts(i+1).InstID,
        s"Error in Trace HugeBuffer read: the ${i}th inst's InstID + 1.U is not equal to ${i+1}th")
    }
  }
}

class TraceReaderFPGASmallBufferIO(implicit p: Parameters) extends TraceBundle {
  val readValid = Input(Bool())
  val readReady = Output(Bool())
  val readInsts = Output(Vec(trtl.TraceFetchWidth, new TraceInstrInnerBundle))

  val writeValid = Input(Bool())
  val writeReady = Output(Bool())
  val writeInsts = Input(Vec(trtl.TraceFpgaHugeBufferReadWidth, new TraceInstrInnerBundle))

  val redirect = Input(Valid(UInt(64.W)))
}

class TraceReaderFPGASmallBuffer(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGASmallBufferIO)

  class SmallBufferPtr(entries: Int) extends CircularQueuePtr[SmallBufferPtr](entries) with HasCircularQueuePtrHelper{}
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
  when (RegNext(writeFire, init = false.B)) {
    for (i <- 0 until trtl.TraceFpgaHugeBufferReadWidth) {
      buffer((writePtrForWrite + i.U).value) := io.writeInsts(i)
    }
  }

  io.readReady := distanceBetween(writePtrForWrite, readPtr) >= trtl.TraceFetchWidth.U
  (0 until trtl.TraceFetchWidth).foreach{ i =>
    io.readInsts(i) := buffer((readPtr + i.U).value)
  }
  when (io.readValid && io.readReady) {
    readPtr := readPtr + trtl.TraceFetchWidth.U
  }

  val curPtrInstID = io.readInsts(0).InstID
  val redirectNextPtr = readPtr - (curPtrInstID - io.redirect.bits)
  when (io.redirect.valid) {
    readPtr := redirectNextPtr
  }

  val commitValid = WireInit(false.B)
  val commitInstNum = WireInit(0.U(log2Ceil(CommitWidth*RenameWidth+1).W))
  BoringUtils.addSink(commitValid, "TraceRTLFPGACommitValid")
  BoringUtils.addSink(commitInstNum, "TraceRTLFPGACommitInstNum")
  when (commitValid) {
    commitPtr := commitPtr + commitInstNum
  }

  XSError(redirectNextPtr.value < trtl.TraceFpgaSmallBufferSize.U,
    "SmallBuffer redirect error: redirectNextPtr out of range")
  XSError(readPtr.value < trtl.TraceFpgaSmallBufferSize.U,
    "SmallBuffer read error: readPtr out of range")
  XSError(io.redirect.valid && buffer(redirectNextPtr.value).InstID =/= io.redirect.bits,
    "SmallBuffer redirect error: InstID not match")

  val firstInstCheck = RegInit(true.B)
  when (RegNext(io.writeValid, init = false.B) && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.writeInsts(0).pcVA =/= 0x80000000L.U,
      "Error in Trace SmallBuffer write first: the first inst's PC is not 0x80000000")
    for (i <- 0 until (trtl.TraceFetchWidth - 1)) {
      XSError(io.writeInsts(i).InstID =/= (i + 1).U,
        s"Error in Trace SmallBuffer write first: the ${i}th inst's InstID is not ${(i + 1)}")
    }
  }
  when (io.readValid && io.readReady) {
    for (i <- 0 until (trtl.TraceFetchWidth - 1)) {
      XSError((io.readInsts(i).InstID + 1.U) =/= (io.readInsts(i+1).InstID),
        s"Error in Trace SmallBuffer read: the ${i}th inst's InstID is not equal to ${i+1}th")
    }
  }
}

class TraceReaderFPGA(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGAIO)

  private val TraceInstWidth = (new TraceInstrInnerBundle).getWidth

  val fpgaTraces = WireInit(0.U.asTypeOf(Vec(trtl.TraceFpgaRecvWidth, new TraceInstrInnerBundle)))
  val fpgaTracesValid = WireInit(false.B)
  val fpgaTracesReady = Wire(Bool())
  val fpgaTraces_tmp = WireInit(0.U(fpgaTraces.getWidth.W))
  BoringUtils.addSink(fpgaTraces_tmp, "TraceRTLFPGATraces")
  BoringUtils.addSink(fpgaTracesValid, "TraceRTLFPGATracesValid")
  BoringUtils.addSource(fpgaTracesReady, "TraceRTLFPGATracesReady")
  for (i <- 0 until trtl.TraceFpgaRecvWidth) {
    fpgaTraces(i) := TraceInstrInnerBundle.readRaw(
      fpgaTraces_tmp((i + 1) * TraceInstWidth - 1, i * TraceInstWidth))
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
    (0 until trtl.TraceFpgaRecvWidth-1).foreach(i => {
      XSError((fpgaTraces(i).InstID+1.U) =/= fpgaTraces(i + 1).InstID,
        s"Error in TraceReaderFPGA. Fpga Recv: the ${i}th inst's next instID is not equal to ${i+1}th inst's instID")
    })
  }
}