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
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper, XSError}
import xiangshan.RedirectLevel
import xiangshan.XSModule
import chisel3.util.experimental.BoringUtils

trait HasTraceReaderFPGAParam {
  val TraceFPGAHugeBufferSize = 1 * 1024 * 1024 // 1M
  val TraceRecvWidth = 16 // set it same to TraceFetchWidth now
}

class TraceReaderFPGAIO(implicit p: Parameters) extends TraceBundle
  with HasTraceReaderFPGAParam {
  // fpga platform, from host, in trace format
  // val tracesFromHost = Flipped(DecoupledIO((Vec(TraceRecvWidth, new TraceInstrInnerBundle()))))

  // normal trace reader io
  val enable = Input(Bool())
  val instsValid = Output(Bool())
  val instsToDut = Output(Vec(TraceFetchWidth, new TraceInstrInnerBundle))
  val redirect = Input(Valid(UInt(64.W)))
  val workingState = Input(Bool())
}

class TraceFPGAHugeBufferPtr(entries: Int) extends CircularQueuePtr[TraceFPGAHugeBufferPtr](entries)
  with HasCircularQueuePtrHelper {}

class TraceReaderFPGA(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasTraceReaderFPGAParam
  with HasCircularQueuePtrHelper {
  val io = IO(new TraceReaderFPGAIO)

  private val TraceInstWidth = (new TraceInstrInnerBundle).getWidth

  // TraceReader should maintain a very huge buffer, for:
  // maybe: (1) dual port(read/write); (2) two seperated mem
  val hugeBuffer = Mem(TraceFPGAHugeBufferSize, new TraceInstrInnerBundle)
  val writePtr = RegInit(0.U.asTypeOf(new TraceFPGAHugeBufferPtr(TraceFPGAHugeBufferSize)))
  val readPtr = RegInit(0.U.asTypeOf(new TraceFPGAHugeBufferPtr(TraceFPGAHugeBufferSize)))

  // FIXME: ugly code, use raw BoringUtils.addSource/addSink, beautify me
  val fpgaTraces = WireInit(0.U.asTypeOf(Vec(TraceRecvWidth, new TraceInstrInnerBundle)))
  val fpgaTracesValid = WireInit(false.B)
  val fpgaTracesReady = Wire(Bool())
  val fpgaTraces_tmp = WireInit(0.U(fpgaTraces.getWidth.W))
  BoringUtils.addSink(fpgaTraces_tmp, "TraceRTLFPGATraces")
  BoringUtils.addSink(fpgaTracesValid, "TraceRTLFPGATracesValid")
  BoringUtils.addSource(fpgaTracesReady, "TraceRTLFPGATracesReady")
  for (i <- 0 until TraceRecvWidth) {
    fpgaTraces(i) := TraceInstrInnerBundle.readRaw(
      fpgaTraces_tmp((i + 1) * TraceInstWidth - 1, i * TraceInstWidth))
  }

  // FIXME: should concern redirect, change TraceRecvWidth to OOO windows size
  fpgaTracesReady := hasFreeEntries(writePtr, readPtr) >= TraceRecvWidth.U
  when (fpgaTracesValid && fpgaTracesReady) {
    // FIXME: may generate TraceRecvWidth ports, unsynthsis, optimize it
    //        maybe use SRAM, one line has TraceFetchWidth, need set TraceRecvWidth same with Recv or add one more input buffer
    (0 until TraceRecvWidth).foreach{ i =>
      hugeBuffer(writePtr.value + i.U) := fpgaTraces(i)
    }
    writePtr := writePtr + TraceRecvWidth.U
  }

  io.instsValid := !isEmpty(writePtr, readPtr)
  (0 until TraceFetchWidth).foreach{ i =>
    io.instsToDut(i) := hugeBuffer(readPtr.value + i.U)
  }
  when (io.enable && io.instsValid) {
    readPtr := readPtr + TraceFetchWidth.U
  }
  dontTouch(io.instsToDut)
  dontTouch(io.enable)
  dontTouch(io.instsValid)

  val curPtrInstID = io.instsToDut(0).InstID
  val redirectNextPtr = readPtr - (curPtrInstID - io.redirect.bits)
  // FIXME: when hugeBuffer changes, change the redirect logic
  when (io.redirect.valid) {
    readPtr := redirectNextPtr
  }

  // debug
  val firstInstCheck = RegInit(true.B)
  when (io.instsValid && firstInstCheck) {
    firstInstCheck := false.B
    XSError(io.instsValid && (io.instsToDut(0).pcVA =/= 0x80000000L.U),
      "Error in TraceReaderFPGA: the first inst's PC is not 0x80000000")
    for (i <- 0 until (TraceFetchWidth - 1)) {
      XSError(io.instsToDut(i).InstID =/= (i + 1).U,
        s"Error in TraceReaderFPGA: the ${i}th inst's InstID is not ${(i + 1)}")
    }
  }
  for (i <- 0 until (TraceFetchWidth - 1)) {
    XSError(io.instsValid && ((io.instsToDut(i).InstID + 1.U) =/= io.instsToDut(i + 1).InstID),
      s"Error in TraceReaderFPGA: the ${i}th and ${i+1}th inst's InstID are not consecutive")
    XSError(io.instsValid && ((io.instsToDut(i+1).pcVA =/= io.instsToDut(i).nextPC)),
      s"Error in TraceReaderFPGA: the ${i}th inst's nextPC is not equal to ${i+1}th inst's PC")
  }
}