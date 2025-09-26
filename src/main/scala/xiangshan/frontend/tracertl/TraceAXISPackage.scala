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
import chisel3.util.experimental.BoringUtils
import org.chipsalliance.cde.config.Parameters
import utility._

// size must be multiple of 8bit(byte)
class TraceFPGACollectBundle extends Bundle {
  val padding = UInt(17.W) // padding to make sure the whole bundle is multiple of 8bit(byte)
  val instNum = UInt(8.W)
  val pcVA = UInt(39.W)
}

class TraceCollectQueuePtr(entries: Int) extends CircularQueuePtr[TraceCollectQueuePtr](entries)
  with HasCircularQueuePtrHelper

// Collect valid commit information
// transport to XSTop and axis format
class TraceFPGACollectQueue(CommitCheckWidth: Int = 128)(implicit p: Parameters) extends TraceModule
  with HasCircularQueuePtrHelper {

  private val TRACE_FPGA_COLLECT_WIDTH = (new TraceFPGACollectBundle).getWidth

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec(CommitWidth, Valid(new TraceFPGACollectBundle))))
  })

  // must make sure 2^n
  val CollectBatchNum = 2
  val CollectQueueSize = CommitCheckWidth * CollectBatchNum
  val collectQueue = Reg(Vec(CollectQueueSize, new TraceFPGACollectBundle))
  val writePtr = RegInit(0.U.asTypeOf(new TraceCollectQueuePtr(CollectQueueSize)))
  val readPtr = RegInit(0.U.asTypeOf(new TraceCollectQueuePtr(CollectQueueSize)))

  // FIXME: when io.in.valid but not ready, we will lose data. So record and sum the lost instNum, add it to the next fire inst
  io.in.ready := hasFreeEntries(writePtr, readPtr) >= CommitWidth.U
  when (io.in.fire) {
    for (i <- 0 until CommitWidth) {
      when (io.in.bits(i).valid) {
        collectQueue(writePtr.value + i.U) := io.in.bits(i).bits
      }
    }
    writePtr := writePtr + PopCount(io.in.bits.map(_.valid))
  }
  XSError(io.in.valid && !io.in.ready, s"TraceFPGACollectQueue: Queue Full")
  // XSError(io.in.fire && io.in.bits(i).valid && io.in.bits.map(_.valid).take(i).foldLeft(false.B)(!_ || !_), s"Error in TraceFPGACollectBundle: port ${i} valid but older invalid ports")

  // output to XSTop by BoringUtil.addSink and addSource
  // wrap many instruction in one transaction
  val collectQueueReformat = WireInit(collectQueue.asTypeOf(
    Vec(CollectBatchNum, UInt((TRACE_FPGA_COLLECT_WIDTH * CommitCheckWidth).W)
  )))
  val reformatReadIdx = WireInit(readPtr.value(readPtr.value.getWidth-1, log2Ceil(CommitCheckWidth)))
  val out = WireInit(collectQueueReformat(reformatReadIdx))
  val outValid = WireInit(distanceBetween(writePtr, readPtr) >= CommitCheckWidth.U)
  val outReady = WireInit(false.B)
  when (outValid && outReady) {
    readPtr := readPtr + CommitCheckWidth.U
  }

  dontTouch(collectQueueReformat)
  dontTouch(reformatReadIdx)
  dontTouch(outValid)
  dontTouch(outReady)

  BoringUtils.addSink(outReady, "TraceRTLFPGATracesCollectReady")
  BoringUtils.addSource(outValid, "TraceRTLFPGATracesCollectValid")
  BoringUtils.addSource(out, "TraceRTLFPGATracesCollect")
}

class TraceAXISPackage(PACKET_INST_NUM: Int, AXIS_DATA_WIDTH: Int) extends Module {

  // PACKET_INST_NUM should be equal TraceCollectQueue.CommitCheckWidth
  private val TRACE_FPGA_COLLECT_WIDTH = (new TraceFPGACollectBundle).getWidth
  private val PACKET_INST_WIDTH = TRACE_FPGA_COLLECT_WIDTH * PACKET_INST_NUM
  private val PACKET_CYCLE_NUM = (PACKET_INST_WIDTH + AXIS_DATA_WIDTH - 1) / AXIS_DATA_WIDTH
  private val PLUS_EXTRA_WIDTH = PACKET_CYCLE_NUM * AXIS_DATA_WIDTH
  private val AXIS_DATA_BYTES = AXIS_DATA_WIDTH / 8
  private val TRACE_COLLECT_BYTES = TRACE_FPGA_COLLECT_WIDTH / 8 // FIXME: when TRACE_FPGA_COLLECT_WIDTH not multiple of 8bit(byte)

  println(s"[TraceAXISPackage] PACKET_INST_NUM: $PACKET_INST_NUM, PACKET_INST_WIDTH: $PACKET_INST_WIDTH, AXIS_DATA_WIDTH: $AXIS_DATA_WIDTH PACKET_CYCLE_NUM: $PACKET_CYCLE_NUM, PLUS_EXTRA_WIDTH: $PLUS_EXTRA_WIDTH")
  require(TRACE_FPGA_COLLECT_WIDTH == 64, "TraceFPGACollectBundle width should be 64 for simplicity")

  class TracePackageBufferPtr(entries: Int) extends CircularQueuePtr[TracePackageBufferPtr](entries) with HasCircularQueuePtrHelper

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(UInt(PACKET_INST_WIDTH.W)))
    val axis = new TraceRTLAXISIO(AXIS_DATA_WIDTH)
  })
  dontTouch(io)

  val BufferNum = 2
  val buffer = Reg(Vec(BufferNum, UInt(PLUS_EXTRA_WIDTH.W)))
  val bufferValid = RegInit(VecInit(Seq.fill(BufferNum)(false.B)))

  val recvIdx = RegInit(0.U.asTypeOf(new TracePackageBufferPtr(BufferNum)))
  val tranIdx = RegInit(0.U.asTypeOf(new TracePackageBufferPtr(BufferNum)))

  io.in.ready := !bufferValid(recvIdx.value)
  val in_fire = io.in.valid && io.in.ready

  // TODO: one pakcet one cycle, optimize to several cycle or add self-check(just cfi)
  when (in_fire) {
    buffer(recvIdx.value) := io.in.bits
    bufferValid(recvIdx.value) := true.B
    recvIdx := recvIdx + 1.U
  }

  // tranfer to axis
  val bufferReformat = buffer(tranIdx.value).asTypeOf(Vec(PACKET_CYCLE_NUM, UInt(AXIS_DATA_WIDTH.W)))
  val axis_fire = io.axis.tvalid && io.axis.tready
  val axis_cycle_index = Counter(axis_fire, PACKET_CYCLE_NUM)._1

  io.axis.tvalid := bufferValid(tranIdx.value)
  io.axis.tdata := bufferReformat(axis_cycle_index)
  io.axis.tkeep := Mux(axis_cycle_index === (PACKET_CYCLE_NUM-1).U,
    ((BigInt(1) << ((PACKET_INST_WIDTH - (PACKET_CYCLE_NUM-1)*AXIS_DATA_WIDTH).toInt / 8)) - 1).U(AXIS_DATA_BYTES.W),
    ~(0.U((AXIS_DATA_WIDTH/8).W))
  )
  io.axis.tlast := axis_cycle_index === (PACKET_CYCLE_NUM-1).U

  when (axis_fire && io.axis.tlast) {
    bufferValid(tranIdx.value) := false.B
    tranIdx := tranIdx + 1.U
  }

  for (i <- 0 until (AXIS_DATA_WIDTH / TRACE_FPGA_COLLECT_WIDTH)) {
    when (io.axis.tvalid && io.axis.tkeep((i + 1) * TRACE_COLLECT_BYTES - 1, i * TRACE_COLLECT_BYTES) =/= 0.U) {
      assert(io.axis.tdata((i+1)*TRACE_FPGA_COLLECT_WIDTH - 1, i * TRACE_FPGA_COLLECT_WIDTH) =/= 0.U, s"ERROR in TraceAXISPackage, tdata ${i} is zero but tkeep is not zero")
    }
  }

  dontTouch(bufferReformat)
  dontTouch(buffer)
  dontTouch(io.axis)
  dontTouch(io.in)
  dontTouch(tranIdx)
  dontTouch(recvIdx)
  dontTouch(axis_cycle_index)
}