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
import utility._
import xiangshan.DebugOptionsKey

class TraceUnpackageBufferPtr(entries: Int) extends CircularQueuePtr[TraceUnpackageBufferPtr](entries) with HasCircularQueuePtrHelper

// recv data from xdma-axis-steam bus to a buffer with MAX_DATA_WIDTH width
// tran data to core in format of UInt of DATA_WIDTH
// FIXME: these *DATA_WIDTH may not suit each other
// DATA_WIDTH = 16 * 48 = 768
// AXIS_DATA_WIDTH = 512
// MAX_DATA_WIDTH = ~20000(maybe)
class TraceAXISUnpackage(PACKET_INST_NUM: Int, AXIS_DATA_WIDTH: Int, DUT_BUS_INST_NUM: Int)(implicit p: Parameters) extends Module {

  // private val TRACE_OUTER_INST_ALIGN_WIDTH = (new TraceInstrFpgaBundle).getWidth
  private val TRACE_OUTER_INST_ALIGN_WIDTH = TraceInstrFpgaBundle.alignWidth()
  private val TRACE_OUTER_INST_PURE_WIDTH = (new TraceInstrFpgaBundle).getWidth

  // private val BUS_CORE_DATA_WIDTH = TRACE_INNER_INST_WIDTH  * DUT_BUS_INST_NUM
  private val PACKET_INST_WIDTH = TRACE_OUTER_INST_ALIGN_WIDTH  * PACKET_INST_NUM
  private val PACKET_CYCLE_NUM = (PACKET_INST_WIDTH + AXIS_DATA_WIDTH - 1) / AXIS_DATA_WIDTH
  private val PLUS_EXTRA_WIDTH = PACKET_CYCLE_NUM * AXIS_DATA_WIDTH

  private val BUS_CORE_DATA_WIDTH = TRACE_OUTER_INST_PURE_WIDTH  * DUT_BUS_INST_NUM

  println(s"[TraceAXISUnpackage] PACKET_INST_NUM: $PACKET_INST_NUM AXIS_DATA_WIDTH: $AXIS_DATA_WIDTH DUT_BUS_INST_NUM: $DUT_BUS_INST_NUM")
  println(s"BUS_CORE_DATA_WIDTH: $BUS_CORE_DATA_WIDTH PACKET_INST_WIDTH: $PACKET_INST_WIDTH PACKET_CYCLE_NUM: $PACKET_CYCLE_NUM PLUS_EXTRA_WIDTH: $PLUS_EXTRA_WIDTH")
  println(s"TRACE_OUTER_INST_ALIGN_WIDTH $TRACE_OUTER_INST_ALIGN_WIDTH TRACE_OUTER_INST_PURE_WIDTH $TRACE_OUTER_INST_PURE_WIDTH")

  val io = IO(new Bundle {
    val axis = Flipped(new TraceRTLAXISIO(AXIS_DATA_WIDTH))
    val data = DecoupledIO(UInt(BUS_CORE_DATA_WIDTH.W))
  })
  dontTouch(io)

  val BufferNum = 3
  val buffer = Reg(Vec(BufferNum, Vec(PACKET_CYCLE_NUM, UInt(AXIS_DATA_WIDTH.W))))
  val bufferValid = RegInit(VecInit(Seq.fill(BufferNum)(false.B)))

  val recvIdx = RegInit(0.U.asTypeOf(new TraceUnpackageBufferPtr(BufferNum)))
  val tranIdx = RegInit(0.U.asTypeOf(new TraceUnpackageBufferPtr(BufferNum)))

  // recv
  io.axis.tready := !bufferValid(recvIdx.value)
  val axis_fire = io.axis.tvalid && io.axis.tready
  val axis_cycle_index = Counter(axis_fire, PACKET_CYCLE_NUM)._1
  val packageLast = axis_cycle_index === (PACKET_CYCLE_NUM-1).U

  when (axis_fire) {
    buffer(recvIdx.value)(axis_cycle_index) := io.axis.tdata
  }
  when (axis_fire && packageLast) {
    recvIdx := recvIdx + 1.U
    bufferValid(recvIdx.value) := true.B
  }
  when (axis_fire && (axis_cycle_index === (PACKET_CYCLE_NUM-1).U)) {
    assert(io.axis.tlast, "ERROR in TraceAXISUnpackage, last cycle but tlast is false")
  }
  if (!p(DebugOptionsKey).FPGAPlatform) {
    XSPerfAccumulate("AXIS_CountFinishAndLast", axis_fire && (axis_cycle_index === (PACKET_CYCLE_NUM-1).U) && io.axis.tlast)
    XSPerfAccumulate("AXIS_CountFinishNotLast", axis_fire && (axis_cycle_index === (PACKET_CYCLE_NUM-1).U) && !io.axis.tlast)
  }

  // tranfer to core
  require(PACKET_INST_NUM % DUT_BUS_INST_NUM == 0, s"ERROR in TraceAXISUnpackage, PACKET_INST_NUM $PACKET_INST_NUM should be times of DUT_BUS_INST_NUM $DUT_BUS_INST_NUM")
  val MAX_CORE_CYCLE = PACKET_INST_NUM / DUT_BUS_INST_NUM
  val data_cycle_index = Counter(io.data.fire, MAX_CORE_CYCLE)._1

  val dataFlatten = buffer(tranIdx.value).asUInt
  val dataOuterVec = dataFlatten(PACKET_INST_WIDTH-1, 0).asTypeOf(
    Vec(MAX_CORE_CYCLE, UInt((TRACE_OUTER_INST_ALIGN_WIDTH * DUT_BUS_INST_NUM).W)))
  val dataOuterVecAtIndex = WireInit(dataOuterVec(data_cycle_index))
  val dataPureVec = Wire(Vec(DUT_BUS_INST_NUM, UInt(TRACE_OUTER_INST_PURE_WIDTH.W)))
  for (i <- 0 until DUT_BUS_INST_NUM) {
    dataPureVec(i) := dataOuterVecAtIndex((i+1) * TRACE_OUTER_INST_ALIGN_WIDTH - 1, i * TRACE_OUTER_INST_ALIGN_WIDTH)( TRACE_OUTER_INST_PURE_WIDTH - 1, 0)
  }

  dontTouch(dataOuterVec)
  dontTouch(dataOuterVecAtIndex)

  require(dataPureVec.asUInt.getWidth == io.data.bits.getWidth, s"ERROR in TraceAXISUnpackage, dataPureVec.asUInt.getWidth ${dataPureVec.asUInt.getWidth} != io.data.bits.getWidth ${io.data.bits.getWidth}")

  io.data.bits := dataPureVec.asUInt
  io.data.valid := bufferValid(tranIdx.value)

  when (io.data.fire) {
    when (data_cycle_index === (MAX_CORE_CYCLE-1).U) {
      bufferValid(tranIdx.value) := false.B
      tranIdx := tranIdx + 1.U
    }
  }

  // assert the first instruction
  val firstChecked = RegInit(false.B)
  when (io.data.fire) {
    when (firstChecked) {
      firstChecked := false.B
     assert(io.data.bits(39-1,0) === 0x80000000L.U(39.W), "ERROR in TraceAXISUnpackage, the first instruction pcVA is not 0x80000000")
     assert(io.data.bits(39+48-1, 39) === 0x80000000L.U(48.W), "ERROR in TraceAXISUnpackage, the first instruction pcPA is not 0x80000000")
    }
  }

  dontTouch(io.data)
  dontTouch(io.axis)
  dontTouch(recvIdx)
  dontTouch(tranIdx)
  dontTouch(data_cycle_index)
}