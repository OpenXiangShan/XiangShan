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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import chisel3.experimental.ExtModule
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSModule
import utility.{CircularQueuePtr, HasCircularQueuePtrHelper}
import utils.{XSError}

class TraceICacheHelper extends ExtModule
  with HasExtModuleInline
{
  val clock = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val addr = IO(Input(UInt(64.W)))
  val data0 = IO(Output(UInt(64.W)))
  val data1 = IO(Output(UInt(64.W)))
  val data2 = IO(Output(UInt(64.W)))
  val data3 = IO(Output(UInt(64.W)))
  val data4 = IO(Output(UInt(64.W)))
  val data5 = IO(Output(UInt(64.W)))
  val data6 = IO(Output(UInt(64.W)))
  val data7 = IO(Output(UInt(64.W)))
  val legal_addr = IO(Output(Bool()))
}

// Replace ICache's data
// IFU1 --fire--> IFU2
class FakeICache()(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(VAddrBits.W)
    }))
    val resp = Valid(new Bundle {
      val data0 = UInt(256.W)
      val data1 = UInt(256.W)
      val addr = UInt(VAddrBits.W)
    })
  })

  val helper = Module(new TraceICacheHelper)
  helper.clock := clock
  helper.enable := io.req.valid
  helper.addr := io.req.bits.addr
  io.resp.valid := helper.legal_addr(0) && RegNext(io.req.valid)
  io.resp.bits.data0 := Cat(helper.data3, helper.data2, helper.data1, helper.data0)
  io.resp.bits.data1 := Cat(helper.data7, helper.data6, helper.data5, helper.data4)
  io.resp.bits.addr := RegEnable(helper.addr, io.req.valid)
}


trait TraceParams {
  val TracePCWidth = 64
  val TraceInstrWidth = 32
  val TraceFetchWidth = 16
  val TraceBufferSize = TraceFetchWidth * 3 //
}

class TraceBundle extends Bundle with TraceParams

class TraceInstrBundle extends TraceBundle {
  val pc = UInt(TracePCWidth.W)
  val instr = UInt(TraceInstrWidth.W)
}

object TraceInstrBundle {
  def apply(pc: UInt, instr: UInt): TraceInstrBundle = {
    val bundle = Wire(new TraceInstrBundle)
    bundle.pc := pc
    bundle.instr := instr
    bundle
  }
}

class TraceRecvInfo extends TraceBundle {
  val instrNum = UInt(log2Ceil(TraceFetchWidth).W)
}

class TraceReaderIO extends TraceBundle {
  // tracedInstr should always be valid
  val tracedInstr = Output(Vec(TraceFetchWidth, new TraceInstrBundle()))
  // recv.valid from f3_fire, bits.instrNum from range
  val recv = Flipped(Valid(new TraceRecvInfo()))
}

class TraceBufferPtr(Size: Int)(implicit p: Parameters) extends CircularQueuePtr[TraceBufferPtr](Size)

class TraceReader(implicit p: Parameters) extends XSModule
  with TraceParams
  with HasCircularQueuePtrHelper
{
  val io = IO(new TraceReaderIO)
  dontTouch(io)

  val traceBuffer = Reg(Vec(TraceBufferSize, new TraceInstrBundle()))
  val traceReaderHelper = Module(new TraceReaderHelper)
  val deqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))
  val enqPtr = RegInit(0.U.asTypeOf(new TraceBufferPtr(TraceBufferSize)))

  XSError(!isFull(enqPtr, deqPtr) && (enqPtr < deqPtr), "enqPtr should always be larger than deqPtr")
  XSError(io.recv.valid && ((deqPtr.value + io.recv.bits.instrNum) >= enqPtr.value),
    "Reader should not read more than what is in the buffer. Error in ReaderHelper or Ptr logic.")

  when (io.recv.valid) {
    deqPtr := deqPtr + io.recv.bits.instrNum
  }

  val readTraceEnable = !isFull(enqPtr, deqPtr) && (hasFreeEntries(enqPtr, deqPtr) >= TraceFetchWidth.U)
  when (readTraceEnable) {
    enqPtr := enqPtr + TraceFetchWidth.U
    (0 until TraceFetchWidth).foreach{i => {
      bufferInsert(enqPtr + i.U, TraceInstrBundle(traceReaderHelper.pc(i), traceReaderHelper.instr(i)))
    }}
  }

  def bufferInsert(ptr: TraceBufferPtr, data: TraceInstrBundle) = {
    traceBuffer(ptr.value) := data
    // traceBuffer(ptr.value) := 0.U
  }

  traceReaderHelper.clock := clock
  traceReaderHelper.reset := reset
  traceReaderHelper.enable := readTraceEnable
  io.tracedInstr.zipWithIndex.foreach{ case (instr, i) =>
    instr := traceBuffer(deqPtr.value + i.U)
  }
}

class TraceReaderHelper extends ExtModule with TraceParams {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val enable = IO(Input(Bool()))

  val pc = IO(Output(Vec(TraceFetchWidth, UInt(64.W))))
  val instr = IO(Output(Vec(TraceFetchWidth, UInt(32.W))))
}