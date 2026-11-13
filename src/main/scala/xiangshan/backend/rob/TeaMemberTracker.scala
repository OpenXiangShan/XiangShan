/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

class TeaPsvUpdate(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val rank = UInt(log2Ceil(RenameWidth).W)
  val mask = UInt(PerfEvent.width.W)
}

class TeaSlotEvent(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val mask = UInt(PerfEvent.width.W)
}

class TeaCompletion(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val rank = UInt(log2Ceil(RenameWidth).W)
}

class TeaMemberTrackerIO(writebackPorts: Int, eventPorts: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val cycle = Input(UInt(64.W))
  val addrTransType = Input(new AddrTransType)
  val alloc = Input(Vec(RenameWidth, Valid(new TeaRenameMember)))
  val enqPsv = Input(Vec(RenameWidth, Valid(new TeaPsvUpdate)))
  val writeback = Input(Vec(writebackPorts, Valid(new TeaCompletion)))
  val events = Input(Vec(eventPorts, Valid(new TeaSlotEvent)))
  val redirect = Input(Valid(new Redirect))
  val slotReady = Input(Vec(RobSize, Vec(2, Bool())))
  val commits = Input(Vec(2 * CommitWidth, Valid(new RobPtr)))
  val head = Input(new RobPtr)
  val readPtr = Input(Vec(2, new RobPtr))
  val readPc = Input(Vec(2, UInt(XLEN.W)))
  val read = Output(Vec(2, Valid(UInt(PerfEvent.width.W))))
  val stalled = Output(Valid(new TeaTarget))
  val markStalled = Input(Bool())
  val committed = Output(Vec(CommitWidth * RenameWidth, Valid(new TeaMemberResult)))
  val finals = Output(Vec(RenameWidth, Valid(new TeaDelayedFinal)))
}

// Debug state lives in the simulator and is updated only for accepted events.
class TeaMemberTracker(writebackPorts: Int, eventPorts: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new TeaMemberTrackerIO(writebackPorts, eventPorts))
  private val phase = RegInit(false.B)
  when(io.enable) { phase := !phase }

  private val inputs = scala.collection.mutable.ArrayBuffer.empty[UInt]
  private def put(fields: Data*): Unit = fields.foreach { x =>
    val word = Wire(UInt(64.W))
    word := x.asUInt
    inputs += word
  }
  private def ptr(value: RobPtr): Unit = put(value.value, value.flag, value.isFormer)
  put(io.cycle, io.markStalled)
  ptr(io.head)
  io.slotReady(io.head.value).foreach(x => put(x))
  io.readPtr.zip(io.readPc).foreach { case (pointer, pc) =>
    ptr(pointer)
    put(pc)
  }
  put(io.redirect.valid)
  ptr(io.redirect.bits.robIdx)
  put(io.redirect.bits.flushItself())
  io.alloc.foreach { x =>
    put(x.valid)
    ptr(x.bits.robIdx)
    put(x.bits.rank, x.bits.owner, io.addrTransType.extend(x.bits.pc, XLEN), x.bits.psv, x.bits.numWB)
  }
  io.enqPsv.foreach { x =>
    put(x.valid)
    ptr(x.bits.robIdx)
    put(x.bits.rank, x.bits.mask)
  }
  io.writeback.foreach { x =>
    put(x.valid)
    ptr(x.bits.robIdx)
    put(x.bits.rank)
  }
  io.events.foreach { x =>
    put(x.valid)
    ptr(x.bits.robIdx)
    put(x.bits.mask)
  }
  io.commits.foreach { x =>
    put(x.valid)
    ptr(x.bits)
  }

  private val outputWords = 2 * 2 + 4 + CommitWidth * RenameWidth * 6 + RenameWidth * 9
  private val helper = Module(new TeaMemberTrackerNative(
    RobSize, RenameWidth, CommitWidth, writebackPorts, eventPorts, p(XSCoreParamsKey).HartId,
    inputs.size, outputWords))
  helper.io.reset := reset.asBool || !io.enable
  helper.io.phase := phase
  helper.io.in := Mux(io.enable, VecInit(inputs.toSeq).asUInt, 0.U)
  private val outputs = helper.io.out.asTypeOf(Vec(outputWords, UInt(64.W)))
  private var position = 0
  private def get(): UInt = {
    val word = outputs(position)
    position += 1
    word
  }
  private def target(value: TeaTarget): Unit = {
    value.allocationCycle := get()
    value.robIndex := get()
    value.rank := get()
  }
  private def result(value: TeaMemberResult): Unit = {
    target(value.target)
    value.pc := get()
    value.psv := get()
  }
  io.read.foreach { x =>
    x.valid := get()(0)
    x.bits := get()
  }
  io.stalled.valid := get()(0)
  target(io.stalled.bits)
  io.committed.foreach { x =>
    x.valid := get()(0)
    result(x.bits)
  }
  io.finals.foreach { x =>
    x.valid := get()(0)
    x.bits.cycle := get()
    x.bits.isDrain := get()(0)
    x.bits.cancelled := get()(0)
    result(x.bits.member)
  }
  require(position == outputWords)
}
