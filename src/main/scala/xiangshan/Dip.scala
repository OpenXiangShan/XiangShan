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

package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.rob.RobPtr

object DipState {
  val base = 0
  val stall = 1
  val frontend = 2
  val misspeculation = 3
}

// Multi-hot causes, interpreted in the context of the slot state.
object DipCause {
  val rob = 0
  val intIq = 1
  val fpIq = 2
  val vecIq = 3
  val memIq = 4
  val lq = 5
  val sq = 6
  val rename = 7
  val serial = 8
  val lsqRecovery = 9
  val policy = 10
  val lsq = 11
  val width = 12
  def bit(index: Int): UInt = (BigInt(1) << index).U(width.W)
}

class DipUop(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val pc = UInt(XLEN.W)
  val psv = UInt(TeaEvent.width.W)
}

class DipDispatch(implicit p: Parameters) extends XSBundle {
  val validMask = UInt(RenameWidth.W)
  val fireMask = UInt(RenameWidth.W)
  val uops = Vec(RenameWidth, new DipUop)
  val stallCause = UInt(DipCause.width.W)
}

class DipFlush(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val self = Bool()
  val pc = UInt(XLEN.W)
  val cause = UInt(DipCause.width.W)
}

class DipRob(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val retired = UInt(log2Ceil(CommitWidth * RenameWidth + 1).W)
  val commits = Vec(2 * CommitWidth, Valid(new RobPtr))
  val flush = Valid(new DipFlush)
}

// References exist only for sampled work. The generation prevents ROB reuse from
// joining a new instruction to an old sample.
class DipTarget(implicit p: Parameters) extends XSBundle {
  val generation = UInt(64.W)
  val robIdx = new RobPtr
}

object DipTargetKind {
  val resolved = 0
  val rob = 1
  val successor = 2
}

class DipSlot(implicit p: Parameters) extends XSBundle {
  val state = UInt(2.W)
  val cause = UInt(DipCause.width.W)
  val pc = UInt(XLEN.W)
  val kind = UInt(2.W)
  val target = new DipTarget
}

class DipEntry(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val totalRetired = UInt(64.W) // formal-window retirement prefix before this cycle
  val retired = UInt(log2Ceil(CommitWidth * RenameWidth + 1).W)
  val slots = Vec(RenameWidth, new DipSlot)
}

object DipEventKind {
  val commit = 0
  val flush = 1
  val successor = 2
  val watermark = 3
}

// Only sampled targets generate events. Flush records reparent sampled work;
// subsequent flushes can reparent it again before the cause becomes stable.
class DipDelayedFinal(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val kind = UInt(2.W)
  val target = new DipTarget
  val next = new DipTarget
  val pc = UInt(XLEN.W)
  val cause = UInt(DipCause.width.W)
  val self = Bool()
}

class DipSamplerIO(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val period = Input(UInt(64.W))
  val dispatch = Input(new DipDispatch)
  val rob = Input(new DipRob)
  val trace = Output(Valid(new DipEntry))
  val finals = Output(Vec(2 * CommitWidth + 3, Valid(new DipDelayedFinal)))
}

class DipSampler(implicit p: Parameters) extends XSModule {
  val io = IO(new DipSamplerIO)
  private val phase = RegInit(false.B)
  phase := !phase
  private val inputs = scala.collection.mutable.ArrayBuffer.empty[UInt]
  private def put(fields: Data*): Unit = fields.foreach { x =>
    val word = Wire(UInt(64.W))
    word := x.asUInt
    inputs += word
  }
  private def ptr(value: RobPtr): Unit = put(value.value, value.flag, value.isFormer)
  put(io.enable, io.period, io.dispatch.validMask, io.dispatch.fireMask, io.dispatch.stallCause)
  io.dispatch.uops.foreach { u =>
    ptr(u.robIdx)
    put(u.pc, u.psv)
  }
  put(io.rob.cycle, io.rob.retired)
  io.rob.commits.foreach { c =>
    put(c.valid)
    ptr(c.bits)
  }
  put(io.rob.flush.valid)
  ptr(io.rob.flush.bits.robIdx)
  put(io.rob.flush.bits.self, io.rob.flush.bits.pc, io.rob.flush.bits.cause)
  private val words = 4 + RenameWidth * 8 + (2 * CommitWidth + 3) * 14
  private val helper = Module(new DipNative(RobSize, RenameWidth, CommitWidth,
    p(XSCoreParamsKey).HartId, inputs.size, words))
  helper.io.reset := reset.asBool
  helper.io.phase := phase
  helper.io.in := VecInit(inputs.toSeq).asUInt
  private val outputs = helper.io.out.asTypeOf(Vec(words, UInt(64.W)))
  private var position = 0
  private def get(): UInt = {
    val word = outputs(position)
    position += 1
    word
  }
  private def pointer(value: RobPtr): Unit = {
    value.value := get()
    value.flag := get()(0)
    value.isFormer := get()(0)
  }
  private def target(value: DipTarget): Unit = {
    value.generation := get()
    pointer(value.robIdx)
  }
  io.trace.valid := get()(0)
  io.trace.bits.cycle := get()
  io.trace.bits.totalRetired := get()
  io.trace.bits.retired := get()
  io.trace.bits.slots.foreach { s =>
    s.state := get()
    s.cause := get()
    s.pc := get()
    s.kind := get()
    target(s.target)
  }
  io.finals.foreach { f =>
    f.valid := get()(0)
    f.bits.cycle := get()
    f.bits.kind := get()
    target(f.bits.target)
    target(f.bits.next)
    f.bits.pc := get()
    f.bits.cause := get()
    f.bits.self := get()(0)
  }
  require(position == words)
}
