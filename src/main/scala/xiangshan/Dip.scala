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
  val frontendPsv = UInt(2.W)
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
  val totalRetired = UInt(64.W) // prefix before this cycle, for exact warmup filtering
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

class DipSampler(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val period = Input(UInt(64.W))
    val dispatch = Input(new DipDispatch)
    val rob = Input(new DipRob)
    val trace = Output(Valid(new DipEntry))
    val finals = Output(Vec(2 * CommitWidth + 3, Valid(new DipDelayedFinal)))
  })
  val countdown = RegInit(0.U(64.W))
  val recovering = RegInit(false.B)
  val lastFlush = Reg(new DipFlush)
  val flushSettled = RegInit(false.B)
  val due = io.enable && countdown === 0.U
  when(!io.enable) {
    countdown := 0.U
  }.elsewhen(countdown === 0.U) {
    countdown := Mux(io.period === 0.U, 0.U, io.period - 1.U)
  }.otherwise {
    countdown := countdown - 1.U
  }
  val totalRetired = RegInit(0.U(64.W))
  totalRetired := totalRetired + io.rob.retired

  val tracked = RegInit(VecInit.fill(2 * RobSize)(false.B))
  val generations = Reg(Vec(2 * RobSize, UInt(64.W)))
  val flags = Reg(Vec(2 * RobSize, Bool()))
  val causes = Reg(Vec(2 * RobSize, Bool()))
  val waiting = RegInit(false.B)
  val waitingGeneration = Reg(UInt(64.W))

  def index(ptr: RobPtr): UInt = Cat(ptr.value, !ptr.isFormer)
  def killed(ptr: RobPtr): Bool = ptr.isAfterSlot(io.rob.flush.bits.robIdx) ||
    io.rob.flush.bits.self && ptr.isSameSlot(io.rob.flush.bits.robIdx)
  def atCommit(ptr: RobPtr): Bool = io.rob.commits.map(c => c.valid && c.bits.isSameSlot(ptr)).reduce(_ || _)
  def settled(ptr: RobPtr): Bool = io.rob.commits.map(c => c.valid && c.bits.isNotBeforeSlot(ptr)).reduce(_ || _)

  val pointers = Wire(Vec(2 * RobSize, new RobPtr))
  val ending = Wire(Vec(2 * RobSize, Bool()))
  val flushed = Wire(Vec(2 * RobSize, Bool()))
  val causeSettled = Wire(Vec(2 * RobSize, Bool()))
  for (i <- 0 until 2 * RobSize) {
    pointers(i).value := (i / 2).U
    pointers(i).isFormer := (i % 2 == 0).B
    pointers(i).flag := flags(i)
    flushed(i) := tracked(i) && io.rob.flush.valid && killed(pointers(i))
    causeSettled(i) := tracked(i) && causes(i) && settled(pointers(i))
    ending(i) := flushed(i) || causeSettled(i) || tracked(i) && atCommit(pointers(i))
  }
  def reference(ptr: RobPtr): DipTarget = {
    val ref = Wire(new DipTarget)
    val i = index(ptr)
    ref.robIdx := ptr
    // A flushing cause can retain its identity while its previous work is
    // reclassified. A retired/reused slot must receive a fresh generation.
    val reusable = tracked(i) && flags(i) === ptr.flag && !atCommit(ptr) &&
      !(causes(i) && settled(ptr))
    ref.generation := Mux(reusable, generations(i), io.rob.cycle)
    ref
  }

  val cause = Mux(io.rob.flush.valid, io.rob.flush.bits, lastFlush)
  val stableCause = Mux(io.rob.flush.valid, settled(cause.robIdx), flushSettled || settled(cause.robIdx))
  when(io.rob.flush.valid) {
    recovering := true.B
    lastFlush := io.rob.flush.bits
    flushSettled := settled(io.rob.flush.bits.robIdx)
  }.otherwise {
    when(settled(lastFlush.robIdx)) { flushSettled := true.B }
    when(io.dispatch.validMask.orR) { recovering := false.B }
  }

  val bind = waiting && io.dispatch.validMask.orR && !io.rob.flush.valid
  val first = io.dispatch.uops(PriorityEncoder(io.dispatch.validMask))
  val firstRef = reference(first.robIdx)
  val causeRef = reference(cause.robIdx)
  val survivingWait = waiting && !bind && !io.rob.flush.valid
  val nextWaitGeneration = Mux(survivingWait, waitingGeneration, io.rob.cycle)

  val entry = WireInit(0.U.asTypeOf(new DipEntry))
  entry.cycle := io.rob.cycle
  entry.totalRetired := totalRetired
  entry.retired := io.rob.retired
  val blocked = io.dispatch.validMask & ~io.dispatch.fireMask
  val blockedUop = io.dispatch.uops(PriorityEncoder(blocked))
  for (slot <- 0 until RenameWidth) {
    val firedOH = VecInit((0 until RenameWidth).map(lane =>
      io.dispatch.fireMask(lane) && PopCount(io.dispatch.fireMask & ((BigInt(1) << lane) - 1).U(RenameWidth.W)) === slot.U))
    val out = entry.slots(slot)
    when(io.rob.flush.valid || recovering && !io.dispatch.validMask.orR) {
      out.state := DipState.misspeculation.U
      out.cause := cause.cause
      out.pc := cause.pc
      out.kind := Mux(stableCause, DipTargetKind.resolved.U, DipTargetKind.rob.U)
      out.target := causeRef
    }.elsewhen(firedOH.asUInt.orR) {
      val uop = Mux1H(firedOH, io.dispatch.uops)
      out.state := DipState.base.U
      out.pc := uop.pc
      out.kind := DipTargetKind.rob.U
      out.target := reference(uop.robIdx)
    }.elsewhen(blocked.orR) {
      out.state := DipState.stall.U
      out.cause := io.dispatch.stallCause
      out.pc := blockedUop.pc
      out.kind := DipTargetKind.rob.U
      out.target := reference(blockedUop.robIdx)
    }.otherwise {
      out.state := Mux(io.dispatch.stallCause.orR, DipState.stall.U, DipState.frontend.U)
      out.cause := io.dispatch.stallCause
      out.kind := DipTargetKind.successor.U
      out.target.generation := nextWaitGeneration
    }
  }
  val newWait = due && entry.slots.map(_.kind === DipTargetKind.successor.U).reduce(_ || _)
  waiting := survivingWait || newWait
  when(newWait) { waitingGeneration := nextWaitGeneration }
  io.trace.valid := due
  io.trace.bits := entry

  io.finals := 0.U.asTypeOf(io.finals)
  val flushEvent = io.finals(2 * CommitWidth)
  flushEvent.valid := io.rob.flush.valid && (flushed.asUInt.orR || waiting)
  flushEvent.bits.kind := DipEventKind.flush.U
  flushEvent.bits.target := causeRef
  flushEvent.bits.pc := cause.pc
  flushEvent.bits.cause := cause.cause
  flushEvent.bits.self := cause.self

  val successor = io.finals(2 * CommitWidth + 1)
  successor.valid := bind
  successor.bits.kind := DipEventKind.successor.U
  successor.bits.target.generation := waitingGeneration
  successor.bits.next := firstRef
  successor.bits.pc := first.pc
  successor.bits.cause := Mux(first.frontendPsv.orR, first.frontendPsv, 4.U)

  val watermark = io.finals(2 * CommitWidth + 2)
  val latestCommit = io.rob.commits.reduce { (a, b) =>
    Mux(!a.valid || b.valid && b.bits.isAfterSlot(a.bits), b, a)
  }
  watermark.valid := causeSettled.asUInt.orR || flushEvent.valid && stableCause
  watermark.bits.kind := DipEventKind.watermark.U
  watermark.bits.target.robIdx := latestCommit.bits
  for ((commit, event) <- io.rob.commits.zip(io.finals.take(2 * CommitWidth))) {
    val i = index(commit.bits)
    event.valid := commit.valid && tracked(i) && flags(i) === commit.bits.flag && !flushed(i)
    event.bits.kind := DipEventKind.commit.U
    event.bits.target.robIdx := commit.bits
    event.bits.target.generation := generations(i)
  }
  io.finals.foreach(_.bits.cycle := io.rob.cycle)

  for (i <- 0 until 2 * RobSize) {
    val sampled = entry.slots.map(s => due && s.kind === DipTargetKind.rob.U && index(s.target.robIdx) === i.U)
    val fromSample = sampled.reduce(_ || _)
    val fromBind = bind && index(first.robIdx) === i.U
    val fromFlush = flushEvent.valid && !stableCause && index(cause.robIdx) === i.U
    val touch = fromSample || fromBind || fromFlush
    val ref = Mux(fromFlush, causeRef, Mux(fromBind, firstRef, Mux1H(sampled, entry.slots.map(_.target))))
    when(ending(i)) { tracked(i) := false.B }
    when(touch) {
      tracked(i) := true.B
      generations(i) := ref.generation
      flags(i) := ref.robIdx.flag
      causes(i) := fromFlush || fromSample && entry.slots.map(s =>
        s.kind === DipTargetKind.rob.U && index(s.target.robIdx) === i.U && s.state === DipState.misspeculation.U).reduce(_ || _) ||
        tracked(i) && !ending(i) && flags(i) === ref.robIdx.flag && causes(i)
    }
  }
}
