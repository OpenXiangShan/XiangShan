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

class DipSlot(implicit p: Parameters) extends XSBundle {
  val state = UInt(2.W)
  val cause = UInt(DipCause.width.W)
  val targetValid = Bool()
  val target = new DipUop
}

// Lifecycle information is retained between sample boundaries for offline DIT
// finalization. A target remains tentative until its ROB slot commits or flushes.
class DipEntry(implicit p: Parameters) extends XSBundle {
  val rob = new DipRob
  val dispatch = new DipDispatch
  val sampled = Bool()
  val slots = Vec(RenameWidth, new DipSlot)
}

class DipSampler(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val period = Input(UInt(64.W))
    val dispatch = Input(new DipDispatch)
    val rob = Input(new DipRob)
    val trace = Output(Valid(new DipEntry))
  })
  val countdown = RegInit(0.U(64.W))
  val tracking = RegInit(false.B)
  val recovering = RegInit(false.B)
  val lastFlush = Reg(new DipFlush)
  val due = io.enable && countdown === 0.U
  when(io.enable) { tracking := true.B }
  when(!io.enable) {
    countdown := 0.U
  }.elsewhen(countdown === 0.U) {
    countdown := Mux(io.period === 0.U, 0.U, io.period - 1.U)
  }.otherwise {
    countdown := countdown - 1.U
  }
  when(io.rob.flush.valid) {
    recovering := true.B
    lastFlush := io.rob.flush.bits
  }.elsewhen(io.dispatch.validMask.orR) {
    recovering := false.B
  }

  val entry = WireInit(0.U.asTypeOf(new DipEntry))
  entry.rob := io.rob
  entry.dispatch := io.dispatch
  entry.sampled := due
  val blocked = io.dispatch.validMask & ~io.dispatch.fireMask
  val blockedUop = io.dispatch.uops(PriorityEncoder(blocked))
  // Compact fired lanes so holes left by partial dispatch still count as capacity.
  for (slot <- 0 until RenameWidth) {
    val firedOH = VecInit((0 until RenameWidth).map(lane =>
      io.dispatch.fireMask(lane) && PopCount(io.dispatch.fireMask & ((BigInt(1) << lane) - 1).U(RenameWidth.W)) === slot.U))
    val out = entry.slots(slot)
    when(io.rob.flush.valid || recovering && !io.dispatch.validMask.orR) {
      val cause = Mux(io.rob.flush.valid, io.rob.flush.bits, lastFlush)
      out.state := DipState.misspeculation.U
      out.cause := cause.cause
      out.targetValid := true.B
      out.target.pc := cause.pc
    }.elsewhen(firedOH.asUInt.orR) {
      out.state := DipState.base.U
      out.targetValid := true.B
      out.target := Mux1H(firedOH, io.dispatch.uops)
    }.elsewhen(blocked.orR) {
      out.state := DipState.stall.U
      out.cause := io.dispatch.stallCause
      out.targetValid := true.B
      out.target := blockedUop
    }.otherwise {
      // Empty slots wait for the next uop, including its frontend miss signature.
      out.state := Mux(io.dispatch.stallCause.orR, DipState.stall.U, DipState.frontend.U)
      out.cause := io.dispatch.stallCause
    }
  }
  io.trace.valid := (tracking || io.enable) && (due || io.dispatch.validMask.orR ||
    io.rob.commits.map(_.valid).reduce(_ || _) || io.rob.flush.valid)
  io.trace.bits := entry
}
