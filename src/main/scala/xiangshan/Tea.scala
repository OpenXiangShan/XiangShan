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
import xiangshan.backend.Bundles.NormalUopNumWidth
import xiangshan.backend.rob.RobPtr

object TeaState {
  def computing: UInt = 0.U(4.W)
  def stalled: UInt = 1.U(4.W)
  def flushed: UInt = 2.U(4.W)
  def drained: UInt = 3.U(4.W)

  def classify(isEmpty: Bool, isWalking: Bool, hasCommit: Bool, flushCauseValid: Bool): UInt =
    Mux(hasCommit, computing,
      Mux(isWalking || isEmpty && flushCauseValid, flushed, Mux(isEmpty, drained, stalled)))
}

// Architectural membership is captured before fusion clears its follower.
class TeaRenameMember(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val rank = UInt(log2Ceil(RenameWidth).W)
  val owner = UInt(log2Ceil(RenameWidth).W)
  val pc = UInt(VAddrBits.W)
  val psv = UInt(PerfEvent.width.W)
  val numWB = UInt(NormalUopNumWidth.W)
}

class TeaTarget(implicit p: Parameters) extends XSBundle {
  val allocationCycle = UInt(64.W)
  val robIndex = UInt(log2Ceil(RobSize).W)
  val rank = UInt(log2Ceil(RenameWidth).W)
}

class TeaMemberResult(implicit p: Parameters) extends XSBundle {
  val target = new TeaTarget
  val pc = UInt(XLEN.W)
  val psv = UInt(PerfEvent.width.W)
}

class TeaDelayedFinal(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val isDrain = Bool()
  val cancelled = Bool()
  val member = new TeaMemberResult
}

class TeaFlushCause(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val pc = UInt(XLEN.W)
  val psv = UInt(PerfEvent.width.W)
}

// Contemporaneous context; the existing Top-down reason describes an Entry, not a rank.
class TeaContext(implicit p: Parameters) extends XSBundle {
  val retired = UInt(log2Ceil(CommitWidth * RenameWidth + 1).W)
  val commitBlockMask = UInt(9.W)
  val headReady = Bool()
  val headReasonValid = Bool()
  val headReason = UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)
}

// Every sampling boundary is logged immediately. Deferred PSV is joined by target.
class TeaEntry(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val state = UInt(4.W)
  // Keep database integers below 2^63 even when the full mask has 64 bits.
  val validMask = Vec(CommitWidth, UInt(RenameWidth.W))
  val pcVec = Vec(CommitWidth * RenameWidth, UInt(XLEN.W))
  val psvVec = Vec(CommitWidth * RenameWidth, UInt(PerfEvent.width.W))
  val targetValid = Bool()
  val target = new TeaTarget
  val context = new TeaContext
}

class TeaSampleSelector(implicit val p: Parameters) extends Module with HasXSParameter {
  private val width = CommitWidth * RenameWidth
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val samplePeriod = Input(UInt(64.W))
    val state = Input(UInt(4.W))
    val commitMask = Input(UInt(width.W))
    val commitPc = Input(Vec(width, UInt(XLEN.W)))
    val commitPsv = Input(Vec(width, UInt(PerfEvent.width.W)))
    val stalledTarget = Input(Valid(new TeaTarget))
    val flushCause = Input(new TeaFlushCause)
    val cycle = Input(UInt(64.W))
    val sampleValid = Output(Bool())
    val sample = Output(new TeaEntry)
    val drainFinal = Output(Valid(new TeaDelayedFinal))
  })

  val countdown = RegInit(0.U(64.W))
  val drainPending = RegInit(false.B)
  val due = io.enable && countdown === 0.U
  val hasCommit = io.commitMask.orR
  when(!io.enable) {
    countdown := 0.U
  }.elsewhen(countdown === 0.U) {
    countdown := Mux(io.samplePeriod === 0.U, 0.U, io.samplePeriod - 1.U)
  }.otherwise {
    countdown := countdown - 1.U
  }

  val sample = WireInit(0.U.asTypeOf(new TeaEntry))
  sample.cycle := io.cycle
  sample.state := io.state
  when(io.state === TeaState.computing && hasCommit) {
    sample.validMask := io.commitMask.asTypeOf(sample.validMask)
    sample.pcVec := io.commitPc
    sample.psvVec := io.commitPsv
  }.elsewhen(io.state === TeaState.flushed && io.flushCause.valid) {
    sample.validMask(0) := 1.U
    sample.pcVec(0) := io.flushCause.pc
    sample.psvVec(0) := io.flushCause.psv
  }.elsewhen(io.state === TeaState.stalled) {
    sample.targetValid := io.stalledTarget.valid
    sample.target := io.stalledTarget.bits
  }

  io.drainFinal := 0.U.asTypeOf(io.drainFinal)
  io.drainFinal.valid := drainPending && hasCommit
  io.drainFinal.bits.cycle := io.cycle
  io.drainFinal.bits.isDrain := true.B
  io.drainFinal.bits.member.pc := PriorityMux(io.commitMask.asBools, io.commitPc)
  io.drainFinal.bits.member.psv := PriorityMux(io.commitMask.asBools, io.commitPsv)
  when(io.drainFinal.valid) {
    drainPending := false.B
  }
  when(due && io.state === TeaState.drained) {
    drainPending := true.B
  }

  io.sampleValid := due
  io.sample := sample
}
