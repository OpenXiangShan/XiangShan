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
import xiangshan.backend.Bundles.NormalUopNumWidth

class TeaPsvUpdate(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val rank = UInt(log2Ceil(RenameWidth).W)
  val mask = UInt(TeaEvent.width.W)
}

class TeaSlotEvent(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val mask = UInt(TeaEvent.width.W)
}

class TeaCompletion(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val rank = UInt(log2Ceil(RenameWidth).W)
}

private class TeaStoredMember(implicit p: Parameters) extends XSBundle {
  val isFormer = Bool()
  val owner = UInt(log2Ceil(RenameWidth).W)
  val pc = UInt(XLEN.W)
  val psv = UInt(TeaEvent.width.W)
  val remaining = UInt(NormalUopNumWidth.W)
}

// A debug-only companion to the functional ROB. Callers supply accepted events.
class TeaMemberTracker(writebackPorts: Int, eventPorts: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
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
    val read = Output(Vec(2, new TeaMemberResult))
    val stalled = Output(Valid(new TeaTarget))
    val markStalled = Input(Bool())
    val committed = Output(Vec(CommitWidth * RenameWidth, Valid(new TeaMemberResult)))
    val finals = Output(Vec(RenameWidth, Valid(new TeaDelayedFinal)))
  })

  private val live = RegInit(VecInit.fill(RobSize)(VecInit.fill(RenameWidth)(false.B)))
  private val sampled = RegInit(VecInit.fill(RobSize)(VecInit.fill(RenameWidth)(false.B)))
  private val members = Reg(Vec(RobSize, Vec(RenameWidth, new TeaStoredMember)))
  private val flags = Reg(Vec(RobSize, Bool()))
  private val allocationCycles = Reg(Vec(RobSize, UInt(64.W)))
  private val sampledEntry = RegInit(0.U.asTypeOf(new RobPtr))
  private val visiblePsv = Wire(Vec(RobSize, Vec(RenameWidth, UInt(TeaEvent.width.W))))
  private val retired = Wire(Vec(RobSize, Vec(RenameWidth, Bool())))
  private val killed = Wire(Vec(RobSize, Vec(RenameWidth, Bool())))

  private def ptrAt(entry: Int, rank: Int): RobPtr = {
    val ptr = Wire(new RobPtr)
    ptr.value := entry.U
    ptr.flag := flags(entry)
    ptr.isFormer := members(entry)(rank).isFormer
    ptr
  }

  private def mergedPsv(ptr: RobPtr, rank: UInt, base: UInt): UInt = {
    val fromEnq = io.enqPsv.map(x =>
      Mux(x.valid && x.bits.robIdx.isSameSlot(ptr) && x.bits.rank === rank, x.bits.mask, TeaPsvOps.empty))
    val fromEvents = io.events.map(x =>
      Mux(x.valid && x.bits.robIdx.isSameSlot(ptr), x.bits.mask, TeaPsvOps.empty))
    (Seq(base) ++ fromEnq ++ fromEvents).reduce(_ | _)
  }

  for (entry <- 0 until RobSize) {
    val entryAllocs = io.alloc.map(x => x.valid && x.bits.robIdx.value === entry.U)
    val allocating = VecInit(entryAllocs).asUInt.orR
    when(allocating) {
      flags(entry) := PriorityMux(entryAllocs, io.alloc.map(_.bits.robIdx.flag))
      allocationCycles(entry) := io.cycle
    }
    for (rank <- 0 until RenameWidth) {
      val ptr = ptrAt(entry, rank)
      val allocOH = io.alloc.map(x => x.valid && x.bits.robIdx.value === entry.U && x.bits.rank === rank.U)
      val allocValid = VecInit(allocOH).asUInt.orR
      val allocMember = Mux1H(allocOH, io.alloc.map(_.bits))
      val wbCount = PopCount(io.writeback.map(x =>
        x.valid && x.bits.robIdx.isSameSlot(ptr) && x.bits.rank === rank.U))
      visiblePsv(entry)(rank) := mergedPsv(ptr, rank.U, members(entry)(rank).psv)
      retired(entry)(rank) := live(entry)(rank) && VecInit(io.commits.map(x =>
        x.valid && x.bits.isSameSlot(ptr))).asUInt.orR
      killed(entry)(rank) := live(entry)(rank) && io.redirect.valid && ptr.needFlush(io.redirect)

      when(allocating) {
        live(entry)(rank) := allocValid
        sampled(entry)(rank) := false.B
        when(allocValid) {
          assert(PopCount(allocOH) === 1.U, "duplicate TEA architectural member")
          members(entry)(rank).isFormer := allocMember.robIdx.isFormer
          members(entry)(rank).owner := allocMember.owner
          members(entry)(rank).pc := io.addrTransType.extend(allocMember.pc, XLEN)
          members(entry)(rank).psv := mergedPsv(allocMember.robIdx, rank.U, allocMember.psv)
          members(entry)(rank).remaining := allocMember.numWB
        }
      }.elsewhen(live(entry)(rank)) {
        members(entry)(rank).psv := visiblePsv(entry)(rank)
        members(entry)(rank).remaining := Mux(wbCount >= members(entry)(rank).remaining,
          0.U, members(entry)(rank).remaining - wbCount)
        when(retired(entry)(rank) || killed(entry)(rank)) {
          live(entry)(rank) := false.B
          sampled(entry)(rank) := false.B
        }
      }
      when(io.markStalled && io.stalled.valid && io.head.value === entry.U &&
        flags(entry) === io.head.flag && io.stalled.bits.rank === rank.U &&
        !retired(entry)(rank) && !killed(entry)(rank)) {
        sampled(entry)(rank) := true.B
      }
    }
  }

  private def result(entry: UInt, rank: UInt): TeaMemberResult = {
    val res = Wire(new TeaMemberResult)
    res.target.allocationCycle := allocationCycles(entry)
    res.target.robIndex := entry
    res.target.rank := rank
    res.pc := members(entry)(rank).pc
    res.psv := visiblePsv(entry)(rank)
    res
  }

  private val headMembers = members(io.head.value)
  val headLive = live(io.head.value)
  val unfinished = VecInit((0 until RenameWidth).map { rank =>
    val sameSlotCount = PopCount((0 until RenameWidth).map(other =>
      headLive(other) && headMembers(other).isFormer === headMembers(rank).isFormer))
    val slotReady = io.slotReady(io.head.value)((!headMembers(rank).isFormer).asUInt)
    val ready = slotReady || sameSlotCount > 1.U && headMembers(headMembers(rank).owner).remaining === 0.U
    headLive(rank) && flags(io.head.value) === io.head.flag && !ready
  })
  io.stalled.valid := unfinished.asUInt.orR
  io.stalled.bits := result(io.head.value, PriorityEncoder(unfinished)).target
  when(io.markStalled && io.stalled.valid) {
    sampledEntry := io.head
  }

  for (port <- 0 until 2) {
    val ptr = io.readPtr(port)
    val mask = VecInit((0 until RenameWidth).map(rank =>
      live(ptr.value)(rank) && flags(ptr.value) === ptr.flag &&
        members(ptr.value)(rank).isFormer === ptr.isFormer))
    io.read(port) := result(ptr.value, PriorityEncoder(mask))
  }

  for (entryLane <- 0 until CommitWidth; rank <- 0 until RenameWidth) {
    val ptr = io.commits(2 * entryLane).bits
    val member = members(ptr.value)(rank)
    val matchingCommit = (0 until 2).map(slot => {
      val commit = io.commits(2 * entryLane + slot)
      commit.valid && commit.bits.flag === flags(ptr.value) &&
        commit.bits.isFormer === member.isFormer
    }).reduce(_ || _)
    val out = io.committed(entryLane * RenameWidth + rank)
    out.valid := live(ptr.value)(rank) && matchingCommit
    out.bits := result(ptr.value, rank.U)
  }

  // In-order retirement means only the sampled head entry can have live marked targets.
  for (rank <- 0 until RenameWidth) {
    val entry = sampledEntry.value
    val out = io.finals(rank)
    out.valid := live(entry)(rank) && sampled(entry)(rank) && flags(entry) === sampledEntry.flag &&
      (retired(entry)(rank) || killed(entry)(rank))
    out.bits.cycle := io.cycle
    out.bits.isDrain := false.B
    out.bits.cancelled := killed(entry)(rank) && !retired(entry)(rank)
    out.bits.member := result(entry, rank.U)
  }
}
