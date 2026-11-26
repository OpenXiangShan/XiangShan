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
package xiangshan.mem

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._
import xiangshan.cache._

class LoadQueueRAR(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(Valid(new Redirect))

    // violation query
    val query = Vec(LoadPipelineWidth, Flipped(new LoadRARNukeQuery()))

    // release cacheline
    val release = Flipped(Valid(new Release))

    // dequeue boundaries
    val ldWbPtr = Input(new LqPtr)
    val robPendingPtr = Input(new RobPtr)

    // global
    val lqFull = Output(Bool())

    val validCount = Output(UInt(log2Ceil(LoadQueueRARSize + 1).W))
  })

  private val LineOffsetBits = 6 // Byte offset within a 64-byte cache line.
  private val PAddrHashBits = 16
  private val LowHashBits = (PAddrHashBits - LineOffsetBits) / 2
  private val HighHashBits = PAddrHashBits - LowHashBits

  // The low hash bits fold address bits [15:6] in pairs from opposite ends.
  private val lowHashInputs = Seq.tabulate(LowHashBits) { bit =>
    Seq(LineOffsetBits + bit, PAddrHashBits - 1 - bit)
  }
  // The high hash bits fold four 11-bit groups, skipping bits beyond the physical address width.
  private val highHashInputs = Seq.tabulate(HighHashBits) { bit =>
    Seq.tabulate(4)(group => LineOffsetBits + bit + group * HighHashBits).filter(_ < PAddrBits)
  }
  private val hashInputs = lowHashInputs ++ highHashInputs
  private def hashLineAddress(paddr: UInt): UInt =
    VecInit(hashInputs.map(inputs => inputs.map(bit => paddr(bit)).reduce(_ ^ _))).asUInt

  println("LoadQueueRAR: size: " + LoadQueueRARSize)

  private val ports = 0 until LoadPipelineWidth
  private val slots = 0 until LoadQueueRARSize
  private val slotWidth = log2Ceil(LoadQueueRARSize)

  object SlotState extends ChiselEnum {
    val free, reserved, live, recovering = Value
  }
  import SlotState._

  // Table state is shared by all pipeline stages.
  val state = RegInit(VecInit(slots.map(_ => free)))
  val robUpper = Reg(Vec(LoadQueueRARSize, new RobPtr))
  val lqUpper = Reg(Vec(LoadQueueRARSize, new LqPtr))
  val tag = Reg(Vec(LoadQueueRARSize, UInt(PAddrHashBits.W)))
  val released = Reg(Vec(LoadQueueRARSize, Bool()))
  val shared = Reg(Vec(LoadQueueRARSize, Bool()))
  val freeMask = VecInit(state.map(_ === free)).asUInt
  val tracked = VecInit(state.map(s => s === live || s === recovering))
  val flushed = robUpper.map(_.needFlush(io.redirect))
  val lqCompleted = lqUpper.map(ptr => !isBefore(io.ldWbPtr, ptr))

  val releaseTag = hashLineAddress(io.release.bits.paddr)
  val previousReleaseTag = RegEnable(releaseTag, io.release.valid)
  val previousReleaseValid = RegNext(io.release.valid, false.B)

  // S2 pipeline registers, written by S1.
  val s2HasReservation = RegInit(VecInit(ports.map(_ => false.B)))
  val s2ReservedSlot = Reg(Vec(LoadPipelineWidth, UInt(slotWidth.W)))
  val s2RobOlder = Reg(Vec(LoadPipelineWidth, Vec(LoadPipelineWidth, Bool())))
  val s2LqOlder = Reg(Vec(LoadPipelineWidth, Vec(LoadPipelineWidth, Bool())))

  // S3 pipeline registers, written by S2. Only a group's leader holds a target.
  class RarIntent extends Bundle {
    val robIdx = new RobPtr
    val lqIdx = new LqPtr
    val tag = UInt(PAddrHashBits.W)
    val released = Bool()
  }
  val s3Intent = Reg(Vec(LoadPipelineWidth, new RarIntent))
  val s3IntentValid = RegInit(VecInit(ports.map(_ => false.B)))
  val s3GroupMembers = RegInit(VecInit(ports.map(_ => 0.U(LoadPipelineWidth.W))))
  val s3Target = Reg(Vec(LoadPipelineWidth, UInt(slotWidth.W)))
  val s3NewEntry = Reg(Vec(LoadPipelineWidth, Bool()))
  val s3PrivateHead = Reg(Vec(LoadPipelineWidth, Bool()))
  val s3RobOlder = Reg(Vec(LoadPipelineWidth, Vec(LoadPipelineWidth, Bool())))
  val s3LqOlder = Reg(Vec(LoadPipelineWidth, Vec(LoadPipelineWidth, Bool())))
  val s3Violation = Reg(Vec(LoadPipelineWidth, UInt((LoadQueueRARSize + LoadPipelineWidth).W)))
  val s3ResponseValid = RegInit(VecInit(ports.map(_ => false.B)))
  val s3TargetOH = ports.map(port => Mux(s3GroupMembers(port).orR,
    UIntToOH(s3Target(port), LoadQueueRARSize), 0.U))
  val s3TargetMask = s3TargetOH.reduce(_ | _)

  // S4 private-head registers. Their lifetime ends before the slot can be reused.
  val s4HeadValid = RegInit(VecInit(ports.map(_ => false.B)))
  val s4HeadSlot = Reg(Vec(LoadPipelineWidth, UInt(slotWidth.W)))
  val s4HeadOH = ports.map(port => Mux(s4HeadValid(port), UIntToOH(s4HeadSlot(port), LoadQueueRARSize), 0.U))
  val s4ProtectedMask = s4HeadOH.reduce(_ | _)
  val s4RevokeMask = ports.map(port =>
    Mux(io.query(port).revokeLastLastCycle, s4HeadOH(port), 0.U)).reduce(_ | _)

  // S1 — reserve slots and compare the incoming ports' ages.
  val s1Request = io.query.map(_.s1Reserve)
  val s1NeedsReservation = s1Request.map(r => r.valid && isAfter(r.bits.lqIdx, io.ldWbPtr) &&
    !r.bits.robIdx.needFlush(io.redirect))
  val freeList = Module(new BitMapFreeList(LoadQueueRARSize, LoadPipelineWidth))
  freeList.io.freeBitmap := freeMask
  val s1Rank = Wire(Vec(LoadPipelineWidth, UInt(log2Up(LoadPipelineWidth).W)))
  for (port <- ports) s1Rank(port) := PopCount(s1NeedsReservation.take(port))
  val s1Granted = VecInit(ports.map(port =>
    s1NeedsReservation(port) && freeList.io.canAllocate(s1Rank(port))))
  val s1AllocatedSlot = s1Rank.map(rank => freeList.io.allocateSlot(rank))
  val s1AllocateOH = ports.map(port => Mux(s1Granted(port),
    UIntToOH(s1AllocatedSlot(port), LoadQueueRARSize), 0.U))
  freeList.io.allocateCount := PopCount(s1Granted)

  val s1RobOlder = VecInit(ports.map(i => VecInit(ports.map { j =>
    val a = s1Request(i).bits
    val b = s1Request(j).bits
    a.robIdx.isBeforeSlot(b.robIdx) || (a.robIdx.isSameSlot(b.robIdx) &&
      (isBefore(a.lqIdx, b.lqIdx) || (a.lqIdx === b.lqIdx && (i < j).B)))
  })))
  val s1LqOlder = VecInit(ports.map(i => VecInit(ports.map { j =>
    val a = s1Request(i).bits.lqIdx
    val b = s1Request(j).bits.lqIdx
    isBefore(a, b) || (a === b && (i < j).B)
  })))

  s2HasReservation := s1Granted
  s2RobOlder := s1RobOlder
  s2LqOlder := s1LqOlder
  for (port <- ports) {
    when (s1Granted(port)) { s2ReservedSlot(port) := s1AllocatedSlot(port) }
  }

  // S2 — query the table and the registered S3 bypass, then choose each group's target.
  val s2Request = io.query.map(_.req)
  val s2Tag = s2Request.map(r => hashLineAddress(r.bits.paddr))
  val s2NeedsEntry = s2Request.map(r => r.valid && isAfter(r.bits.lqIdx, io.ldWbPtr) &&
    !r.bits.robIdx.needFlush(io.redirect))
  val s2Accepted = VecInit(ports.map(port => s2NeedsEntry(port) && s2HasReservation(port)))
  val s2MergeableMask = VecInit(slots.map(i => state(i) === live && !flushed(i) && !lqCompleted(i))).asUInt &
    ~(s3TargetMask | s4ProtectedMask)
  val s2RetainReservation = Wire(Vec(LoadPipelineWidth, Bool()))
  val s2RetainedMask = ports.map(port => Mux(s2RetainReservation(port),
    UIntToOH(s2ReservedSlot(port), LoadQueueRARSize), 0.U)).reduce(_ | _)

  for (port <- ports) {
    val request = s2Request(port)
    request.ready := !s2NeedsEntry(port) || s2HasReservation(port)
    val tableMatch = VecInit(tag.map(_ === s2Tag(port)))
    val tableViolation = slots.map(i => tableMatch(i) && tracked(i) && released(i) &&
      robUpper(i).isAfterSlot(request.bits.robIdx))
    val pendingViolation = ports.map(i => s3IntentValid(i) && s3Intent(i).tag === s2Tag(port) &&
      s3Intent(i).released && s3Intent(i).robIdx.isAfterSlot(request.bits.robIdx))
    s3Violation(port) := VecInit(tableViolation ++ pendingViolation).asUInt
    s3ResponseValid(port) := request.valid

    val privateHead = io.query(port).mayLateRevoke
    val groupMembers = VecInit(ports.map(j => s2Accepted(j) && ((j == port).B ||
      (!privateHead && !io.query(j).mayLateRevoke && s2Tag(port) === s2Tag(j))))).asUInt
    val olderPeers = VecInit(ports.map(j => s2RobOlder(j)(port))).asUInt
    val groupLeader = s2Accepted(port) && !(groupMembers & olderPeers).orR
    val existingMatch = tableMatch.asUInt & s2MergeableMask
    val newEntry = privateHead || !existingMatch.orR
    s2RetainReservation(port) := groupLeader && newEntry

    s3GroupMembers(port) := Mux(groupLeader, groupMembers, 0.U)
    s3IntentValid(port) := s2Accepted(port)
    when (groupLeader) {
      s3Target(port) := Mux(newEntry, s2ReservedSlot(port), PriorityEncoder(existingMatch))
      s3NewEntry(port) := newEntry
      s3PrivateHead(port) := privateHead
    }
    when (s2Accepted(port)) {
      s3Intent(port).robIdx := request.bits.robIdx
      s3Intent(port).lqIdx := request.bits.lqIdx
      s3Intent(port).tag := s2Tag(port)
      s3Intent(port).released := request.bits.nc || request.bits.dataValid &&
        ((io.release.valid && releaseTag === s2Tag(port)) ||
          (previousReleaseValid && previousReleaseTag === s2Tag(port)))
    }
    XSError(s2Accepted(port) && (state(s2ReservedSlot(port)) =/= reserved ||
      !robUpper(s2ReservedSlot(port)).isSameSlot(request.bits.robIdx)),
      "RAR: S2 request does not own its S1 reservation")
  }
  s3RobOlder := s2RobOlder
  s3LqOlder := s2LqOlder

  // S3 — respond to the query and commit only the surviving members.
  val s3Survives = VecInit(ports.map(port => s3IntentValid(port) && !io.query(port).revokeLastCycle &&
    !s3Intent(port).robIdx.needFlush(io.redirect) && isAfter(s3Intent(port).lqIdx, io.ldWbPtr)))
  val s3Writes = Wire(Vec(LoadPipelineWidth, Bool()))
  val s3GroupRob = Wire(Vec(LoadPipelineWidth, new RobPtr))
  val s3GroupLq = Wire(Vec(LoadPipelineWidth, new LqPtr))
  val s3GroupReleased = Wire(Vec(LoadPipelineWidth, Bool()))
  val s3GroupShared = Wire(Vec(LoadPipelineWidth, Bool()))
  for (port <- ports) {
    io.query(port).resp.valid := s3ResponseValid(port)
    io.query(port).resp.bits.nuke := ParallelORR(s3Violation(port))

    val remaining = s3GroupMembers(port) & s3Survives.asUInt
    val youngestRob = VecInit(ports.map(i => remaining(i) && !(s3RobOlder(i).asUInt & remaining).orR))
    val youngestLq = VecInit(ports.map(i => remaining(i) && !(s3LqOlder(i).asUInt & remaining).orR))
    s3Writes(port) := remaining.orR
    s3GroupRob(port) := Mux1H(youngestRob, s3Intent.map(_.robIdx))
    s3GroupLq(port) := Mux1H(youngestLq, s3Intent.map(_.lqIdx))
    s3GroupReleased(port) := (remaining & VecInit(s3Intent.map(_.released)).asUInt).orR
    s3GroupShared(port) := PopCount(remaining) > 1.U

    s4HeadValid(port) := s3Writes(port) && s3PrivateHead(port)
    when (s3Writes(port) && s3PrivateHead(port)) { s4HeadSlot(port) := s3Target(port) }
    XSError(s3GroupMembers(port).orR && Mux(s3NewEntry(port),
      state(s3Target(port)) =/= reserved, state(s3Target(port)) =/= live),
      "RAR: S3 destination changed while its group was in flight")
    XSError(s3Writes(port) && (PopCount(youngestRob) =/= 1.U || PopCount(youngestLq) =/= 1.U),
      "RAR: group ages must select one ROB upper bound and one LQ upper bound")
    XSError(s3IntentValid(port) && PopCount(s3GroupMembers.map(_(port))) =/= 1.U,
      "RAR: request must belong to one group")
  }

  // Each row has one state update, combining reservation, commit and retirement.
  val recover = VecInit(slots.map(i => flushed(i) && (state(i) === recovering || state(i) === live && shared(i))))
  val retireByRob = VecInit(slots.map(i => state(i) === recovering &&
    robUpper(i).isNotAfterSlot(io.robPendingPtr) && !recover(i)))
  val inserted = Wire(Vec(LoadQueueRARSize, Bool()))
  for (i <- slots) {
    val allocationPorts = VecInit(ports.map(port => s1AllocateOH(port)(i)))
    val writingGroups = VecInit(ports.map(port => s3Writes(port) && s3TargetOH(port)(i)))
    val hasWriter = writingGroups.asUInt.orR
    val retire = state(i) === live && (flushed(i) || lqCompleted(i) || s4RevokeMask(i)) || retireByRob(i)
    val keepOld = tracked(i) && (!retire || recover(i))
    inserted(i) := hasWriter && !keepOld

    // A delayed release belongs to a continuing row or its matching new occupants.
    val incomingTag = Mux1H(writingGroups, s3Intent.map(_.tag))
    val markReleased = RegNext(io.release.valid &&
      (keepOld && tag(i) === releaseTag || hasWriter && incomingTag === releaseTag), false.B)
    when (allocationPorts.asUInt.orR) {
      state(i) := reserved
      robUpper(i) := Mux1H(allocationPorts, s1Request.map(_.bits.robIdx))
    }.elsewhen (recover(i)) {
      state(i) := recovering
      robUpper(i) := io.redirect.bits.robIdx
    }.elsewhen (hasWriter) {
      val incomingRob = Mux1H(writingGroups, s3GroupRob)
      val incomingLq = Mux1H(writingGroups, s3GroupLq)
      state(i) := live
      when (!keepOld || incomingRob.isAfterSlot(robUpper(i))) { robUpper(i) := incomingRob }
      when (!keepOld || isAfter(incomingLq, lqUpper(i))) { lqUpper(i) := incomingLq }
      when (!keepOld) { tag(i) := incomingTag }
      shared(i) := keepOld || (writingGroups.asUInt & s3GroupShared.asUInt).orR
    }.elsewhen (retire || state(i) === reserved && (!s2RetainedMask(i) || flushed(i))) {
      state(i) := free
    }
    when (hasWriter) {
      released(i) := (writingGroups.asUInt & s3GroupReleased.asUInt).orR ||
        keepOld && (released(i) || markReleased)
    }.elsewhen (markReleased) {
      released(i) := true.B
    }

    XSError(PopCount(ports.map(port => s3TargetOH(port)(i))) > 1.U,
      "RAR: independent groups chose the same target")
    XSError(hasWriter && (state(i) === free || state(i) === recovering || s4ProtectedMask(i) ||
      (state(i) === reserved && flushed(i))), "RAR: surviving group lost ownership of its target")
  }

  val allocatedCount = PopCount(tracked)
  val freeCount = PopCount(freeMask)
  io.lqFull := freeCount < (2 * LoadPipelineWidth).U
  io.validCount := allocatedCount
  val queryFireCount = PopCount(s2Request.map(_.fire))
  val ldLdViolationCount = PopCount(io.query.map(q => q.resp.valid && q.resp.bits.nuke))
  val insertCount = PopCount(inserted)
  val commitCount = PopCount(s3Survives)
  val writerCount = PopCount(s3Writes)
  QueuePerf(LoadQueueRARSize, allocatedCount, allocatedCount > (LoadQueueRARSize - LoadPipelineWidth).U)
  XSPerfAccumulate("enq", queryFireCount)
  XSPerfAccumulate("ld_ld_violation", ldLdViolationCount)
  XSPerfAccumulate("reserve", PopCount(s1Granted))
  XSPerfAccumulate("cancel", PopCount(s2HasReservation) - PopCount(s2Accepted) +&
    (PopCount(s3IntentValid) - commitCount))
  XSPerfAccumulate("insert", insertCount)
  XSPerfAccumulate("merge_existing", writerCount - insertCount)
  XSPerfAccumulate("merge_peer", commitCount - writerCount)
  XSPerfAccumulate("merge_saved_high", Mux(freeCount <= (LoadQueueRARSize / 4).U, commitCount - insertCount, 0.U))
  XSPerfAccumulate("reserve_nack", PopCount(s2NeedsEntry) - PopCount(s2Accepted))
  XSPerfAccumulate("pending_release_hit", PopCount(ports.map(port =>
    s3Survives(port) && io.release.valid && s3Intent(port).tag === releaseTag)))
  XSPerfAccumulate("merged_redirect", PopCount(recover))
  XSPerfAccumulate("retire_by_rob", PopCount(retireByRob))
  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", queryFireCount),
    ("ld_ld_violation", ldLdViolationCount)
  )
  generatePerfEvent()
}
