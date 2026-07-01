/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3.{Mem, Mux, Vec, _}
import chisel3.util._
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.backend.{BackendParams, IntERFallbackReason, IntERRobReadDoneStatus, IntERRobUopMeta, IntERSrcValueReadDone, IntERSTGuardDec}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput, UopIdx, EnqRobUop}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
import xiangshan.backend.fu.vector.Bundles.{Nf, VLmul, VSew, VType}
import xiangshan.backend.rename.SnapshotGenerator
import xiangshan.backend.trace._

import scala.collection.immutable.Nil



object RobBundles extends HasCircularQueuePtrHelper {
  class RobEntryBundle(implicit p: Parameters) extends XSBundle {

    // data begin
    val vls = Bool()
    // some instructions are not allowed to trigger interrupts
    // They have side effects on the states of the processor before they write back
    val interrupt_safe = Bool()
    val fpWen = Bool()
    val rfWen = Bool()
    val wflags = Bool()
    val dirtyVs = Bool()
    val commitType = CommitType()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val isRVC = Bool()
    // VTypeBuffer
    val needVTB = Bool()
    val isHls = Bool()
    // data end

    // trace
    val traceBlockInPipe = new TracePipe(IretireWidthEncoded)
    // status begin
    val valid = Bool()
    val fflags = UInt(5.W)
    val mmio = Bool()
    val vxsat = Bool()
    val realDestSize = UInt(log2Up(MaxUopSize + 1).W)
    val uopNum = UInt(log2Up(MaxUopSize + 1).W)
    val needFlush = Bool()
    val intER = Option.when(EnableIntEarlyRegRelease)(new IntERRobUopMeta)
    // status end

    // debug_begin
    val debug_pc         = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr      = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest      = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_move_src   = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_pdest      = OptionWrapper(backendParams.basicDebugEn, UInt(PhyRegIdxWidth.W))
    val debug_fuType     = OptionWrapper(backendParams.debugEn, FuType())
    val debug_fusionNum  = OptionWrapper(backendParams.debugEn, UInt(2.W))
    val debug_fuOpType   = OptionWrapper(backendParams.debugEn, FuOpType())
    val perfDebugInfo    = OptionWrapper(backendParams.debugEn, new PerfDebugInfo)
    val debug_lqIdx      = OptionWrapper(backendParams.debugEn, new LqPtr )
    val debug_sqIdx      = OptionWrapper(backendParams.debugEn, new SqPtr )
    val debug_rfWen      = OptionWrapper(backendParams.debugEn, Bool() )
    val debug_seqNum     = OptionWrapper(backendParams.debugEn, InstSeqNum() )
    val debug_sim_trig   = OptionWrapper(backendParams.debugEn, Bool() )
    val debug_vecWen     = OptionWrapper(backendParams.debugEn, Bool() )
    val debug_v0Wen      = OptionWrapper(backendParams.debugEn, Bool() )
    val debug_commitType = OptionWrapper(backendParams.debugEn, CommitType() )
    // debug_end
    // topdown
    val topdownIssued    = OptionWrapper(backendParams.debugEn, Bool())
    val topdownIssueTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))

    def isWritebacked: Bool = !uopNum.orR
    def isUopWritebacked: Bool = !uopNum.orR

  }

  class RobCommitEntryBundle(implicit p: Parameters) extends XSBundle {
    val walk_v = Bool()
    val commit_v = Bool()
    val commit_w = Bool()
    val realDestSize = UInt(log2Up(MaxUopSize + 1).W)
    val interrupt_safe = Bool()
    val wflags = Bool()
    val fflags = UInt(5.W)
    val vxsat = Bool()
    val isRVC = Bool()
    val needVTB = Bool()
    val isHls = Bool()
    val isVls = Bool()
    val vls = Bool()
    val mmio = Bool()
    val commitType = CommitType()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val fpWen = Bool()
    val rfWen = Bool()
    val needFlush = Bool()
    val intER = Option.when(EnableIntEarlyRegRelease)(new IntERRobUopMeta)
    // trace
    val traceBlockInPipe = new TracePipe(IretireWidthEncoded)
    // debug_begin
    val debug_pc = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_move_src = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_pdest = OptionWrapper(backendParams.basicDebugEn, UInt(PhyRegIdxWidth.W))
    val debug_otherPdest = OptionWrapper(backendParams.basicDebugEn, Vec(7, UInt(PhyRegIdxWidth.W)))
    val debug_fuType = OptionWrapper(backendParams.debugEn, FuType())
    val debug_fusionNum = OptionWrapper(backendParams.debugEn, UInt(2.W))
    // debug_end
    val dirtyFs = Bool()
    val dirtyVs = Bool()
  }

  def connectEnq(robEntry: RobEntryBundle, robEnq: EnqRobUop): Unit = {
    robEntry.wflags := robEnq.wfflags
    robEntry.commitType := robEnq.commitType
    robEntry.ftqIdx := robEnq.ftqPtr
    robEntry.ftqOffset := robEnq.ftqOffset
    robEntry.isRVC := robEnq.isRVC
    // robEntry.needVTB will be asserted by the first uop, so set it false here
    robEntry.needVTB := robEnq.isVset || robEnq.vpu.isVleff
    robEntry.isHls := robEnq.isHls
    robEntry.rfWen := robEnq.rfWen
    robEntry.fpWen := robEnq.dirtyFs
    robEntry.dirtyVs := robEnq.dirtyVs
    // flushPipe needFlush but not exception
    robEntry.needFlush := robEnq.hasException || robEnq.flushPipe
    // trace
    robEntry.traceBlockInPipe := robEnq.traceBlockInPipe
    robEntry.debug_ldest.foreach(_ := robEnq.ldest)
    robEntry.debug_move_src.foreach(_ := robEnq.moveSrcLReg)
    robEntry.debug_pdest.foreach(_ := robEnq.pdest)
    robEntry.debug_fuType.foreach(_ := robEnq.fuType)
    robEntry.debug_fuOpType.foreach(_ := robEnq.fuOpType)
    robEntry.debug_rfWen.foreach(_ := robEnq.rfWen)
    robEntry.debug_vecWen.foreach(_ := robEnq.vecWen)
    robEntry.debug_v0Wen.foreach(_ := robEnq.v0Wen)
    robEntry.debug_commitType.foreach(_ := robEnq.commitType)
    robEnq.debug.foreach { debug =>
      robEntry.debug_pc.foreach(_ := debug.pc)
      robEntry.debug_seqNum.foreach(_ := debug.debug_seqNum)
      robEntry.debug_instr.foreach(_ := debug.instr)
      robEntry.debug_fusionNum.foreach(_ := debug.fusionNum)
      robEntry.perfDebugInfo.foreach(_ := debug.perfDebugInfo)
      robEntry.debug_sim_trig.foreach(_ := debug.debug_sim_trig)
    }
    robEntry.intER.foreach { meta =>
      val robLocalIntERSafe = robEnq.firstUop && robEnq.lastUop && robEnq.numUops === 1.U
      val hasRobLocalTrackedIntER = VecInit(
        meta.src.indices.map(src => robEnq.intER.get.src(src).valid) ++
          Seq(robEnq.intER.get.dest.valid)
      ).asUInt.orR
      assert(
        !hasRobLocalTrackedIntER || robLocalIntERSafe,
        "ROB Int ER tracked metadata requires a single-uop entry"
      )
      for (src <- meta.src.indices) {
        meta.src(src).valid := robLocalIntERSafe && robEnq.intER.get.src(src).valid
        meta.src(src).trackId := robEnq.intER.get.src(src).trackId
        meta.src(src).trackGen := robEnq.intER.get.src(src).trackGen
        meta.src(src).srcIdx := robEnq.intER.get.src(src).srcIdx
        meta.src(src).psrc := robEnq.intER.get.src(src).psrc
        meta.src(src).readDone := false.B
      }
      meta.dest := Mux(robLocalIntERSafe, robEnq.intER.get.dest, 0.U.asTypeOf(meta.dest))
      meta.redef := Mux(robLocalIntERSafe, robEnq.intER.get.redef, 0.U.asTypeOf(meta.redef))
      meta.resolved := false.B
      meta.guardEmitted := false.B
    }
    robEntry.topdownIssued.foreach(_ := false.B)
    robEntry.topdownIssueTime.foreach(_ := 0.U)
  }

  def connectCommitEntry(robCommitEntry: RobCommitEntryBundle, robEntry: RobEntryBundle): Unit = {
    robCommitEntry.walk_v := robEntry.valid
    robCommitEntry.commit_v := robEntry.valid
    robCommitEntry.commit_w := robEntry.uopNum === 0.U
    robCommitEntry.realDestSize := robEntry.realDestSize
    robCommitEntry.interrupt_safe := robEntry.interrupt_safe
    robCommitEntry.rfWen := robEntry.rfWen
    robCommitEntry.fpWen := robEntry.fpWen
    robCommitEntry.fflags := robEntry.fflags
    robCommitEntry.wflags := robEntry.wflags
    robCommitEntry.vxsat := robEntry.vxsat
    robCommitEntry.isRVC := robEntry.isRVC
    robCommitEntry.needVTB := robEntry.needVTB
    robCommitEntry.isHls := robEntry.isHls
    robCommitEntry.isVls := robEntry.vls
    robCommitEntry.vls := robEntry.vls
    robCommitEntry.mmio := robEntry.mmio
    robCommitEntry.ftqIdx := robEntry.ftqIdx
    robCommitEntry.ftqOffset := robEntry.ftqOffset
    robCommitEntry.commitType := robEntry.commitType
    robCommitEntry.dirtyFs := robEntry.fpWen || robEntry.wflags
    robCommitEntry.dirtyVs := robEntry.dirtyVs
    robCommitEntry.needFlush := robEntry.needFlush
    robCommitEntry.intER.foreach(_ := robEntry.intER.get)
    robCommitEntry.traceBlockInPipe := robEntry.traceBlockInPipe
    robCommitEntry.debug_pc.foreach(_ := robEntry.debug_pc.get)
    robCommitEntry.debug_instr.foreach(_ := robEntry.debug_instr.get)
    robCommitEntry.debug_ldest.foreach(_ := robEntry.debug_ldest.get)
    robCommitEntry.debug_move_src.foreach(_ := robEntry.debug_move_src.get)
    robCommitEntry.debug_pdest.foreach(_ := robEntry.debug_pdest.get)
    robCommitEntry.debug_fuType.foreach(_ := robEntry.debug_fuType.get)
    robCommitEntry.debug_fusionNum.foreach(_ := robEntry.debug_fusionNum.get)
  }
}

object RobIntDiffOps {
  def selectStoredWriteData(
    storedValid: Seq[Bool],
    storedFlag: Seq[Bool],
    storedPdest: Seq[UInt],
    storedData: Seq[UInt],
    commitRobIdx: RobPtr,
    commitPdest: UInt
  ): UInt = {
    require(storedValid.length == storedFlag.length, "ROB direct integer diff stored valid/flag slot counts must match")
    require(storedValid.length == storedPdest.length, "ROB direct integer diff stored valid/pdest slot counts must match")
    require(storedValid.length == storedData.length, "ROB direct integer diff stored valid/data slot counts must match")

    val selected = WireDefault(0.U(storedData.head.getWidth.W))
    for (slot <- storedValid.indices.reverse) {
      when(storedValid(slot) && storedFlag(slot) === commitRobIdx.flag && storedPdest(slot) === commitPdest) {
        selected := storedData(slot)
      }
    }
    selected
  }

  def selectCommitWriteData(
    stored: UInt,
    commitRobIdx: RobPtr,
    commitPdest: UInt,
    writebackValid: Seq[Bool],
    writebackRobIdx: Seq[RobPtr],
    writebackPdest: Seq[UInt],
    writebackData: Seq[UInt]
  ): UInt = {
    require(writebackValid.length == writebackRobIdx.length, "ROB direct integer diff writeback valid/robIdx counts must match")
    require(writebackValid.length == writebackPdest.length, "ROB direct integer diff writeback valid/pdest counts must match")
    require(writebackValid.length == writebackData.length, "ROB direct integer diff writeback valid/data counts must match")

    val selected = WireDefault(stored)
    for (port <- writebackValid.indices.reverse) {
      when(
        writebackValid(port) &&
          writebackRobIdx(port).asUInt === commitRobIdx.asUInt &&
          writebackPdest(port) === commitPdest
      ) {
        selected := writebackData(port)
      }
    }
    selected
  }

  def selectExpandedCommitWriteData(
    shadowValid: Vec[Vec[Bool]],
    shadowFlag: Vec[Vec[Bool]],
    shadowPdest: Vec[Vec[UInt]],
    shadowData: Vec[Vec[UInt]],
    commitRobIdx: Seq[RobPtr],
    commitPdest: Seq[UInt],
    writebackValid: Seq[Bool],
    writebackRobIdx: Seq[RobPtr],
    writebackPdest: Seq[UInt],
    writebackData: Seq[UInt]
  ): Vec[UInt] = {
    require(commitRobIdx.length == commitPdest.length, "ROB direct integer diff expanded robIdx/pdest lane counts must match")
    require(writebackData.nonEmpty, "ROB direct integer diff expanded selector requires at least one writeback data port")
    val selected = Wire(Vec(commitPdest.length, UInt(writebackData.head.getWidth.W)))
    for (lane <- commitPdest.indices) {
      val ptr = commitRobIdx(lane).value
      val stored = selectStoredWriteData(
        storedValid = shadowValid(ptr),
        storedFlag = shadowFlag(ptr),
        storedPdest = shadowPdest(ptr),
        storedData = shadowData(ptr),
        commitRobIdx = commitRobIdx(lane),
        commitPdest = commitPdest(lane)
      )
      selected(lane) := selectCommitWriteData(
        stored = stored,
        commitRobIdx = commitRobIdx(lane),
        commitPdest = commitPdest(lane),
        writebackValid = writebackValid,
        writebackRobIdx = writebackRobIdx,
        writebackPdest = writebackPdest,
        writebackData = writebackData
      )
    }
    selected
  }

  def updateWriteDataShadow(
    shadowValid: Vec[Vec[Bool]],
    shadowFlag: Vec[Vec[Bool]],
    shadowPdest: Vec[Vec[UInt]],
    shadowData: Vec[Vec[UInt]],
    writebackValid: Seq[Bool],
    writebackRobIdx: Seq[RobPtr],
    writebackPdest: Seq[UInt],
    writebackData: Seq[UInt]
  ): Unit = {
    require(writebackValid.length == writebackRobIdx.length, "ROB direct integer diff writeback valid/robIdx counts must match")
    require(writebackValid.length == writebackPdest.length, "ROB direct integer diff writeback valid/pdest counts must match")
    require(writebackValid.length == writebackData.length, "ROB direct integer diff writeback valid/data counts must match")
    require(shadowValid.length == shadowPdest.length, "ROB direct integer diff shadow valid/pdest entry counts must match")
    require(shadowValid.length == shadowData.length, "ROB direct integer diff shadow valid/data entry counts must match")
    require(shadowValid.length == shadowFlag.length, "ROB direct integer diff shadow valid/flag entry counts must match")
    require(shadowValid.nonEmpty, "ROB direct integer diff write-data shadow must have entries")
    require(shadowValid.head.nonEmpty, "ROB direct integer diff write-data shadow must have slots")
    require(shadowFlag.forall(_.length == shadowValid.head.length), "ROB direct integer diff flag slots must match valid slots")
    require(shadowPdest.forall(_.length == shadowValid.head.length), "ROB direct integer diff pdest slots must match valid slots")
    require(shadowData.forall(_.length == shadowValid.head.length), "ROB direct integer diff data slots must match valid slots")

    var nextValid = Wire(chiselTypeOf(shadowValid))
    var nextFlag = Wire(chiselTypeOf(shadowFlag))
    var nextPdest = Wire(chiselTypeOf(shadowPdest))
    var nextData = Wire(chiselTypeOf(shadowData))
    nextValid := shadowValid
    nextFlag := shadowFlag
    nextPdest := shadowPdest
    nextData := shadowData

    val entryIdxWidth = log2Ceil(shadowValid.length max 2)
    val slotCount = shadowValid.head.length
    for (port <- writebackValid.indices.reverse) {
      val afterValid = Wire(chiselTypeOf(shadowValid))
      val afterFlag = Wire(chiselTypeOf(shadowFlag))
      val afterPdest = Wire(chiselTypeOf(shadowPdest))
      val afterData = Wire(chiselTypeOf(shadowData))
      afterValid := nextValid
      afterFlag := nextFlag
      afterPdest := nextPdest
      afterData := nextData

      val entryIdx = writebackRobIdx(port).value(entryIdxWidth - 1, 0)
      val entryIdxInRange = writebackRobIdx(port).value < shadowValid.length.U
      val matchOH = VecInit((0 until slotCount).map { slot =>
        nextValid(entryIdx)(slot) &&
          nextFlag(entryIdx)(slot) === writebackRobIdx(port).flag &&
          nextPdest(entryIdx)(slot) === writebackPdest(port)
      })
      val freeOH = VecInit((0 until slotCount).map(slot => !nextValid(entryIdx)(slot)))
      val updateOH = Mux(matchOH.asUInt.orR, matchOH.asUInt, PriorityEncoderOH(freeOH.asUInt))
      val hasSlot = matchOH.asUInt.orR || freeOH.asUInt.orR

      assert(!writebackValid(port) || entryIdxInRange, "ROB direct integer diff write-data shadow robIdx out of range")
      assert(!writebackValid(port) || hasSlot, "ROB direct integer diff write-data shadow has no free slot")

      when(writebackValid(port) && entryIdxInRange && hasSlot) {
        for (slot <- 0 until slotCount) {
          when(updateOH(slot)) {
            afterValid(entryIdx)(slot) := true.B
            afterFlag(entryIdx)(slot) := writebackRobIdx(port).flag
            afterPdest(entryIdx)(slot) := writebackPdest(port)
            afterData(entryIdx)(slot) := writebackData(port)
          }
        }
      }

      nextValid = afterValid
      nextFlag = afterFlag
      nextPdest = afterPdest
      nextData = afterData
    }

    shadowValid := nextValid
    shadowFlag := nextFlag
    shadowPdest := nextPdest
    shadowData := nextData
  }

  def clearWriteDataShadow(
    shadowValid: Vec[Vec[Bool]],
    shadowFlag: Vec[Vec[Bool]],
    clearValid: Seq[Bool],
    clearRobIdx: Seq[RobPtr],
    matchFlag: Boolean = true
  ): Unit = {
    require(clearValid.length == clearRobIdx.length, "ROB direct integer diff shadow clear valid/robIdx counts must match")
    require(shadowValid.length == shadowFlag.length, "ROB direct integer diff shadow clear valid/flag entry counts must match")
    require(shadowFlag.forall(_.length == shadowValid.head.length), "ROB direct integer diff shadow clear flag slots must match valid slots")
    val entryIdxWidth = log2Ceil(shadowValid.length max 2)
    for (lane <- clearValid.indices) {
      val entryIdx = clearRobIdx(lane).value(entryIdxWidth - 1, 0)
      val entryIdxInRange = clearRobIdx(lane).value < shadowValid.length.U
      assert(!clearValid(lane) || entryIdxInRange, "ROB direct integer diff write-data shadow clear robIdx out of range")
      when(clearValid(lane) && entryIdxInRange) {
        for (slot <- 0 until shadowValid.head.length) {
          when((!matchFlag).B || shadowFlag(entryIdx)(slot) === clearRobIdx(lane).flag) {
            shadowValid(entryIdx)(slot) := false.B
          }
        }
      }
    }
  }

  def updateShadow(
    current: Vec[UInt],
    next: Vec[UInt],
    commitData: Vec[UInt],
    valid: Seq[Bool],
    rfWen: Seq[Bool],
    isMove: Seq[Bool],
    ldest: Seq[UInt],
    moveSrc: Seq[UInt],
    writeData: Seq[UInt]
  ): Unit = {
    val numRegs = current.length
    require(numRegs == 32, "ROB direct integer diff shadow must model 32 architectural registers")
    require(next.length == numRegs, "ROB direct integer diff next shadow width must match current shadow")
    require(valid.length == rfWen.length, "ROB direct integer diff valid/rfWen lane counts must match")
    require(valid.length == isMove.length, "ROB direct integer diff valid/isMove lane counts must match")
    require(valid.length == ldest.length, "ROB direct integer diff valid/ldest lane counts must match")
    require(valid.length == moveSrc.length, "ROB direct integer diff valid/moveSrc lane counts must match")
    require(valid.length == writeData.length, "ROB direct integer diff valid/writeData lane counts must match")
    require(commitData.length == valid.length, "ROB direct integer diff commit data lane count must match")

    var shadow = current

    for (lane <- valid.indices) {
      val srcIdx = moveSrc(lane)(log2Ceil(numRegs) - 1, 0)
      val dstIdx = ldest(lane)(log2Ceil(numRegs) - 1, 0)
      val moveData = Mux1H(UIntToOH(srcIdx, numRegs), shadow)
      val laneData = Mux(isMove(lane), moveData, writeData(lane))
      val write = valid(lane) && rfWen(lane) && dstIdx =/= 0.U
      val nextShadow = Wire(Vec(numRegs, UInt(current.head.getWidth.W)))

      commitData(lane) := laneData
      for (reg <- 0 until numRegs) {
        nextShadow(reg) := Mux(write && dstIdx === reg.U, laneData, shadow(reg))
      }
      nextShadow(0) := 0.U
      shadow = nextShadow
    }

    next := shadow
    next(0) := 0.U
  }
}

object RobIntEROps {
  def assertGuardEmittedRedefNotFlushed(
    entry: RobBundles.RobEntryBundle,
    invalidatedByRedirect: Bool
  )(implicit p: Parameters): Unit = {
    val guardEmittedRedefFlushed =
      entry.valid && invalidatedByRedirect && entry.intER.get.redef.valid && entry.intER.get.guardEmitted
    assert(!guardEmittedRedefFlushed, "ROB ER guard-emitted redefiner flushed by redirect")
  }

  def emitSTGuardDec(
    out: Vec[ValidIO[IntERSTGuardDec]],
    markGuardEmitted: Vec[Bool],
    cursor: RobPtr,
    stop: Bool,
    entries: Vec[RobBundles.RobEntryBundle],
    safeToCross: Vec[Bool]
  )(implicit p: Parameters): UInt = {
    require(out.length >= 1, "ROB ER speculation tracker must have at least one output lane")
    require(markGuardEmitted.length == entries.length, "ROB ER guard-emitted mark width must match entry count")
    require(safeToCross.length == entries.length, "ROB ER safe-to-cross width must match entry count")

    out := 0.U.asTypeOf(out)
    markGuardEmitted := 0.U.asTypeOf(markGuardEmitted)

    val entryIdxWidth = log2Ceil(entries.length max 2)
    val laneCanCross = Wire(Vec(out.length, Bool()))
    for (lane <- out.indices) {
      val ptr = cursor + lane.U
      val idx = ptr.value(entryIdxWidth - 1, 0)
      val entry = entries(idx)
      val inRange = ptr.value < entries.length.U
      val olderClear = if (lane == 0) {
        !stop
      } else {
        !stop && laneCanCross.take(lane).foldLeft(true.B)(_ && _)
      }
      val canCross = olderClear && inRange && entry.valid && safeToCross(idx) && !entry.needFlush
      val emit = canCross && entry.intER.get.redef.valid && !entry.intER.get.guardEmitted

      laneCanCross(lane) := canCross
      out(lane).valid := emit
      out(lane).bits.valid := emit
      out(lane).bits.robIdx := ptr
      out(lane).bits.trackId := entry.intER.get.redef.trackId
      out(lane).bits.trackGen := entry.intER.get.redef.trackGen
      out(lane).bits.oldPdest := entry.intER.get.redef.oldPdest
      out(lane).bits.fallback := false.B
      out(lane).bits.reason := IntERFallbackReason.none
    }

    for (entryIdx <- entries.indices) {
      val hits = out.indices.map { lane =>
        val ptr = cursor + lane.U
        out(lane).valid && ptr.value === entryIdx.U
      }
      markGuardEmitted(entryIdx) := VecInit(hits).asUInt.orR
    }

    PopCount(laneCanCross)
  }

  def validateReadDoneEvents(
    out: Vec[ValidIO[IntERSrcValueReadDone]],
    markReadDone: Vec[Vec[Bool]],
    raw: Vec[ValidIO[IntERSrcValueReadDone]],
    redirect: Valid[Redirect],
    entries: Vec[RobBundles.RobEntryBundle],
    status: Option[Vec[IntERRobReadDoneStatus]] = None
  )(implicit p: Parameters): Unit = {
    val logicalSrcWidth = p(XSCoreParamsKey).backendParams.numSrc
    require(out.length == raw.length, "ROB ER readDone output width must match raw input width")
    require(markReadDone.length == entries.length, "ROB ER readDone mark width must match entry count")
    require(markReadDone.forall(_.length == logicalSrcWidth), "ROB ER readDone marks must use full logical source width")

    out := 0.U.asTypeOf(out)
    markReadDone := 0.U.asTypeOf(markReadDone)

    status.foreach(_ := 0.U.asTypeOf(status.get))

    var earlierAccepted = Seq.empty[(Int, Int, Bool)]
    val sawSource = Seq.tabulate(raw.length) { lane =>
      VecInit(raw(lane).bits.src.map(src => raw(lane).valid && src.valid)).asUInt.orR
    }
    val duplicateSource = Wire(Vec(raw.length, Vec(logicalSrcWidth, Bool())))
    duplicateSource := 0.U.asTypeOf(duplicateSource)
    val accepted = Seq.tabulate(raw.length) { lane =>
      Seq.tabulate(logicalSrcWidth) { slot =>
        val srcEvent = raw(lane).bits.src(slot)
        val entryIdxWidth = log2Ceil(entries.length max 2)
        val entryIdx = raw(lane).bits.robIdx.value(entryIdxWidth - 1, 0)
        val robEntry = entries(entryIdx)
        val entryIdxInRange = raw(lane).bits.robIdx.value < entries.length.U
        val killedByRedirect = raw(lane).bits.robIdx.needFlush(redirect)
        val srcIdxInRange = srcEvent.srcIdx < logicalSrcWidth.U
        val validLiveSource = raw(lane).valid && srcEvent.valid && !killedByRedirect
        assert(!validLiveSource || srcIdxInRange, "ROB ER readDone source index out of range")
        val safeSrcIdx = Mux(srcIdxInRange, srcEvent.srcIdx, 0.U)
        val stored = robEntry.intER.get.src(safeSrcIdx)
        val duplicate = if (earlierAccepted.isEmpty) {
          false.B
        } else {
          VecInit(earlierAccepted.map { case (prevLane, prevSlot, prevAccepted) =>
            prevAccepted &&
              raw(prevLane).bits.robIdx.value === raw(lane).bits.robIdx.value &&
              raw(prevLane).bits.src(prevSlot).srcIdx === srcEvent.srcIdx
          }).asUInt.orR
        }
        val storedMatches =
          entryIdxInRange &&
            !killedByRedirect &&
            robEntry.valid &&
            srcIdxInRange &&
            stored.valid &&
            stored.trackId === srcEvent.trackId &&
            stored.trackGen === srcEvent.trackGen &&
            stored.psrc === srcEvent.psrc &&
            !stored.readDone
        val acceptedSource = raw(lane).valid && srcEvent.valid && storedMatches && !duplicate
        duplicateSource(lane)(slot) := raw(lane).valid && srcEvent.valid && duplicate
        earlierAccepted = earlierAccepted :+ (lane, slot, acceptedSource)
        acceptedSource
      }
    }

    for (lane <- raw.indices) {
      out(lane).bits := raw(lane).bits
      for (slot <- 0 until logicalSrcWidth) {
        out(lane).bits.src(slot).valid := accepted(lane)(slot)
      }
      out(lane).valid := VecInit(accepted(lane)).asUInt.orR
      status.foreach { s =>
        val acceptedLane = VecInit(accepted(lane)).asUInt.orR
        val duplicateLane = duplicateSource(lane).asUInt.orR && !acceptedLane
        s(lane).sawRaw := sawSource(lane)
        s(lane).accepted := acceptedLane
        s(lane).fallback := acceptedLane && raw(lane).bits.fallback
        s(lane).duplicate := duplicateLane
        s(lane).stale := sawSource(lane) && !acceptedLane && !duplicateLane
      }
    }

    for (entryIdx <- entries.indices) {
      for (slot <- 0 until logicalSrcWidth) {
        val hits = accepted.zipWithIndex.flatMap { case (acceptedLane, lane) =>
          acceptedLane.zipWithIndex.map { case (acceptedSource, sourceSlot) =>
            acceptedSource &&
              raw(lane).bits.robIdx.value === entryIdx.U &&
              raw(lane).bits.src(sourceSlot).srcIdx === slot.U
          }
        }
        markReadDone(entryIdx)(slot) := VecInit(hits).asUInt.orR
      }
    }
  }
}

import RobBundles._

class RobPtr(entries: Int) extends CircularQueuePtr[RobPtr](
  entries
) with HasCircularQueuePtrHelper {

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).RobSize)

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
    redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR

  def lineHeadPtr(implicit p: Parameters): RobPtr = {
    val CommitWidth = p(XSCoreParamsKey).CommitWidth
    val out = Wire(new RobPtr)
    out.flag := this.flag
    out.value := Cat(this.value(this.PTR_WIDTH-1, log2Up(CommitWidth)), 0.U(log2Up(CommitWidth).W))
    out
  }

}

object RobPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RobPtr = {
    val ptr = Wire(new RobPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class RobCSRIO(implicit p: Parameters) extends XSBundle {
  val intrBitSet = Input(Bool())
  val trapTarget = Input(new TargetPCBundle)
  val wfiEvent   = Input(Bool())
  val criticalErrorState = Input(Bool())

  val fflags     = Output(Valid(UInt(5.W)))
  val vxsat      = Output(Valid(Bool()))
  val vstart     = Output(Valid(UInt(XLEN.W)))
  val dirty_fs   = Output(Bool())
  val dirty_vs   = Output(Bool())
  val perfinfo   = new Bundle {
    val retiredInstr = Output(UInt(7.W))
  }
}

class RobLsqIO(implicit p: Parameters) extends XSBundle {
  val lcommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val scommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val commit = Output(Bool())
  val pendingPtr = Output(new RobPtr)
  val pendingPtrNext = Output(new RobPtr)

  val mmioBusy = Input(Bool())
}

class RobEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val canAcceptForDispatch = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for robIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new EnqRobUop)))
  val resp = Vec(RenameWidth, Output(new RobPtr))
}

class RobCoreTopDownIO(implicit p: Parameters) extends XSBundle {
  val robHeadVaddr = Valid(UInt(VAddrBits.W))
  val robHeadPaddr = Valid(UInt(PAddrBits.W))
}

class RobDispatchTopDownIO extends Bundle {
  val robTrueCommit = Output(UInt(64.W))
  val robHeadLsIssue = Output(Bool())
}

class RobDebugRollingIO extends Bundle {
  val robTrueCommit = Output(UInt(64.W))
}

class RobExceptionInfo(exceptList: Seq[Int]=ExceptionNO.all)(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val robIdx = new RobPtr
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  // set 1 if there is 1 exists in exceptionVec
  val hasException = Bool()
  // This signal is valid iff currentValid is true
  // 0: is execute exception, 1: is fetch exception
  val isEnqExcp = Bool()
  val exceptionVec = ExceptSparseVec(exceptList)
  val isFetchMalAddr = Bool()
  val flushPipe = Bool()
  val isVset = Bool()
  val replayInst = Bool() // redirect to that inst itself
  val singleStep = Bool() // TODO add frontend hit beneath
  val crossPageIPFFix = Bool()
  val trigger = TriggerAction()
  // if vstart is udpated by vector unit
  val vstartEn = Bool()
  val vstart = UInt(XLEN.W)
  val vuopIdx = UopIdx()
  val isVecLoad = Bool()
  val isVlm = Bool()
  val isStrided = Bool()
  val isIndexed = Bool()
  val isWhole = Bool()
  val nf = Nf()
  val vsew = VSew()
  val veew = VSew()
  val vlmul = VLmul()

  def has_exception = hasException || flushPipe || singleStep || replayInst || TriggerAction.isDmode(trigger)
  def not_commit = hasException || singleStep || replayInst || TriggerAction.isDmode(trigger)
  // only exceptions are allowed to writeback when enqueue
  def can_writeback = hasException || singleStep || TriggerAction.isDmode(trigger)
}

class RobFlushInfo(implicit p: Parameters) extends XSBundle {
  val ftqIdx = new FtqPtr
  val robIdx = new RobPtr
  val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  val replayInst = Bool()
}
