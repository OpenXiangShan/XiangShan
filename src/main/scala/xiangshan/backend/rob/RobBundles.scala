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
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput, UopIdx, EnqRobUop}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
import xiangshan.backend.fu.vector.Bundles.{Nf, VLmul, VSew, VType}
import xiangshan.backend.rename.{SnapshotGenerator, CompressType}
import xiangshan.backend.trace._

import scala.collection.immutable.Nil



object RobBundles extends HasCircularQueuePtrHelper {
  def NormalUopNumWidth(implicit p: Parameters): Int = log2Up(p(XSCoreParamsKey).MaxUopSize + 1)
  def CompressedSlotUopNumWidth(implicit p: Parameters): Int = log2Ceil(2 * p(XSCoreParamsKey).RenameWidth)
  def PackedUopStateWidth(implicit p: Parameters): Int = {
    math.max(NormalUopNumWidth + 1, 2 * CompressedSlotUopNumWidth)
  }

  private def fitUopStateWidth(value: UInt, width: Int): UInt = {
    val valueWidth = value.getWidth
    if (valueWidth == width) {
      value
    } else if (valueWidth > width) {
      value(width - 1, 0)
    } else {
      Cat(0.U((width - valueWidth).W), value)
    }
  }

  def encodeUopState(entryPairType: UInt, formerUopNum: UInt, latterUopNum: UInt)(implicit p: Parameters): UInt = {
    val normalState = fitUopStateWidth(formerUopNum, PackedUopStateWidth)
    val compressedState = fitUopStateWidth(
      Cat(
        fitUopStateWidth(latterUopNum, CompressedSlotUopNumWidth),
        fitUopStateWidth(formerUopNum, CompressedSlotUopNumWidth)
      ),
      PackedUopStateWidth
    )
    Mux(CompressType.isNORMAL(entryPairType), normalState, compressedState)
  }

  def decodeFormerUopNum(entryPairType: UInt, uopState: UInt)(implicit p: Parameters): UInt = {
    Mux(
      CompressType.isNORMAL(entryPairType),
      uopState(NormalUopNumWidth - 1, 0),
      fitUopStateWidth(uopState(CompressedSlotUopNumWidth - 1, 0), NormalUopNumWidth)
    )
  }

  def decodeLatterUopNum(entryPairType: UInt, uopState: UInt)(implicit p: Parameters): UInt = {
    Mux(
      CompressType.isNORMAL(entryPairType),
      0.U(NormalUopNumWidth.W),
      fitUopStateWidth(
        uopState(2 * CompressedSlotUopNumWidth - 1, CompressedSlotUopNumWidth),
        NormalUopNumWidth
      )
    )
  }

  class RobEntryBundle(implicit p: Parameters) extends XSBundle {
    val valid = Bool()

    val entryPairType = CompressType()
    val noCompressReason = UInt(2.W) // used for Perf

    val uopState = UInt(PackedUopStateWidth.W)
    val realDestSize = UInt(log2Up(MaxUopSize + 1).W)
    val complexSlotHasDest = UInt(1.W)
    val entryHasStore = Bool()

    val vls = Bool()
    val interruptSafe = Bool()
    val rfWen = Bool()
    val commitType = CommitType()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val slotHeadRvcMask = UInt(2.W)
    val predTaken = Bool()
    val needVTB = Bool()

    val traceBlockInPipe = new TracePipe(IretireWidthEncoded)
    // Preserve the former slot's exact trace metadata so a latter-slot squash
    // can rebuild the surviving physical entry without stale metadata.
    val formerTraceBlockInPipe = new TracePipe(IretireWidthEncoded)
    val slotNeedFlushMask = UInt(2.W)

    // Entry-level fields continue to describe the former slot. Rob.scala keeps
    // a second current-API debug copy for exact latter-slot selection.
    val debug_pc         = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr      = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest      = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_pdest      = OptionWrapper(backendParams.basicDebugEn, UInt(PhyRegIdxWidth.W))
    val debug_fuType     = OptionWrapper(backendParams.debugEn, FuType())
    val debug_fusionNum  = OptionWrapper(backendParams.debugEn, UInt(log2Ceil(RenameWidth + 1).W))
    val debug_fuOpType   = OptionWrapper(backendParams.debugEn, FuOpType())
    val perfDebugInfo    = OptionWrapper(backendParams.debugEn, new PerfDebugInfo)
    val debug_lqIdx      = OptionWrapper(backendParams.debugEn, new LqPtr)
    val debug_sqIdx      = OptionWrapper(backendParams.debugEn, new SqPtr)
    val debug_rfWen      = OptionWrapper(backendParams.debugEn, Bool())
    val debug_seqNum     = OptionWrapper(backendParams.debugEn, InstSeqNum())
    val debug_sim_trig   = OptionWrapper(backendParams.debugEn, Bool())
    val debug_vecWen     = OptionWrapper(backendParams.debugEn, Bool())
    val debug_v0Wen      = OptionWrapper(backendParams.debugEn, Bool())
    val debug_commitType = OptionWrapper(backendParams.debugEn, CommitType())
    val topdownIssued    = OptionWrapper(backendParams.debugEn, Bool())
    val topdownIssueTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))

    def formerUopNum: UInt = decodeFormerUopNum(entryPairType, uopState)
    def latterUopNum: UInt = decodeLatterUopNum(entryPairType, uopState)
    def isWritebacked: Bool = !formerUopNum.orR && !latterUopNum.orR
    def isUopWritebacked: Bool = !formerUopNum.orR && !latterUopNum.orR
  }

  class RobCommitEntryBundle(implicit p: Parameters) extends XSBundle {
    val walk_v = Bool()
    val commit_v = Bool()
    val commit_w = Bool()
    val entryPairType = CompressType()
    val noCompressReason = UInt(2.W)
    val uopState = UInt(PackedUopStateWidth.W)
    val realDestSize = UInt(log2Up(MaxUopSize + 1).W)
    val interruptSafe = Bool()
    val slotHeadRvcMask = UInt(2.W)
    val predTaken = Bool()
    val needVTB = Bool()
    val isVls = Bool()
    val vls = Bool()
    val commitType = CommitType()
    val entryHasStore = Bool()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)

    val rfWen = Bool()
    val slotNeedFlushMask = UInt(2.W)
    // trace
    val traceBlockInPipe = new TracePipe(IretireWidthEncoded)
    val formerTraceIretire = UInt(IretireWidthEncoded.W)
    // debug_begin
    val debug_pc = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest = OptionWrapper(backendParams.basicDebugEn, UInt(LogicRegsWidth.W))
    val debug_pdest = OptionWrapper(backendParams.basicDebugEn, UInt(PhyRegIdxWidth.W))
    val debug_otherPdest = OptionWrapper(backendParams.basicDebugEn, Vec(7, UInt(PhyRegIdxWidth.W)))
    val debug_fuType = OptionWrapper(backendParams.debugEn, FuType())
    val debug_fusionNum = OptionWrapper(backendParams.debugEn, UInt(log2Ceil(RenameWidth + 1).W))
    // debug_end

    def formerUopNum: UInt = decodeFormerUopNum(entryPairType, uopState)
    def latterUopNum: UInt = decodeLatterUopNum(entryPairType, uopState)
  }

  def connectEnq(robEntry: RobEntryBundle, robEnq: EnqRobUop)(implicit p: Parameters): Unit = {
    robEntry.entryPairType := robEnq.entryPairType
    robEntry.noCompressReason := robEnq.noCompressReason

    robEntry.uopState := encodeUopState(robEnq.entryPairType, robEnq.formerNumWB, robEnq.latterNumWB)
    robEntry.complexSlotHasDest := robEnq.complexSlotHasDest
    robEntry.entryHasStore := robEnq.entryHasStore

    robEntry.commitType := robEnq.commitType
    robEntry.ftqIdx := robEnq.ftqPtr
    robEntry.ftqOffset := robEnq.ftqOffset
    robEntry.slotHeadRvcMask := robEnq.slotHeadRvcMask
    robEntry.predTaken := robEnq.predTaken
    robEntry.needVTB := robEnq.isVset || robEnq.vpu.isVleff
    robEntry.vls := robEnq.vlsInstr
    robEntry.rfWen := robEnq.rfWen
    robEntry.interruptSafe := robEnq.interruptSafe
    robEntry.slotNeedFlushMask := robEnq.slotNeedFlushMask
    // trace
    robEntry.traceBlockInPipe := robEnq.traceBlockInPipe
    robEntry.formerTraceBlockInPipe := robEnq.traceBlockInPipe
    robEntry.debug_ldest.foreach(_ := robEnq.ldest)
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
    robEntry.topdownIssued.foreach(_ := false.B)
    robEntry.topdownIssueTime.foreach(_ := 0.U)
  }

  def connectCommitEntry(robCommitEntry: RobCommitEntryBundle, robEntry: RobEntryBundle)(implicit p: Parameters): Unit = {
    robCommitEntry.walk_v := robEntry.valid
    robCommitEntry.commit_v := robEntry.valid
    robCommitEntry.commit_w := robEntry.isWritebacked
    robCommitEntry.entryPairType := robEntry.entryPairType
    robCommitEntry.noCompressReason := robEntry.noCompressReason
    robCommitEntry.uopState := robEntry.uopState
    robCommitEntry.realDestSize := robEntry.realDestSize
    robCommitEntry.interruptSafe := robEntry.interruptSafe
    robCommitEntry.rfWen := robEntry.rfWen
    robCommitEntry.needVTB := robEntry.needVTB
    robCommitEntry.slotHeadRvcMask := robEntry.slotHeadRvcMask
    robCommitEntry.predTaken := robEntry.predTaken
    robCommitEntry.isVls := robEntry.vls
    robCommitEntry.vls := robEntry.vls // TODO: it is Duplicate
    robCommitEntry.ftqIdx := robEntry.ftqIdx
    robCommitEntry.ftqOffset := robEntry.ftqOffset
    robCommitEntry.commitType := robEntry.commitType
    robCommitEntry.entryHasStore := robEntry.entryHasStore
    robCommitEntry.slotNeedFlushMask := robEntry.slotNeedFlushMask
    robCommitEntry.traceBlockInPipe := robEntry.traceBlockInPipe
    robCommitEntry.formerTraceIretire := robEntry.formerTraceBlockInPipe.iretire
    robCommitEntry.debug_pc.foreach(_ := robEntry.debug_pc.get)
    robCommitEntry.debug_instr.foreach(_ := robEntry.debug_instr.get)
    robCommitEntry.debug_ldest.foreach(_ := robEntry.debug_ldest.get)
    robCommitEntry.debug_pdest.foreach(_ := robEntry.debug_pdest.get)
    robCommitEntry.debug_fuType.foreach(_ := robEntry.debug_fuType.get)
    robCommitEntry.debug_fusionNum.foreach(_ := robEntry.debug_fusionNum.get)
  }
}

import RobBundles._

class RobPtr(entries: Int) extends CircularQueuePtr[RobPtr](
  entries
) with HasCircularQueuePtrHelper {

  val slotIsFormer = Bool()

  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).RobSize)

  def isSameEntry(that: RobPtr): Bool = this.flag === that.flag && this.value === that.value
  def isSameSlot(that: RobPtr): Bool = isSameEntry(that) && this.slotIsFormer === that.slotIsFormer

  def isAfterSlot(that: RobPtr): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value > that.value
    val sameEntry = this.flag === that.flag && this.value === that.value
    (differentFlag ^ compare) || (sameEntry && !this.slotIsFormer && that.slotIsFormer)
  }

  def isBeforeSlot(that: RobPtr): Bool = {
    val differentFlag = this.flag ^ that.flag
    val compare = this.value < that.value
    val sameEntry = this.flag === that.flag && this.value === that.value
    (differentFlag ^ compare) || (sameEntry && this.slotIsFormer && !that.slotIsFormer)
  }

  def isNotAfterSlot(that: RobPtr): Bool = isBeforeSlot(that) || isSameSlot(that)
  def isNotBeforeSlot(that: RobPtr): Bool = isAfterSlot(that) || isSameSlot(that)

  def addEntries(v: UInt): RobPtr = {
    val newPtr = Wire(new RobPtr(entries))
    if (isPow2(entries)) {
      val raw = Cat(this.flag, this.value) + v
      newPtr.flag := raw(PTR_WIDTH)
      newPtr.value := raw(PTR_WIDTH - 1, 0)
    } else {
      val newValue = this.value +& v
      val diff = Cat(0.U(1.W), newValue).asSInt - Cat(0.U(1.W), entries.U.asTypeOf(newValue)).asSInt
      val reverseFlag = diff >= 0.S
      newPtr.flag := Mux(reverseFlag, !this.flag, this.flag)
      newPtr.value := Mux(reverseFlag, diff.asUInt, newValue)
    }
    newPtr.slotIsFormer := this.slotIsFormer
    newPtr
  }

  def subEntries(v: UInt): RobPtr = {
    val flippedPtr = addEntries(entries.U - v)
    val newPtr = Wire(new RobPtr(entries))
    newPtr.flag := !flippedPtr.flag
    newPtr.value := flippedPtr.value
    newPtr.slotIsFormer := this.slotIsFormer
    newPtr
  }

  def asFormer: RobPtr = {
    val ptr = Wire(new RobPtr(entries))
    ptr := this
    ptr.slotIsFormer := true.B
    ptr
  }

  def needFlush(redirect: Valid[Redirect]): Bool = {
    val flushItself = redirect.bits.flushItself() && isSameSlot(redirect.bits.robIdx)
    redirect.valid && (flushItself || isAfterSlot(redirect.bits.robIdx))
  }

  def needFlush(redirect: Seq[Valid[Redirect]]): Bool = VecInit(redirect.map(needFlush)).asUInt.orR

  def lineHeadPtr(implicit p: Parameters): RobPtr = {
    val CommitWidth = p(XSCoreParamsKey).CommitWidth
    val out = Wire(new RobPtr)
    out.flag := this.flag
    out.value := Cat(this.value(this.PTR_WIDTH-1, log2Up(CommitWidth)), 0.U(log2Up(CommitWidth).W))
    out.slotIsFormer := true.B
    out
  }

  def isBeforeWithChannel(that: RobPtr, thisChannelIdx: UInt, thatChannelIdx: UInt): Bool = {
    isBeforeSlot(that) || isSameSlot(that) && (thisChannelIdx < thatChannelIdx)
  }

}

object RobPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): RobPtr = {
    val ptr = Wire(new RobPtr)
    ptr.flag := f
    ptr.value := v
    ptr.slotIsFormer := true.B
    ptr
  }
}

class RobCSRIO(implicit p: Parameters) extends XSBundle {
  val intrBitSet = Input(Bool())
  val trapTarget = Input(new TargetPCBundle)
  val wfiEvent   = Input(Bool())
  val criticalErrorState = Input(Bool())

  val fflags     = Output(Vec(5, Bool()))
  val vxsat      = Output(Bool())
  val vstart     = Output(Valid(UInt(XLEN.W)))
  val dirty_fs   = Output(Bool())
  val dirty_vs   = Output(Bool())
  val perfinfo   = new Bundle {
    val retiredInstr = Output(UInt(7.W))
  }
}

class RobLsqIO(implicit p: Parameters) extends XSBundle {
  // A compressed physical ROB entry can retire two load slots.
  val lcommit = Output(UInt(log2Up(2 * CommitWidth + 1).W))
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

class RobFlushPcInfo(implicit p: Parameters) extends XSBundle {
  val formerLen = UInt(log2Ceil(RenameWidth * 4 + 1).W)
  val flushIsRVC = Bool()
}
