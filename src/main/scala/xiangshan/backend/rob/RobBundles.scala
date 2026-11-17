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
import chisel3._
import chisel3.util._
import utility.InstSeqNum
import utils._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.{Nf, VLmul, VSew}
import xiangshan.backend.rename.CompressType
import xiangshan.backend.trace._
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, SqPtr}

class BasicDebugInfo(implicit p: Parameters) extends XSBundle {
  val ldest = UInt(LogicRegsWidth.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val vd = UInt(LogicRegsWidth.W)
  val fpWen = Bool()
  val vecWen = Bool()
  val v0Wen = Bool()
  val eliminatedMove = Bool()
  val isXSTrap = Bool()

  def connectBasicDebug(robEnq: EnqRobUop): Unit = {
    this.ldest := robEnq.ldest
    this.pdest := robEnq.pdest
    this.vd := robEnq.debug.map(_.instr.asTypeOf(new XSInstBitFields).VD).getOrElse(0.U)
    this.fpWen := robEnq.fpWen
    this.vecWen := robEnq.vecWen
    this.v0Wen := robEnq.v0Wen
    this.eliminatedMove := robEnq.isMove
    this.isXSTrap := robEnq.isXSTrap
  }
}

/**
 * Functional per-slot information kept by the ROB for exception handling and
 * performance accounting.  This used to live in a debug-named copy of the full
 * enqueue uop; only these fields are actually required by the RTL.
 */
class RobSlotInfo(implicit p: Parameters) extends XSBundle {
  val commitType = CommitType()
  val fuType = FuType()
  val fuOpType = FuOpType()

  def isVectorMemory: Bool = FuType.isLoadStore(fuType) && LSUOpType.isVecMemOp(fuOpType)

  def isHls: Bool = (fuType === FuType.ldu.U && LSUOpType.isHlv(fuOpType)) ||
    (fuType === FuType.stu.U && LSUOpType.isHsv(fuOpType))
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
  val basicDebug       = OptionWrapper(backendParams.basicDebugEn, new BasicDebugInfo)
  val debug_fuType     = OptionWrapper(backendParams.debugEn, FuType())
  val debug_fusionNum  = OptionWrapper(backendParams.debugEn, UInt(log2Ceil(RenameWidth / 2 + 1).W))
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
  val topdownCanceled  = OptionWrapper(backendParams.debugEn, Bool())
  val topdownRobHead   = OptionWrapper(backendParams.debugEn, Bool())
  val topdownIdealIssue  = OptionWrapper(backendParams.debugEn, Bool())
  val topdownIssueTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))
  val topdownLastIssueTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))
  val topdownRobHeadTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))
  val topdownIdealIssueTime = OptionWrapper(backendParams.debugEn, UInt(XLEN.W))
  val topdownCancelSource = OptionWrapper(backendParams.debugEn, IQCancelSource())
  val topdownCancelTimeVec = OptionWrapper(backendParams.debugEn, Vec(IQCancelSource.num, UInt(XLEN.W)))
  val topdownCancelTimeFixVec = OptionWrapper(backendParams.debugEn, Vec(IQCancelSource.num, UInt(XLEN.W)))

  def formerUopNum: UInt = decodeFormerUopNum(entryPairType, uopState)
  def latterUopNum: UInt = decodeLatterUopNum(entryPairType, uopState)
  def isWritebacked: Bool = !formerUopNum.orR && !latterUopNum.orR
  def isUopWritebacked: Bool = !formerUopNum.orR && !latterUopNum.orR

  def PackedUopStateWidth: Int = {
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

  def encodeUopState(entryPairType: UInt, formerUopNum: UInt, latterUopNum: UInt): UInt = {
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

  def decodeFormerUopNum(entryPairType: UInt, uopState: UInt): UInt = {
    Mux(
      CompressType.isNORMAL(entryPairType),
      uopState(NormalUopNumWidth - 1, 0),
      fitUopStateWidth(uopState(CompressedSlotUopNumWidth - 1, 0), NormalUopNumWidth)
    )
  }

  def decodeLatterUopNum(entryPairType: UInt, uopState: UInt): UInt = {
    Mux(
      CompressType.isNORMAL(entryPairType),
      0.U(NormalUopNumWidth.W),
      fitUopStateWidth(
        uopState(2 * CompressedSlotUopNumWidth - 1, CompressedSlotUopNumWidth),
        NormalUopNumWidth
      )
    )
  }

  def connectEnq(robEnq: EnqRobUop): Unit = {
    this.entryPairType := robEnq.entryPairType
    this.noCompressReason := robEnq.noCompressReason

    this.uopState := encodeUopState(robEnq.entryPairType, robEnq.formerNumWB, robEnq.latterNumWB)
    this.complexSlotHasDest := robEnq.complexSlotHasDest
    this.entryHasStore := robEnq.entryHasStore

    this.commitType := robEnq.commitType
    this.ftqIdx := robEnq.ftqPtr
    this.ftqOffset := robEnq.ftqOffset
    this.slotHeadRvcMask := robEnq.slotHeadRvcMask
    this.predTaken := robEnq.predTaken
    this.needVTB := robEnq.isVset
    this.vls := robEnq.isVectorMemory
    this.rfWen := robEnq.rfWen
    this.interruptSafe := robEnq.interruptSafe
    this.slotNeedFlushMask := robEnq.slotNeedFlushMask
    // trace
    this.traceBlockInPipe := robEnq.traceBlockInPipe
    this.formerTraceBlockInPipe := robEnq.traceBlockInPipe
    this.basicDebug.foreach(_.connectBasicDebug(robEnq))
    this.debug_fuType.foreach(_ := robEnq.fuType)
    this.debug_fuOpType.foreach(_ := robEnq.fuOpType)
    this.debug_rfWen.foreach(_ := robEnq.rfWen)
    this.debug_vecWen.foreach(_ := robEnq.vecWen)
    this.debug_v0Wen.foreach(_ := robEnq.v0Wen)
    this.debug_commitType.foreach(_ := robEnq.commitType)
    robEnq.debug.foreach { debug =>
      this.debug_pc.foreach(_ := debug.pc)
      this.debug_seqNum.foreach(_ := debug.debug_seqNum)
      this.debug_instr.foreach(_ := debug.instr)
      this.debug_fusionNum.foreach(_ := debug.fusionNum)
      this.perfDebugInfo.foreach(_ := debug.perfDebugInfo)
      this.debug_sim_trig.foreach(_ := debug.debug_sim_trig)
    }
    this.topdownIssued.foreach(_ := false.B)
    this.topdownCanceled.foreach(_ := false.B)
    this.topdownRobHead.foreach(_ := false.B)
    this.topdownIdealIssue.foreach(_ := false.B)
    this.topdownIssueTime.foreach(_ := 0.U)
    this.topdownLastIssueTime.foreach(_ := 0.U)
    this.topdownRobHeadTime.foreach(_ := 0.U)
    this.topdownIdealIssueTime.foreach(_ := 0.U)
    this.topdownCancelSource.foreach(_ := IQCancelSource.none)
    this.topdownCancelTimeVec.foreach(_.foreach(_ := 0.U))
    this.topdownCancelTimeFixVec.foreach(_.foreach(_ := 0.U))
  }
}

class RobCommitEntryBundle(implicit p: Parameters) extends XSBundle {
  val walk_v = Bool()
  val commit_v = Bool()
  val commit_w = Bool()
  val entryPairType = CompressType()
  val noCompressReason = UInt(2.W)
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
  val basicDebug = OptionWrapper(backendParams.basicDebugEn, new BasicDebugInfo)
  val debug_otherPdest = OptionWrapper(backendParams.basicDebugEn, Vec(7, UInt(PhyRegIdxWidth.W)))
  val debug_fuType = OptionWrapper(backendParams.debugEn, FuType())
  val debug_fusionNum = OptionWrapper(backendParams.debugEn, UInt(log2Ceil(RenameWidth / 2 + 1).W))
  // debug_end

  def connectCommitEntry(
    robEntry: RobEntryBundle,
    commitW: Bool
  ): Unit = {
    this.walk_v := robEntry.valid
    this.commit_v := robEntry.valid
    this.commit_w := commitW
    this.entryPairType := robEntry.entryPairType
    this.noCompressReason := robEntry.noCompressReason
    this.realDestSize := robEntry.realDestSize
    this.interruptSafe := robEntry.interruptSafe
    this.rfWen := robEntry.rfWen
    this.needVTB := robEntry.needVTB
    this.slotHeadRvcMask := robEntry.slotHeadRvcMask
    this.predTaken := robEntry.predTaken
    this.isVls := robEntry.vls
    this.vls := robEntry.vls // TODO: it is Duplicate
    this.ftqIdx := robEntry.ftqIdx
    this.ftqOffset := robEntry.ftqOffset
    this.commitType := robEntry.commitType
    this.entryHasStore := robEntry.entryHasStore
    this.slotNeedFlushMask := robEntry.slotNeedFlushMask
    this.traceBlockInPipe := robEntry.traceBlockInPipe
    this.formerTraceIretire := robEntry.formerTraceBlockInPipe.iretire
    this.debug_pc.foreach(_ := robEntry.debug_pc.get)
    this.debug_instr.foreach(_ := robEntry.debug_instr.get)
    this.basicDebug.foreach(_ := robEntry.basicDebug.get)
    this.debug_fuType.foreach(_ := robEntry.debug_fuType.get)
    this.debug_fusionNum.foreach(_ := robEntry.debug_fusionNum.get)
  }
}

class RobExceptionInfo(exceptList: Seq[Int]=ExceptionNO.all)(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val robIdx = new RobPtr
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
  val isRVC = Bool()
  // set 1 if there is 1 exists in exceptionVec
  val hasException = Bool()
  // This signal is valid iff currentValid is true
  // 0: is execute exception, 1: is fetch exception
  val isEnqExcp = Bool()
  val exceptionVec = ExceptSparseVec(exceptList)
  val satpFlushFirstFetchFault = Bool()
  val isFetchMalAddr = Bool()
  val flushPipe = Bool()
  val satpFlush = Bool()
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
