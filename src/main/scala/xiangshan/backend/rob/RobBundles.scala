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
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.frontend.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.Bundles.{DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator

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
    val ftqOffset = UInt(log2Up(PredictWidth).W)
    val isRVC = Bool()
    val isVset = Bool()
    val isHls = Bool()
    val instrSize = UInt(log2Ceil(RenameWidth + 1).W)
    val loadWaitBit = Bool()    // for perfEvents
    val eliminatedMove = Bool() // for perfEvents
    // data end

    // status begin
    val valid = Bool()
    val fflags = UInt(5.W)
    val mmio = Bool()
    // store will be commited if both sta & std have been writebacked
    val stdWritebacked = Bool()
    val vxsat = Bool()
    val realDestSize = UInt(log2Up(MaxUopSize + 1).W)
    val uopNum = UInt(log2Up(MaxUopSize + 1).W)
    val commitTrigger = Bool()
    val needFlush = Bool()
    // status end

    // debug_begin
    val debug_pc = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest = OptionWrapper(backendParams.debugEn, UInt(LogicRegsWidth.W))
    val debug_pdest = OptionWrapper(backendParams.debugEn, UInt(PhyRegIdxWidth.W))
    val debug_fuType = OptionWrapper(backendParams.debugEn, FuType())
    // debug_end

    def isWritebacked: Bool = !uopNum.orR && stdWritebacked
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
    val isVset = Bool()
    val isHls = Bool()
    val commitType = CommitType()
    val ftqIdx = new FtqPtr
    val ftqOffset = UInt(log2Up(PredictWidth).W)
    val instrSize = UInt(log2Ceil(RenameWidth + 1).W)
    val fpWen = Bool()
    val rfWen = Bool()
    val loadWaitBit = Bool() // for perfEvents
    val isMove = Bool()      // for perfEvents
    val needFlush = Bool()
    // debug_begin
    val debug_pc = OptionWrapper(backendParams.debugEn, UInt(VAddrBits.W))
    val debug_instr = OptionWrapper(backendParams.debugEn, UInt(32.W))
    val debug_ldest = OptionWrapper(backendParams.debugEn, UInt(LogicRegsWidth.W))
    val debug_pdest = OptionWrapper(backendParams.debugEn, UInt(PhyRegIdxWidth.W))
    val debug_fuType = OptionWrapper(backendParams.debugEn, FuType())
    // debug_end
    def dirtyFs = fpWen
    val dirtyVs = Bool()
  }

  def connectEnq(robEntry: RobEntryBundle, robEnq: DynInst): Unit = {
    robEntry.wflags := robEnq.wfflags
    robEntry.commitType := robEnq.commitType
    robEntry.ftqIdx := robEnq.ftqPtr
    robEntry.ftqOffset := robEnq.ftqOffset
    robEntry.isRVC := robEnq.preDecodeInfo.isRVC
    robEntry.isVset := robEnq.isVset
    robEntry.isHls := robEnq.isHls
    robEntry.instrSize := robEnq.instrSize
    robEntry.rfWen := robEnq.rfWen
    robEntry.fpWen := robEnq.dirtyFs
    robEntry.dirtyVs := robEnq.dirtyVs
    robEntry.loadWaitBit := robEnq.loadWaitBit
    robEntry.eliminatedMove := robEnq.eliminatedMove
    // flushPipe needFlush but not exception
    robEntry.needFlush := robEnq.hasException || robEnq.flushPipe
    robEntry.debug_pc.foreach(_ := robEnq.pc)
    robEntry.debug_instr.foreach(_ := robEnq.instr)
    robEntry.debug_ldest.foreach(_ := robEnq.ldest)
    robEntry.debug_pdest.foreach(_ := robEnq.pdest)
    robEntry.debug_fuType.foreach(_ := robEnq.fuType)
  }

  def connectCommitEntry(robCommitEntry: RobCommitEntryBundle, robEntry: RobEntryBundle): Unit = {
    robCommitEntry.walk_v := robEntry.valid
    robCommitEntry.commit_v := robEntry.valid
    robCommitEntry.commit_w := (robEntry.uopNum === 0.U) && (robEntry.stdWritebacked === true.B)
    robCommitEntry.realDestSize := robEntry.realDestSize
    robCommitEntry.interrupt_safe := robEntry.interrupt_safe
    robCommitEntry.rfWen := robEntry.rfWen
    robCommitEntry.fpWen := robEntry.fpWen
    robCommitEntry.fflags := robEntry.fflags
    robCommitEntry.wflags := robEntry.wflags
    robCommitEntry.vxsat := robEntry.vxsat
    robCommitEntry.isRVC := robEntry.isRVC
    robCommitEntry.isVset := robEntry.isVset
    robCommitEntry.isHls := robEntry.isHls
    robCommitEntry.ftqIdx := robEntry.ftqIdx
    robCommitEntry.ftqOffset := robEntry.ftqOffset
    robCommitEntry.commitType := robEntry.commitType
    robCommitEntry.instrSize := robEntry.instrSize
    robCommitEntry.loadWaitBit := robEntry.loadWaitBit
    robCommitEntry.isMove := robEntry.eliminatedMove
    robCommitEntry.dirtyVs := robEntry.dirtyVs
    robCommitEntry.needFlush := robEntry.needFlush
    robCommitEntry.debug_pc.foreach(_ := robEntry.debug_pc.get)
    robCommitEntry.debug_instr.foreach(_ := robEntry.debug_instr.get)
    robCommitEntry.debug_ldest.foreach(_ := robEntry.debug_ldest.get)
    robCommitEntry.debug_pdest.foreach(_ := robEntry.debug_pdest.get)
    robCommitEntry.debug_fuType.foreach(_ := robEntry.debug_fuType.get)
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

  def lineHeadPtr()(implicit p: Parameters): RobPtr = {
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
  val trapTarget = Input(UInt(VAddrBits.W))
  val isXRet     = Input(Bool())
  val wfiEvent   = Input(Bool())

  val fflags     = Output(Valid(UInt(5.W)))
  val vxsat      = Output(Valid(Bool()))
  val vstart     = Output(Valid(UInt(XLEN.W)))
  val dirty_fs   = Output(Bool())
  val dirty_vs   = Output(Bool())
  val perfinfo   = new Bundle {
    val retiredInstr = Output(UInt(3.W))
  }
}

class RobLsqIO(implicit p: Parameters) extends XSBundle {
  val lcommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val scommit = Output(UInt(log2Up(CommitWidth + 1).W))
  val pendingld = Output(Bool())
  val pendingst = Output(Bool())
  // set when vector store at the head of ROB
  val pendingVst = Output(Bool())
  val commit = Output(Bool())
  val pendingPtr = Output(new RobPtr)
  val pendingPtrNext = Output(new RobPtr)

  val mmio = Input(Vec(LoadPipelineWidth, Bool()))
  // Todo: what's this?
  val uop = Input(Vec(LoadPipelineWidth, new DynInst))
}

class RobEnqIO(implicit p: Parameters) extends XSBundle {
  val canAccept = Output(Bool())
  val isEmpty = Output(Bool())
  // valid vector, for robIdx gen and walk
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(ValidIO(new DynInst)))
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

class RobExceptionInfo(implicit p: Parameters) extends XSBundle {
  // val valid = Bool()
  val robIdx = new RobPtr
  val ftqPtr = new FtqPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val exceptionVec = ExceptionVec()
  val exceptionFromBackend = Bool()
  val flushPipe = Bool()
  val isVset = Bool()
  val replayInst = Bool() // redirect to that inst itself
  val singleStep = Bool() // TODO add frontend hit beneath
  val crossPageIPFFix = Bool()
  val trigger = new TriggerCf
  val vstartEn = Bool()
  val vstart = UInt(XLEN.W)

  def has_exception = exceptionVec.asUInt.orR || flushPipe || singleStep || replayInst || trigger.canFire
  def not_commit = exceptionVec.asUInt.orR || singleStep || replayInst || trigger.canFire
  // only exceptions are allowed to writeback when enqueue
  def can_writeback = exceptionVec.asUInt.orR || singleStep || trigger.canFire
}

class RobFlushInfo(implicit p: Parameters) extends XSBundle {
  val ftqIdx = new FtqPtr
  val robIdx = new RobPtr
  val ftqOffset = UInt(log2Up(PredictWidth).W)
  val replayInst = Bool()
}
