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
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] James E. Smith, and Andrew R. Pleszkun. "[Implementation of precise interrupts in pipelined processors.]
* (https://dl.acm.org/doi/10.5555/327010.327125)" 12th Annual International Symposium on Computer Architecture (ISCA).
* 1985.
***************************************************************************************/

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import difftest.common._
import difftest.plugin.topdown._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.PerfDebugInfo
import xiangshan.backend.{BackendParams, Bundles, GPAMemEntry, RatToVecExcpMod, RegWriteFromRab, VecExcpInfo}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.rob.RobBundles._
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._

class RobIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  private val LduCnt = params.LduCnt
  private val HyuCnt = params.HyuCnt

    /**
     * Core identifier used by difftest and observation logic.
     */
    val hartId = Input(UInt(hartIdLen.W))
    /**
     * Accepted backend recovery boundary from CtrlBlock. Pointer age includes the
     * former/latter slot bit; flushItself distinguishes inclusive and exclusive cuts.
     */
    val redirect = Input(Valid(new Redirect))
    /**
     * Shared admission and per-uop metadata from the registered dispatch path.
     */
    val enq = new RobEnqIO
    /**
     * Head-generated recovery request. CtrlBlock registers it before feeding it
     * back through redirect; flushPcInfo supplies compressed-entry PC reconstruction.
     */
    val flushOut = ValidIO(new Redirect)
    val flushPcInfo = Output(new RobFlushPcInfo)
    /**
     * CSR trap payload, valid one cycle after exceptionHappen; excludes plain flushPipe/replay.
     */
    val exception = ValidIO(new ExceptionInfo)
    /**
     * Observation handshake: release a latter-slot trap together with its surviving former retirement.
     */
    val diffCommitForTrap = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Output(Bool()))
    val diffArchEvent = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Input(Valid(Bool())))
    // exu + brq
    /**
     * Architectural completion stream, delayed and filtered against older redirects by CtrlBlock.
     */
    val writeback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)
    /**
     * Execution observation stream with unfiltered valid bits; used for debug data and branch trace updates.
     */
    val exuWriteback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)
    /**
     * Compatibility input: this implementation counts matching writeback ports directly instead.
     */
    val writebackNums = Flipped(Vec(writeback.size, ValidIO(UInt(writeback.size.U.getWidth.W))))
    /**
     * Per-exception-port summary of exception, replay, flushPipe, or debug-trigger completion.
     */
    val writebackNeedFlush = Input(Vec(params.getWrite2RobSize(_.needExceptionGen), Bool()))
    /**
     * Physical-entry retirement/walk masks; actual retirement requires isCommit AND commitValid.
     */
    val commits = Output(new RobCommitIO)
    /**
     * Trace backpressure blocks retirement; trace payload retains FTQ coordinates rather than a full PC.
     */
    val trace = new Bundle {
      val blockCommit = Input(Bool())
      val traceCommitInfo = new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthCommited)
    }
    /**
     * RAB register mappings and VL mappings are each registered once at this ROB output boundary.
     */
    val rabCommits = Output(new RabCommitIO)
    val vlCommits = Output(new VlCommitBundle(RabCommitWidth))
    /**
     * Diff RAT boundary names the youngest surviving slot of each retired physical entry.
     */
    val diffRatCommitRobIdx = if (backendParams.basicDebugEn) Some(Output(Valid(new RobPtr))) else None
    val diffRatCommitRobIdxVec =
      if (backendParams.basicDebugEn) Some(Output(Vec(CommitWidth, Valid(new RobPtr)))) else None
    /**
     * Vset flush/resume notification derived from the already-flushed head entry.
     */
    val isVsetFlushPipe = Output(Bool())
    /**
     * Memory ordering boundary: load/store counts and a slot-aware pending instruction pointer.
     */
    val lsq = new RobLsqIO
    /**
     * Current oldest physical-entry pointer; the LSQ pending pointer can additionally select its latter slot.
     */
    val robDeqPtr = Output(new RobPtr)
    /**
     * Interrupt/WFI/error inputs and delayed retirement-side CSR updates.
     */
    val csr = new RobCSRIO
    /**
     * Snapshot selection/lifetime is coordinated by CtrlBlock; local enqueue is recomputed from ROB requests.
     */
    val snpt = Input(new SnapshotPort)
    /**
     * Capacity-only admission summary; other blockers can reject enqueue while robFull remains false.
     */
    val robFull = Output(Bool())
    /**
     * Safe-to-idle indication requires both memory and frontend acknowledgement of the WFI request.
     */
    val cpu_wfi = Output(Bool())
    val wfi = new Bundle {
      val wfiReq = Output(Bool())
      val safeFromMem = Input(Bool())
      val safeFromFrontend = Input(Bool())
    }
    val wfi_enable = Input(Bool())
    /**
     * VTypeBuffer recovery and architectural vtype notifications forwarded to decode/frontend.
     */
    val toDecode = new Bundle {
      val isResumeVType = Output(Bool())
      val walkToArchVType = Output(Bool())
      val walkVType = ValidIO(VType())
      val commitVType = new Bundle {
        val vtype = ValidIO(VType())
        val hasVsetvl = Output(Bool())
      }
    }
    /**
     * Vector exception data recovery stalls new ROB/RAB admission while busy.
     */
    val fromVecExcpMod = Input(new Bundle {
      val busy = Bool()
    })
    /**
     * One-cycle GPA lookup launched with exceptionHappen and aligned with io.exception.valid.
     */
    val readGPAMemAddr = ValidIO(new Bundle {
      val ftqPtr = new FtqPtr()
      val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
      val isRVC = Bool()
    })
    val readGPAMemData = Input(new GPAMemEntry)
    val vstartIsZero = Input(Bool())

    /**
     * Vector exception format plus the RAB-produced logical-to-physical destination map.
     */
    val toVecExcpMod = Output(new Bundle {
      val logicPhyRegMap = Vec(RabCommitWidth, ValidIO(new RegWriteFromRab))
      val excpInfo = ValidIO(new VecExcpInfo)
    })

    /**
     * Optional or observation-oriented ports for memory history, TopDown classification, and instruction timing.
     */
    val debug_ls = Flipped(new DebugLSIO)
    val debugBlockBackward = Option.when(backendParams.debugEn)(Output(Bool()))
    val debugWaitForward   = Option.when(backendParams.debugEn)(Output(Bool()))
    val topdownIQInfoVec = Option.when(backendParams.debugEn)(Input(Vec(backendParams.iqEntryNum, Flipped(ValidIO(new TopdownIQExtendedInfo())))))
    val debugRobHeadStall = Option.when(backendParams.debugEn)(ValidIO(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))
    val debugEnqLsq = Input(new LsqEnqIO)
    val debugHeadLsIssue = Input(Bool())
    val lsTopdownInfo = Vec(LduCnt + HyuCnt, Input(new LsTopdownInfo))
    val debugTopDown = new Bundle {
      val toCore = new RobCoreTopDownIO
      val toDispatch = new RobDispatchTopDownIO
      val robHeadLqIdx = Valid(new LqPtr)
      val fromCore = new CoreDispatchTopDownIO
    }
    val debugRolling = new RobDebugRollingIO
    val debugInstrAddrTransType = Input(new AddrTransType)

    // store event difftest information
    /**
     * Store event lookup is slot-aware; preCommit identifies the current physical head entry.
     */
    val storeDebugInfo = Vec(EnsbufferWidth, new Bundle {
      val robidx = Input(new RobPtr)
      val pc     = Output(UInt(VAddrBits.W))
      val storeOffset = Option.when(env.EnableDifftest)(Output(UInt(log2Ceil(RenameWidth + 1).W)))
      val preCommit = Option.when(env.EnableDifftest)(Output(Bool()))
    })
  }
