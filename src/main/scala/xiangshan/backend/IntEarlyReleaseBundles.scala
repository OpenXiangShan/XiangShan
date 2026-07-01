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

package xiangshan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.datapath.DataConfig.IntData
import xiangshan.backend.rob.RobPtr

object IntERFallbackReason {
  val width = 4

  def none = 0.U(width.W)
  def unsupportedConsumer = 1.U(width.W)
  def unsupportedReadPath = 2.U(width.W)
  def counterSaturated = 3.U(width.W)
  def redirectKill = 4.U(width.W)
  def staleEvent = 5.U(width.W)
  def duplicateSource = 6.U(width.W)
  def invalidPdest = 7.U(width.W)
}

object IntEREntryState {
  val width = 2

  def invalid = 0.U(width.W)
  def counting = 1.U(width.W)
  def fallbackWaitCommit = 2.U(width.W)
  def releasedWaitCommit = 3.U(width.W)
}

object IntERBundleHelper {
  def sourceIndexWidth(numSrc: Int): Int = log2Ceil(numSrc max 2)

  def logicalSrcVec(implicit p: Parameters): Vec[IntERSrcTrack] =
    Vec(p(XSCoreParamsKey).backendParams.numSrc, new IntERSrcTrack)

  def localSrcVec(n: Int)(implicit p: Parameters): Vec[IntERSrcTrack] =
    Vec(n, new IntERSrcTrack)

  def logicalRobSrcVec(implicit p: Parameters): Vec[IntERSrcRobState] =
    Vec(p(XSCoreParamsKey).backendParams.numSrc, new IntERSrcRobState)

  def localRobSrcVec(n: Int)(implicit p: Parameters): Vec[IntERSrcRobState] =
    Vec(n, new IntERSrcRobState)

  def logicalReadDoneSrcVec(implicit p: Parameters): Vec[IntERSrcReadDone] =
    Vec(p(XSCoreParamsKey).backendParams.numSrc, new IntERSrcReadDone)

  def localReadDoneSrcVec(n: Int)(implicit p: Parameters): Vec[IntERSrcReadDone] =
    Vec(n, new IntERSrcReadDone)

  def connectLocalSrcFromFull(dst: Vec[IntERSrcTrack], src: Vec[IntERSrcTrack]): Unit = {
    require(dst.length <= src.length, "local ER source metadata cannot be wider than full metadata")
    dst.zipWithIndex.foreach { case (d, i) => d := src(i) }
  }

  def connectLocalUopMeta(dst: IntERLocalUopMeta, src: IntERUopMeta): Unit = {
    connectLocalSrcFromFull(dst.src, src.src)
  }
}

class IntERSrcTrack(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val srcIdx = UInt(IntERSrcIdxWidth.W)
  val psrc = UInt(PhyRegIdxWidth.W)
}

class IntERSrcRobState(implicit p: Parameters) extends IntERSrcTrack {
  val readDone = Bool()
}

class IntERDestTrack(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val pdest = UInt(PhyRegIdxWidth.W)
}

class IntERRedefTrack(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val oldPdest = UInt(PhyRegIdxWidth.W)
}

class IntERCommitRedef(implicit p: Parameters) extends XSBundle {
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val oldPdest = UInt(PhyRegIdxWidth.W)
  val redefinerRobIdx = new RobPtr
}

class IntERUopMeta(implicit p: Parameters) extends XSBundle {
  val src = IntERBundleHelper.logicalSrcVec
  val dest = new IntERDestTrack
  val redef = new IntERRedefTrack
  val eligible = Bool()
}

class IntERRobUopMeta(implicit p: Parameters) extends XSBundle {
  val src = IntERBundleHelper.logicalRobSrcVec
  val dest = new IntERDestTrack
  val redef = new IntERRedefTrack
  val resolved = Bool()
  val guardEmitted = Bool()
}

class IntERLocalUopMeta(val numSrc: Int)(implicit p: Parameters) extends XSBundle {
  val src = IntERBundleHelper.localSrcVec(numSrc)
}

class IntERSrcReadDone(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val srcIdx = UInt(IntERSrcIdxWidth.W)
  val psrc = UInt(PhyRegIdxWidth.W)
}

class IntERSrcValueReadDone(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val src = IntERBundleHelper.logicalReadDoneSrcVec
  val fallback = Bool()
  val reason = UInt(IntERFallbackReason.width.W)
}

class IntERDataPathReadDoneStatus extends Bundle {
  val tracked = Bool()
  val accepted = Bool()
  val fallback = Bool()
  val suppressed = Bool()
  val unsupportedReadPath = Bool()
  val replayProne = Bool()
  val uncertain = Bool()
  val trackedRegOHSourceCount = UInt(4.W)
  val trackedRegCacheSourceCount = UInt(4.W)
  val trackedForwardSourceCount = UInt(4.W)
  val trackedBypassSourceCount = UInt(4.W)
  val trackedBypass2SourceCount = UInt(4.W)
  val trackedOtherSourceCount = UInt(4.W)
  val acceptedForwardSourceCount = UInt(4.W)
  val acceptedBypassSourceCount = UInt(4.W)
  val fallbackForwardSourceCount = UInt(4.W)
  val fallbackBypassSourceCount = UInt(4.W)
  val unsupportedForwardSourceCount = UInt(4.W)
  val unsupportedBypassSourceCount = UInt(4.W)
  val replayForwardSourceCount = UInt(4.W)
  val replayBypassSourceCount = UInt(4.W)
  val uncertainForwardSourceCount = UInt(4.W)
  val uncertainBypassSourceCount = UInt(4.W)
}

class IntERRobReadDoneStatus extends Bundle {
  val sawRaw = Bool()
  val accepted = Bool()
  val fallback = Bool()
  val stale = Bool()
  val duplicate = Bool()
}

class IntERSquashSource(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val src = IntERBundleHelper.logicalSrcVec
}

class IntERProducerReady(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val robIdx = new RobPtr
  val pdest = UInt(PhyRegIdxWidth.W)
}

class IntCommitWriteback(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val pdest = UInt(PhyRegIdxWidth.W)
  val data = UInt(XLEN.W)
}

class IntERSTGuardDec(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val robIdx = new RobPtr
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val oldPdest = UInt(PhyRegIdxWidth.W)
  val fallback = Bool()
  val reason = UInt(IntERFallbackReason.width.W)
}

class RenameIntERIO(implicit p: Parameters) extends XSBundle {
  val redirectKill = Bool()
  val producerReady = Vec(backendParams.numPregWb(IntData()), ValidIO(new IntERProducerReady))
  val readDone = Vec(IntERReadDoneWidth, ValidIO(new IntERSrcValueReadDone))
  val squash = Vec(IntERReadDoneWidth, ValidIO(new IntERSquashSource))
  val stGuardDec = Vec(IntERSTWalkWidth, ValidIO(new IntERSTGuardDec))
}

class IntEREarlyFreeReq(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val pdest = UInt(PhyRegIdxWidth.W)
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
  val redefRobIdx = new RobPtr
}

class IntERCommitSuppress(implicit p: Parameters) extends XSBundle {
  val suppress = Bool()
  val oldPdest = UInt(PhyRegIdxWidth.W)
  val trackId = UInt(IntERTrackIdWidth.W)
  val trackGen = UInt(IntERTrackGenBits.W)
}

class IntERRenameSourceProbe(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val psrc = UInt(PhyRegIdxWidth.W)
  val srcIdx = UInt(IntERSrcIdxWidth.W)
}

class IntERProducerAlloc(implicit p: Parameters) extends XSBundle {
  val pdest = UInt(PhyRegIdxWidth.W)
  val robIdx = new RobPtr
}

class IntERRedefProbe(implicit p: Parameters) extends XSBundle {
  val oldPdest = UInt(PhyRegIdxWidth.W)
  val robIdx = new RobPtr
}

class IntEREntryDebug(implicit p: Parameters) extends XSBundle {
  val state = UInt(IntEREntryState.width.W)
  val pdest = UInt(PhyRegIdxWidth.W)
  val producerRobIdx = new RobPtr
  val redefinerRobIdx = new RobPtr
  val userCounter = UInt(IntERCounterWidth.W)
  val gen = UInt(IntERTrackGenBits.W)
  val fallback = Bool()
  val redefinerSeen = Bool()
  val redefinerNS = Bool()
  val producedReady = Bool()
  val earlyFreeIssued = Bool()
}

class IntERDebugBundle(implicit p: Parameters) extends XSBundle {
  val entries = Vec(IntERTrackEntries, new IntEREntryDebug)
  val activeCount = UInt(log2Ceil(IntERTrackEntries + 1).W)
  val allocCount = UInt(32.W)
  val fullUntrackedCount = UInt(32.W)
  val sourceMatchCount = UInt(32.W)
  val sourceDuplicateCount = UInt(32.W)
  val readDoneDecCount = UInt(32.W)
  val squashDecCount = UInt(32.W)
  val guardDecCount = UInt(32.W)
  val fallbackCount = UInt(32.W)
  val saturatedFallbackCount = UInt(32.W)
  val producerReadyCount = UInt(32.W)
  val earlyFreeOpportunityCount = UInt(32.W)
  val earlyFreeCount = UInt(32.W)
  val commitSuppressCount = UInt(32.W)
  val commitIdentityMismatchCount = UInt(32.W)
  val genMismatchCount = UInt(32.W)
  val redirectKillCount = UInt(32.W)
}
