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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utils._
import xiangshan._
import xiangshan.backend.fu.FuConfig.LduCfg
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.Bundles.DynInst
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.LoadStage._

sealed trait HasLoadPipeBundleParam {
  def hasPAddr: Boolean = false
  def hasAddrTrans: Boolean = false
  def hasNoQuery: Boolean = false
  def hasPAddrChecked: Boolean = false
  def hasNC: Boolean = false
  def hasMMIO: Boolean = false
  def replayToLRQ: Boolean = false
  def replayFromLRQ: Boolean = false
  def hasVector: Boolean = false
  def hasS3PreProcess: Boolean = false
  def hasS4PreProcess: Boolean = false
  def hasWritebacked: Boolean = false
  def hasUnalignHandling: Boolean = false

  def replayFromToLRQ = replayToLRQ || replayFromLRQ
}
case class DefaultLoadPipeBundleParam() extends HasLoadPipeBundleParam

class LoadPipeBundle(
  param: HasLoadPipeBundleParam = DefaultLoadPipeBundleParam()
)(
  implicit p: Parameters
) extends XSBundle
  with HasLoadPipeBundleParam
  with HasTlbConst
  with HasDCacheParameters
  with HasVLSUParameters
  with HasMemBlockParameters {
  // basic info
  val entrance = LoadEntrance()
  val accessType = LoadAccessType()
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val fullva = UInt(XLEN.W)
  val size = UInt(MemorySize.Size.width.W)
  val mask = UInt((VLEN/8).W)
  val paddr = Option.when(param.hasAddrTrans || param.hasPAddr)(UInt(PAddrBits.W))
  val noQuery = Option.when(param.hasNoQuery)(Bool())

  // unalign handling
  val align = Option.when(param.hasUnalignHandling)(Bool())
  val unalignHead = Option.when(param.hasUnalignHandling)(Bool())
  val readWholeBank = Option.when(param.hasUnalignHandling)(Bool()) // TODO: remove this

  // MMU & exception handling
  val tlbAccessResult = Option.when(param.hasAddrTrans)(TlbAccessResult())
  val tlbException = Option.when(param.hasAddrTrans)(new TlbRespExcp)
  val pbmt = Option.when(param.hasAddrTrans)(Pbmt())
  val gpaddr = Option.when(param.hasAddrTrans)(UInt(XLEN.W))
  val isForVSnonLeafPTE = Option.when(param.hasAddrTrans)(Bool())

  val pmp = Option.when(param.hasPAddrChecked)(new PMPRespBundle)
  val nc = Option.when(param.hasNC)(Bool())
  val mmio = Option.when(param.hasMMIO)(Bool())

  // replay
  val mshrId = Option.when(param.replayFromToLRQ)(UInt(log2Up(cfg.nMissEntries).W)) // valid when `handledByMSHR` is HIGH
  val replayQueueIdx = Option.when(param.replayFromToLRQ)(UInt(log2Up(LoadQueueReplaySize+1).W)) // valid when `entrance` is replay
  val cause = Option.when(param.replayFromToLRQ)(Vec(LoadReplayCauses.allCauses, Bool()))

  val handledByMSHR = Option.when(param.replayToLRQ)(Bool())
  val dataInvalidSqIdx = Option.when(param.replayToLRQ)(new SqPtr)
  val addrInvalidSqIdx = Option.when(param.replayToLRQ)(new SqPtr)
  val tlbId = Option.when(param.replayToLRQ)(UInt(log2Up(loadfiltersize).W))
  val tlbFull = Option.when(param.replayToLRQ)(Bool())

  val forwardDChannel = Option.when(param.replayFromLRQ)(Bool())
  val uncacheReplay = Option.when(param.replayFromLRQ)(Bool())
  val ncReplay = Option.when(param.replayFromLRQ)(Bool())
  def isNCReplay(): Bool = uncacheReplay.getOrElse(false.B) && ncReplay.getOrElse(false.B)
  def isMMIOReplay(): Bool = uncacheReplay.getOrElse(false.B) && !ncReplay.getOrElse(false.B)
  def isUncacheReplay(): Bool = uncacheReplay.getOrElse(false.B)

  // vector
  val elemIdx = Option.when(param.hasVector)(UInt(elemIdxBits.W))
  val mbIndex = Option.when(param.hasVector)(UInt(vlmBindexBits.W))
  val regOffset = Option.when(param.hasVector)(UInt(vOffsetBits.W))
  val elemIdxInsideVd = Option.when(param.hasVector)(UInt(elemIdxBits.W))
  val vecBaseVaddr = Option.when(param.hasVector)(UInt(VAddrBits.W))
  val vecVaddrOffset = Option.when(param.hasVector)(UInt(VAddrBits.W)) // only used in s1 & s2, to generate vstart
  val vecTriggerMask = Option.when(param.hasVector)(UInt((VLEN/8).W))

  // To optimize timing, part of the combinational logic is precomputed in advance
  // S2 -> S3
  val troubleMaker = Option.when(param.hasS3PreProcess)(Bool())
  val shouldFastReplay = Option.when(param.hasS3PreProcess)(Bool())
  val matchInvalid = Option.when(param.hasS3PreProcess)(Bool())
  val shouldWakeup = Option.when(param.hasS3PreProcess)(Bool())
  val shouldWriteback = Option.when(param.hasS3PreProcess)(Bool())
  // S3 -> S4
  val hasException = Option.when(param.hasS4PreProcess)(Bool())
  val headAlwaysWriteback = Option.when(param.hasS4PreProcess)(Bool())
  val writebackDependOnTail = Option.when(param.hasS4PreProcess)(Bool())
  val shouldRarViolation = Option.when(param.hasS4PreProcess)(Bool())

  // debug info and top-down
  // TODO: use Option
  val hasROBEntry = Bool()
  val missDbUpdated = Bool()

  def offset(): UInt = vaddr.take(DCacheLineOffset)
  def bankOffset(): UInt = vaddr.take(DCacheVWordOffset)
  def DontCarePAddr(): Unit = {
    paddr.get := DontCare
  }
  def DontCareUnalign(): Unit = {
    align.get := DontCare
    unalignHead.get := DontCare
    readWholeBank.get := DontCare
  }
  def DontCareReplayFromLRQFields(): Unit = {
    mshrId.get := DontCare
    replayQueueIdx.get := DontCare
    cause.get := 0.U.asTypeOf(cause.get)
    forwardDChannel.get := false.B
    uncacheReplay.get := false.B
    ncReplay.get := false.B
  }
  def DontCareVectorFields(): Unit = {
    elemIdx.get := 0.U
    mbIndex.get := 0.U
    regOffset.get := 0.U
    elemIdxInsideVd.get := 0.U
    vecBaseVaddr.get := 0.U
    vecVaddrOffset.get := 0.U
    vecTriggerMask.get := 0.U
  }
  def isFirstIssue(): Bool = {
    LoadEntrance.isScalarIssue(entrance) || LoadEntrance.isVectorIssue(entrance)
  }
}

case class LoadReplayIOParam(
  override val replayFromLRQ: Boolean = true,
  override val hasVector: Boolean = true
) extends HasLoadPipeBundleParam

case class FastReplayIOParam(
  override val hasPAddr: Boolean = true,
  override val hasAddrTrans: Boolean = true,
  override val replayFromLRQ: Boolean = true,
  override val hasVector: Boolean = true
) extends HasLoadPipeBundleParam

case class VectorLoadInParam(
  override val hasVector: Boolean = true
) extends HasLoadPipeBundleParam

case class LoadStageIOParam()(
  implicit val s: LoadStage
) extends HasLoadPipeBundleParam with OnLoadStage {
  override val hasPAddr: Boolean = true
  override val hasAddrTrans: Boolean = afterS1
  override val hasNoQuery: Boolean = isS0
  override val hasPAddrChecked: Boolean = afterS2
  override val hasNC: Boolean = afterS2
  override val hasMMIO: Boolean = afterS2
  override val replayToLRQ: Boolean = afterS2
  override val replayFromLRQ: Boolean = true
  override val hasVector: Boolean = true
  override val hasS3PreProcess: Boolean = afterS2
  override val hasS4PreProcess: Boolean = afterS3
  // override val hasWritebacked: Boolean =
  override val hasUnalignHandling: Boolean = true
}

class LoadReplayIO(implicit p: Parameters)
  extends LoadPipeBundle(LoadReplayIOParam())

class FastReplayIO(implicit p: Parameters)
  extends LoadPipeBundle(FastReplayIOParam())

class VectorLoadIn(implicit p: Parameters)
  extends LoadPipeBundle(VectorLoadInParam())

class LoadStageIO(implicit p: Parameters, implicit val s: LoadStage)
  extends LoadPipeBundle(LoadStageIOParam())