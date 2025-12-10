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
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.Bundles.DynInst
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.mem.LoadStage._

sealed trait HasLoadPipeBundleParam {
  def hasAddrTrans: Boolean = false
  def hasPAddrChecked: Boolean = false
  def replayToLRQ: Boolean = false
  def replayFromLRQ: Boolean = false
  def hasVector: Boolean = false
  def hasData: Boolean = false
  def hasWritebacked: Boolean = false

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
  with HasVLSUParameters {
  // basic info
  val entrance = LoadEntrance()
  val accessType = LoadAccessType()
  val uop = new DynInst
  val vaddr = UInt(VAddrBits.W)
  val size = UInt(3.W)
  val mask = UInt((VLEN/8).W)
  val paddr = Option.when(param.hasAddrTrans)(UInt(PAddrBits.W))
  val fullva = Option.when(param.hasAddrTrans)(UInt(XLEN.W))

  // MMU & exception handling
  val tlbAccessResult = Option.when(param.hasAddrTrans)(TlbAccessResult())
  val tlbException = Option.when(param.hasAddrTrans)(new TlbRespExcp)
  val pbmt = Option.when(param.hasAddrTrans)(Pbmt())
  val isForVSnonLeafPTE = Option.when(param.hasAddrTrans)(Bool())

  val pmp = Option.when(param.hasPAddrChecked)(new PMPRespBundle)

  // replay
  val mshrId = Option.when(param.replayFromToLRQ)(UInt(log2Up(cfg.nMissEntries).W)) // valid when `handledByMSHR` is HIGH
  val replayQueueIdx = Option.when(param.replayFromToLRQ)(UInt(log2Up(LoadQueueReplaySize+1).W)) // valid when `entrance` is replay
  val cause = Option.when(param.replayFromToLRQ)(Vec(LoadReplayCauses.allCauses, Bool()))

  val handledByMSHR = Option.when(param.replayToLRQ)(Bool())
  val fullForward = Option.when(param.replayToLRQ)(Bool())
  val dataInvalidSqIdx = Option.when(param.replayToLRQ)(new SqPtr)
  val addrInvalidSqIdx = Option.when(param.replayToLRQ)(new SqPtr)
  val tlbId = Option.when(param.replayToLRQ)(UInt(log2Up(loadfiltersize).W))
  val tlbFull = Option.when(param.replayToLRQ)(Bool())

  val forwardDChannel = Option.when(param.replayFromLRQ)(Bool())

  // vector
  val elemIdx = Option.when(param.hasVector)(UInt(elemIdxBits.W))
  val mbIndex = Option.when(param.hasVector)(UInt(vlmBindexBits.W))
  val regOffset = Option.when(param.hasVector)(UInt(vOffsetBits.W))
  val elemIdxInsideVd = Option.when(param.hasVector)(UInt(elemIdxBits.W))
  val vecBaseVaddr = Option.when(param.hasVector)(UInt(VAddrBits.W))
  val vecVaddrOffset = Option.when(param.hasVector)(UInt(VAddrBits.W)) // only used in s1 & s2, to generate vstart
  val vecTriggerMask = Option.when(param.hasVector)(UInt((VLEN/8).W))

  // data
  val data = Option.when(param.hasData)(UInt((VLEN+1).W))

  // virtualLoadQueue
  val writebacked = Option.when(hasWritebacked)(Bool()) // `updateAddrValid` in the original version

  // debug info and top-down
  // TODO: use Option
  val hasROBEntry = Bool()
  val missDbUpdated = Bool()
}

case class LoadReplayIOParam(
  override val replayFromLRQ: Boolean = true,
  override val hasVector: Boolean = true
) extends HasLoadPipeBundleParam

case class VectorLoadInParam(
  override val hasVector: Boolean = true
) extends HasLoadPipeBundleParam

case class LoadStageIOParam()(
  implicit val s: LoadStage
) extends HasLoadPipeBundleParam with OnLoadStage {
  // TODO
  // override val hasData: Boolean = afterS2
}

class LoadReplayIO(implicit p: Parameters) extends LoadPipeBundle(LoadReplayIOParam())
class VectorLoadIn(implicit p: Parameters) extends LoadPipeBundle(VectorLoadInParam())

class LoadStageIO(implicit p: Parameters, implicit val s: LoadStage) extends LoadPipeBundle(LoadStageIOParam()) {
  val unalignHead = Bool()
}