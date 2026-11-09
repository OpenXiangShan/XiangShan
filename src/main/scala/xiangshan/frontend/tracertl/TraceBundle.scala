/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.FrontendBundle
import xiangshan.frontend.FrontendModule
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.PcInit
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit

class TraceBundle(implicit p: Parameters) extends FrontendBundle {
  def trtl = p(TraceRTLParamKey)
}
class TraceModule(implicit p: Parameters) extends FrontendModule {
  def trtl = p(TraceRTLParamKey)

  // Trace addresses are TraceVAddrWidth wide; the frontend keeps VAddrBits.
  def traceGuardedPc(addr: UInt): GuardedPc = PcInit(addr(VAddrBits - 1, 0)).signGuard
  def tracePrunedAddr(addr: UInt): PrunedAddr = PrunedAddrInit(addr(VAddrBits - 1, 0))
}

class TraceInstrBundle(implicit p: Parameters) extends Bundle {
  def trtl = p(TraceRTLParamKey)

  val pcVA = UInt(trtl.TraceVAddrWidth.W)
  val pcPA = UInt(trtl.TracePAddrWidth.W)
  // Trace format v3 reuses these two 64-bit words for arithmetic operands.
  val memoryAddrVA = UInt(64.W)
  val memoryAddrPA = UInt(64.W)
  val target       = UInt(trtl.TraceVAddrWidth.W)
  val inst         = UInt(trtl.TraceInstCodeWidth.W)
  val memoryType   = UInt(4.W)
  val memorySize   = UInt(4.W)
  val branchType   = UInt(8.W)
  val branchTaken  = UInt(8.W)
  val exception    = UInt(8.W)

  val fastSimulation = UInt(8.W)
  val InstID         = UInt(trtl.TraceInstIDWidth.W)

  def arthiSrc0    = memoryAddrVA
  def arthiSrc1    = memoryAddrPA
  def isTaken      = branchTaken(0) === 1.U
  def hasException = exception =/= 0.U
  def isForceJump  = exception(7)
  def isRVC        = inst(1, 0) =/= 3.U
  def nextPc       = pcVA + Mux(isRVC, 2.U, 4.U)
}

/** High from the fifth cycle after reset, once the trace DPI side has initialised. */
object TraceWarmUp {
  def apply(): Bool = {
    val count = RegInit(0.U(3.W))
    when(count =/= 5.U) {
      count := count + 1.U
    }
    count === 5.U
  }
}

class TraceRecvInfo(implicit p: Parameters) extends TraceBundle {
  val instNum = UInt(log2Ceil(IBufferEnqueueWidth + 1).W)
}

/** One instruction after removing holes from the position-indexed representation. */
class TraceCompactEntry(implicit p: Parameters) extends TraceBundle {
  val valid             = Bool()
  val traceInfo         = new TraceInstrBundle
  val instrEndOffset    = UInt(FetchBlockInstOffsetWidth.W)
  val selectBlock       = Bool()
  val isPrevEndHalfRvi  = Bool()
  val isCrossBlockInstr = Bool()
}

/** A compact instruction enriched by TracePreDecoder. */
class TraceDecodedEntry(implicit p: Parameters) extends TraceBundle {
  val compact     = new TraceCompactEntry
  val pd          = new PreDecodeInfo
  val jumpOffset  = GuardedPc()
  val isPredTaken = Bool()
}

/** Complete combinational result of TraceAlignParallel for one S2 packet. */
class TraceAlignResult(implicit p: Parameters) extends TraceBundle {
  // halfword slots of the prediction window covered by the trace instructions
  val traceRange     = UInt(trtl.TracePredictWidth.W)
  val compact        = Vec(IBufferEnqueueWidth, new TraceCompactEntry)
  val candidateCount = UInt(log2Ceil(IBufferEnqueueWidth + 1).W)
  val concede2Bytes  = Bool()
  val traceForceJump = Bool()
}

class TraceFetchBlockPredictInfo(implicit p: Parameters) extends TraceBundle {
  val valid     = Bool()
  val startAddr = UInt(VAddrBits.W)
  val instRange = UInt(trtl.TracePredictWidth.W)
  val size      = UInt(log2Ceil(trtl.TracePredictWidth + 1).W)
  val ftqOffset = Valid(UInt(log2Ceil(trtl.TracePredictWidth).W))
}

class TracePredictInfo(implicit p: Parameters) extends TraceBundle {
  val block = Vec(FetchPorts, new TraceFetchBlockPredictInfo)
}
