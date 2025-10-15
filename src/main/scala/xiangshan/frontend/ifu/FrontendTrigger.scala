// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSDebug
import utils.PrintTriggerInfo
import xiangshan.FrontendTdataDistributeIO
import xiangshan.MatchTriggerIO
import xiangshan.TriggerAction
import xiangshan.backend.fu.NewCSR.TriggerUtil
import xiangshan.backend.fu.util.SdtrigExt
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr

// IFU trigger pre-marks instructions that may be executed.
// Actual triggering is determined by the backend (pre-match stage).
class FrontendTrigger(implicit p: Parameters) extends IfuModule with SdtrigExt {
  class FrontendTriggerIO(implicit p: Parameters) extends IfuBundle {
    val frontendTrigger: FrontendTdataDistributeIO = Input(new FrontendTdataDistributeIO)
    val triggered:       Vec[UInt]                 = Output(Vec(IBufferEnqueueWidth, TriggerAction()))

    val pds: Vec[PreDecodeInfo] = Input(Vec(IBufferEnqueueWidth, new PreDecodeInfo))
    val pc:  Vec[PrunedAddr]    = Input(Vec(IBufferEnqueueWidth, PrunedAddr(VAddrBits)))
    val data: Vec[UInt] =
      if (HasCExtension) Input(Vec(IBufferEnqueueWidth + 1, UInt(16.W)))
      else Input(Vec(IBufferEnqueueWidth, UInt(32.W)))
  }
  val io: FrontendTriggerIO = IO(new FrontendTriggerIO)

  // Currently, FrontendTrigger supports pc match only, data/pds is reserved for future use
  private val data = io.data
  private val rawInsts =
    if (HasCExtension) VecInit((0 until IBufferEnqueueWidth).map(i => Cat(data(i + 1), data(i))))
    else VecInit((0 until IBufferEnqueueWidth).map(i => data(i)))

  private val tdataVec = RegInit(VecInit(Seq.fill(TriggerNum)(0.U.asTypeOf(new MatchTriggerIO))))
  when(io.frontendTrigger.tUpdate.valid) {
    tdataVec(io.frontendTrigger.tUpdate.bits.addr) := io.frontendTrigger.tUpdate.bits.tdata
  }
  private val triggerEnableVec =
    RegInit(VecInit(Seq.fill(TriggerNum)(false.B))) // From CSR, controlled by priv mode, etc.
  triggerEnableVec := io.frontendTrigger.tEnableVec
  XSDebug(triggerEnableVec.asUInt.orR, "Debug Mode: At least one frontend trigger is enabled\n")

  private val triggerTimingVec = VecInit(tdataVec.map(_.timing))
  private val triggerChainVec  = VecInit(tdataVec.map(_.chain))

  for (i <- 0 until TriggerNum) { PrintTriggerInfo(triggerEnableVec(i), tdataVec(i)) }

  private val debugMode            = io.frontendTrigger.debugMode
  private val triggerCanRaiseBpExp = io.frontendTrigger.triggerCanRaiseBpExp
  // val triggerHitVec = Wire(Vec(FetchBlockInstNum, Vec(TriggerNum, Bool())))
  private val triggerHitVec = (0 until TriggerNum).map { j =>
    TriggerCmpConsecutive(
      VecInit(io.pc.map(_.toUInt)),
      tdataVec(j).tdata2,
      tdataVec(j).matchType,
      triggerEnableVec(j)
    ).map(hit => hit && !tdataVec(j).select && !debugMode)
  }.transpose

  for (i <- 0 until IBufferEnqueueWidth) {
    val triggerCanFireVec = Wire(Vec(TriggerNum, Bool()))
    TriggerCheckCanFire(TriggerNum, triggerCanFireVec, VecInit(triggerHitVec(i)), triggerTimingVec, triggerChainVec)

    val actionVec     = VecInit(tdataVec.map(_.action))
    val triggerAction = Wire(TriggerAction())
    TriggerUtil.triggerActionGen(triggerAction, triggerCanFireVec, actionVec, triggerCanRaiseBpExp)

    // Priority may select last when no trigger fire.
    io.triggered(i) := triggerAction
    XSDebug(
      triggerCanFireVec.asUInt.orR,
      p"Debug Mode: Predecode Inst No. $i has trigger action vec ${triggerCanFireVec.asUInt.orR}\n"
    )
  }
}
