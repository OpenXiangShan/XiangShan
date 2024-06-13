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
import utility.ParallelPriorityEncoder
import utils.{XSError}
import xiangshan.frontend.{PreDecodeResp, PredCheckerResp, FaultType}

// TracePredictChecker is just the same with the IFU's PredChecker
// with no regnext for stage2Out and different input
class TracePredictChecker(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new Bundle {
    val fire_in = Input(Bool())
    val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
    val predictInfo = Input(new TracePredictInfo)
    val preDecode = Input(new PreDecodeResp)
    val out = Output(new PredCheckerResp)
  })

  val pds = io.preDecode.pd
  val pcs = io.traceInsts.map(_.bits.pc)

  val predRange = io.predictInfo.instRange
  val takenIdx = io.predictInfo.ftqOffset.bits
  val predTaken = io.predictInfo.ftqOffset.valid
  val predTarget = io.predictInfo.nextStartAddr
  val instValid = io.preDecode.pd.map(_.valid)

  // Fault
  val jalFaultVec = (0 until PredictWidth).map(i => {
    pds(i).isJal &&
      pds(i).valid && predRange(i) &&
      (takenIdx > i.U && predTaken || !predTaken)
  })
  val retFaultVec = (0 until PredictWidth).map(i => {
    pds(i).isRet &&
      pds(i).valid && predRange(i) &&
      (takenIdx > i.U && predTaken || !predTaken)
  })
  val remaskFault = jalFaultVec.zip(retFaultVec).map {
    case (jal, ret) => jal || ret
  }
  val remaskIdx = ParallelPriorityEncoder(remaskFault)
  val needRemask = Cat(remaskFault).orR
  val AllTrueMask = -1.S(PredictWidth.W).asUInt
  val fixedRange = Mux(needRemask, AllTrueMask >> (PredictWidth.U - (remaskIdx + 1.U)), predRange)

  io.out.stage1Out.fixedRange := fixedRange.asTypeOf(Vec(PredictWidth, Bool()))
  io.out.stage1Out.fixedTaken := VecInit((0 until PredictWidth).map(i => {
    pds(i).valid && predRange(i) &&
      (pds(i).isRet || pds(i).isJal ||
        (takenIdx === i.U && predTaken && !pds(i).notCFI))
  }))

  val notCFITaken = (0 until PredictWidth).map(i => {
    fixedRange(i) && pds(i).valid &&
      (i.U === takenIdx) && predTaken && pds(i).notCFI
  })
  val invalidTaken = (0 until PredictWidth).map(i => {
    fixedRange(i) && !pds(i).valid &&
      (i.U === takenIdx) && predTaken
  })

  val jumpTargets = (0 until PredictWidth).map(i => {
    (pcs(i) + io.preDecode.jumpOffset(i)).asTypeOf(UInt(VAddrBits.W))
  })
  val seqTargets = (0 until PredictWidth).map(i => {
    pcs(i) + (Mux(pds(i).isRVC || !pds(i).valid, 2.U, 4.U))
  })

  val targetFault = (0 until PredictWidth).map(i => {
    fixedRange(i) && pds(i).valid &&
      (pds(i).isJal || pds(i).isBr) &&
      (takenIdx === i.U) && predTaken &&
      (jumpTargets(i) =/= predTarget)
  })

  val stage2Out = Wire(chiselTypeOf(io.out.stage2Out))
  (0 until PredictWidth).foreach(i => {
    stage2Out.faultType(i).value := Mux(jalFaultVec(i), FaultType.jalFault,
      Mux(retFaultVec(i), FaultType.retFault,
        Mux(targetFault(i), FaultType.targetFault,
          Mux(notCFITaken(i), FaultType.notCFIFault,
            Mux(invalidTaken(i), FaultType.invalidTaken, FaultType.noFault)))))
    stage2Out.fixedMissPred(i) := jalFaultVec(i) || retFaultVec(i) ||
      targetFault(i) || notCFITaken(i) || invalidTaken(i)
    stage2Out.fixedTarget(i) := Mux(jalFaultVec(i) || targetFault(i), jumpTargets(i), seqTargets(i))
    stage2Out.jalTarget(i) := jumpTargets(i)
  })
  io.out.stage2Out := RegEnable(stage2Out, io.fire_in)

  // debug
  instValid.zip(io.traceInsts).foreach { case (valid, trace) =>
    XSError(valid =/= trace.valid, "instValid should be the same with trace.valid")
  }
}
