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
import utility.ParallelPosteriorityMux
import xiangshan.frontend.tracertl.ChiselRecordForField._
import xiangshan.frontend.{PreDecodeResp, PredCheckerResp}

class TracePredictInfo(implicit p: Parameters) extends TraceBundle {
  val startAddr = UInt(VAddrBits.W)
  val nextStartAddr = UInt(VAddrBits.W)

  val instRange = UInt(PredictWidth.W)
  val ftqOffset = Valid(UInt(log2Ceil(PredictWidth).W))
}

class TraceFromIFU(implicit p: Parameters) extends TraceBundle {
  val redirect = Bool()
  val fire = Bool()
  val valid = Bool()
}

class TraceFromDriver(implicit p: Parameters) extends TraceBundle {
  val endWithCFI = Bool()
}

class TracePreDecodeAndCheckerIO(implicit p: Parameters) extends TraceBundle {
  // IFU info
  val fromIFU = Input(new TraceFromIFU())
  // From TraceReader
  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // From BPU and IFU
  val predInfo = Input(new TracePredictInfo())

  val fromTraceDriver = Input(new TraceFromDriver())

  // Pre-decoder: normal predecoder
  val predecoder = Output(new PreDecodeResp())
  // Predict checker
  val checker = Output(new PredCheckerResp())
  // trace checker
  val traceChecker = Output(new TraceCheckerResp())
  // trace Inst: one-to-one with preDecoder but contains the traceInfo
  val traceAlignInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle())))
//  val traceExpandInsts = Output(Vec(PredictWidth, UInt(32.W)))
}

class TracePreDecodeAndChecker(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new TracePreDecodeAndCheckerIO)
  dontTouch(io)

  val preDecoder = Module(new TracePreDecoder)
  val predChecker = Module(new TracePredictChecker)
  val traceChecker = Module(new TraceChecker)
  val traceAligner = Module(new TraceAlignToIFUCut)

  val concede2Bytes = RegEnable(
    !io.fromIFU.redirect &&
    !io.fromTraceDriver.endWithCFI &&
    (traceAligner.io.instRangeTaken2B || traceAligner.io.traceRangeTaken2B),
    io.fromIFU.fire || io.fromIFU.redirect
  )
  // !lastFetchRedirect && lastFetchTakenMore2B && !lastEndWithCFI
  val traceInstIFUCut = traceAligner.io.cutInsts

  traceAligner.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := io.traceInsts,
    _.predStartAddr := io.predInfo.startAddr,
    _.instRange := io.predInfo.instRange,
    _.lastHalfValid := concede2Bytes
  )

  preDecoder.io.specifyField(
    _.traceInsts := traceInstIFUCut,
  )
  predChecker.io.specifyField(
    _.fire_in := io.fromIFU.fire,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
  )
  traceChecker.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
    _.predChecker := predChecker.io.out,
    _.traceRange := traceAligner.io.traceRange,
  )
  io.specifyField(
    _.predecoder := preDecoder.io.out,
    _.checker := predChecker.io.out,
    _.traceChecker := traceChecker.io.out,
    _.traceAlignInsts := traceAligner.io.cutInsts,
  )
}
