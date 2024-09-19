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
import xiangshan.frontend.{PreDecodeResp, PredCheckerResp, BranchPredictionRedirect}

class TracePredictInfo(implicit p: Parameters) extends TraceBundle {
  val startAddr = UInt(VAddrBits.W)
  val nextStartAddr = UInt(VAddrBits.W)

  val instRange = UInt(PredictWidth.W)
  val ftqOffset = Valid(UInt(log2Ceil(PredictWidth).W))
}

class TraceFromIFU(implicit p: Parameters) extends TraceBundle {
  val redirect = Bool()
  val ibuffer_fire = Bool()
  val wb_enable = Bool()
  val valid = Bool()
}

class TraceFromDriver(implicit p: Parameters) extends TraceBundle {
  val endWithCFI = Bool()
}

class TracePCMatchBundle(implicit p: Parameters) extends TraceBundle {
  val pcVA = Output(UInt(VAddrBits.W))
  val found = Input(Bool())
}

class TracePreDecodeAndCheckerIO(implicit p: Parameters) extends TraceBundle {
  // IFU info
  val fromIFU = Input(new TraceFromIFU())
  // From TraceReader
  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // FakeICache to provide wrong path inst info at one fetch bundle.
  val icacheData = Input(Valid(new TraceFakeICacheRespBundle()))
  // From BPU and IFU
  val predInfo = Input(new TracePredictInfo())

  val fromTraceDriver = Input(new TraceFromDriver())

  val redirect = Input(new Bundle {
   val fromBackend = Valid(new BranchPredictionRedirect()) // backend -> ftq -> ifu
   val fromIFUBPU = Bool()
  })

  // Pre-decoder: normal predecoder
  val predecoder = Output(new PreDecodeResp())
  // Predict checker
  val checker = Output(new PredCheckerResp())
  // trace checker
  val traceChecker = Output(new TraceCheckerResp())
  // trace Inst: one-to-one with preDecoder but contains the traceInfo
  val traceAlignInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val traceForceJump = Output(Bool())

  val traceWrongPathEmu = Output(Bool())
  val traceWrongPathEmuInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle())))
  val traceWrongPathRecv = Output(ValidIO(new TraceRecvInfo()))
  // val traceWrongPathRange = Output(UInt(PredictWidth.W))

  val pcMatch = new TracePCMatchBundle()
}

class TracePreDecodeAndChecker(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new TracePreDecodeAndCheckerIO)
  dontTouch(io)

  val preDecoder = Module(new TracePreDecoder)
  val predChecker = Module(new TracePredictChecker)
  val traceChecker = Module(new TraceChecker)
  val traceAligner = Module(new TraceAlignToIFUCut)
  // This wrong path aligner is used to to generate the wrong path trace insts
  // wrong path instr comes from "arch" path
  val traceAlignerWrongPath = Module(new TraceAlignWrongPath)

  if (TraceEnableWrongPathEmu) {
    // bpu wrong that can only be checked by backend
    // 1. branch taken wrong
    // 2. jump reg target wrong
    // when these two happpen, turn to wrongPathState
    // The simple check:
    //   when stuck for pc-mismatch, and no ifu redirect, then go to wrongPathState
    val wrongPathStuckIFU = RegInit(false.B)
    val convergenceCheck =
      if (TraceWrongPathEmuWhenConvergence) io.pcMatch.found
      else true.B
    when (io.fromIFU.valid &&
      !Cat(traceAligner.io.cutInsts.map(_.valid)).orR &&
      convergenceCheck
      ) {
      wrongPathStuckIFU := true.B
    }
    when (io.redirect.fromBackend.valid || io.redirect.fromIFUBPU) {
      wrongPathStuckIFU := false.B
    }
    io.traceWrongPathEmu := wrongPathStuckIFU
    io.traceWrongPathRecv.valid := io.fromIFU.ibuffer_fire
    io.traceWrongPathRecv.bits.instNum := PopCount(io.traceWrongPathEmuInsts.map(_.valid))
  } else {
    io.traceWrongPathEmu := false.B
    io.traceWrongPathRecv.valid := false.B
    io.traceWrongPathRecv.bits := DontCare
  }

  val concede2Bytes = RegEnable(
    !io.fromIFU.redirect &&
    !io.fromTraceDriver.endWithCFI &&
    (traceAligner.io.instRangeTaken2B || traceAligner.io.traceRangeTaken2B),
    false.B,
    io.fromIFU.ibuffer_fire || io.fromIFU.redirect
  )
  // !lastFetchRedirect && lastFetchTakenMore2B && !lastEndWithCFI
  val traceInstIFUCut = traceAligner.io.cutInsts

  traceAligner.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := io.traceInsts,
    _.predStartAddr := io.predInfo.startAddr,
    _.instRange := io.predInfo.instRange,
    _.lastHalfValid := concede2Bytes,
    _.icacheData := io.icacheData,
  )
  traceAlignerWrongPath.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := io.traceInsts,
  )

  preDecoder.io.specifyField(
    _.traceInsts := traceInstIFUCut,
    _.pdValid := traceAligner.io.pdValid,
  )
  predChecker.io.specifyField(
    _.wb_enable := io.fromIFU.wb_enable,
    _.traceInsts := traceInstIFUCut,
    // _.pdValid := traceAligner.pdValid,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
    _.traceRange := traceAligner.io.traceRange,
  )
  traceChecker.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.predInfo,
    _.preDecode := preDecoder.io.out,
    _.predChecker := predChecker.io.out,
    _.traceRange := traceAligner.io.traceRange,
    _.traceForceJump := traceAligner.io.traceForceJump,
  )
  io.specifyField(
    _.predecoder := preDecoder.io.out,
    _.checker := predChecker.io.out,
    _.traceChecker := traceChecker.io.out,
    _.traceAlignInsts := traceAligner.io.cutInsts,
    _.traceForceJump := traceAligner.io.traceForceJump,
    _.traceWrongPathEmuInsts := traceAlignerWrongPath.io.cutInsts,
    _.pcMatch.pcVA := io.predInfo.startAddr,
    // _.traceWrongPathEmuRange := traceAlignerWrongPath.io.traceRange,
  )
}
