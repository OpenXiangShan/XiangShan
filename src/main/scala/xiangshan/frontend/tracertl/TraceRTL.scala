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
import utility.{ParallelPosteriorityMux, XSError}
import xiangshan.frontend.tracertl.ChiselRecordForField._
import xiangshan.frontend.{FetchRequestBundle, PreDecodeResp, PredCheckerResp, BranchPredictionRedirect}

class TracePredictInfo(implicit p: Parameters) extends TraceBundle {
  val startAddr = UInt(VAddrBits.W)
  val nextStartAddr = UInt(VAddrBits.W)

  val instRange = UInt(PredictWidth.W)
  val ftqOffset = Valid(UInt(log2Ceil(PredictWidth).W))
}

class TraceFromIFU(implicit p: Parameters) extends TraceBundle {
  val redirect = Bool()
  val f2_flush = Bool()
  val f2_fire = Bool()
  val f3_fire = Bool()
  val ibuffer_fire = Bool()
  val wb_enable = Bool()
  val valid = Bool()

  // f3
  val f3_ready = Bool()

  // IFU2
  val f2_ftq_req = Input(new FetchRequestBundle())
  // IFU3
  val predInfo = Input(new TracePredictInfo())
}

class TraceFromDriver(implicit p: Parameters) extends TraceBundle {
  val endWithCFI = Bool()
}

class TracePCMatchBundle(implicit p: Parameters) extends TraceBundle {
  val pcVA = Output(UInt(VAddrBits.W))
  val found = Input(Bool())
}

class TraceRTLIO(implicit p: Parameters) extends TraceBundle {
  val fromIFU = Input(new TraceFromIFU())
  val redirect = Input(new Bundle {
   val fromBackend = Valid(new BranchPredictionRedirect()) // backend -> ftq -> ifu
   val fromIFUBPU = Bool()
  })

  // data
  val predecoder = Output(new PreDecodeResp())
  val checker = Output(new PredCheckerResp())
  val traceChecker = Output(new TraceCheckerResp())
  val traceAlignInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle())))

  val traceForceJump = Output(Bool())
  val traceWrongPathEmu = Output(Bool())
  val block = Output(Bool())
}

class TraceRTL(implicit p: Parameters) extends TraceModule
  with TraceParams {
  val io = IO(new TraceRTLIO)
  dontTouch(io)

if (env.TraceRTLMode) {
  val traceReader = Module(new TraceReader)
  val traceDriver = Module(new TraceDriver)
  val traceFakeICache = Module(new TraceFakeICache)

  val preDecoder = Module(new TracePreDecoder)
  val predChecker = Module(new TracePredictChecker)
  val traceChecker = Module(new TraceChecker)
  // val traceAlignerOld = Module(new TraceAlignToIFUCut)
  val traceAligner = Module(new TraceAlignParallel)
  // This wrong path aligner is used to to generate the wrong path trace insts
  // wrong path instr comes from "arch" path
  // FIXME: add param to control FPGA-WrongPathEmu
  // val traceAlignerWrongPath = Module(new TraceAlignWrongPath)
  val traceFakeICacheWrapper = Module(new TraceFakeICacheWrapper)

  val traceWrongPathEmu = RegInit(false.B)
  val fastSimInst = traceReader.io.traceInsts.bits(0).isFastSim
  if (trtl.TraceEnableWrongPathEmu) {
    // bpu wrong that can only be checked by backend
    // 1. branch taken wrong
    // 2. jump reg target wrong
    // when these two happpen, turn to wrongPathState
    // The simple check:
    //   when stuck for pc-mismatch, and no ifu redirect, then go to wrongPathState
    val convergenceCheck =
      if (trtl.TraceWrongPathEmuWhenConvergence) traceReader.io.pcMatch.found
      else true.B
    when (io.fromIFU.valid &&
      !Cat(traceAligner.io.cutInsts.map(_.valid)).orR &&
      !fastSimInst &&
      convergenceCheck
      ) {
      // traceWrongPathEmu := true.B
    }
    when (io.redirect.fromBackend.valid || io.redirect.fromIFUBPU) {
      // traceWrongPathEmu := false.B
    }
  }

  // f2_flush comes from ftq, from instruction self-modify/changes
  // when ibuffer block, but f2_flush comes from ftq <- pdwb, which is not blocked
  val concede2BytesWire = !io.fromIFU.redirect &&
      !io.fromIFU.f2_flush &&
      !traceDriver.io.out.endWithCFI &&
      (traceAligner.io.instRangeTaken2B || traceAligner.io.traceRangeTaken2B)
  val concede2BytesNonBlock = RegInit(false.B)
  val concede2BytesNonBlockValid = RegInit(false.B)
  val concede2Bytes = RegInit(false.B)

  // f2_flush is acutally from wb_stage(f4_stage)
  when (io.fromIFU.redirect || io.fromIFU.f2_flush) {
    // when redirect or f2_flush, then clear the concede2BytesNonBlock
    concede2BytesNonBlock := false.B
  }.elsewhen (io.fromIFU.valid && !concede2BytesNonBlockValid) {
    // when f3_valid and not repeat set, then set the concede2BytesNonBlock
    concede2BytesNonBlock := concede2BytesWire
  }

  when (io.fromIFU.redirect || io.fromIFU.f3_fire) {
    // when f3_fire or redirect, clear
    concede2BytesNonBlockValid := false.B
  }.elsewhen (io.fromIFU.valid &&
    !io.fromIFU.f3_ready) { // f3 not fire for ibuffer not ready
    // when f3_valid and not blocked by tracertl, and blocked by ibuffer, then set
    concede2BytesNonBlockValid := true.B
  }

  // FIXME: concede2Bytes is too fragile, should be re-designed
  when (io.fromIFU.redirect || (io.fromIFU.f2_flush && !io.fromIFU.valid)) {
    // when redirect or f2_flush, clear
    //   when f3 is vaid, concede2Bytes is for f3, should not clear by f2_flush
    concede2Bytes := false.B
  }.elsewhen (io.fromIFU.f3_fire) {
    // when f3_fire, then set
    concede2Bytes := Mux(concede2BytesNonBlockValid, concede2BytesNonBlock, concede2BytesWire)
  }

  // val concede2Bytes = RegEnable(
  //   !io.fromIFU.redirect &&
  //   !io.fromIFU.f2_flush && // when f2_flush, then f2 will not continue with f3, clear f3's side-effect, f2_flush is actually from wb_stage(f4_stage)
  //   !traceDriver.io.out.endWithCFI &&
  //   (traceAligner.io.instRangeTaken2B || traceAligner.io.traceRangeTaken2B),
  //   false.B,
  //   io.fromIFU.ibuffer_fire || io.fromIFU.redirect || io.fromIFU.f2_flush
  // )
  // !lastFetchRedirect && lastFetchTakenMore2B && !lastEndWithCFI
  val traceInstIFUCut = traceAligner.io.cutInsts

  val traceRecv = WireInit(traceDriver.io.out.recv)
  when (traceWrongPathEmu) {
    traceRecv.valid := io.fromIFU.ibuffer_fire
    traceRecv.bits.instNum := PopCount(io.traceAlignInsts.map(_.valid))
  }

  /*** IO Connect ***/

  traceReader.io.specifyField(
    _.recv := traceRecv,
    _.redirect := io.redirect.fromBackend,
    _.pcMatch.pcVA := io.fromIFU.predInfo.startAddr,
  )
  traceDriver.io.specifyField(
    _.fire := io.fromIFU.f3_fire,
    _.traceInsts := traceAligner.io.cutInsts,
    _.traceRange := traceChecker.io.traceRange,
    _.predInfo := io.fromIFU.predInfo, // duplicate signal, just to speed up waveform debug
    _.ifuRange := predChecker.io.out.stage1Out.fixedRange.asUInt,
    _.redirect := io.redirect,
    _.otherBlock := !traceReader.io.traceInsts.valid,
  )
  traceFakeICache.io.specifyField(
    _.req.valid := io.fromIFU.f2_fire,
    _.req.bits.addr := io.fromIFU.f2_ftq_req.startAddr,
  )
  traceFakeICacheWrapper.io.specifyField(
    _.traceInsts := traceReader.io.traceInsts.bits,
    _.icacheDataIn := traceFakeICache.io.resp,
  )
  traceAligner.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := traceReader.io.traceInsts,
    _.predStartAddr := io.fromIFU.predInfo.startAddr,
    _.instRange := io.fromIFU.predInfo.instRange,
    _.lastHalfValid := concede2Bytes,
    // _.icacheData := traceFakeICache.io.resp,
    _.icacheData := traceFakeICacheWrapper.io.icacheDataOut,
  )
  // traceAlignerWrongPath.io.specifyField(
  //   _.debug_valid := io.fromIFU.valid,
  //   _.traceInsts := traceReader.io.traceInsts,
  // )
  preDecoder.io.specifyField(
    _.traceInsts := traceInstIFUCut,
    _.pdValid := traceAligner.io.pdValid,
  )
  predChecker.io.specifyField(
    _.wb_enable := io.fromIFU.wb_enable,
    _.traceInsts := traceInstIFUCut,
    // _.pdValid := traceAligner.pdValid,
    _.predictInfo := io.fromIFU.predInfo,
    _.preDecode := preDecoder.io.out,
    _.traceRange := traceAligner.io.traceRange,
  )
  traceChecker.io.specifyField(
    _.debug_valid := io.fromIFU.valid,
    _.traceInsts := traceInstIFUCut,
    _.predictInfo := io.fromIFU.predInfo,
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
    _.block := traceDriver.io.out.block,
    _.traceWrongPathEmu := traceWrongPathEmu,
  )
  // when (io.traceWrongPathEmu) {
  //   io.traceAlignInsts := traceAlignerWrongPath.io.cutInsts
  // }

  // check old traceAligner and new
  // traceAlignerOld.io.specifyField(
  //   _.debug_valid := io.fromIFU.valid,
  //   _.traceInsts := traceReader.io.traceInsts,
  //   _.predStartAddr := io.fromIFU.predInfo.startAddr,
  //   _.instRange := io.fromIFU.predInfo.instRange,
  //   _.lastHalfValid := concede2Bytes,
  //   _.icacheData := traceFakeICache.io.resp,
  // )

  // // XSError(traceAligner.io.traceRange.asUInt =/= traceAlignerOld.io.traceRange.asUInt,
  // XSError((traceAligner.io.traceRange.asUInt & traceAlignerOld.io.traceRange.asUInt) =/= traceAlignerOld.io.traceRange.asUInt,
  //   "TraceAlignParallel and TraceAlignToIFUCut should have the same traceRange")
  // XSError(traceAligner.io.pdValid.asUInt =/= traceAlignerOld.io.pdValid.asUInt,
  //   "TraceAlignParallel and TraceAlignToIFUCut should have the same pdValid")
  // XSError(traceAligner.io.traceForceJump =/= traceAlignerOld.io.traceForceJump,
  //   "TraceAlignParallel and TraceAlignToIFUCut should have the same traceForceJump")
  // XSError(traceAligner.io.traceRangeTaken2B =/= (traceAlignerOld.io.traceRangeTaken2B || traceAlignerOld.io.instRangeTaken2B),
  //   "TraceAlignParallel and TraceAlignToIFUCut should have the same traceRangeTaken2B")
} else {
  io <> DontCare
}

}
