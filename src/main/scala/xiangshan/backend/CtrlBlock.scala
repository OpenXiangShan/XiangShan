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

package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfoBundle, LsTopdownInfo, MemCtrl, RedirectGenerator}
import xiangshan.backend.datapath.DataConfig.{FpData, IntData, V0Data, VAddrData, VecData, VlData}
import xiangshan.backend.decode.{DecodeStage, FusionDecoder}
import xiangshan.backend.dispatch.CoreDispatchTopDownIO
import xiangshan.backend.dispatch.NewDispatch
import xiangshan.backend.fu.vector.Bundles.{VType, Vl}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.{Rename, RenameTableWrapper, SnapshotGenerator}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.frontend.ftq.{FtqPtr, FtqRead, HasFtqParameters}
import xiangshan.frontend.PrunedAddr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.issue.{FpScheduler, IntScheduler, VecScheduler}
import xiangshan.backend.trace._

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))

  val redirect = Valid(new Redirect)
  val ftqIdxAhead = Vec(BackendRedirectNum, Valid(new FtqPtr))
  val ftqIdxSelOH = Valid(UInt((BackendRedirectNum).W))

  val resolve = Vec(backendParams.BrhCnt, Valid(new Resolve))
}

class CtrlBlock(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  val rob = LazyModule(new Rob(params))

  lazy val module = new CtrlBlockImp(this)(p, params)

  val gpaMem = LazyModule(new GPAMem())
}

class CtrlBlockImp(
  override val wrapper: CtrlBlock
)(implicit
  p: Parameters,
  params: BackendParams
) extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasCircularQueuePtrHelper
  with HasPerfEvents
  with HasCriticalErrors
{
  val pcMemRdIndexes = new NamedIndexes(Seq(
    "redirect"  -> 1,
    "memPred"   -> 1,
    "robFlush"  -> 1,
    "bjuPc"     -> params.BrhCnt,
    "bjuTarget" -> params.BrhCnt,
    "load"      -> params.LduCnt,
    "hybrid"    -> params.HyuCnt,
    "store"     -> (if(EnableStorePrefetchSMS) params.StaCnt else 0),
    "trace"     -> TraceGroupNum
  ))

  private val numPcMemRead = pcMemRdIndexes.maxIdx

  // now pcMem read for exu is moved to PcTargetMem (OG0)
  println(s"pcMem read num: $numPcMemRead")

  val io = IO(new CtrlBlockIO())

  val dispatch = Module(new NewDispatch)
  val gpaMem = wrapper.gpaMem.module
  val decode = Module(new DecodeStage)
  val fusionDecoder = Module(new FusionDecoder)
  val rat = Module(new RenameTableWrapper)
  val rename = Module(new Rename)
  val redirectGen = Module(new RedirectGenerator)
  private def hasRen: Boolean = true
  private val pcMem = Module(new SyncDataModuleTemplate(PrunedAddr(VAddrBits), FtqSize, numPcMemRead, 1, "BackendPC", hasRen = hasRen))
  private val rob = wrapper.rob.module
  private val memCtrl = Module(new MemCtrl(params))

  private val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable

  private val s0_robFlushRedirect = rob.io.flushOut
  private val s1_robFlushRedirect = Wire(Valid(new Redirect))
  s1_robFlushRedirect.valid := GatedValidRegNext(s0_robFlushRedirect.valid, false.B)
  s1_robFlushRedirect.bits := RegEnable(s0_robFlushRedirect.bits, s0_robFlushRedirect.valid)

  pcMem.io.ren.get(pcMemRdIndexes("robFlush").head) := s0_robFlushRedirect.valid
  pcMem.io.raddr(pcMemRdIndexes("robFlush").head) := s0_robFlushRedirect.bits.ftqIdx.value
  val robFlushPCOffset = Reg(UInt(VAddrBits.W))
  when(s0_robFlushRedirect.valid) {
    robFlushPCOffset := s0_robFlushRedirect.bits.getPcOffset()
  }
  private val s1_robFlushPc = pcMem.io.rdata(pcMemRdIndexes("robFlush").head).toUInt + robFlushPCOffset
  private val s3_redirectGen = redirectGen.io.stage2Redirect
  private val s1_s3_redirect = Mux(s1_robFlushRedirect.valid, s1_robFlushRedirect, s3_redirectGen)
  private val s2_s4_pendingRedirectValid = RegInit(false.B)
  when (s1_s3_redirect.valid) {
    s2_s4_pendingRedirectValid := true.B
  }.elsewhen (GatedValidRegNext(io.frontend.toFtq.redirect.valid)) {
    s2_s4_pendingRedirectValid := false.B
  }

  // Redirect will be RegNext at ExuBlocks and IssueBlocks
  val s2_s4_redirect = RegNextWithEnable(s1_s3_redirect)
  val s3_s5_redirect = RegNextWithEnable(s2_s4_redirect)

  private val delayedNotFlushedWriteBack = io.fromWB.wbData.map(x => {
    val valid = x.valid
    val killedByOlder = x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect))
    val delayed = Wire(Valid(new ExuOutput(x.bits.params)))
    delayed.valid := GatedValidRegNext(valid && !killedByOlder)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed.bits.debugInfo.writebackTime := GTimer()
    delayed
  }).toSeq
  private val delayedWriteBack = Wire(chiselTypeOf(io.fromWB.wbData))
  delayedWriteBack.zipWithIndex.map{ case (x,i) =>
    x.valid := GatedValidRegNext(io.fromWB.wbData(i).valid)
    x.bits := delayedNotFlushedWriteBack(i).bits
  }
  val delayedNotFlushedWriteBackNeedFlush = Wire(Vec(params.allExuParams.filter(_.needExceptionGen).length, Bool()))
  delayedNotFlushedWriteBackNeedFlush := delayedNotFlushedWriteBack.filter(_.bits.params.needExceptionGen).map{ x =>
    x.bits.exceptionVec.get.asUInt.orR || x.bits.flushPipe.getOrElse(false.B) || x.bits.replay.getOrElse(false.B) ||
      (if (x.bits.trigger.nonEmpty) TriggerAction.isDmode(x.bits.trigger.get) else false.B)
  }

  val wbData = io.fromWB.wbData
  val intScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[IntScheduler])
  val fpScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[FpScheduler])
  val vfScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[VecScheduler])
  val storeWbData = io.fromWB.wbData.filter(_.bits.params.hasStoreFu)
  val i2vWbData = intScheWbData.filter(_.bits.params.writeVecRf)
  val f2vWbData = fpScheWbData.filter(_.bits.params.writeVecRf)
  val memVloadWbData = io.fromWB.wbData.filter(x => x.bits.params.hasVLoadFu)
  private val delayedNotFlushedWriteBackNums = wbData.map(x => {
    val valid = x.valid
    val killedByOlder = x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect, s3_s5_redirect))
    val delayed = Wire(Valid(UInt(io.fromWB.wbData.size.U.getWidth.W)))
    delayed.valid := GatedValidRegNext(valid && !killedByOlder)
    val isIntSche = intScheWbData.contains(x)
    val isFpSche = fpScheWbData.contains(x)
    val isVfSche = vfScheWbData.contains(x)
    val isMemVload = memVloadWbData.contains(x)
    val isi2v = i2vWbData.contains(x)
    val isf2v = f2vWbData.contains(x)
    val isStore = storeWbData.contains(x)
    val canSameRobidxWbData = if(isVfSche) {
      i2vWbData ++ f2vWbData ++ vfScheWbData
    } else if(isi2v) {
      intScheWbData ++ fpScheWbData ++ vfScheWbData
    } else if (isf2v) {
      intScheWbData ++ fpScheWbData ++ vfScheWbData
    } else if (isIntSche) {
      intScheWbData ++ fpScheWbData
    } else if (isFpSche) {
      intScheWbData ++ fpScheWbData
    } else if (isStore) {
      storeWbData
    } else if (isMemVload) {
      memVloadWbData
    } else {
      Seq(x)
    }
    val sameRobidxBools = VecInit(canSameRobidxWbData.map( wb => {
      val killedByOlderThat = wb.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect, s3_s5_redirect))
      (wb.bits.robIdx === x.bits.robIdx) && wb.valid && x.valid && !killedByOlderThat && !killedByOlder
    }).toSeq)
    delayed.bits := RegEnable(PopCount(sameRobidxBools), x.valid)
    delayed
  }).toSeq

  private val exuPredecode = VecInit(
    io.fromWB.wbData.filter(_.bits.redirect.nonEmpty).map(x => x.bits.predecodeInfo.get).toSeq
  )

  private val exuRedirects: Seq[ValidIO[Redirect]] = io.fromWB.wbData.filter(_.bits.redirect.nonEmpty).map(x => {
    val hasCSR = x.bits.params.hasCSR
    val out = Wire(Valid(new Redirect()))
    out.valid := x.valid && x.bits.redirect.get.valid &&
      (x.bits.redirect.get.bits.isMisPred ||
        x.bits.redirect.get.bits.hasBackendFault) && !x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect))
    out.bits := x.bits.redirect.get.bits
    out.bits.debugIsCtrl := true.B
    out.bits.debugIsMemVio := false.B
    // for fix timing, next cycle assgin
    if (!hasCSR) {
      out.bits.backendIAF := false.B
      out.bits.backendIPF := false.B
      out.bits.backendIGPF := false.B
    }
    out
  }).toSeq
  private val oldestOneHot = Redirect.selectOldestRedirect(exuRedirects)
  private val CSROH = VecInit(io.fromWB.wbData.filter(_.bits.redirect.nonEmpty).map(x => x.bits.params.hasCSR.B))
  private val oldestExuRedirectIsCSR = oldestOneHot === CSROH
  private val oldestExuRedirect = Mux1H(oldestOneHot, exuRedirects)
  private val oldestExuPredecode = Mux1H(oldestOneHot, exuPredecode)

  private val memViolation = io.fromMem.violation
  val loadReplay = Wire(ValidIO(new Redirect))
  loadReplay.valid := GatedValidRegNext(memViolation.valid)
  loadReplay.bits := RegEnable(memViolation.bits, memViolation.valid)
  loadReplay.bits.debugIsCtrl := false.B
  loadReplay.bits.debugIsMemVio := true.B

  pcMem.io.ren.get(pcMemRdIndexes("redirect").head) := memViolation.valid
  pcMem.io.raddr(pcMemRdIndexes("redirect").head) := memViolation.bits.ftqIdx.value
  pcMem.io.ren.get(pcMemRdIndexes("memPred").head) := memViolation.valid
  pcMem.io.raddr(pcMemRdIndexes("memPred").head) := memViolation.bits.stFtqIdx.value
  val memPredPCOffset = Reg(UInt(VAddrBits.W))
  when(memViolation.valid) {
    memPredPCOffset := memViolation.bits.getPcOffset()
  }
  redirectGen.io.memPredPcRead.data := pcMem.io.rdata(pcMemRdIndexes("memPred").head).toUInt + memPredPCOffset

  for ((pcMemIdx, i) <- pcMemRdIndexes("bjuPc").zipWithIndex) {
    val ren = io.toDataPath.pcToDataPathIO.fromDataPathValid(i)
    val raddr = io.toDataPath.pcToDataPathIO.fromDataPathFtqPtr(i).value
    val roffset = io.toDataPath.pcToDataPathIO.fromDataPathFtqOffset(i)
    pcMem.io.ren.get(pcMemIdx) := ren
    pcMem.io.raddr(pcMemIdx) := raddr
    io.toDataPath.pcToDataPathIO.toDataPathPC(i) := pcMem.io.rdata(pcMemIdx).toUInt
  }

  val newestEn = RegNext(io.frontend.fromFtq.newest_entry_en)
  val newestTarget = RegEnable(io.frontend.fromFtq.newest_entry_target, io.frontend.fromFtq.newest_entry_en)
  val newestPtr = RegEnable(io.frontend.fromFtq.newest_entry_ptr, io.frontend.fromFtq.newest_entry_en)
  val newestTargetNext = RegEnable(newestTarget, newestEn)
  for ((pcMemIdx, i) <- pcMemRdIndexes("bjuTarget").zipWithIndex) {
    val ren = io.toDataPath.pcToDataPathIO.fromDataPathValid(i)
    val baseAddr = io.toDataPath.pcToDataPathIO.fromDataPathFtqPtr(i).value
    val raddr = io.toDataPath.pcToDataPathIO.fromDataPathFtqPtr(i).value + 1.U
    pcMem.io.ren.get(pcMemIdx) := ren
    pcMem.io.raddr(pcMemIdx) := raddr
    val needNewest = RegNext(baseAddr === newestPtr.value)
    io.toDataPath.pcToDataPathIO.toDataPathTargetPC(i) := Mux(needNewest, newestTargetNext, pcMem.io.rdata(pcMemIdx).toUInt)
  }

  val baseIdx = params.BrhCnt
  for ((pcMemIdx, i) <- pcMemRdIndexes("load").zipWithIndex) {
    // load read pcMem (s0) -> get rdata (s1) -> reg next in Memblock (s2) -> reg next in Memblock (s3) -> consumed by pf (s3)
    val ren = io.toDataPath.pcToDataPathIO.fromDataPathValid(baseIdx+i)
    val raddr = io.toDataPath.pcToDataPathIO.fromDataPathFtqPtr(baseIdx+i).value
    val roffset = io.toDataPath.pcToDataPathIO.fromDataPathFtqOffset(baseIdx+i)
    pcMem.io.ren.get(pcMemIdx) := ren
    pcMem.io.raddr(pcMemIdx) := raddr
    io.toDataPath.pcToDataPathIO.toDataPathPC(baseIdx+i) := pcMem.io.rdata(pcMemIdx).toUInt
  }

  for ((pcMemIdx, i) <- pcMemRdIndexes("hybrid").zipWithIndex) {
    // load read pcMem (s0) -> get rdata (s1) -> reg next in Memblock (s2) -> reg next in Memblock (s3) -> consumed by pf (s3)
    pcMem.io.ren.get(pcMemIdx) := io.memHyPcRead(i).valid
    pcMem.io.raddr(pcMemIdx) := io.memHyPcRead(i).ptr.value
    io.memHyPcRead(i).data := pcMem.io.rdata(pcMemIdx).toUInt + (RegEnable(io.memHyPcRead(i).offset, io.memHyPcRead(i).valid) << instOffsetBits)
  }

  if (EnableStorePrefetchSMS) {
    for ((pcMemIdx, i) <- pcMemRdIndexes("store").zipWithIndex) {
      pcMem.io.ren.get(pcMemIdx) := io.memStPcRead(i).valid
      pcMem.io.raddr(pcMemIdx) := io.memStPcRead(i).ptr.value
      // memStPcRead.data is not right bucasue memStPcRead don't have isRvc
      io.memStPcRead(i).data := pcMem.io.rdata(pcMemIdx).toUInt + (RegEnable(io.memStPcRead(i).offset, io.memStPcRead(i).valid) << instOffsetBits)
    }
  } else {
    io.memStPcRead.foreach(_.data := 0.U)
  }

  /**
   * trace begin
   */
  val trace = Module(new Trace)
  trace.io.in.fromEncoder.stall  := io.traceCoreInterface.fromEncoder.stall
  trace.io.in.fromEncoder.enable := io.traceCoreInterface.fromEncoder.enable
  trace.io.in.fromRob            := rob.io.trace.traceCommitInfo
  rob.io.trace.blockCommit       := trace.io.out.blockRobCommit
  val tracePcStart = Wire(Vec(TraceGroupNum, UInt(IaddrWidth.W)))
  for ((pcMemIdx, i) <- pcMemRdIndexes("trace").zipWithIndex) {
    val traceValid = trace.toPcMem.blocks(i).valid
    pcMem.io.ren.get(pcMemIdx) := traceValid
    pcMem.io.raddr(pcMemIdx) := trace.toPcMem.blocks(i).bits.ftqIdx.get.value
    tracePcStart(i) := pcMem.io.rdata(pcMemIdx).toUInt
  }

  // Trap/Xret only occur in block(0).
  val tracePriv = Mux(Itype.isTrapOrXret(trace.toEncoder.blocks(0).bits.tracePipe.itype),
    io.fromCSR.traceCSR.lastPriv,
    io.fromCSR.traceCSR.currentPriv
  )
  io.traceCoreInterface.toEncoder.trap.cause := io.fromCSR.traceCSR.cause.asUInt
  io.traceCoreInterface.toEncoder.trap.tval  := io.fromCSR.traceCSR.tval.asUInt
  io.traceCoreInterface.toEncoder.priv       := tracePriv
  (0 until TraceGroupNum).foreach(i => {
    io.traceCoreInterface.toEncoder.groups(i).valid := trace.io.out.toEncoder.blocks(i).valid
    io.traceCoreInterface.toEncoder.groups(i).bits.iaddr := tracePcStart(i)
    io.traceCoreInterface.toEncoder.groups(i).bits.ftqOffset.foreach(_ := trace.io.out.toEncoder.blocks(i).bits.ftqOffset.getOrElse(0.U))
    io.traceCoreInterface.toEncoder.groups(i).bits.itype := trace.io.out.toEncoder.blocks(i).bits.tracePipe.itype
    io.traceCoreInterface.toEncoder.groups(i).bits.iretire := trace.io.out.toEncoder.blocks(i).bits.tracePipe.iretire
    io.traceCoreInterface.toEncoder.groups(i).bits.ilastsize := trace.io.out.toEncoder.blocks(i).bits.tracePipe.ilastsize
  })
  /**
   * trace end
   */


  redirectGen.io.hartId := io.fromTop.hartId
  redirectGen.io.oldestExuRedirect.valid := GatedValidRegNext(oldestExuRedirect.valid)
  redirectGen.io.oldestExuRedirect.bits := RegEnable(oldestExuRedirect.bits, oldestExuRedirect.valid)
  redirectGen.io.oldestExuRedirectIsCSR := RegEnable(oldestExuRedirectIsCSR, oldestExuRedirect.valid)
  redirectGen.io.instrAddrTransType := RegNext(io.fromCSR.instrAddrTransType)
  redirectGen.io.oldestExuOutPredecode.valid := GatedValidRegNext(oldestExuPredecode.valid)
  redirectGen.io.oldestExuOutPredecode := RegEnable(oldestExuPredecode, oldestExuPredecode.valid)
  redirectGen.io.loadReplay <> loadReplay
  val loadRedirectTargetOffset = Reg(UInt(VAddrBits.W))
  when(memViolation.valid) {
    val thisPcOffset = memViolation.bits.getPcOffset()
    val nextPcOffset = memViolation.bits.getNextPcOffset()
    loadRedirectTargetOffset := Mux(memViolation.bits.flushItself(), thisPcOffset, nextPcOffset)
  }
  val loadRedirectStartPcRead = pcMem.io.rdata(pcMemRdIndexes("redirect").head).toUInt
  val load_target = loadRedirectStartPcRead + loadRedirectTargetOffset
  redirectGen.io.loadReplay.bits.target := load_target
  // TODO loadRedirectPcOffset is useful?
  val loadRedirectPcOffset = Reg(UInt(VAddrBits.W))
  when(memViolation.valid) {
    loadRedirectPcOffset := memViolation.bits.getPcOffset()
  }
  redirectGen.io.loadReplay.bits.pc := loadRedirectStartPcRead + loadRedirectPcOffset

  redirectGen.io.robFlush := s1_robFlushRedirect

  val s5_flushFromRobValidAhead = DelayN(s1_robFlushRedirect.valid, 4)
  val s6_flushFromRobValid = GatedValidRegNext(s5_flushFromRobValidAhead)
  val frontendFlushBits = RegEnable(s1_robFlushRedirect.bits, s1_robFlushRedirect.valid) // ??
  // When ROB commits an instruction with a flush, we notify the frontend of the flush without the commit.
  // Flushes to frontend may be delayed by some cycles and commit before flush causes errors.
  // Thus, we make all flush reasons to behave the same as exceptions for frontend.
  for (i <- 0 until CommitWidth) {
    // why flushOut: instructions with flushPipe are not commited to frontend
    // If we commit them to frontend, it will cause flush after commit, which is not acceptable by frontend.
    val s1_isCommit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit && !s0_robFlushRedirect.valid
    io.frontend.toFtq.rob_commits(i).valid := GatedValidRegNext(s1_isCommit)
    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), s1_isCommit)
  }
  io.frontend.toFtq.redirect.valid := s6_flushFromRobValid || s3_redirectGen.valid
  io.frontend.toFtq.redirect.bits := Mux(s6_flushFromRobValid, frontendFlushBits, s3_redirectGen.bits)
  io.frontend.toFtq.ftqIdxSelOH.valid := s6_flushFromRobValid || redirectGen.io.stage2Redirect.valid
  io.frontend.toFtq.ftqIdxSelOH.bits := Cat(s6_flushFromRobValid, redirectGen.io.stage2oldestOH & Fill(NumRedirect + 1, !s6_flushFromRobValid))

  //jmp/brh, sel oldest first, only use one read port
  io.frontend.toFtq.ftqIdxAhead(0).valid := RegNext(oldestExuRedirect.valid) && !s1_robFlushRedirect.valid && !s5_flushFromRobValidAhead
  io.frontend.toFtq.ftqIdxAhead(0).bits := RegEnable(oldestExuRedirect.bits.ftqIdx, oldestExuRedirect.valid)
  //loadreplay
  io.frontend.toFtq.ftqIdxAhead(NumRedirect).valid := loadReplay.valid && !s1_robFlushRedirect.valid && !s5_flushFromRobValidAhead
  io.frontend.toFtq.ftqIdxAhead(NumRedirect).bits := loadReplay.bits.ftqIdx
  //exception
  io.frontend.toFtq.ftqIdxAhead.last.valid := s5_flushFromRobValidAhead
  io.frontend.toFtq.ftqIdxAhead.last.bits := frontendFlushBits.ftqIdx

  // Be careful here:
  // T0: rob.io.flushOut, s0_robFlushRedirect
  // T1: s1_robFlushRedirect, rob.io.exception.valid
  // T2: csr.redirect.valid
  // T3: csr.exception.valid
  // T4: csr.trapTarget
  // T5: ctrlBlock.trapTarget
  // T6: io.frontend.toFtq.stage2Redirect.valid
  val s2_robFlushPc = RegEnable(Mux(s1_robFlushRedirect.bits.flushItself(),
    s1_robFlushPc, // replay inst
    s1_robFlushPc + Mux(s1_robFlushRedirect.bits.isRVC, 2.U, 4.U) // flush pipe
  ), s1_robFlushRedirect.valid)
  private val s5_csrIsTrap = DelayN(rob.io.exception.valid, 4)
  private val s5_trapTargetFromCsr = io.robio.csr.trapTarget

  val flushTarget = Mux(s5_csrIsTrap, s5_trapTargetFromCsr.pc, s2_robFlushPc)
  val s5_trapTargetIAF = Mux(s5_csrIsTrap, s5_trapTargetFromCsr.raiseIAF, false.B)
  val s5_trapTargetIPF = Mux(s5_csrIsTrap, s5_trapTargetFromCsr.raiseIPF, false.B)
  val s5_trapTargetIGPF = Mux(s5_csrIsTrap, s5_trapTargetFromCsr.raiseIGPF, false.B)
  when (s6_flushFromRobValid) {
    io.frontend.toFtq.redirect.bits.level := RedirectLevel.flush
    io.frontend.toFtq.redirect.bits.target := RegEnable(flushTarget, s5_flushFromRobValidAhead)
    io.frontend.toFtq.redirect.bits.backendIAF := RegEnable(s5_trapTargetIAF, s5_flushFromRobValidAhead)
    io.frontend.toFtq.redirect.bits.backendIPF := RegEnable(s5_trapTargetIPF, s5_flushFromRobValidAhead)
    io.frontend.toFtq.redirect.bits.backendIGPF := RegEnable(s5_trapTargetIGPF, s5_flushFromRobValidAhead)
  }

  for (i <- 0 until DecodeWidth) {
    gpaMem.io.fromIFU := io.frontend.fromIfu
    gpaMem.io.exceptionReadAddr.valid := rob.io.readGPAMemAddr.valid
    gpaMem.io.exceptionReadAddr.bits.ftqPtr := rob.io.readGPAMemAddr.bits.ftqPtr
    gpaMem.io.exceptionReadAddr.bits.ftqOffset := rob.io.readGPAMemAddr.bits.ftqOffset
  }

  // vtype commit
  decode.io.fromCSR := io.fromCSR.toDecode
  decode.io.fromRob.isResumeVType := rob.io.toDecode.isResumeVType
  decode.io.fromRob.walkToArchVType := rob.io.toDecode.walkToArchVType
  decode.io.fromRob.commitVType := rob.io.toDecode.commitVType
  decode.io.fromRob.walkVType := rob.io.toDecode.walkVType

  decode.io.redirect := s1_s3_redirect.valid || s2_s4_pendingRedirectValid

  // add decode Buf for in.ready better timing
  /**
   * Decode buffer: when decode.io.in cannot accept all insts, use this buffer to temporarily store insts that cannot
   * be sent to DecodeStage.
   *
   * Decode buffer is a "DecodeWidth"-element long register Vector of DecodeInUop (in decodeBufBits), with valid signals
   * (in decodeBufValid). At the same time, fetch insts input from frontend and their valid bits. All valid elements
   * in these two vector of insts are at the beginning, with all invalid vector elements followed.
   *
   * After dealing with redirection, try to use all insts in decode buffer to fulfill decoder.io.in. If decode buffer
   * has no valid insts, use insts from frontend to supply decoder.
   */

  /** Insts to be decoded, Registers in vector of DecodeWidth */
  val decodeBufBits = Reg(Vec(DecodeWidth, new DecodeInUop))

  /** Valid receiving signals of instructions to be decoded, Registers in vector of DecodeWidth */
  val decodeBufValid = RegInit(VecInit(Seq.fill(DecodeWidth)(false.B)))

  /** Insts input from frontend, in vector of DecodeWidth */
  val decodeFromFrontend = io.frontend.cfVec

  /** Insts in buffer that is not ready but valid in decodeBufValid */
  val decodeBufNotAccept = VecInit(decodeBufValid.zip(decode.io.in).map(x => x._1 && !x._2.ready))

  /** Number of insts in decode buffer that is accepted. All accepted insts are before the first unaccepted one. */
  val decodeBufAcceptNum = PriorityMuxDefault(decodeBufNotAccept.zip(Seq.tabulate(DecodeWidth)(i => i.U)), DecodeWidth.U)

  /** Input valid insts from frontend that is not ready to be accepted, or decoder prefer insts in decode buffer */
  val decodeFromFrontendNotAccept = VecInit(decodeFromFrontend.zip(decode.io.in).map(x => decodeBufValid(0) || x._1.valid && !x._2.ready))

  /** Number of input insts that is accepted.
   * All accepted insts are before the first unaccepted one. */
  val decodeFromFrontendAcceptNum = PriorityMuxDefault(decodeFromFrontendNotAccept.zip(Seq.tabulate(DecodeWidth)(i => i.U)), DecodeWidth.U)

  if (backendParams.debugEn) {
    dontTouch(decodeBufNotAccept)
    dontTouch(decodeBufAcceptNum)
    dontTouch(decodeFromFrontendNotAccept)
    dontTouch(decodeFromFrontendAcceptNum)
  }

  /**
   * State machine of "decodeBufValid(i)":
   *   redirect || decodeBufValid(i) is the last accepted instr in decodeBuf:
   *     false
   *   decodeBufValid(i) is true, decodeBufNotAccept.drop(i) has some true signals
   *     (decodeBufAcceptNum > DecodeWidth-1-i) ? false
   *                                     if not : decodeBufValid(i+decodeBufAcceptNum)
   *     Pop "decodeBufAcceptNum" insts out of the decodeBufValid, and move others forward
   *   decodeBufValid(0) is false, decodeFromFrontendNotAccept.drop(i) has some true signals
   *     (decodeFromFrontendAcceptNum > DecodeWidth-1-i) ? false
   *                                              if not : decodeFromFrontend(i+decodeFromFrontendAcceptNum).valid
   *     Get first "decodeFromFrontendAcceptNum" insts from decodeFromFrontend, and move others to decodeBufValid
   *
   * State machine of "decodeBufBits(i)":
   *   decodeBufValid(i) is true, decodeBufNotAccept.drop(i) has some true signals
   *     decodeBufBits(i+decodeBufAcceptNum)
   *   decodeBufValid(0) is false, decodeFromFrontendNotAccept.drop(i) has some true signals
   *     decodeFromFrontend(i+decodeFromFrontendAcceptNum)
   */
  for (i <- 0 until DecodeWidth) {
    // decodeBufValid update
    when(decode.io.redirect || decodeBufValid(0) && decodeBufValid(i) && decode.io.in(i).ready && !VecInit(decodeBufNotAccept.drop(i)).asUInt.orR) {
      decodeBufValid(i) := false.B
    }.elsewhen(decodeBufValid(i) && VecInit(decodeBufNotAccept.drop(i)).asUInt.orR) {
      decodeBufValid(i) := Mux(decodeBufAcceptNum > DecodeWidth.U - 1.U - i.U, false.B, decodeBufValid(i.U + decodeBufAcceptNum))
    }.elsewhen(!decodeBufValid(0) && VecInit(decodeFromFrontendNotAccept.drop(i)).asUInt.orR) {
      decodeBufValid(i) := Mux(decodeFromFrontendAcceptNum > DecodeWidth.U - 1.U - i.U, false.B, decodeFromFrontend(i.U + decodeFromFrontendAcceptNum).valid)
    }
    // decodeBufBits update
    when(decodeBufValid(i) && VecInit(decodeBufNotAccept.drop(i)).asUInt.orR) {
      decodeBufBits(i) := decodeBufBits(i.U + decodeBufAcceptNum)
    }.elsewhen(!decodeBufValid(0) && VecInit(decodeFromFrontendNotAccept.drop(i)).asUInt.orR) {
      decodeBufBits(i).connectCtrlFlow(decodeFromFrontend(i.U + decodeFromFrontendAcceptNum).bits)
    }
  }
  /** Insts input from frontend, in vector of DecodeWidth */
  val decodeConnectFromFrontend = Wire(Vec(DecodeWidth, new DecodeInUop))
  decodeConnectFromFrontend.zip(decodeFromFrontend).map(x => x._1.connectCtrlFlow(x._2.bits))

  /**
   * DecodeStage's input:
   *   decode.io.in(i).valid:
   *     decodeBufValid(0) is true : decodeBufValid(i)            | from decode buffer
   *                         false : decodeFromFrontend(i).valid  | from frontend
   *
   *   decodeFromFrontend(i).ready:
   *     decodeFromFrontend(0).valid && !decodeBufValid(0) && decodeFromFrontend(i).valid && !decode.io.redirect
   *     valid instr in input, no instr in decode buffer, decodeFromFrontend(i) is valid, no redirection
   *
   *   decode.io.in(i).bits:
   *     decodeBufValid(i) is true : decodeBufBits(i)             | from decode buffer
   *                         false : decodeConnectFromFrontend(i) | from frontend
   */
  decode.io.in.zipWithIndex.foreach { case (decodeIn, i) =>
    decodeIn.valid := Mux(decodeBufValid(0), decodeBufValid(i), decodeFromFrontend(i).valid)
    decodeFromFrontend(i).ready := decodeFromFrontend(0).valid && !decodeBufValid(0) && decodeFromFrontend(i).valid && !decode.io.redirect
    decodeIn.bits := Mux(decodeBufValid(i), decodeBufBits(i), decodeConnectFromFrontend(i))
  }
  /** no valid instr in decode buffer && no valid instr from frontend --> can accept new instr from frontend */
  io.frontend.canAccept := !decodeBufValid(0) || !decodeFromFrontend(0).valid
  decode.io.csrCtrl := RegNext(io.csrCtrl)
  decode.io.intRat <> rat.io.intReadPorts
  decode.io.fpRat <> rat.io.fpReadPorts
  decode.io.vecRat <> rat.io.vecReadPorts
  decode.io.v0Rat <> rat.io.v0ReadPorts
  decode.io.vlRat <> rat.io.vlReadPorts
  decode.io.fusion := 0.U.asTypeOf(decode.io.fusion) // Todo
  decode.io.stallReason.in <> io.frontend.stallReason

  // snapshot check
  class CFIRobIdx extends Bundle {
    val robIdx = Vec(RenameWidth, new RobPtr)
    val isCFI = Vec(RenameWidth, Bool())
  }
  val genSnapshot = Cat(rename.io.out.map(out => out.fire && out.bits.snapshot)).orR
  val snpt = Module(new SnapshotGenerator(0.U.asTypeOf(new CFIRobIdx)))
  snpt.io.enq := genSnapshot
  snpt.io.enqData.robIdx := rename.io.out.map(_.bits.robIdx)
  snpt.io.enqData.isCFI := rename.io.out.map(_.bits.snapshot)
  snpt.io.deq := snpt.io.valids(snpt.io.deqPtr.value) && rob.io.commits.isCommit &&
    Cat(rob.io.commits.commitValid.zip(rob.io.commits.robIdx).map(x => x._1 && x._2 === snpt.io.snapshots(snpt.io.deqPtr.value).robIdx.head)).orR
  snpt.io.redirect := s1_s3_redirect.valid
  val flushVec = VecInit(snpt.io.snapshots.map { snapshot =>
    val notCFIMask = snapshot.isCFI.map(~_)
    val shouldFlush = snapshot.robIdx.map(robIdx => robIdx >= s1_s3_redirect.bits.robIdx || robIdx.value === s1_s3_redirect.bits.robIdx.value)
    val shouldFlushMask = (1 to RenameWidth).map(shouldFlush take _ reduce (_ || _))
    s1_s3_redirect.valid && Cat(shouldFlushMask.zip(notCFIMask).map(x => x._1 | x._2)).andR
  })
  val flushVecNext = flushVec zip snpt.io.valids map (x => GatedValidRegNext(x._1 && x._2, false.B))
  snpt.io.flushVec := flushVecNext

  val redirectRobidx = s1_s3_redirect.bits.robIdx
  val useSnpt = VecInit.tabulate(RenameSnapshotNum){ case idx =>
    val snptRobidx = snpt.io.snapshots(idx).robIdx.head
    // (redirectRobidx.value =/= snptRobidx.value) for only flag diffrence
    snpt.io.valids(idx) && ((redirectRobidx > snptRobidx) && (redirectRobidx.value =/= snptRobidx.value) ||
      !s1_s3_redirect.bits.flushItself() && redirectRobidx === snptRobidx)
  }.reduceTree(_ || _)
  val snptSelect = MuxCase(
    0.U(log2Ceil(RenameSnapshotNum).W),
    (1 to RenameSnapshotNum).map(i => (snpt.io.enqPtr - i.U).value).map{case idx =>
      val thisSnapRobidx = snpt.io.snapshots(idx).robIdx.head
      (snpt.io.valids(idx) && (redirectRobidx > thisSnapRobidx && (redirectRobidx.value =/= thisSnapRobidx.value) ||
        !s1_s3_redirect.bits.flushItself() && redirectRobidx === thisSnapRobidx), idx)
    }
  )

  rob.io.snpt.snptEnq := DontCare
  rob.io.snpt.snptDeq := snpt.io.deq
  rob.io.snpt.useSnpt := useSnpt
  rob.io.snpt.snptSelect := snptSelect
  rob.io.snpt.flushVec := flushVecNext
  rat.io.snpt.snptEnq := genSnapshot
  rat.io.snpt.snptDeq := snpt.io.deq
  rat.io.snpt.useSnpt := useSnpt
  rat.io.snpt.snptSelect := snptSelect
  rat.io.snpt.flushVec := flushVec

  val decodeHasException = decode.io.out.map(x => x.bits.exceptionVec.asUInt.orR || (!TriggerAction.isNone(x.bits.trigger)))
  // fusion decoder
  fusionDecoder.io.disableFusion := disableFusion
  for (i <- 0 until DecodeWidth) {
    fusionDecoder.io.in(i).valid := decode.io.out(i).valid && !decodeHasException(i)
    fusionDecoder.io.in(i).bits := decode.io.out(i).bits.instr
    if (i > 0) {
      fusionDecoder.io.inReady(i - 1) := decode.io.out(i).ready
    }
  }

  private val decodePipeRename = Wire(Vec(RenameWidth, DecoupledIO(new DecodeOutUop)))
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), decodePipeRename(i), rename.io.in(i).ready,
      s1_s3_redirect.valid || s2_s4_pendingRedirectValid, moduleName = Some("decodePipeRenameModule"))

    decodePipeRename(i).ready := rename.io.in(i).ready
    rename.io.in(i).valid := decodePipeRename(i).valid && !fusionDecoder.io.clear(i)
    rename.io.in(i).bits := decodePipeRename(i).bits
    dispatch.io.renameIn(i).valid := decodePipeRename(i).valid && !fusionDecoder.io.clear(i) && !decodePipeRename(i).bits.isMove
    dispatch.io.renameIn(i).bits := decodePipeRename(i).bits
    rename.io.validVec(i) := decodePipeRename(i).valid
    rename.io.isFusionVec(i) := false.B
    rename.io.fusionCross2FtqVec(i) := false.B
  }

  for (i <- 0 until RenameWidth - 1) {
    fusionDecoder.io.dec(i) := decodePipeRename(i).bits
    rename.io.fusionInfo(i) := fusionDecoder.io.info(i)

    // update the first RenameWidth - 1 instructions
    decode.io.fusion(i) := fusionDecoder.io.out(i).valid && rename.io.out(i).fire
    when (fusionDecoder.io.out(i).valid) {
      fusionDecoder.io.out(i).bits.update(rename.io.in(i).bits)
      fusionDecoder.io.out(i).bits.update(dispatch.io.renameIn(i).bits)
      val cross2Ftq = decodePipeRename(i).bits.isLastInFtqEntry && decodePipeRename(i + 1).bits.isLastInFtqEntry
      val cross1Ftq = decodePipeRename(i).bits.isLastInFtqEntry || decodePipeRename(i + 1).bits.isLastInFtqEntry
      rename.io.in(i + 1).bits.isLastInFtqEntry := cross1Ftq
      rename.io.in(i + 1).bits.canRobCompress := !cross2Ftq
      // if second instruciton of fusion is move and it can also be fusion, it will not act as a move
      rename.io.in(i + 1).bits.isMove := false.B
      rename.io.in(i).bits.isLastInFtqEntry := false.B
      rename.io.in(i).bits.canRobCompress := !cross2Ftq
      rename.io.isFusionVec(i) := true.B
      rename.io.fusionCross2FtqVec(i) := cross2Ftq
    }
  }

  // memory dependency predict
  // when decode, send fold pc to mdp
  private val mdpFlodPcVecVld = Wire(Vec(DecodeWidth, Bool()))
  private val mdpFlodPcVec = Wire(Vec(DecodeWidth, UInt(MemPredPCWidth.W)))
  for (i <- 0 until DecodeWidth) {
    mdpFlodPcVecVld(i) := decode.io.out(i).fire || GatedValidRegNext(decode.io.out(i).fire)
    mdpFlodPcVec(i) := Mux(
      decode.io.out(i).fire,
      decode.io.in(i).bits.foldpc,
      rename.io.in(i).bits.foldpc
    )
  }

  // currently, we only update mdp info when isReplay
  memCtrl.io.redirect := s1_s3_redirect
  memCtrl.io.csrCtrl := io.csrCtrl                          // RegNext in memCtrl
  memCtrl.io.stIn := io.fromMem.stIn                        // RegNext in memCtrl
  memCtrl.io.memPredUpdate := redirectGen.io.memPredUpdate  // RegNext in memCtrl
  memCtrl.io.mdpFoldPcVecVld := mdpFlodPcVecVld
  memCtrl.io.mdpFlodPcVec := mdpFlodPcVec
  memCtrl.io.dispatchLFSTio <> dispatch.io.lfst

  rat.io.redirect := s1_s3_redirect.valid
  rat.io.rabCommits := rob.io.rabCommits
  rat.io.diffCommits.foreach(_ := rob.io.diffCommits.get)
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  rat.io.vecRenamePorts := rename.io.vecRenamePorts
  rat.io.v0RenamePorts := rename.io.v0RenamePorts
  rat.io.vlRenamePorts := rename.io.vlRenamePorts

  rename.io.redirect := s1_s3_redirect
  rename.io.rabCommits := rob.io.rabCommits
  rename.io.singleStep := GatedValidRegNext(io.csrCtrl.singlestep)
  rename.io.waittable := (memCtrl.io.waitTable2Rename zip decode.io.out).map{ case(waittable2rename, decodeOut) =>
    RegEnable(waittable2rename, decodeOut.fire)
  }
  rename.io.ssit := memCtrl.io.ssit2Rename
  // disble mdp
  dispatch.io.lfst.resp := 0.U.asTypeOf(dispatch.io.lfst.resp)
  rename.io.waittable := 0.U.asTypeOf(rename.io.waittable)
  rename.io.ssit := 0.U.asTypeOf(rename.io.ssit)
  rename.io.intReadPorts := VecInit(rat.io.intReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.fpReadPorts := VecInit(rat.io.fpReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.vecReadPorts := VecInit(rat.io.vecReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.v0ReadPorts := VecInit(rat.io.v0ReadPorts.map(x => VecInit(x.data)))
  rename.io.vlReadPorts := VecInit(rat.io.vlReadPorts.map(x => VecInit(x.data)))
  rename.io.int_need_free := rat.io.int_need_free
  rename.io.int_old_pdest := rat.io.int_old_pdest
  rename.io.fp_old_pdest := rat.io.fp_old_pdest
  rename.io.vec_old_pdest := rat.io.vec_old_pdest
  rename.io.v0_old_pdest := rat.io.v0_old_pdest
  rename.io.vl_old_pdest := rat.io.vl_old_pdest
  rename.io.debug_int_rat.foreach(_ := rat.io.debug_int_rat.get)
  rename.io.debug_fp_rat.foreach(_ := rat.io.debug_fp_rat.get)
  rename.io.debug_vec_rat.foreach(_ := rat.io.debug_vec_rat.get)
  rename.io.debug_v0_rat.foreach(_ := rat.io.debug_v0_rat.get)
  rename.io.debug_vl_rat.foreach(_ := rat.io.debug_vl_rat.get)
  rename.io.stallReason.in <> decode.io.stallReason.out
  rename.io.snpt.snptEnq := DontCare
  rename.io.snpt.snptDeq := snpt.io.deq
  rename.io.snpt.useSnpt := useSnpt
  rename.io.snpt.snptSelect := snptSelect
  rename.io.snptIsFull := snpt.io.valids.asUInt.andR
  rename.io.snpt.flushVec := flushVecNext
  rename.io.snptLastEnq.valid := !isEmpty(snpt.io.enqPtr, snpt.io.deqPtr)
  rename.io.snptLastEnq.bits := snpt.io.snapshots((snpt.io.enqPtr - 1.U).value).robIdx.head

  val renameOut = Wire(chiselTypeOf(dispatch.io.fromRename))
  renameOut.zip(rename.io.out).map{ case (sink, source) => {
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits := source.bits
  }}
  // pass all snapshot in the first element for correctness of blockBackward
  renameOut.tail.foreach(_.bits.snapshot := false.B)
  renameOut.head.bits.snapshot := Mux(isFull(snpt.io.enqPtr, snpt.io.deqPtr),
    false.B,
    Cat(rename.io.out.map(out => out.valid && out.bits.snapshot)).orR
  )

  // pipeline between rename and dispatch
  PipeGroupConnect(renameOut, dispatch.io.fromRename, s1_s3_redirect.valid, dispatch.io.toRenameAllFire, "renamePipeDispatch")

  dispatch.io.redirect := s1_s3_redirect
  val enqRob = Wire(chiselTypeOf(rob.io.enq))
  enqRob.canAccept := rob.io.enq.canAccept
  enqRob.canAcceptForDispatch := rob.io.enq.canAcceptForDispatch
  enqRob.isEmpty := rob.io.enq.isEmpty
  enqRob.resp := rob.io.enq.resp
  enqRob.needAlloc := RegNext(dispatch.io.enqRob.needAlloc)
  enqRob.req.zip(dispatch.io.enqRob.req).map { case (sink, source) =>
    sink.valid := RegNext(source.valid && !rob.io.redirect.valid)
    sink.bits := RegEnable(source.bits, source.valid)
  }
  dispatch.io.enqRob.canAccept := enqRob.canAcceptForDispatch && !enqRob.req.map(x => x.valid && x.bits.blockBackward && enqRob.canAccept).reduce(_ || _)
  dispatch.io.enqRob.canAcceptForDispatch := enqRob.canAcceptForDispatch
  dispatch.io.enqRob.isEmpty := enqRob.isEmpty && !enqRob.req.map(_.valid).reduce(_ || _)
  dispatch.io.enqRob.resp := enqRob.resp
  rob.io.enq.needAlloc := enqRob.needAlloc
  rob.io.enq.req := enqRob.req
  dispatch.io.robHead := rob.io.debugRobHead
  dispatch.io.stallReason <> rename.io.stallReason.out
  dispatch.io.lqCanAccept := io.lqCanAccept
  dispatch.io.sqCanAccept := io.sqCanAccept
  dispatch.io.fromMem.lcommit := io.fromMemToDispatch.lcommit
  dispatch.io.fromMem.scommit := io.fromMemToDispatch.scommit
  dispatch.io.fromMem.lqDeqPtr := io.fromMemToDispatch.lqDeqPtr
  dispatch.io.fromMem.sqDeqPtr := io.fromMemToDispatch.sqDeqPtr
  dispatch.io.fromMem.lqCancelCnt := io.fromMemToDispatch.lqCancelCnt
  dispatch.io.fromMem.sqCancelCnt := io.fromMemToDispatch.sqCancelCnt
  io.toMem.lsqEnqIO <> dispatch.io.toMem.lsqEnqIO
  dispatch.io.wakeUpAll.wakeUpInt := io.toDispatch.wakeUpInt
  dispatch.io.wakeUpAll.wakeUpFp  := io.toDispatch.wakeUpFp
  dispatch.io.wakeUpAll.wakeUpVec := io.toDispatch.wakeUpVec
  dispatch.io.IQValidNumVec := io.toDispatch.IQValidNumVec
  dispatch.io.ldCancel := io.toDispatch.ldCancel
  dispatch.io.og0Cancel := io.toDispatch.og0Cancel
  dispatch.io.wbPregsInt := io.toDispatch.wbPregsInt
  dispatch.io.wbPregsFp := io.toDispatch.wbPregsFp
  dispatch.io.wbPregsVec := io.toDispatch.wbPregsVec
  dispatch.io.wbPregsV0 := io.toDispatch.wbPregsV0
  dispatch.io.wbPregsVl := io.toDispatch.wbPregsVl
  dispatch.io.vlWriteBackInfo := io.toDispatch.vlWriteBackInfo
  dispatch.io.robHeadNotReady := rob.io.headNotReady
  dispatch.io.robFull := rob.io.robFull
  dispatch.io.singleStep := GatedValidRegNext(io.csrCtrl.singlestep)

  val toIssueBlockUops = Seq(io.toIssueBlock.intUops, io.toIssueBlock.fpUops, io.toIssueBlock.vfUops).flatten
  println(s"[CtrlBlock] toIssueBlockUops.size = ${toIssueBlockUops.size}")
  println(s"[CtrlBlock] io.toIssueBlock.intUops.size = ${io.toIssueBlock.intUops.size}")
  println(s"[CtrlBlock] io.toIssueBlock.fpUops.size = ${io.toIssueBlock.fpUops.size}")
  println(s"[CtrlBlock] io.toIssueBlock.vfUops.size = ${io.toIssueBlock.vfUops.size}")
  println(s"[CtrlBlock] dispatch.io.toIssueQueues.size = ${dispatch.io.toIssueQueues.size}")
  toIssueBlockUops.zip(dispatch.io.toIssueQueues).map{ case (iq, dispatch) => {
    iq.valid := dispatch.valid
    iq.bits := dispatch.bits
    dispatch.ready := iq.ready
  }}
  io.toIssueBlock.flush <> s2_s4_redirect

  pcMem.io.wen.head   := GatedValidRegNext(io.frontend.fromFtq.pc_mem_wen)
  pcMem.io.waddr.head := RegEnable(io.frontend.fromFtq.pc_mem_waddr, io.frontend.fromFtq.pc_mem_wen)
  pcMem.io.wdata.head := RegEnable(io.frontend.fromFtq.pc_mem_wdata, io.frontend.fromFtq.pc_mem_wen)

  io.toDataPath.flush := s2_s4_redirect
  io.toExuBlock.flush := s2_s4_redirect


  rob.io.hartId := io.fromTop.hartId
  rob.io.redirect := s1_s3_redirect
  rob.io.writeback := delayedNotFlushedWriteBack
  rob.io.exuWriteback := delayedWriteBack
  rob.io.writebackNums := VecInit(delayedNotFlushedWriteBackNums)
  rob.io.writebackNeedFlush := delayedNotFlushedWriteBackNeedFlush
  rob.io.readGPAMemData := gpaMem.io.exceptionReadData
  rob.io.fromVecExcpMod.busy := io.fromVecExcpMod.busy

  io.redirect := s1_s3_redirect

  // rob to int block
  io.robio.csr <> rob.io.csr
  // When wfi is disabled, it will not block ROB commit.
  rob.io.csr.wfiEvent := io.robio.csr.wfiEvent
  rob.io.wfi_enable := decode.io.csrCtrl.wfi_enable

  io.toTop.cpuHalt := DelayN(rob.io.cpu_halt, 5)

  io.robio.csr.perfinfo.retiredInstr <> RegNext(rob.io.csr.perfinfo.retiredInstr)
  io.robio.exception := rob.io.exception
  io.robio.exception.bits.pc := s1_robFlushPc
  // bju resolve
  io.frontend.toFtq.resolve := io.fromBJUResolve
  // wfi
  io.frontend.wfi.wfiReq := rob.io.wfi.wfiReq
  rob.io.wfi.safeFromFrontend := io.frontend.wfi.wfiSafe
  io.toMem.wfi.wfiReq := rob.io.wfi.wfiReq
  rob.io.wfi.safeFromMem := io.toMem.wfi.wfiSafe

  // rob to mem block
  io.robio.lsq <> rob.io.lsq

  io.diff_int_rat.foreach(_ := rat.io.diff_int_rat.get)
  io.diff_fp_rat .foreach(_ := rat.io.diff_fp_rat.get)
  io.diff_vec_rat.foreach(_ := rat.io.diff_vec_rat.get)
  io.diff_v0_rat .foreach(_ := rat.io.diff_v0_rat.get)
  io.diff_vl_rat .foreach(_ := rat.io.diff_vl_rat.get)

  rob.io.debug_ls := io.robio.debug_ls
  rob.io.debugHeadLsIssue := io.robio.robHeadLsIssue
  rob.io.lsTopdownInfo := io.robio.lsTopdownInfo
  rob.io.csr.criticalErrorState := io.robio.csr.criticalErrorState
  rob.io.debugEnqLsq := io.debugEnqLsq
  rob.io.debugInstrAddrTransType := io.fromCSR.instrAddrTransType

  io.robio.robDeqPtr := rob.io.robDeqPtr

  io.robio.storeDebugInfo <> rob.io.storeDebugInfo

  // rob to backend
  io.robio.commitVType := rob.io.toDecode.commitVType
  // exu block to decode
  decode.io.vsetvlVType := io.toDecode.vsetvlVType
  // backend to decode
  decode.io.vstart := io.toDecode.vstart
  // backend to rob
  rob.io.vstartIsZero := io.toDecode.vstart === 0.U

  io.toCSR.trapInstInfo := decode.io.toCSR.trapInstInfo

  io.toVecExcpMod.logicPhyRegMap := rob.io.toVecExcpMod.logicPhyRegMap
  io.toVecExcpMod.excpInfo       := rob.io.toVecExcpMod.excpInfo
  // T  : rat receive rabCommit
  // T+1: rat return oldPdest
  io.toVecExcpMod.ratOldPest match {
    case fromRat =>
      (0 until RabCommitWidth).foreach { idx =>
        val v0Valid = RegNext(
          rat.io.rabCommits.isCommit &&
          rat.io.rabCommits.isWalk &&
          rat.io.rabCommits.commitValid(idx) &&
          rat.io.rabCommits.info(idx).v0Wen
        )
        fromRat.v0OldVdPdest(idx).valid := RegNext(v0Valid)
        fromRat.v0OldVdPdest(idx).bits := RegEnable(rat.io.v0_old_pdest(idx), v0Valid)
        val vecValid = RegNext(
          rat.io.rabCommits.isCommit &&
          rat.io.rabCommits.isWalk &&
          rat.io.rabCommits.commitValid(idx) &&
          rat.io.rabCommits.info(idx).vecWen
        )
        fromRat.vecOldVdPdest(idx).valid := RegNext(vecValid)
        fromRat.vecOldVdPdest(idx).bits := RegEnable(rat.io.vec_old_pdest(idx), vecValid)
      }
  }

  io.debugTopDown.fromRob := rob.io.debugTopDown.toCore
  dispatch.io.debugTopDown.fromRob := rob.io.debugTopDown.toDispatch
  dispatch.io.debugTopDown.fromCore := io.debugTopDown.fromCore
  io.debugRolling := rob.io.debugRolling

  io.perfInfo.ctrlInfo.robFull := GatedValidRegNext(rob.io.robFull)
  io.perfInfo.ctrlInfo.intdqFull := false.B
  io.perfInfo.ctrlInfo.fpdqFull := false.B
  io.perfInfo.ctrlInfo.lsdqFull := false.B

  val perfEvents = Seq(decode, rename, dispatch, rob).flatMap(_.getPerfEvents)
  generatePerfEvent()

  val criticalErrors = rob.getCriticalErrors
  generateCriticalErrors()
}

class CtrlBlockIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val toTop = new Bundle {
    val cpuHalt = Output(Bool())
  }
  val frontend = Flipped(new FrontendToCtrlIO())
  val fromBJUResolve = Flipped(Vec(backendParams.BrhCnt, Valid(new Resolve)))
  val fromCSR = new Bundle{
    val toDecode = Input(new CSRToDecode)
    val traceCSR = Input(new TraceCSR)
    val instrAddrTransType = Input(new AddrTransType)
  }
  val toIssueBlock = new Bundle {
    val flush = ValidIO(new Redirect)
    val intUopsNum = backendParams.intSchdParams.get.issueBlockParams.filter(_.StdCnt == 0).map(_.numEnq).sum
    val fpUopsNum = backendParams.fpSchdParams.get.issueBlockParams.map(_.numEnq).sum
    val vfUopsNum = backendParams.vecSchdParams.get.issueBlockParams.map(_.numEnq).sum
    val intUops = Vec(intUopsNum, DecoupledIO(new DispatchOutUop))
    val fpUops = Vec(fpUopsNum, DecoupledIO(new DispatchOutUop))
    val vfUops = Vec(vfUopsNum, DecoupledIO(new DispatchOutUop))
  }
  val fromMemToDispatch = new Bundle {
    val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
    val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W)) // connected to `memBlock.io.sqDeq` instead of ROB
    val lqDeqPtr = Input(new LqPtr)
    val sqDeqPtr = Input(new SqPtr)
    // from lsq
    val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
  }
  //toMem
  val toMem = new Bundle {
    val lsqEnqIO = Flipped(new LsqEnqIO)
    val wfi = new WfiReqBundle
  }
  val toDispatch = new Bundle {
    val wakeUpInt = Flipped(backendParams.intSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpFp  = Flipped(backendParams.fpSchdParams.get.genIQWakeUpOutValidBundle)
    val wakeUpVec = Flipped(backendParams.vecSchdParams.get.genIQWakeUpOutValidBundle)
    val allIssueParams = backendParams.allIssueParams.filter(_.StdCnt == 0)
    val allExuParams = allIssueParams.map(_.exuBlockParams).flatten
    val exuNum = allExuParams.size
    val maxIQSize = allIssueParams.map(_.numEntries).max
    val IQValidNumVec = Vec(exuNum, Input(UInt(maxIQSize.U.getWidth.W)))
    val og0Cancel = Input(ExuVec())
    val ldCancel = Vec(backendParams.LdExuCnt, Flipped(new LoadCancelIO))
    val wbPregsInt = Vec(backendParams.numPregWb(IntData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsFp = Vec(backendParams.numPregWb(FpData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsVec = Vec(backendParams.numPregWb(VecData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsV0 = Vec(backendParams.numPregWb(V0Data()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val wbPregsVl = Vec(backendParams.numPregWb(VlData()), Flipped(ValidIO(UInt(PhyRegIdxWidth.W))))
    val vlWriteBackInfo = new Bundle {
      val vlFromIntIsZero  = Input(Bool())
      val vlFromIntIsVlmax = Input(Bool())
      val vlFromVfIsZero   = Input(Bool())
      val vlFromVfIsVlmax  = Input(Bool())
    }
  }
  val toDataPath = new Bundle {
    val flush = ValidIO(new Redirect)
    val pcToDataPathIO = new PcToDataPathIO(params)
  }
  val toExuBlock = new Bundle {
    val flush = ValidIO(new Redirect)
  }
  val toCSR = new Bundle {
    val trapInstInfo = Output(ValidIO(new TrapInstInfo))
  }
  val fromWB = new Bundle {
    val wbData = Flipped(MixedVec(params.genWrite2CtrlBundles))
  }
  val redirect = ValidIO(new Redirect)
  val fromMem = new Bundle {
    val stIn = Vec(params.StaExuCnt, Flipped(ValidIO(new StoreUnitToLFST))) // use storeSetHit, ssid, robIdx
    val violation = Flipped(ValidIO(new Redirect))
  }
  val memStPcRead = Vec(params.StaCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val memHyPcRead = Vec(params.HyuCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  val csrCtrl = Input(new CustomCSRCtrlIO)
  val robio = new Bundle {
    val csr = new RobCSRIO
    val exception = ValidIO(new ExceptionInfo)
    val lsq = new RobLsqIO
    val lsTopdownInfo = Vec(params.LduCnt + params.HyuCnt, Input(new LsTopdownInfo))
    val debug_ls = Input(new DebugLSIO())
    val robHeadLsIssue = Input(Bool())
    val robDeqPtr = Output(new RobPtr)
    val commitVType = new Bundle {
      val vtype = Output(ValidIO(VType()))
      val hasVsetvl = Output(Bool())
    }
    // store event difftest information
    val storeDebugInfo = Vec(EnsbufferWidth, new Bundle {
      val robidx = Input(new RobPtr)
      val pc     = Output(UInt(VAddrBits.W))
    })
  }

  val toDecode = new Bundle {
    val vsetvlVType = Input(VType())
    val vstart = Input(Vl())
  }

  val fromVecExcpMod = Input(new Bundle {
    val busy = Bool()
  })

  val toVecExcpMod = Output(new Bundle {
    val logicPhyRegMap = Vec(RabCommitWidth, ValidIO(new RegWriteFromRab))
    val excpInfo = ValidIO(new VecExcpInfo)
    val ratOldPest = new RatToVecExcpMod
  })

  val traceCoreInterface = new TraceCoreInterface(hasOffset = true)

  val perfInfo = Output(new Bundle{
    val ctrlInfo = new Bundle {
      val robFull   = Bool()
      val intdqFull = Bool()
      val fpdqFull  = Bool()
      val lsdqFull  = Bool()
    }
  })
  val diff_int_rat = if (params.basicDebugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
  val diff_fp_rat  = if (params.basicDebugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
  val diff_vec_rat = if (params.basicDebugEn) Some(Vec(31, Output(UInt(PhyRegIdxWidth.W)))) else None
  val diff_v0_rat  = if (params.basicDebugEn) Some(Vec(1, Output(UInt(PhyRegIdxWidth.W)))) else None
  val diff_vl_rat  = if (params.basicDebugEn) Some(Vec(1, Output(UInt(PhyRegIdxWidth.W)))) else None

  val sqCanAccept = Input(Bool())
  val lqCanAccept = Input(Bool())

  val debugTopDown = new Bundle {
    val fromRob = new RobCoreTopDownIO
    val fromCore = new CoreDispatchTopDownIO
  }
  val debugRolling = new RobDebugRollingIO
  val debugEnqLsq = Input(new LsqEnqIO)
}

class NamedIndexes(namedCnt: Seq[(String, Int)]) {
  require(namedCnt.map(_._1).distinct.size == namedCnt.size, "namedCnt should not have the same name")

  val maxIdx = namedCnt.map(_._2).sum
  val nameRangeMap: Map[String, (Int, Int)] = namedCnt.indices.map { i =>
    val begin = namedCnt.slice(0, i).map(_._2).sum
    val end = begin + namedCnt(i)._2
    (namedCnt(i)._1, (begin, end))
  }.toMap

  def apply(name: String): Seq[Int] = {
    require(nameRangeMap.contains(name))
    nameRangeMap(name)._1 until nameRangeMap(name)._2
  }
}
