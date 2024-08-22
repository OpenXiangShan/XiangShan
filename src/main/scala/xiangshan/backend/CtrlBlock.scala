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
import xiangshan.backend.Bundles.{DecodedInst, DynInst, ExceptionInfo, ExuOutput, StaticInst}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfoBundle, LsTopdownInfo, MemCtrl, RedirectGenerator}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.decode.{DecodeStage, FusionDecoder}
import xiangshan.backend.dispatch.{CoreDispatchTopDownIO, Dispatch, DispatchQueue}
import xiangshan.backend.fu.PFEvent
import xiangshan.backend.fu.vector.Bundles.{VType, Vl}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rename.{Rename, RenameTableWrapper, SnapshotGenerator}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.frontend.{FtqPtr, FtqRead, Ftq_RF_Components}
import xiangshan.mem.{LqPtr, LsqEnqIO}
import xiangshan.backend.issue.{FpScheduler, IntScheduler, MemScheduler, VfScheduler}

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
  val ftqIdxAhead = Vec(BackendRedirectNum, Valid(new FtqPtr))
  val ftqIdxSelOH = Valid(UInt((BackendRedirectNum).W))
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
{
  val pcMemRdIndexes = new NamedIndexes(Seq(
    "redirect"  -> 1,
    "memPred"   -> 1,
    "robFlush"  -> 1,
    "load"      -> params.LduCnt,
    "hybrid"    -> params.HyuCnt,
    "store"     -> (if(EnableStorePrefetchSMS) params.StaCnt else 0)
  ))

  private val numPcMemReadForExu = params.numPcReadPort
  private val numPcMemRead = pcMemRdIndexes.maxIdx

  // now pcMem read for exu is moved to PcTargetMem (OG0)
  println(s"pcMem read num: $numPcMemRead")
  println(s"pcMem read num for exu: $numPcMemReadForExu")

  val io = IO(new CtrlBlockIO())

  val gpaMem = wrapper.gpaMem.module
  val decode = Module(new DecodeStage)
  val fusionDecoder = Module(new FusionDecoder)
  val rat = Module(new RenameTableWrapper)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intDq0 = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth/2, dqIndex = 0))
  val intDq1 = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth/2, dqIndex = 1))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.VecDqDeqWidth))
  val vecDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.VecDqDeqWidth))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth))
  val redirectGen = Module(new RedirectGenerator)
  private def hasRen: Boolean = true
  private val pcMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numPcMemRead, 1, "BackendPC", hasRen = hasRen))
  private val rob = wrapper.rob.module
  private val memCtrl = Module(new MemCtrl(params))

  private val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable

  private val s0_robFlushRedirect = rob.io.flushOut
  private val s1_robFlushRedirect = Wire(Valid(new Redirect))
  s1_robFlushRedirect.valid := GatedValidRegNext(s0_robFlushRedirect.valid, false.B)
  s1_robFlushRedirect.bits := RegEnable(s0_robFlushRedirect.bits, s0_robFlushRedirect.valid)

  pcMem.io.ren.get(pcMemRdIndexes("robFlush").head) := s0_robFlushRedirect.valid
  pcMem.io.raddr(pcMemRdIndexes("robFlush").head) := s0_robFlushRedirect.bits.ftqIdx.value
  private val s1_robFlushPc = pcMem.io.rdata(pcMemRdIndexes("robFlush").head).getPc(RegEnable(s0_robFlushRedirect.bits.ftqOffset, s0_robFlushRedirect.valid))
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

  val wbDataNoStd = io.fromWB.wbData.filter(!_.bits.params.hasStdFu)
  val intScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[IntScheduler])
  val fpScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[FpScheduler])
  val vfScheWbData = io.fromWB.wbData.filter(_.bits.params.schdType.isInstanceOf[VfScheduler])
  val intCanCompress = intScheWbData.filter(_.bits.params.CanCompress)
  val i2vWbData = intScheWbData.filter(_.bits.params.writeVecRf)
  val f2vWbData = fpScheWbData.filter(_.bits.params.writeVecRf)
  val memVloadWbData = io.fromWB.wbData.filter(x => x.bits.params.schdType.isInstanceOf[MemScheduler] && x.bits.params.hasVLoadFu)
  private val delayedNotFlushedWriteBackNums = wbDataNoStd.map(x => {
    val valid = x.valid
    val killedByOlder = x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect, s3_s5_redirect))
    val delayed = Wire(Valid(UInt(io.fromWB.wbData.size.U.getWidth.W)))
    delayed.valid := GatedValidRegNext(valid && !killedByOlder)
    val isIntSche = intCanCompress.contains(x)
    val isFpSche = fpScheWbData.contains(x)
    val isVfSche = vfScheWbData.contains(x)
    val isMemVload = memVloadWbData.contains(x)
    val isi2v = i2vWbData.contains(x)
    val isf2v = f2vWbData.contains(x)
    val canSameRobidxWbData = if(isVfSche) {
      i2vWbData ++ f2vWbData ++ vfScheWbData
    } else if(isi2v) {
      intCanCompress ++ fpScheWbData ++ vfScheWbData
    } else if (isf2v) {
      intCanCompress ++ fpScheWbData ++ vfScheWbData
    } else if (isIntSche) {
      intCanCompress ++ fpScheWbData
    } else if (isFpSche) {
      intCanCompress ++ fpScheWbData
    }  else if (isMemVload) {
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
    val out = Wire(Valid(new Redirect()))
    out.valid := x.valid && x.bits.redirect.get.valid && x.bits.redirect.get.bits.cfiUpdate.isMisPred && !x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect))
    out.bits := x.bits.redirect.get.bits
    out.bits.debugIsCtrl := true.B
    out.bits.debugIsMemVio := false.B
    out
  }).toSeq
  private val oldestOneHot = Redirect.selectOldestRedirect(exuRedirects)
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
  redirectGen.io.memPredPcRead.data := pcMem.io.rdata(pcMemRdIndexes("memPred").head).getPc(RegEnable(memViolation.bits.stFtqOffset, memViolation.valid))

  for ((pcMemIdx, i) <- pcMemRdIndexes("load").zipWithIndex) {
    // load read pcMem (s0) -> get rdata (s1) -> reg next in Memblock (s2) -> reg next in Memblock (s3) -> consumed by pf (s3)
    pcMem.io.ren.get(pcMemIdx) := io.memLdPcRead(i).valid
    pcMem.io.raddr(pcMemIdx) := io.memLdPcRead(i).ptr.value
    io.memLdPcRead(i).data := pcMem.io.rdata(pcMemIdx).getPc(RegEnable(io.memLdPcRead(i).offset, io.memLdPcRead(i).valid))
  }

  for ((pcMemIdx, i) <- pcMemRdIndexes("hybrid").zipWithIndex) {
    // load read pcMem (s0) -> get rdata (s1) -> reg next in Memblock (s2) -> reg next in Memblock (s3) -> consumed by pf (s3)
    pcMem.io.ren.get(pcMemIdx) := io.memHyPcRead(i).valid
    pcMem.io.raddr(pcMemIdx) := io.memHyPcRead(i).ptr.value
    io.memHyPcRead(i).data := pcMem.io.rdata(pcMemIdx).getPc(RegEnable(io.memHyPcRead(i).offset, io.memHyPcRead(i).valid))
  }

  if (EnableStorePrefetchSMS) {
    for ((pcMemIdx, i) <- pcMemRdIndexes("store").zipWithIndex) {
      pcMem.io.ren.get(pcMemIdx) := io.memStPcRead(i).valid
      pcMem.io.raddr(pcMemIdx) := io.memStPcRead(i).ptr.value
      io.memStPcRead(i).data := pcMem.io.rdata(pcMemIdx).getPc(RegEnable(io.memStPcRead(i).offset, io.memStPcRead(i).valid))
    }
  } else {
    io.memStPcRead.foreach(_.data := 0.U)
  }

  redirectGen.io.hartId := io.fromTop.hartId
  redirectGen.io.oldestExuRedirect.valid := GatedValidRegNext(oldestExuRedirect.valid)
  redirectGen.io.oldestExuRedirect.bits := RegEnable(oldestExuRedirect.bits, oldestExuRedirect.valid)
  redirectGen.io.oldestExuOutPredecode.valid := GatedValidRegNext(oldestExuPredecode.valid)
  redirectGen.io.oldestExuOutPredecode := RegEnable(oldestExuPredecode, oldestExuPredecode.valid)
  redirectGen.io.loadReplay <> loadReplay
  val loadRedirectPcRead = pcMem.io.rdata(pcMemRdIndexes("redirect").head).getPc(RegEnable(memViolation.bits.ftqOffset, memViolation.valid))
  redirectGen.io.loadReplay.bits.cfiUpdate.pc := loadRedirectPcRead
  val load_pc_offset = Mux(loadReplay.bits.flushItself(), 0.U, Mux(loadReplay.bits.isRVC, 2.U, 4.U))
  val load_target = loadRedirectPcRead + load_pc_offset
  redirectGen.io.loadReplay.bits.cfiUpdate.target := load_target

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

  val flushTarget = Mux(s5_csrIsTrap, s5_trapTargetFromCsr, s2_robFlushPc)
  when (s6_flushFromRobValid) {
    io.frontend.toFtq.redirect.bits.level := RedirectLevel.flush
    io.frontend.toFtq.redirect.bits.cfiUpdate.target := RegEnable(flushTarget, s5_flushFromRobValidAhead)
  }

  for (i <- 0 until DecodeWidth) {
    gpaMem.io.fromIFU := io.frontend.fromIfu
    gpaMem.io.exceptionReadAddr.valid := rob.io.readGPAMemAddr.valid
    gpaMem.io.exceptionReadAddr.bits.ftqPtr := rob.io.readGPAMemAddr.bits.ftqPtr
    gpaMem.io.exceptionReadAddr.bits.ftqOffset := rob.io.readGPAMemAddr.bits.ftqOffset
  }

  // vtype commit
  decode.io.fromCSR := io.fromCSR.toDecode
  decode.io.isResumeVType := rob.io.toDecode.isResumeVType
  decode.io.commitVType := rob.io.toDecode.commitVType
  decode.io.walkVType := rob.io.toDecode.walkVType

  decode.io.redirect := s1_s3_redirect.valid || s2_s4_pendingRedirectValid
  decode.io.vtypeRedirect := s1_s3_redirect.valid

  // add decode Buf for in.ready better timing
  val decodeBufBits = Reg(Vec(DecodeWidth, new StaticInst))
  val decodeBufValid = RegInit(VecInit(Seq.fill(DecodeWidth)(false.B)))
  val decodeFromFrontend = io.frontend.cfVec
  val decodeBufNotAccept = VecInit(decodeBufValid.zip(decode.io.in).map(x => x._1 && !x._2.ready))
  val decodeBufAcceptNum = PriorityMuxDefault(decodeBufNotAccept.zip(Seq.tabulate(DecodeWidth)(i => i.U)), DecodeWidth.U)
  val decodeFromFrontendNotAccept = VecInit(decodeFromFrontend.zip(decode.io.in).map(x => decodeBufValid(0) || x._1.valid && !x._2.ready))
  val decodeFromFrontendAcceptNum = PriorityMuxDefault(decodeFromFrontendNotAccept.zip(Seq.tabulate(DecodeWidth)(i => i.U)), DecodeWidth.U)
  if (backendParams.debugEn) {
    dontTouch(decodeBufNotAccept)
    dontTouch(decodeBufAcceptNum)
    dontTouch(decodeFromFrontendNotAccept)
    dontTouch(decodeFromFrontendAcceptNum)
  }
  val a = decodeBufNotAccept.drop(2)
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
  val decodeConnectFromFrontend = Wire(Vec(DecodeWidth, new StaticInst))
  decodeConnectFromFrontend.zip(decodeFromFrontend).map(x => x._1.connectCtrlFlow(x._2.bits))
  decode.io.in.zipWithIndex.foreach { case (decodeIn, i) =>
    decodeIn.valid := Mux(decodeBufValid(0), decodeBufValid(i), decodeFromFrontend(i).valid)
    decodeFromFrontend(i).ready := decodeFromFrontend(0).valid && !decodeBufValid(0) && decodeFromFrontend(i).valid && !decode.io.redirect
    decodeIn.bits := Mux(decodeBufValid(i), decodeBufBits(i), decodeConnectFromFrontend(i))
  }
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

  val useSnpt = VecInit.tabulate(RenameSnapshotNum)(idx =>
    snpt.io.valids(idx) && (s1_s3_redirect.bits.robIdx > snpt.io.snapshots(idx).robIdx.head ||
      !s1_s3_redirect.bits.flushItself() && s1_s3_redirect.bits.robIdx === snpt.io.snapshots(idx).robIdx.head)
  ).reduceTree(_ || _)
  val snptSelect = MuxCase(
    0.U(log2Ceil(RenameSnapshotNum).W),
    (1 to RenameSnapshotNum).map(i => (snpt.io.enqPtr - i.U).value).map(idx =>
      (snpt.io.valids(idx) && (s1_s3_redirect.bits.robIdx > snpt.io.snapshots(idx).robIdx.head ||
        !s1_s3_redirect.bits.flushItself() && s1_s3_redirect.bits.robIdx === snpt.io.snapshots(idx).robIdx.head), idx)
    )
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

  val decodeHasException = decode.io.out.map(x => x.bits.exceptionVec(instrPageFault) || x.bits.exceptionVec(instrAccessFault))
  // fusion decoder
  for (i <- 0 until DecodeWidth) {
    fusionDecoder.io.in(i).valid := decode.io.out(i).valid && !(decodeHasException(i) || disableFusion)
    fusionDecoder.io.in(i).bits := decode.io.out(i).bits.instr
    if (i > 0) {
      fusionDecoder.io.inReady(i - 1) := decode.io.out(i).ready
    }
  }

  private val decodePipeRename = Wire(Vec(RenameWidth, DecoupledIO(new DecodedInst)))

  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), decodePipeRename(i), rename.io.in(i).ready,
      s1_s3_redirect.valid || s2_s4_pendingRedirectValid, moduleName = Some("decodePipeRenameModule"))

    decodePipeRename(i).ready := rename.io.in(i).ready
    rename.io.in(i).valid := decodePipeRename(i).valid && !fusionDecoder.io.clear(i)
    rename.io.in(i).bits := decodePipeRename(i).bits
  }

  for (i <- 0 until RenameWidth - 1) {
    fusionDecoder.io.dec(i) := decodePipeRename(i).bits
    rename.io.fusionInfo(i) := fusionDecoder.io.info(i)

    // update the first RenameWidth - 1 instructions
    decode.io.fusion(i) := fusionDecoder.io.out(i).valid && rename.io.out(i).fire
    when (fusionDecoder.io.out(i).valid) {
      fusionDecoder.io.out(i).bits.update(rename.io.in(i).bits)
      // TODO: remove this dirty code for ftq update
      val sameFtqPtr = rename.io.in(i).bits.ftqPtr.value === rename.io.in(i + 1).bits.ftqPtr.value
      val ftqOffset0 = rename.io.in(i).bits.ftqOffset
      val ftqOffset1 = rename.io.in(i + 1).bits.ftqOffset
      val ftqOffsetDiff = ftqOffset1 - ftqOffset0
      val cond1 = sameFtqPtr && ftqOffsetDiff === 1.U
      val cond2 = sameFtqPtr && ftqOffsetDiff === 2.U
      val cond3 = !sameFtqPtr && ftqOffset1 === 0.U
      val cond4 = !sameFtqPtr && ftqOffset1 === 1.U
      rename.io.in(i).bits.commitType := Mux(cond1, 4.U, Mux(cond2, 5.U, Mux(cond3, 6.U, 7.U)))
      XSError(!cond1 && !cond2 && !cond3 && !cond4, p"new condition $sameFtqPtr $ftqOffset0 $ftqOffset1\n")
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

  val renameOut = Wire(chiselTypeOf(rename.io.out))
  renameOut <> rename.io.out
  // pass all snapshot in the first element for correctness of blockBackward
  renameOut.tail.foreach(_.bits.snapshot := false.B)
  renameOut.head.bits.snapshot := Mux(isFull(snpt.io.enqPtr, snpt.io.deqPtr),
    false.B,
    Cat(rename.io.out.map(out => out.valid && out.bits.snapshot)).orR
  )

  // pipeline between rename and dispatch
  PipeGroupConnect(renameOut, dispatch.io.fromRename, s1_s3_redirect.valid, dispatch.io.toRenameAllFire, "renamePipeDispatch")
  dispatch.io.intIQValidNumVec := io.intIQValidNumVec
  dispatch.io.fpIQValidNumVec := io.fpIQValidNumVec
  dispatch.io.fromIntDQ.intDQ0ValidDeq0Num := intDq0.io.validDeq0Num
  dispatch.io.fromIntDQ.intDQ0ValidDeq1Num := intDq0.io.validDeq1Num
  dispatch.io.fromIntDQ.intDQ1ValidDeq0Num := intDq1.io.validDeq0Num
  dispatch.io.fromIntDQ.intDQ1ValidDeq1Num := intDq1.io.validDeq1Num

  dispatch.io.hartId := io.fromTop.hartId
  dispatch.io.redirect := s1_s3_redirect
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.robHead := rob.io.debugRobHead
  dispatch.io.stallReason <> rename.io.stallReason.out
  dispatch.io.lqCanAccept := io.lqCanAccept
  dispatch.io.sqCanAccept := io.sqCanAccept
  dispatch.io.robHeadNotReady := rob.io.headNotReady
  dispatch.io.robFull := rob.io.robFull
  dispatch.io.singleStep := GatedValidRegNext(io.csrCtrl.singlestep)

  intDq0.io.enq <> dispatch.io.toIntDq0
  intDq0.io.redirect <> s2_s4_redirect
  intDq1.io.enq <> dispatch.io.toIntDq1
  intDq1.io.redirect <> s2_s4_redirect

  fpDq.io.enq <> dispatch.io.toFpDq
  fpDq.io.redirect <> s2_s4_redirect

  vecDq.io.enq <> dispatch.io.toVecDq
  vecDq.io.redirect <> s2_s4_redirect

  lsDq.io.enq <> dispatch.io.toLsDq
  lsDq.io.redirect <> s2_s4_redirect

  io.toIssueBlock.intUops <> (intDq0.io.deq :++ intDq1.io.deq)
  io.toIssueBlock.fpUops <> fpDq.io.deq
  io.toIssueBlock.vfUops  <> vecDq.io.deq
  io.toIssueBlock.memUops <> lsDq.io.deq
  io.toIssueBlock.allocPregs <> dispatch.io.allocPregs
  io.toIssueBlock.flush   <> s2_s4_redirect

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

  // rob to mem block
  io.robio.lsq <> rob.io.lsq

  io.debug_int_rat    .foreach(_ := rat.io.diff_int_rat.get)
  io.debug_fp_rat     .foreach(_ := rat.io.diff_fp_rat.get)
  io.debug_vec_rat    .foreach(_ := rat.io.diff_vec_rat.get)
  io.debug_v0_rat.foreach(_ := rat.io.diff_v0_rat.get)
  io.debug_vl_rat.foreach(_ := rat.io.diff_vl_rat.get)

  rob.io.debug_ls := io.robio.debug_ls
  rob.io.debugHeadLsIssue := io.robio.robHeadLsIssue
  rob.io.lsTopdownInfo := io.robio.lsTopdownInfo
  rob.io.debugEnqLsq := io.debugEnqLsq

  io.robio.robDeqPtr := rob.io.robDeqPtr

  // rob to backend
  io.robio.commitVType := rob.io.toDecode.commitVType
  // exu block to decode
  decode.io.vsetvlVType := io.toDecode.vsetvlVType
  // backend to decode
  decode.io.vstart := io.toDecode.vstart
  // backend to rob
  rob.io.vstartIsZero := io.toDecode.vstart === 0.U

  io.debugTopDown.fromRob := rob.io.debugTopDown.toCore
  dispatch.io.debugTopDown.fromRob := rob.io.debugTopDown.toDispatch
  dispatch.io.debugTopDown.fromCore := io.debugTopDown.fromCore
  io.debugRolling := rob.io.debugRolling

  io.perfInfo.ctrlInfo.robFull := GatedValidRegNext(rob.io.robFull)
  io.perfInfo.ctrlInfo.intdqFull := GatedValidRegNext(intDq0.io.dqFull || intDq1.io.dqFull)
  io.perfInfo.ctrlInfo.fpdqFull := GatedValidRegNext(vecDq.io.dqFull)
  io.perfInfo.ctrlInfo.lsdqFull := GatedValidRegNext(lsDq.io.dqFull)

  val perfEvents = Seq(decode, rename, dispatch, intDq0, intDq1, vecDq, lsDq, rob).flatMap(_.getPerfEvents)
  generatePerfEvent()
}

class CtrlBlockIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }
  val toTop = new Bundle {
    val cpuHalt = Output(Bool())
  }
  val frontend = Flipped(new FrontendToCtrlIO())
  val fromCSR = new Bundle{
    val toDecode = Input(new CSRToDecode)
  }
  val toIssueBlock = new Bundle {
    val flush = ValidIO(new Redirect)
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val intUops = Vec(dpParams.IntDqDeqWidth, DecoupledIO(new DynInst))
    val vfUops = Vec(dpParams.VecDqDeqWidth, DecoupledIO(new DynInst))
    val fpUops = Vec(dpParams.FpDqDeqWidth, DecoupledIO(new DynInst))
    val memUops = Vec(dpParams.LsDqDeqWidth, DecoupledIO(new DynInst))
  }
  val toDataPath = new Bundle {
    val flush = ValidIO(new Redirect)
  }
  val toExuBlock = new Bundle {
    val flush = ValidIO(new Redirect)
  }
  val intIQValidNumVec = Input(MixedVec(params.genIntIQValidNumBundle))
  val fpIQValidNumVec = Input(MixedVec(params.genFpIQValidNumBundle))
  val fromWB = new Bundle {
    val wbData = Flipped(MixedVec(params.genWrite2CtrlBundles))
  }
  val redirect = ValidIO(new Redirect)
  val fromMem = new Bundle {
    val stIn = Vec(params.StaExuCnt, Flipped(ValidIO(new DynInst))) // use storeSetHit, ssid, robIdx
    val violation = Flipped(ValidIO(new Redirect))
  }
  val memLdPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
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
  }

  val toDecode = new Bundle {
    val vsetvlVType = Input(VType())
    val vstart = Input(Vl())
  }

  val perfInfo = Output(new Bundle{
    val ctrlInfo = new Bundle {
      val robFull   = Bool()
      val intdqFull = Bool()
      val fpdqFull  = Bool()
      val lsdqFull  = Bool()
    }
  })
  val debug_int_rat     = if (params.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
  val debug_fp_rat      = if (params.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
  val debug_vec_rat     = if (params.debugEn) Some(Vec(31, Output(UInt(PhyRegIdxWidth.W)))) else None
  val debug_v0_rat      = if (params.debugEn) Some(Vec(1, Output(UInt(PhyRegIdxWidth.W)))) else None
  val debug_vl_rat      = if (params.debugEn) Some(Vec(1, Output(UInt(PhyRegIdxWidth.W)))) else None

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
