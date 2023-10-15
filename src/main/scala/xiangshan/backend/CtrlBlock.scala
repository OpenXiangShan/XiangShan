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
import xiangshan.backend.Bundles.{DecodedInst, DynInst, ExceptionInfo, ExuOutput}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfoBundle, LsTopdownInfo, MemCtrl, RedirectGenerator}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.decode.{DecodeStage, FusionDecoder}
import xiangshan.backend.dispatch.{CoreDispatchTopDownIO, Dispatch, DispatchQueue}
import xiangshan.backend.fu.PFEvent
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{Rename, RenameTableWrapper, SnapshotGenerator}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobCoreTopDownIO, RobDebugRollingIO, RobLsqIO, RobPtr}
import xiangshan.frontend.{FtqPtr, FtqRead, Ftq_RF_Components}
import xiangshan.mem.{LqPtr, LsqEnqIO}

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
    "exu"       -> params.numPcReadPort,
    "redirect"  -> 1,
    "memPred"   -> 1,
    "robFlush"  -> 1,
    "load"      -> params.LduCnt,
    "store"     -> (if(EnableStorePrefetchSMS) params.StaCnt else 0)
  ))

  private val numPcMemReadForExu = params.numPcReadPort
  private val numPcMemRead = pcMemRdIndexes.maxIdx

  println(s"pcMem read num: $numPcMemRead")
  println(s"pcMem read num for exu: $numPcMemReadForExu")

  val io = IO(new CtrlBlockIO())

  val decode = Module(new DecodeStage)
  val fusionDecoder = Module(new FusionDecoder)
  val rat = Module(new RenameTableWrapper)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth))
  val redirectGen = Module(new RedirectGenerator)
  private val pcMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numPcMemRead, 1, "BackendPC"))
  private val rob = wrapper.rob.module
  private val memCtrl = Module(new MemCtrl(params))

  private val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable

  private val s0_robFlushRedirect = rob.io.flushOut
  private val s1_robFlushRedirect = Wire(Valid(new Redirect))
  s1_robFlushRedirect.valid := RegNext(s0_robFlushRedirect.valid)
  s1_robFlushRedirect.bits := RegEnable(s0_robFlushRedirect.bits, s0_robFlushRedirect.valid)

  pcMem.io.raddr(pcMemRdIndexes("robFlush").head) := s0_robFlushRedirect.bits.ftqIdx.value
  private val s1_robFlushPc = pcMem.io.rdata(pcMemRdIndexes("robFlush").head).getPc(RegNext(s0_robFlushRedirect.bits.ftqOffset))
  private val s3_redirectGen = redirectGen.io.stage2Redirect
  private val s1_s3_redirect = Mux(s1_robFlushRedirect.valid, s1_robFlushRedirect, s3_redirectGen)
  private val s2_s4_pendingRedirectValid = RegInit(false.B)
  when (s1_s3_redirect.valid) {
    s2_s4_pendingRedirectValid := true.B
  }.elsewhen (RegNext(io.frontend.toFtq.redirect.valid)) {
    s2_s4_pendingRedirectValid := false.B
  }

  // Redirect will be RegNext at ExuBlocks and IssueBlocks
  val s2_s4_redirect = RegNextWithEnable(s1_s3_redirect)
  val s3_s5_redirect = RegNextWithEnable(s2_s4_redirect)

  private val delayedNotFlushedWriteBack = io.fromWB.wbData.map(x => {
    val valid = x.valid
    val killedByOlder = x.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect, s3_s5_redirect))
    val delayed = Wire(Valid(new ExuOutput(x.bits.params)))
    delayed.valid := RegNext(valid && !killedByOlder)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed.bits.debugInfo.writebackTime := GTimer()
    delayed
  }).toSeq

  private val exuPredecode = VecInit(
    delayedNotFlushedWriteBack.filter(_.bits.redirect.nonEmpty).map(x => x.bits.predecodeInfo.get).toSeq
  )

  private val exuRedirects: Seq[ValidIO[Redirect]] = delayedNotFlushedWriteBack.filter(_.bits.redirect.nonEmpty).map(x => {
    val out = Wire(Valid(new Redirect()))
    out.valid := x.valid && x.bits.redirect.get.valid && x.bits.redirect.get.bits.cfiUpdate.isMisPred
    out.bits := x.bits.redirect.get.bits
    out.bits.debugIsCtrl := true.B
    out.bits.debugIsMemVio := false.B
    out
  }).toSeq

  private val memViolation = io.fromMem.violation
  val loadReplay = Wire(ValidIO(new Redirect))
  loadReplay.valid := RegNext(memViolation.valid &&
    !memViolation.bits.robIdx.needFlush(Seq(s1_s3_redirect, s2_s4_redirect))
  )
  loadReplay.bits := RegEnable(memViolation.bits, memViolation.valid)
  loadReplay.bits.debugIsCtrl := false.B
  loadReplay.bits.debugIsMemVio := true.B

  val pdestReverse = rob.io.commits.info.map(info => info.pdest).reverse

  pcMem.io.raddr(pcMemRdIndexes("redirect").head) := redirectGen.io.redirectPcRead.ptr.value
  redirectGen.io.redirectPcRead.data := pcMem.io.rdata(pcMemRdIndexes("redirect").head).getPc(RegNext(redirectGen.io.redirectPcRead.offset))
  pcMem.io.raddr(pcMemRdIndexes("memPred").head) := redirectGen.io.memPredPcRead.ptr.value
  redirectGen.io.memPredPcRead.data := pcMem.io.rdata(pcMemRdIndexes("memPred").head).getPc(RegNext(redirectGen.io.memPredPcRead.offset))

  for ((pcMemIdx, i) <- pcMemRdIndexes("load").zipWithIndex) {
    pcMem.io.raddr(pcMemIdx) := io.memLdPcRead(i).ptr.value
    io.memLdPcRead(i).data := pcMem.io.rdata(pcMemIdx).getPc(RegNext(io.memLdPcRead(i).offset))
  }

  if (EnableStorePrefetchSMS) {
    for ((pcMemIdx, i) <- pcMemRdIndexes("store").zipWithIndex) {
      pcMem.io.raddr(pcMemIdx) := io.memStPcRead(i).ptr.value
      io.memStPcRead(i).data := pcMem.io.rdata(pcMemIdx).getPc(RegNext(io.memStPcRead(i).offset))
    }
  } else {
    io.memStPcRead.foreach(_.data := 0.U)
  }

  redirectGen.io.hartId := io.fromTop.hartId
  redirectGen.io.exuRedirect := exuRedirects.toSeq
  redirectGen.io.exuOutPredecode := exuPredecode // guarded by exuRedirect.valid
  redirectGen.io.loadReplay <> loadReplay

  redirectGen.io.robFlush := s1_robFlushRedirect.valid

  val s5_flushFromRobValidAhead = DelayN(s1_robFlushRedirect.valid, 4)
  val s6_flushFromRobValid = RegNext(s5_flushFromRobValidAhead)
  val frontendFlushBits = RegEnable(s1_robFlushRedirect.bits, s1_robFlushRedirect.valid) // ??
  // When ROB commits an instruction with a flush, we notify the frontend of the flush without the commit.
  // Flushes to frontend may be delayed by some cycles and commit before flush causes errors.
  // Thus, we make all flush reasons to behave the same as exceptions for frontend.
  for (i <- 0 until CommitWidth) {
    // why flushOut: instructions with flushPipe are not commited to frontend
    // If we commit them to frontend, it will cause flush after commit, which is not acceptable by frontend.
    val s1_isCommit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit && !s0_robFlushRedirect.valid
    io.frontend.toFtq.rob_commits(i).valid := RegNext(s1_isCommit)
    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), s1_isCommit)
  }
  io.frontend.toFtq.redirect.valid := s6_flushFromRobValid || s3_redirectGen.valid
  io.frontend.toFtq.redirect.bits := Mux(s6_flushFromRobValid, frontendFlushBits, s3_redirectGen.bits)
  io.frontend.toFtq.ftqIdxSelOH.valid := s6_flushFromRobValid || redirectGen.io.stage2Redirect.valid
  io.frontend.toFtq.ftqIdxSelOH.bits := Cat(s6_flushFromRobValid, redirectGen.io.stage2oldestOH & Fill(NumRedirect + 1, !s6_flushFromRobValid))

  //jmp/brh
  for (i <- 0 until NumRedirect) {
    io.frontend.toFtq.ftqIdxAhead(i).valid := exuRedirects(i).valid && exuRedirects(i).bits.cfiUpdate.isMisPred && !s1_robFlushRedirect.valid && !s5_flushFromRobValidAhead
    io.frontend.toFtq.ftqIdxAhead(i).bits := exuRedirects(i).bits.ftqIdx
  }
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
  private val s2_csrIsXRet = io.robio.csr.isXRet
  private val s5_csrIsTrap = DelayN(rob.io.exception.valid, 4)
  private val s2_s5_trapTargetFromCsr = io.robio.csr.trapTarget

  val flushTarget = Mux(s2_csrIsXRet || s5_csrIsTrap, s2_s5_trapTargetFromCsr, s2_robFlushPc)
  when (s6_flushFromRobValid) {
    io.frontend.toFtq.redirect.bits.level := RedirectLevel.flush
    io.frontend.toFtq.redirect.bits.cfiUpdate.target := RegNext(flushTarget)
  }

  // vtype commit
  decode.io.commitVType.bits := io.fromDataPath.vtype
  decode.io.commitVType.valid := RegNext(rob.io.isVsetFlushPipe)

  io.toDataPath.vtypeAddr := rob.io.vconfigPdest

  // vtype walk
  val isVsetSeq = rob.io.commits.walkValid.zip(rob.io.commits.info).map { case (valid, info) => valid && info.isVset }.reverse
  val walkVTypeReverse = rob.io.commits.info.map(info => info.vtype).reverse
  val walkVType = PriorityMux(isVsetSeq, walkVTypeReverse)

  decode.io.walkVType.bits := walkVType.asTypeOf(new VType)
  decode.io.walkVType.valid := rob.io.commits.isWalk && isVsetSeq.reduce(_ || _)

  decode.io.isRedirect := s1_s3_redirect.valid

  decode.io.in.zip(io.frontend.cfVec).foreach { case (decodeIn, frontendCf) =>
    decodeIn.valid := frontendCf.valid
    frontendCf.ready := decodeIn.ready
    decodeIn.bits.connectCtrlFlow(frontendCf.bits)
  }
  decode.io.csrCtrl := RegNext(io.csrCtrl)
  decode.io.intRat <> rat.io.intReadPorts
  decode.io.fpRat <> rat.io.fpReadPorts
  decode.io.vecRat <> rat.io.vecReadPorts
  decode.io.fusion := 0.U.asTypeOf(decode.io.fusion) // Todo
  decode.io.stallReason.in <> io.frontend.stallReason

  // snapshot check
  val snpt = Module(new SnapshotGenerator(rename.io.out.head.bits.robIdx))
  snpt.io.enq := rename.io.out.head.bits.snapshot && rename.io.out.head.fire
  snpt.io.enqData.head := rename.io.out.head.bits.robIdx
  snpt.io.deq := snpt.io.valids(snpt.io.deqPtr.value) && rob.io.commits.isCommit &&
    Cat(rob.io.commits.commitValid.zip(rob.io.commits.robIdx).map(x => x._1 && x._2 === snpt.io.snapshots(snpt.io.deqPtr.value))).orR
  snpt.io.flush := s1_s3_redirect.valid

  val useSnpt = VecInit.tabulate(RenameSnapshotNum)(idx =>
    snpt.io.valids(idx) && s1_s3_redirect.bits.robIdx >= snpt.io.snapshots(idx)
  ).reduceTree(_ || _)
  val snptSelect = MuxCase(
    0.U(log2Ceil(RenameSnapshotNum).W),
    (1 to RenameSnapshotNum).map(i => (snpt.io.enqPtr - i.U).value).map(idx =>
      (snpt.io.valids(idx) && s1_s3_redirect.bits.robIdx >= snpt.io.snapshots(idx), idx)
    )
  )

  rob.io.snpt.snptEnq := DontCare
  rob.io.snpt.snptDeq := snpt.io.deq
  rob.io.snpt.useSnpt := useSnpt
  rob.io.snpt.snptSelect := snptSelect
  rat.io.snpt.snptEnq := rename.io.out.head.bits.snapshot && rename.io.out.head.fire
  rat.io.snpt.snptDeq := snpt.io.deq
  rat.io.snpt.useSnpt := useSnpt
  rat.io.snpt.snptSelect := snptSelect

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
  private val mdpFlodPcVec = Wire(Vec(DecodeWidth, UInt(MemPredPCWidth.W)))
  for (i <- 0 until DecodeWidth) {
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
  memCtrl.io.mdpFlodPcVec := mdpFlodPcVec
  memCtrl.io.dispatchLFSTio <> dispatch.io.lfst

  rat.io.redirect := s1_s3_redirect.valid
  rat.io.robCommits := rob.io.rabCommits
  rat.io.diffCommits := rob.io.diffCommits
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  rat.io.vecRenamePorts := rename.io.vecRenamePorts

  rename.io.redirect := s1_s3_redirect
  rename.io.robCommits <> rob.io.rabCommits
  rename.io.waittable := (memCtrl.io.waitTable2Rename zip decode.io.out).map{ case(waittable2rename, decodeOut) =>
    RegEnable(waittable2rename, decodeOut.fire)
  }
  rename.io.ssit := memCtrl.io.ssit2Rename
  rename.io.intReadPorts := VecInit(rat.io.intReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.fpReadPorts := VecInit(rat.io.fpReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.vecReadPorts := VecInit(rat.io.vecReadPorts.map(x => VecInit(x.map(_.data))))
  rename.io.int_need_free := rat.io.int_need_free
  rename.io.int_old_pdest := rat.io.int_old_pdest
  rename.io.fp_old_pdest := rat.io.fp_old_pdest
  rename.io.vec_old_pdest := rat.io.vec_old_pdest
  rename.io.debug_int_rat.foreach(_ := rat.io.debug_int_rat.get)
  rename.io.debug_fp_rat.foreach(_ := rat.io.debug_fp_rat.get)
  rename.io.debug_vec_rat.foreach(_ := rat.io.debug_vec_rat.get)
  rename.io.debug_vconfig_rat.foreach(_ := rat.io.debug_vconfig_rat.get)
  rename.io.stallReason.in <> decode.io.stallReason.out
  rename.io.snpt.snptEnq := DontCare
  rename.io.snpt.snptDeq := snpt.io.deq
  rename.io.snpt.useSnpt := useSnpt
  rename.io.snpt.snptSelect := snptSelect

  // prevent rob from generating snapshot when full here
  val renameOut = Wire(chiselTypeOf(rename.io.out))
  renameOut <> rename.io.out
  when(isFull(snpt.io.enqPtr, snpt.io.deqPtr)) {
    renameOut.head.bits.snapshot := false.B
  }


  // pipeline between rename and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(renameOut(i), dispatch.io.fromRename(i), dispatch.io.recv(i), s1_s3_redirect.valid)
  }

  dispatch.io.hartId := io.fromTop.hartId
  dispatch.io.redirect := s1_s3_redirect
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.robHead := rob.io.debugRobHead
  dispatch.io.stallReason <> rename.io.stallReason.out
  dispatch.io.lqCanAccept := io.lqCanAccept
  dispatch.io.sqCanAccept := io.sqCanAccept
  dispatch.io.robHeadNotReady := rob.io.headNotReady
  dispatch.io.robFull := rob.io.robFull
  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)

  intDq.io.enq <> dispatch.io.toIntDq
  intDq.io.redirect <> s2_s4_redirect

  fpDq.io.enq <> dispatch.io.toFpDq
  fpDq.io.redirect <> s2_s4_redirect

  lsDq.io.enq <> dispatch.io.toLsDq
  lsDq.io.redirect <> s2_s4_redirect

  io.toIssueBlock.intUops <> intDq.io.deq
  io.toIssueBlock.vfUops  <> fpDq.io.deq
  io.toIssueBlock.memUops <> lsDq.io.deq
  io.toIssueBlock.allocPregs <> dispatch.io.allocPregs
  io.toIssueBlock.flush   <> s2_s4_redirect

  pcMem.io.wen.head   := RegNext(io.frontend.fromFtq.pc_mem_wen)
  pcMem.io.waddr.head := RegNext(io.frontend.fromFtq.pc_mem_waddr)
  pcMem.io.wdata.head := RegNext(io.frontend.fromFtq.pc_mem_wdata)

  private val jumpPcVec         : Vec[UInt] = Wire(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
  io.toIssueBlock.pcVec := jumpPcVec

  io.toDataPath.flush := s2_s4_redirect
  io.toExuBlock.flush := s2_s4_redirect

  for ((pcMemIdx, i) <- pcMemRdIndexes("exu").zipWithIndex) {
    pcMem.io.raddr(pcMemIdx) := intDq.io.deqNext(i).ftqPtr.value
    jumpPcVec(i) := pcMem.io.rdata(pcMemIdx).getPc(RegNext(intDq.io.deqNext(i).ftqOffset))
  }

  val dqOuts = Seq(io.toIssueBlock.intUops) ++ Seq(io.toIssueBlock.vfUops) ++ Seq(io.toIssueBlock.memUops)
  dqOuts.zipWithIndex.foreach { case (dqOut, dqIdx) =>
    dqOut.map(_.bits.pc).zipWithIndex.map{ case (pc, portIdx) =>
      if(params.allSchdParams(dqIdx).numPcReadPort > 0){
        val realJumpPcVec = jumpPcVec.drop(params.allSchdParams.take(dqIdx).map(_.numPcReadPort).sum).take(params.allSchdParams(dqIdx).numPcReadPort)
        pc := realJumpPcVec(portIdx)
      }
    }
  }

  rob.io.hartId := io.fromTop.hartId
  rob.io.redirect := s1_s3_redirect
  rob.io.writeback := delayedNotFlushedWriteBack

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
  io.debug_vconfig_rat.foreach(_ := rat.io.diff_vconfig_rat.get)

  rob.io.debug_ls := io.robio.debug_ls
  rob.io.debugHeadLsIssue := io.robio.robHeadLsIssue
  rob.io.lsTopdownInfo := io.robio.lsTopdownInfo
  rob.io.debugEnqLsq := io.debugEnqLsq

  io.robio.robDeqPtr := rob.io.robDeqPtr

  io.debugTopDown.fromRob := rob.io.debugTopDown.toCore
  dispatch.io.debugTopDown.fromRob := rob.io.debugTopDown.toDispatch
  dispatch.io.debugTopDown.fromCore := io.debugTopDown.fromCore
  io.debugRolling := rob.io.debugRolling

  io.perfInfo.ctrlInfo.robFull := RegNext(rob.io.robFull)
  io.perfInfo.ctrlInfo.intdqFull := RegNext(intDq.io.dqFull)
  io.perfInfo.ctrlInfo.fpdqFull := RegNext(fpDq.io.dqFull)
  io.perfInfo.ctrlInfo.lsdqFull := RegNext(lsDq.io.dqFull)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := RegNext(io.csrCtrl.distribute_csr)
  val csrevents = pfevent.io.hpmevent.slice(8,16)

  val perfinfo = IO(new Bundle(){
    val perfEventsRs      = Input(Vec(params.IqCnt, new PerfEvent))
    val perfEventsEu0     = Input(Vec(6, new PerfEvent))
    val perfEventsEu1     = Input(Vec(6, new PerfEvent))
  })

  val allPerfEvents = Seq(decode, rename, dispatch, intDq, fpDq, lsDq, rob).flatMap(_.getPerf)
  val hpmEvents = allPerfEvents ++ perfinfo.perfEventsEu0 ++ perfinfo.perfEventsEu1 ++ perfinfo.perfEventsRs
  val perfEvents = HPerfMonitor(csrevents, hpmEvents).getPerfEvents
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
  val toIssueBlock = new Bundle {
    val flush = ValidIO(new Redirect)
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val intUops = Vec(dpParams.IntDqDeqWidth, DecoupledIO(new DynInst))
    val vfUops = Vec(dpParams.FpDqDeqWidth, DecoupledIO(new DynInst))
    val memUops = Vec(dpParams.LsDqDeqWidth, DecoupledIO(new DynInst))
    val pcVec = Output(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
  }
  val fromDataPath = new Bundle{
    val vtype = Input(new VType)
  }
  val toDataPath = new Bundle {
    val vtypeAddr = Output(UInt(PhyRegIdxWidth.W))
    val flush = ValidIO(new Redirect)
  }
  val toExuBlock = new Bundle {
    val flush = ValidIO(new Redirect)
  }
  val fromWB = new Bundle {
    val wbData = Flipped(MixedVec(params.genWrite2CtrlBundles))
  }
  val redirect = ValidIO(new Redirect)
  val fromMem = new Bundle {
    val stIn = Vec(params.StaCnt, Flipped(ValidIO(new DynInst))) // use storeSetHit, ssid, robIdx
    val violation = Flipped(ValidIO(new Redirect))
  }
  val memLdPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
  val memStPcRead = Vec(params.StaCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  val csrCtrl = Input(new CustomCSRCtrlIO)
  val robio = new Bundle {
    val csr = new RobCSRIO
    val exception = ValidIO(new ExceptionInfo)
    val lsq = new RobLsqIO
    val lsTopdownInfo = Vec(params.LduCnt, Input(new LsTopdownInfo))
    val debug_ls = Input(new DebugLSIO())
    val robHeadLsIssue = Input(Bool())
    val robDeqPtr = Output(new RobPtr)
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
  val debug_vec_rat     = if (params.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
  val debug_vconfig_rat = if (params.debugEn) Some(Output(UInt(PhyRegIdxWidth.W))) else None // TODO: use me

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
