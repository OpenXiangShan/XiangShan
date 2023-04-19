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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import xiangshan.backend.decode.{DecodeStage, FusionDecoder, ImmUnion}
import xiangshan.backend.dispatch.{Dispatch, Dispatch2Rs, DispatchQueue}
import xiangshan.backend.fu.PFEvent
import xiangshan.backend.rename.{Rename, RenameTableWrapper}
import xiangshan.backend.rob.{DebugLSIO, Rob, RobCSRIO, RobLsqIO}
import xiangshan.frontend.{FtqPtr, FtqRead, Ftq_RF_Components}
import xiangshan.mem.mdp.{LFST, SSIT, WaitTable}
import xiangshan.ExceptionNO._
import xiangshan.backend.exu.ExuConfig
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO}

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  def numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class RedirectGeneratorIO(implicit p: Parameters) extends XSBundle {
    def numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
    val hartId = Input(UInt(8.W))
    val exuMispredict = Vec(numRedirect, Flipped(ValidIO(new ExuOutput)))
    val loadReplay = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val redirectPcRead = new FtqRead(UInt(VAddrBits.W))
    val stage2Redirect = ValidIO(new Redirect)
    val stage3Redirect = ValidIO(new Redirect)
    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredPcRead = new FtqRead(UInt(VAddrBits.W)) // read req send form stage 2
    val isMisspreRedirect = Output(Bool())
  }
  val io = IO(new RedirectGeneratorIO)
  /*
        LoadQueue  Jump  ALU0  ALU1  ALU2  ALU3   exception    Stage1
          |         |      |    |     |     |         |
          |============= reg & compare =====|         |       ========
                            |                         |
                            |                         |
                            |                         |        Stage2
                            |                         |
                    redirect (flush backend)          |
                    |                                 |
               === reg ===                            |       ========
                    |                                 |
                    |----- mux (exception first) -----|        Stage3
                            |
                redirect (send to frontend)
   */
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }

  def getRedirect(exuOut: Valid[ExuOutput]): ValidIO[Redirect] = {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := exuOut.valid && exuOut.bits.redirect.cfiUpdate.isMisPred
    redirect.bits := exuOut.bits.redirect
    redirect.bits.debugIsCtrl := true.B
    redirect.bits.debugIsMemVio := false.B
    redirect
  }

  val jumpOut = io.exuMispredict.head
  val allRedirect = VecInit(io.exuMispredict.map(x => getRedirect(x)) :+ io.loadReplay)
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(io.stage2Redirect) || io.flush))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map{ case (v, f) => v && !f }).asUInt.orR
  val oldestExuOutput = Mux1H(io.exuMispredict.indices.map(oldestOneHot), io.exuMispredict)
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)
  io.isMisspreRedirect := VecInit(io.exuMispredict.map(x => getRedirect(x).valid)).asUInt.orR
  io.redirectPcRead.ptr := oldestRedirect.bits.ftqIdx
  io.redirectPcRead.offset := oldestRedirect.bits.ftqOffset

  val s1_jumpTarget = RegEnable(jumpOut.bits.redirect.cfiUpdate.target, jumpOut.valid)
  val s1_imm12_reg = RegNext(oldestExuOutput.bits.uop.ctrl.imm(11, 0))
  val s1_pd = RegNext(oldestExuOutput.bits.uop.cf.pd)
  val s1_redirect_bits_reg = RegNext(oldestRedirect.bits)
  val s1_redirect_valid_reg = RegNext(oldestValid)
  val s1_redirect_onehot = RegNext(oldestOneHot)

  // stage1 -> stage2
  io.stage2Redirect.valid := s1_redirect_valid_reg && !io.flush
  io.stage2Redirect.bits := s1_redirect_bits_reg

  val s1_isReplay = s1_redirect_onehot.last
  val s1_isJump = s1_redirect_onehot.head
  val real_pc = io.redirectPcRead.data
  val brTarget = real_pc + SignExt(ImmUnion.B.toImm32(s1_imm12_reg), XLEN)
  val snpc = real_pc + Mux(s1_pd.isRVC, 2.U, 4.U)
  val target = Mux(s1_isReplay,
    real_pc, // replay from itself
    Mux(s1_redirect_bits_reg.cfiUpdate.taken,
      Mux(s1_isJump, s1_jumpTarget, brTarget),
      snpc
    )
  )

  val stage2CfiUpdate = io.stage2Redirect.bits.cfiUpdate
  stage2CfiUpdate.pc := real_pc
  stage2CfiUpdate.pd := s1_pd
  // stage2CfiUpdate.predTaken := s1_redirect_bits_reg.cfiUpdate.predTaken
  stage2CfiUpdate.target := target
  // stage2CfiUpdate.taken := s1_redirect_bits_reg.cfiUpdate.taken
  // stage2CfiUpdate.isMisPred := s1_redirect_bits_reg.cfiUpdate.isMisPred

  val s2_target = RegEnable(target, s1_redirect_valid_reg)
  val s2_pc = RegEnable(real_pc, s1_redirect_valid_reg)
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, s1_redirect_valid_reg)
  val s2_redirect_valid_reg = RegNext(s1_redirect_valid_reg && !io.flush, init = false.B)

  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg

  // get pc from ftq
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  val store_pc = io.memPredPcRead(s1_redirect_bits_reg.stFtqIdx, s1_redirect_bits_reg.stFtqOffset)

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(s1_isReplay && s1_redirect_valid_reg, init = false.B)
  // update wait table
  io.memPredUpdate.waddr := RegNext(XORFold(real_pc(VAddrBits-1, 1), MemPredPCWidth))
  io.memPredUpdate.wdata := true.B
  // update store set
  io.memPredUpdate.ldpc := RegNext(XORFold(real_pc(VAddrBits-1, 1), MemPredPCWidth))
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.stpc := XORFold(store_pc(VAddrBits-1, 1), MemPredPCWidth)

  // // recover runahead checkpoint if redirect
  // if (!env.FPGAPlatform) {
  //   val runahead_redirect = Module(new DifftestRunaheadRedirectEvent)
  //   runahead_redirect.io.clock := clock
  //   runahead_redirect.io.coreid := io.hartId
  //   runahead_redirect.io.valid := io.stage3Redirect.valid
  //   runahead_redirect.io.pc :=  s2_pc // for debug only
  //   runahead_redirect.io.target_pc := s2_target // for debug only
  //   runahead_redirect.io.checkpoint_id := io.stage3Redirect.bits.debug_runahead_checkpoint_id // make sure it is right
  // }
}

class CtrlBlock(dpExuConfigs: Seq[Seq[Seq[ExuConfig]]])(implicit p: Parameters) extends LazyModule
  with HasWritebackSink with HasWritebackSource {
  val rob = LazyModule(new Rob)

  override def addWritebackSink(source: Seq[HasWritebackSource], index: Option[Seq[Int]]): HasWritebackSink = {
    rob.addWritebackSink(Seq(this), Some(Seq(writebackSinks.length)))
    super.addWritebackSink(source, index)
  }

  // duplicated dispatch2 here to avoid cross-module timing path loop.
  val dispatch2 = dpExuConfigs.map(c => LazyModule(new Dispatch2Rs(c)))
  lazy val module = new CtrlBlockImp(this)

  override lazy val writebackSourceParams: Seq[WritebackSourceParams] = {
    writebackSinksParams
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module

  override def generateWritebackIO(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
  ): Unit = {
    module.io.writeback.zip(writebackSinksImp(thisMod, thisModImp)).foreach(x => x._1 := x._2)
  }
}

class CtrlBlockImp(outer: CtrlBlock)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasCircularQueuePtrHelper
  with HasWritebackSourceImp
  with HasPerfEvents
{
  val writebackLengths = outer.writebackSinksParams.map(_.length)

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val cpu_halt = Output(Bool())
    val frontend = Flipped(new FrontendToCtrlIO)
    // to exu blocks
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val dispatch = Vec(3*dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
    val rsReady = Vec(outer.dispatch2.map(_.module.io.out.length).sum, Input(Bool()))
    val enqLsq = Flipped(new LsqEnqIO)
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
    val sqDeq = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
    val ld_pc_read = Vec(exuParameters.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
    // from int block
    val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val memoryViolation = Flipped(ValidIO(new Redirect))
    val jumpPc = Output(UInt(VAddrBits.W))
    val jalr_target = Output(UInt(VAddrBits.W))
    val robio = new Bundle {
      // to int block
      val toCSR = new RobCSRIO
      val exception = ValidIO(new ExceptionInfo)
      // to mem block
      val lsq = new RobLsqIO
      // debug
      val debug_ls = Flipped(new DebugLSIO)
    }
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val perfInfo = Output(new Bundle{
      val ctrlInfo = new Bundle {
        val robFull   = Input(Bool())
        val intdqFull = Input(Bool())
        val fpdqFull  = Input(Bool())
        val lsdqFull  = Input(Bool())
      }
    })
    val writeback = MixedVec(writebackLengths.map(num => Vec(num, Flipped(ValidIO(new ExuOutput)))))
    // redirect out
    val redirect = ValidIO(new Redirect)
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  override def writebackSource: Option[Seq[Seq[Valid[ExuOutput]]]] = {
    Some(io.writeback.map(writeback => {
      val exuOutput = WireInit(writeback)
      val timer = GTimer()
      for ((wb_next, wb) <- exuOutput.zip(writeback)) {
        wb_next.valid := RegNext(wb.valid && !wb.bits.uop.robIdx.needFlush(Seq(stage2Redirect, redirectForExu)))
        wb_next.bits := RegNext(wb.bits)
        wb_next.bits.uop.debugInfo.writebackTime := timer
      }
      exuOutput
    }))
  }

  val decode = Module(new DecodeStage)
  val fusionDecoder = Module(new FusionDecoder)
  val rat = Module(new RenameTableWrapper)
  val ssit = Module(new SSIT)
  val waittable = Module(new WaitTable)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth))
  val redirectGen = Module(new RedirectGenerator)
  val rob = outer.rob.module

  // jumpPc (2) + redirects (1) + loadPredUpdate (1) + jalr_target (1) + [ld pc (LduCnt)] + robWriteback (sum(writebackLengths)) + robFlush (1)
  val PCMEMIDX_LD = 5
  val pcMem = Module(new SyncDataModuleTemplate(
    new Ftq_RF_Components, FtqSize,
    6 + exuParameters.LduCnt, 1, "CtrlPcMem")
  )
  pcMem.io.wen.head   := RegNext(io.frontend.fromFtq.pc_mem_wen)
  pcMem.io.waddr.head := RegNext(io.frontend.fromFtq.pc_mem_waddr)
  pcMem.io.wdata.head := RegNext(io.frontend.fromFtq.pc_mem_wdata)

  pcMem.io.raddr.last := rob.io.flushOut.bits.ftqIdx.value
  val flushPC = pcMem.io.rdata.last.getPc(RegNext(rob.io.flushOut.bits.ftqOffset))

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := RegNext(rob.io.flushOut.valid)
  flushRedirect.bits := RegEnable(rob.io.flushOut.bits, rob.io.flushOut.valid)
  flushRedirect.bits.debugIsCtrl := false.B
  flushRedirect.bits.debugIsMemVio := false.B

  val flushRedirectReg = Wire(Valid(new Redirect))
  flushRedirectReg.valid := RegNext(flushRedirect.valid, init = false.B)
  flushRedirectReg.bits := RegEnable(flushRedirect.bits, flushRedirect.valid)

  val stage2Redirect = Mux(flushRedirect.valid, flushRedirect, redirectGen.io.stage2Redirect)
  // Redirect will be RegNext at ExuBlocks.
  val redirectForExu = RegNextWithEnable(stage2Redirect)

  val exuRedirect = io.exuRedirect.map(x => {
    val valid = x.valid && x.bits.redirectValid
    val killedByOlder = x.bits.uop.robIdx.needFlush(Seq(stage2Redirect, redirectForExu))
    val delayed = Wire(Valid(new ExuOutput))
    delayed.valid := RegNext(valid && !killedByOlder, init = false.B)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed
  })
  val loadReplay = Wire(Valid(new Redirect))
  loadReplay.valid := RegNext(io.memoryViolation.valid &&
    !io.memoryViolation.bits.robIdx.needFlush(Seq(stage2Redirect, redirectForExu)),
    init = false.B
  )
  val memVioBits = WireDefault(io.memoryViolation.bits)
  memVioBits.debugIsCtrl := false.B
  memVioBits.debugIsMemVio := true.B
  loadReplay.bits := RegEnable(memVioBits, io.memoryViolation.valid)
  pcMem.io.raddr(2) := redirectGen.io.redirectPcRead.ptr.value
  redirectGen.io.redirectPcRead.data := pcMem.io.rdata(2).getPc(RegNext(redirectGen.io.redirectPcRead.offset))
  pcMem.io.raddr(3) := redirectGen.io.memPredPcRead.ptr.value
  redirectGen.io.memPredPcRead.data := pcMem.io.rdata(3).getPc(RegNext(redirectGen.io.memPredPcRead.offset))
  redirectGen.io.hartId := io.hartId
  redirectGen.io.exuMispredict <> exuRedirect
  redirectGen.io.loadReplay <> loadReplay
  redirectGen.io.flush := flushRedirect.valid

  val frontendFlushValid = DelayN(flushRedirect.valid, 5)
  val frontendFlushBits = RegEnable(flushRedirect.bits, flushRedirect.valid)
  // When ROB commits an instruction with a flush, we notify the frontend of the flush without the commit.
  // Flushes to frontend may be delayed by some cycles and commit before flush causes errors.
  // Thus, we make all flush reasons to behave the same as exceptions for frontend.
  for (i <- 0 until CommitWidth) {
    // why flushOut: instructions with flushPipe are not commited to frontend
    // If we commit them to frontend, it will cause flush after commit, which is not acceptable by frontend.
    val is_commit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit && !rob.io.flushOut.valid
    io.frontend.toFtq.rob_commits(i).valid := RegNext(is_commit)
    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), is_commit)
  }
  io.frontend.toFtq.redirect.valid := frontendFlushValid || redirectGen.io.stage2Redirect.valid
  io.frontend.toFtq.redirect.bits := Mux(frontendFlushValid, frontendFlushBits, redirectGen.io.stage2Redirect.bits)
  // Be careful here:
  // T0: flushRedirect.valid, exception.valid
  // T1: csr.redirect.valid
  // T2: csr.exception.valid
  // T3: csr.trapTarget
  // T4: ctrlBlock.trapTarget
  // T5: io.frontend.toFtq.stage2Redirect.valid
  val pc_from_csr = io.robio.toCSR.isXRet || DelayN(rob.io.exception.valid, 4)
  val rob_flush_pc = RegEnable(Mux(flushRedirect.bits.flushItself(),
    flushPC, // replay inst
    flushPC + 4.U // flush pipe
  ), flushRedirect.valid)
  val flushTarget = Mux(pc_from_csr, io.robio.toCSR.trapTarget, rob_flush_pc)
  when (frontendFlushValid) {
    io.frontend.toFtq.redirect.bits.level := RedirectLevel.flush
    io.frontend.toFtq.redirect.bits.cfiUpdate.target := RegNext(flushTarget)
  }


  val pendingRedirect = RegInit(false.B)
  when (stage2Redirect.valid) {
    pendingRedirect := true.B
  }.elsewhen (RegNext(io.frontend.toFtq.redirect.valid)) {
    pendingRedirect := false.B
  }

  if (env.EnableTopDown) {
    val stage2Redirect_valid_when_pending = pendingRedirect && stage2Redirect.valid

    val stage2_redirect_cycles = RegInit(false.B)                                         // frontend_bound->fetch_lantency->stage2_redirect
    val MissPredPending = RegInit(false.B); val branch_resteers_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->branch_resteers
    val RobFlushPending = RegInit(false.B); val robFlush_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->robflush_bubble
    val LdReplayPending = RegInit(false.B); val ldReplay_bubble_cycles = RegInit(false.B) // frontend_bound->fetch_lantency->stage2_redirect->ldReplay_bubble
    
    when(redirectGen.io.isMisspreRedirect) { MissPredPending := true.B }
    when(flushRedirect.valid)              { RobFlushPending := true.B }
    when(redirectGen.io.loadReplay.valid)  { LdReplayPending := true.B }
    
    when (RegNext(io.frontend.toFtq.redirect.valid)) {
      when(pendingRedirect) {                             stage2_redirect_cycles := true.B }
      when(MissPredPending) { MissPredPending := false.B; branch_resteers_cycles := true.B }
      when(RobFlushPending) { RobFlushPending := false.B; robFlush_bubble_cycles := true.B }
      when(LdReplayPending) { LdReplayPending := false.B; ldReplay_bubble_cycles := true.B }
    }

    when(VecInit(decode.io.out.map(x => x.valid)).asUInt.orR){
      when(stage2_redirect_cycles) { stage2_redirect_cycles := false.B }
      when(branch_resteers_cycles) { branch_resteers_cycles := false.B }
      when(robFlush_bubble_cycles) { robFlush_bubble_cycles := false.B }
      when(ldReplay_bubble_cycles) { ldReplay_bubble_cycles := false.B }
    }

    XSPerfAccumulate("stage2_redirect_cycles", stage2_redirect_cycles)
    XSPerfAccumulate("branch_resteers_cycles", branch_resteers_cycles)
    XSPerfAccumulate("robFlush_bubble_cycles", robFlush_bubble_cycles)
    XSPerfAccumulate("ldReplay_bubble_cycles", ldReplay_bubble_cycles)
    XSPerfAccumulate("s2Redirect_pend_cycles", stage2Redirect_valid_when_pending)
  }

  decode.io.in <> io.frontend.cfVec
  decode.io.stallReason.in <> io.frontend.stallReason
  decode.io.csrCtrl := RegNext(io.csrCtrl)
  decode.io.intRat <> rat.io.intReadPorts
  decode.io.fpRat <> rat.io.fpReadPorts

  // memory dependency predict
  // when decode, send fold pc to mdp
  for (i <- 0 until DecodeWidth) {
    val mdp_foldpc = Mux(
      decode.io.out(i).fire,
      decode.io.in(i).bits.foldpc,
      rename.io.in(i).bits.cf.foldpc
    )
    ssit.io.raddr(i) := mdp_foldpc
    waittable.io.raddr(i) := mdp_foldpc
  }
  // currently, we only update mdp info when isReplay
  ssit.io.update <> RegNext(redirectGen.io.memPredUpdate)
  ssit.io.csrCtrl := RegNext(io.csrCtrl)
  waittable.io.update <> RegNext(redirectGen.io.memPredUpdate)
  waittable.io.csrCtrl := RegNext(io.csrCtrl)

  // LFST lookup and update
  val lfst = Module(new LFST)
  lfst.io.redirect <> RegNext(io.redirect)
  lfst.io.storeIssue <> RegNext(io.stIn)
  lfst.io.csrCtrl <> RegNext(io.csrCtrl)
  lfst.io.dispatch <> dispatch.io.lfst

  rat.io.redirect := stage2Redirect.valid
  rat.io.robCommits := rob.io.commits
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  rat.io.debug_int_rat <> io.debug_int_rat
  rat.io.debug_fp_rat <> io.debug_fp_rat

  // pipeline between decode and rename
  for (i <- 0 until RenameWidth) {
    // fusion decoder
    val decodeHasException = io.frontend.cfVec(i).bits.exceptionVec(instrPageFault) || io.frontend.cfVec(i).bits.exceptionVec(instrAccessFault)
    val disableFusion = decode.io.csrCtrl.singlestep || !decode.io.csrCtrl.fusion_enable
    fusionDecoder.io.in(i).valid := io.frontend.cfVec(i).valid && !(decodeHasException || disableFusion)
    fusionDecoder.io.in(i).bits := io.frontend.cfVec(i).bits.instr
    if (i > 0) {
      fusionDecoder.io.inReady(i - 1) := decode.io.out(i).ready
    }

    // Pipeline
    val renamePipe = PipelineNext(decode.io.out(i), rename.io.in(i).ready,
      stage2Redirect.valid || pendingRedirect)
    renamePipe.ready := rename.io.in(i).ready
    rename.io.in(i).valid := renamePipe.valid && !fusionDecoder.io.clear(i)
    rename.io.in(i).bits := renamePipe.bits
    rename.io.intReadPorts(i) := rat.io.intReadPorts(i).map(_.data)
    rename.io.fpReadPorts(i) := rat.io.fpReadPorts(i).map(_.data)
    rename.io.waittable(i) := RegEnable(waittable.io.rdata(i), decode.io.out(i).fire)

    if (i < RenameWidth - 1) {
      // fusion decoder sees the raw decode info
      fusionDecoder.io.dec(i) := renamePipe.bits.ctrl
      rename.io.fusionInfo(i) := fusionDecoder.io.info(i)

      // update the first RenameWidth - 1 instructions
      decode.io.fusion(i) := fusionDecoder.io.out(i).valid && rename.io.out(i).fire
      when (fusionDecoder.io.out(i).valid) {
        fusionDecoder.io.out(i).bits.update(rename.io.in(i).bits.ctrl)
        // TODO: remove this dirty code for ftq update
        val sameFtqPtr = rename.io.in(i).bits.cf.ftqPtr.value === rename.io.in(i + 1).bits.cf.ftqPtr.value
        val ftqOffset0 = rename.io.in(i).bits.cf.ftqOffset
        val ftqOffset1 = rename.io.in(i + 1).bits.cf.ftqOffset
        val ftqOffsetDiff = ftqOffset1 - ftqOffset0
        val cond1 = sameFtqPtr && ftqOffsetDiff === 1.U
        val cond2 = sameFtqPtr && ftqOffsetDiff === 2.U
        val cond3 = !sameFtqPtr && ftqOffset1 === 0.U
        val cond4 = !sameFtqPtr && ftqOffset1 === 1.U
        rename.io.in(i).bits.ctrl.commitType := Mux(cond1, 4.U, Mux(cond2, 5.U, Mux(cond3, 6.U, 7.U)))
        XSError(!cond1 && !cond2 && !cond3 && !cond4, p"new condition $sameFtqPtr $ftqOffset0 $ftqOffset1\n")
      }
    }
  }

  rename.io.redirect := stage2Redirect
  rename.io.robCommits <> rob.io.commits
  rename.io.ssit <> ssit.io.rdata
  rename.io.debug_int_rat <> rat.io.debug_int_rat
  rename.io.debug_fp_rat <> rat.io.debug_fp_rat
  rename.io.stallReason.in <> decode.io.stallReason.out

  // pipeline between rename and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(rename.io.out(i), dispatch.io.fromRename(i), dispatch.io.recv(i), stage2Redirect.valid)
  }

  dispatch.io.hartId := io.hartId
  dispatch.io.redirect := stage2Redirect
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.toIntDq <> intDq.io.enq
  dispatch.io.toFpDq <> fpDq.io.enq
  dispatch.io.toLsDq <> lsDq.io.enq
  dispatch.io.allocPregs <> io.allocPregs
  dispatch.io.stallReason <> rename.io.stallReason.out
  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)

  intDq.io.redirect <> redirectForExu
  fpDq.io.redirect <> redirectForExu
  lsDq.io.redirect <> redirectForExu

  val dpqOut = intDq.io.deq ++ lsDq.io.deq ++ fpDq.io.deq
  io.dispatch <> dpqOut

  for (dp2 <- outer.dispatch2.map(_.module.io)) {
    dp2.redirect := redirectForExu
    if (dp2.readFpState.isDefined) {
      dp2.readFpState.get := DontCare
    }
    if (dp2.readIntState.isDefined) {
      dp2.readIntState.get := DontCare
    }
    if (dp2.enqLsq.isDefined) {
      val lsqCtrl = Module(new LsqEnqCtrl)
      lsqCtrl.io.redirect <> redirectForExu
      lsqCtrl.io.enq <> dp2.enqLsq.get
      lsqCtrl.io.lcommit := rob.io.lsq.lcommit
      lsqCtrl.io.scommit := io.sqDeq
      lsqCtrl.io.lqCancelCnt := io.lqCancelCnt
      lsqCtrl.io.sqCancelCnt := io.sqCancelCnt
      io.enqLsq <> lsqCtrl.io.enqLsq
    }
  }
  for ((dp2In, i) <- outer.dispatch2.flatMap(_.module.io.in).zipWithIndex) {
    dp2In.valid := dpqOut(i).valid
    dp2In.bits := dpqOut(i).bits
    // override ready here to avoid cross-module loop path
    dpqOut(i).ready := dp2In.ready
  }
  for ((dp2Out, i) <- outer.dispatch2.flatMap(_.module.io.out).zipWithIndex) {
    dp2Out.ready := io.rsReady(i)
  }

  val pingpong = RegInit(false.B)
  pingpong := !pingpong
  pcMem.io.raddr(0) := intDq.io.deqNext(0).cf.ftqPtr.value
  pcMem.io.raddr(1) := intDq.io.deqNext(2).cf.ftqPtr.value
  val jumpPcRead0 = pcMem.io.rdata(0).getPc(RegNext(intDq.io.deqNext(0).cf.ftqOffset))
  val jumpPcRead1 = pcMem.io.rdata(1).getPc(RegNext(intDq.io.deqNext(2).cf.ftqOffset))
  io.jumpPc := Mux(pingpong && (exuParameters.AluCnt > 2).B, jumpPcRead1, jumpPcRead0)
  val jalrTargetReadPtr = Mux(pingpong && (exuParameters.AluCnt > 2).B,
    io.dispatch(2).bits.cf.ftqPtr,
    io.dispatch(0).bits.cf.ftqPtr)
  pcMem.io.raddr(4) := (jalrTargetReadPtr + 1.U).value
  val jalrTargetRead = pcMem.io.rdata(4).startAddr
  val read_from_newest_entry = RegNext(jalrTargetReadPtr) === RegNext(io.frontend.fromFtq.newest_entry_ptr)
  io.jalr_target := Mux(read_from_newest_entry, RegNext(io.frontend.fromFtq.newest_entry_target), jalrTargetRead)
  for(i <- 0 until exuParameters.LduCnt){
    // load s0 -> get rdata (s1) -> reg next (s2) -> output (s2)
    pcMem.io.raddr(i + PCMEMIDX_LD) := io.ld_pc_read(i).ptr.value
    io.ld_pc_read(i).data := pcMem.io.rdata(i + 5).getPc(RegNext(io.ld_pc_read(i).offset))
  }

  rob.io.hartId := io.hartId
  io.cpu_halt := DelayN(rob.io.cpu_halt, 5)
  rob.io.redirect := stage2Redirect
  outer.rob.generateWritebackIO(Some(outer), Some(this))

  io.redirect := stage2Redirect

  // rob to int block
  io.robio.toCSR <> rob.io.csr
  // When wfi is disabled, it will not block ROB commit.
  rob.io.csr.wfiEvent := io.robio.toCSR.wfiEvent
  rob.io.wfi_enable := decode.io.csrCtrl.wfi_enable
  io.robio.toCSR.perfinfo.retiredInstr <> RegNext(rob.io.csr.perfinfo.retiredInstr)
  io.robio.exception := rob.io.exception
  io.robio.exception.bits.uop.cf.pc := flushPC

  // rob to mem block
  io.robio.lsq <> rob.io.lsq

  rob.io.debug_ls := io.robio.debug_ls

  io.perfInfo.ctrlInfo.robFull := RegNext(rob.io.robFull)
  io.perfInfo.ctrlInfo.intdqFull := RegNext(intDq.io.dqFull)
  io.perfInfo.ctrlInfo.fpdqFull := RegNext(fpDq.io.dqFull)
  io.perfInfo.ctrlInfo.lsdqFull := RegNext(lsDq.io.dqFull)

  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := RegNext(io.csrCtrl.distribute_csr)
  val csrevents = pfevent.io.hpmevent.slice(8,16)

  val perfinfo = IO(new Bundle(){
    val perfEventsRs      = Input(Vec(NumRs, new PerfEvent))
    val perfEventsEu0     = Input(Vec(6, new PerfEvent))
    val perfEventsEu1     = Input(Vec(6, new PerfEvent))
  })

  val allPerfEvents = Seq(decode, rename, dispatch, intDq, fpDq, lsDq, rob).flatMap(_.getPerf)
  val hpmEvents = allPerfEvents ++ perfinfo.perfEventsEu0 ++ perfinfo.perfEventsEu1 ++ perfinfo.perfEventsRs
  val perfEvents = HPerfMonitor(csrevents, hpmEvents).getPerfEvents
  generatePerfEvent()
}
