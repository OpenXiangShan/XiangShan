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
import difftest._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.backend.decode.{DecodeStage, ImmUnion}
import xiangshan.backend.dispatch.{Dispatch, DispatchQueue}
import xiangshan.backend.fu.PFEvent
import xiangshan.backend.rename.{Rename, RenameTableWrapper}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobLsqIO}
import xiangshan.frontend.FtqRead
import xiangshan.mem.mdp.{LFST, SSIT, WaitTable}

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  def numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val redirect = Valid(new Redirect)
  val for_redirect_gen = new Bundle {
    val rawRedirect = Valid(new Redirect)
    val s1_redirect_onehot = Output(Vec(numRedirect+1, Bool()))
    val s1_oldest_redirect = ValidIO(new Redirect)
    val s1_oldest_exu_output = ValidIO(new ExuOutput)
    val s1_jumpTarget = Output(UInt(VAddrBits.W))
    val flushRedirect = Valid(new Redirect)
    val frontendFlushTarget = Output(UInt(VAddrBits.W))
  }
}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class RedirectGeneratorIO(implicit p: Parameters) extends XSBundle {
    def numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
    val hartId = Input(UInt(8.W))
    val exuMispredict = Vec(numRedirect, Flipped(ValidIO(new ExuOutput)))
    val loadReplay = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val stage1PcRead = Vec(numRedirect+1, new FtqRead(UInt(VAddrBits.W)))
    val stage2Redirect = ValidIO(new Redirect)
    val stage3Redirect = ValidIO(new Redirect)
    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredPcRead = new FtqRead(UInt(VAddrBits.W)) // read req send form stage 2
    val for_frontend_redirect_gen = new Bundle {
      val s1_jumpTarget = Output(UInt(VAddrBits.W))
      val s1_redirect_onehot = Output(Vec(numRedirect+1, Bool()))
      val s1_oldest_redirect = ValidIO(new Redirect)
      val s1_oldest_exu_output = ValidIO(new ExuOutput)
      val s1_real_pc = Input(UInt(VAddrBits.W))
    }
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
  private class Wrapper(val n: Int) extends Bundle {
    val redirect = new Redirect
    val valid = Bool()
    val idx = UInt(log2Up(n).W)
  }
  def selectOldestRedirect(xs: Seq[Valid[Redirect]]): Vec[Bool] = {
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.robIdx, xs(i).bits.robIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }

  val redirects = io.exuMispredict.map(_.bits.redirect) :+ io.loadReplay.bits
  val stage1FtqReadPcs =
    (io.stage1PcRead zip redirects).map{ case (r, redirect) =>
      r(redirect.ftqIdx, redirect.ftqOffset)
    }

  def getRedirect(exuOut: Valid[ExuOutput]): ValidIO[Redirect] = {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := exuOut.valid && exuOut.bits.redirect.cfiUpdate.isMisPred
    redirect.bits := exuOut.bits.redirect
    redirect
  }

  val jumpOut = io.exuMispredict.head
  val allRedirect = VecInit(io.exuMispredict.map(x => getRedirect(x)) :+ io.loadReplay)
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(io.stage2Redirect) || io.flush))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map{ case (v, f) => v && !f }).asUInt.orR
  val oldestExuOutput = Mux1H(io.exuMispredict.indices.map(oldestOneHot), io.exuMispredict)
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)

  val s1_jumpTarget = RegEnable(jumpOut.bits.redirect.cfiUpdate.target, jumpOut.valid)
  val s1_imm12_reg = RegNext(oldestExuOutput.bits.uop.ctrl.imm(11, 0))
  val s1_pd = RegNext(oldestExuOutput.bits.uop.cf.pd)
  val s1_redirect_bits_reg = RegNext(oldestRedirect.bits)
  val s1_redirect_valid_reg = RegNext(oldestValid)
  val s1_redirect_onehot = RegNext(oldestOneHot)
  io.for_frontend_redirect_gen.s1_jumpTarget := s1_jumpTarget
  io.for_frontend_redirect_gen.s1_redirect_onehot := s1_redirect_onehot
  io.for_frontend_redirect_gen.s1_oldest_redirect.valid := s1_redirect_valid_reg
  io.for_frontend_redirect_gen.s1_oldest_redirect.bits := s1_redirect_bits_reg
  io.for_frontend_redirect_gen.s1_oldest_exu_output := RegNext(oldestExuOutput)

  // stage1 -> stage2
  io.stage2Redirect.valid := s1_redirect_valid_reg && !io.flush
  io.stage2Redirect.bits := s1_redirect_bits_reg

  val s1_isReplay = s1_redirect_onehot.last
  val s1_isJump = s1_redirect_onehot.head
  val real_pc = Mux1H(s1_redirect_onehot, stage1FtqReadPcs)
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

  val s2_target = RegEnable(target, enable = s1_redirect_valid_reg)
  val s2_pc = RegEnable(real_pc, enable = s1_redirect_valid_reg)
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, enable = s1_redirect_valid_reg)
  val s2_redirect_valid_reg = RegNext(s1_redirect_valid_reg && !io.flush, init = false.B)

  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg

  // get pc from ftq
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  val store_pc = io.memPredPcRead(s1_redirect_bits_reg.stFtqIdx, s1_redirect_bits_reg.stFtqOffset)

  val s1_real_pc_from_frontend = io.for_frontend_redirect_gen.s1_real_pc
  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(s1_isReplay && s1_redirect_valid_reg, init = false.B)
  // update wait table
  io.memPredUpdate.waddr := RegNext(XORFold(s1_real_pc_from_frontend(VAddrBits-1, 1), MemPredPCWidth))
  io.memPredUpdate.wdata := true.B
  // update store set
  io.memPredUpdate.ldpc := RegNext(XORFold(s1_real_pc_from_frontend(VAddrBits-1, 1), MemPredPCWidth))
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.stpc := XORFold(store_pc(VAddrBits-1, 1), MemPredPCWidth)

  XSError(io.memPredUpdate.valid && RegNext(s1_real_pc_from_frontend) =/= RegNext(real_pc), "s1_real_pc error")

  // recover runahead checkpoint if redirect
  if (!env.FPGAPlatform) {
    val runahead_redirect = Module(new DifftestRunaheadRedirectEvent)
    runahead_redirect.io.clock := clock
    runahead_redirect.io.coreid := io.hartId
    runahead_redirect.io.valid := io.stage3Redirect.valid
    runahead_redirect.io.pc :=  s2_pc // for debug only
    runahead_redirect.io.target_pc := s2_target // for debug only
    runahead_redirect.io.checkpoint_id := io.stage3Redirect.bits.debug_runahead_checkpoint_id // make sure it is right
  }
}

class CtrlBlock(implicit p: Parameters) extends LazyModule
  with HasWritebackSink with HasWritebackSource {
  val rob = LazyModule(new Rob)

  override def addWritebackSink(source: Seq[HasWritebackSource], index: Option[Seq[Int]]): HasWritebackSink = {
    rob.addWritebackSink(Seq(this), Some(Seq(writebackSinks.length)))
    super.addWritebackSink(source, index)
  }

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
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val dispatch = Vec(3*dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
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
        wb_next.valid := RegNext(wb.valid && !wb.bits.uop.robIdx.needFlush(stage2Redirect))
        wb_next.bits := RegNext(wb.bits)
        wb_next.bits.uop.debugInfo.writebackTime := timer
      }
      exuOutput
    }))
  }

  val decode = Module(new DecodeStage)
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

  val robPcRead = io.frontend.fromFtq.getRobFlushPcRead
  val flushPC = robPcRead(rob.io.flushOut.bits.ftqIdx, rob.io.flushOut.bits.ftqOffset)

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := RegNext(rob.io.flushOut.valid)
  flushRedirect.bits := RegEnable(rob.io.flushOut.bits, rob.io.flushOut.valid)

  val flushRedirectReg = Wire(Valid(new Redirect))
  flushRedirectReg.valid := RegNext(flushRedirect.valid, init = false.B)
  flushRedirectReg.bits := RegEnable(flushRedirect.bits, enable = flushRedirect.valid)

  val stage2Redirect = Mux(flushRedirect.valid, flushRedirect, redirectGen.io.stage2Redirect)
  // val stage3Redirect = Mux(flushRedirectReg.valid, flushRedirectReg, redirectGen.io.stage3Redirect)

  val exuRedirect = io.exuRedirect.map(x => {
    val valid = x.valid && x.bits.redirectValid
    val killedByOlder = x.bits.uop.robIdx.needFlush(stage2Redirect)
    val delayed = Wire(Valid(new ExuOutput))
    delayed.valid := RegNext(valid && !killedByOlder, init = false.B)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed
  })
  val loadReplay = Wire(Valid(new Redirect))
  loadReplay.valid := RegNext(io.memoryViolation.valid &&
    !io.memoryViolation.bits.robIdx.needFlush(stage2Redirect),
    init = false.B
  )
  loadReplay.bits := RegEnable(io.memoryViolation.bits, io.memoryViolation.valid)
  io.frontend.fromFtq.getRedirectPcRead <> redirectGen.io.stage1PcRead
  io.frontend.fromFtq.getMemPredPcRead <> redirectGen.io.memPredPcRead
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
    val is_commit = rob.io.commits.valid(i) && !rob.io.commits.isWalk && !rob.io.flushOut.valid
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
  redirectGen.io.for_frontend_redirect_gen.s1_real_pc := io.frontend.fromFtq.redirect_s1_real_pc
  io.frontend.toFtq.for_redirect_gen.s1_oldest_redirect := redirectGen.io.for_frontend_redirect_gen.s1_oldest_redirect
  io.frontend.toFtq.for_redirect_gen.s1_oldest_exu_output := redirectGen.io.for_frontend_redirect_gen.s1_oldest_exu_output
  io.frontend.toFtq.for_redirect_gen.s1_redirect_onehot := redirectGen.io.for_frontend_redirect_gen.s1_redirect_onehot
  io.frontend.toFtq.for_redirect_gen.s1_jumpTarget := redirectGen.io.for_frontend_redirect_gen.s1_jumpTarget
  io.frontend.toFtq.for_redirect_gen.rawRedirect := redirectGen.io.stage2Redirect
  io.frontend.toFtq.for_redirect_gen.flushRedirect.valid := frontendFlushValid
  io.frontend.toFtq.for_redirect_gen.flushRedirect.bits := frontendFlushBits

  io.frontend.toFtq.for_redirect_gen.frontendFlushTarget := RegNext(flushTarget)


  val pendingRedirect = RegInit(false.B)
  when (stage2Redirect.valid) {
    pendingRedirect := true.B
  }.elsewhen (RegNext(io.frontend.toFtq.redirect.valid)) {
    pendingRedirect := false.B
  }

  decode.io.in <> io.frontend.cfVec
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

  rat.io.robCommits := rob.io.commits
  rat.io.intRenamePorts := rename.io.intRenamePorts
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  rat.io.debug_int_rat <> io.debug_int_rat
  rat.io.debug_fp_rat <> io.debug_fp_rat

  // pipeline between decode and rename
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      stage2Redirect.valid || pendingRedirect)
    rename.io.intReadPorts(i) := rat.io.intReadPorts(i).map(_.data)
    rename.io.fpReadPorts(i) := rat.io.fpReadPorts(i).map(_.data)
    if (i < RenameWidth - 1) {
      rename.io.fusionInfo(i) := RegEnable(decode.io.fusionInfo(i), decode.io.out(i).fire)
    }
    rename.io.waittable(i) := RegEnable(waittable.io.rdata(i), decode.io.out(i).fire)
  }

  rename.io.redirect <> stage2Redirect
  rename.io.robCommits <> rob.io.commits
  rename.io.ssit <> ssit.io.rdata

  // pipeline between rename and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(rename.io.out(i), dispatch.io.fromRename(i), dispatch.io.recv(i), stage2Redirect.valid)
  }

  dispatch.io.hartId := io.hartId
  dispatch.io.redirect <> stage2Redirect
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.toIntDq <> intDq.io.enq
  dispatch.io.toFpDq <> fpDq.io.enq
  dispatch.io.toLsDq <> lsDq.io.enq
  dispatch.io.allocPregs <> io.allocPregs
  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)

  intDq.io.redirect <> stage2Redirect
  fpDq.io.redirect <> stage2Redirect
  lsDq.io.redirect <> stage2Redirect

  io.dispatch <> intDq.io.deq ++ lsDq.io.deq ++ fpDq.io.deq

  val pingpong = RegInit(false.B)
  pingpong := !pingpong
  val jumpInst = Mux(pingpong && (exuParameters.AluCnt > 2).B, io.dispatch(2).bits, io.dispatch(0).bits)
  val jumpPcRead = io.frontend.fromFtq.getJumpPcRead
  io.jumpPc := jumpPcRead(jumpInst.cf.ftqPtr, jumpInst.cf.ftqOffset)
  val jumpTargetRead = io.frontend.fromFtq.target_read
  io.jalr_target := jumpTargetRead(jumpInst.cf.ftqPtr, jumpInst.cf.ftqOffset)

  rob.io.hartId := io.hartId
  io.cpu_halt := DelayN(rob.io.cpu_halt, 5)
  rob.io.redirect <> stage2Redirect
  outer.rob.generateWritebackIO(Some(outer), Some(this))

  io.redirect <> stage2Redirect

  // rob to int block
  io.robio.toCSR <> rob.io.csr
  io.robio.toCSR.perfinfo.retiredInstr <> RegNext(rob.io.csr.perfinfo.retiredInstr)
  io.robio.exception := rob.io.exception
  io.robio.exception.bits.uop.cf.pc := flushPC

  // rob to mem block
  io.robio.lsq <> rob.io.lsq

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

  val perfFromUnits = Seq(decode, rename, dispatch, intDq, fpDq, lsDq, rob).flatMap(_.getPerfEvents)
  val perfFromIO    = perfinfo.perfEventsEu0.map(x => ("perfEventsEu0", x.value)) ++
                        perfinfo.perfEventsEu1.map(x => ("perfEventsEu1", x.value)) ++
                        perfinfo.perfEventsRs.map(x => ("perfEventsRs", x.value))
  val perfBlock     = Seq()
  // let index = 0 be no event
  val allPerfEvents = Seq(("noEvent", 0.U)) ++ perfFromUnits ++ perfFromIO ++ perfBlock

  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("CtrlBlock perfEvents Set", name, inc, i)
    }
  }

  val allPerfInc = allPerfEvents.map(_._2.asTypeOf(new PerfEvent))
  val perfEvents = HPerfMonitor(csrevents, allPerfInc).getPerfEvents
  generatePerfEvent()
}