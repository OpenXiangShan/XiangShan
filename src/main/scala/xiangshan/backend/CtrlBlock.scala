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
import utils._
import xiangshan._
import xiangshan.backend.decode.{DecodeStage, ImmUnion}
import xiangshan.backend.dispatch.{Dispatch, DispatchQueue}
import xiangshan.backend.rename.{Rename, RenameTableWrapper}
import xiangshan.backend.rob.{Rob, RobCSRIO, RobLsqIO}
import xiangshan.backend.fu.{PFEvent}
import xiangshan.frontend.{FtqPtr, FtqRead}
import xiangshan.mem.LsqEnqIO
import difftest._

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val stage2Redirect = Valid(new Redirect)
  val stage3Redirect = ValidIO(new Redirect)
  val robFlush = Valid(new Bundle {
    val ftqIdx = Output(new FtqPtr)
    val ftqOffset = Output(UInt(log2Up(PredictWidth).W))
    val replayInst = Output(Bool()) // not used for now
  })
}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
  val io = IO(new Bundle() {
    val exuMispredict = Vec(numRedirect, Flipped(ValidIO(new ExuOutput)))
    val loadReplay = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val stage1PcRead = Vec(numRedirect+1, new FtqRead(UInt(VAddrBits.W)))
    val stage2Redirect = ValidIO(new Redirect)
    val stage3Redirect = ValidIO(new Redirect)
    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredPcRead = new FtqRead(UInt(VAddrBits.W)) // read req send form stage 2
  })
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
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(io.stage2Redirect, io.flush)))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map{ case (v, f) => v && !f }).asUInt.orR
  val oldestExuOutput = Mux1H(io.exuMispredict.indices.map(oldestOneHot), io.exuMispredict)
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)

  val s1_jumpTarget = RegEnable(jumpOut.bits.redirect.cfiUpdate.target, jumpOut.valid)
  val s1_imm12_reg = RegNext(oldestExuOutput.bits.uop.ctrl.imm(11, 0))
  val s1_pd = RegNext(oldestExuOutput.bits.uop.cf.pd)
  val s1_redirect_bits_reg = RegNext(oldestRedirect.bits)
  val s1_redirect_valid_reg = RegNext(oldestValid)
  val s1_redirect_onehot = RegNext(oldestOneHot)

  // stage1 -> stage2
  io.stage2Redirect.valid := s1_redirect_valid_reg && !io.flush
  io.stage2Redirect.bits := s1_redirect_bits_reg
  io.stage2Redirect.bits.cfiUpdate := DontCare

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

  val s2_target = RegEnable(target, enable = s1_redirect_valid_reg)
  val s2_pd = RegEnable(s1_pd, enable = s1_redirect_valid_reg)
  val s2_pc = RegEnable(real_pc, enable = s1_redirect_valid_reg)
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, enable = s1_redirect_valid_reg)
  val s2_redirect_valid_reg = RegNext(s1_redirect_valid_reg && !io.flush, init = false.B)

  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg
  val stage3CfiUpdate = io.stage3Redirect.bits.cfiUpdate
  stage3CfiUpdate.pc := s2_pc
  stage3CfiUpdate.pd := s2_pd
  stage3CfiUpdate.predTaken := s2_redirect_bits_reg.cfiUpdate.predTaken
  stage3CfiUpdate.target := s2_target
  stage3CfiUpdate.taken := s2_redirect_bits_reg.cfiUpdate.taken
  stage3CfiUpdate.isMisPred := s2_redirect_bits_reg.cfiUpdate.isMisPred

  // recover runahead checkpoint if redirect
  if (!env.FPGAPlatform) {
    val runahead_redirect = Module(new DifftestRunaheadRedirectEvent)
    runahead_redirect.io.clock := clock
    runahead_redirect.io.coreid := hardId.U
    runahead_redirect.io.valid := io.stage3Redirect.valid
    runahead_redirect.io.pc :=  s2_pc // for debug only
    runahead_redirect.io.target_pc := s2_target // for debug only
    runahead_redirect.io.checkpoint_id := io.stage3Redirect.bits.debug_runahead_checkpoint_id // make sure it is right
  }
}

class CtrlBlock(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val frontend = Flipped(new FrontendToCtrlIO)
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val dispatch = Vec(3*dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
    // from int block
    val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val stOut = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuOutput)))
    val memoryViolation = Flipped(ValidIO(new Redirect))
    val enqLsq = Flipped(new LsqEnqIO)
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
    val writeback = Vec(NRIntWritePorts + NRFpWritePorts, Flipped(ValidIO(new ExuOutput)))
    // redirect out
    val redirect = ValidIO(new Redirect)
    val flush = Output(Bool())
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val perfEventsEu0      = Input(new PerfEventsBundle(numPCntCtrl))
    val perfEventsEu1      = Input(new PerfEventsBundle(numPCntCtrl))
    val perfEvents      = Output(new PerfEventsBundle(numCSRPCntCtrl))
  })

  val decode = Module(new DecodeStage)
  val rat = Module(new RenameTableWrapper)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth, "int"))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth, "fp"))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth, "ls"))
  val redirectGen = Module(new RedirectGenerator)

  val robWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt
  val rob = Module(new Rob(robWbSize))

  val stage2Redirect = redirectGen.io.stage2Redirect
  val stage3Redirect = redirectGen.io.stage3Redirect
  val flush = rob.io.flushOut.valid
  val flushReg = RegNext(flush)

  val exuRedirect = io.exuRedirect.map(x => {
    val valid = x.valid && x.bits.redirectValid
    val killedByOlder = x.bits.uop.robIdx.needFlush(stage2Redirect, flushReg)
    val delayed = Wire(Valid(new ExuOutput))
    delayed.valid := RegNext(valid && !killedByOlder, init = false.B)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed
  })
  val loadReplay = Wire(Valid(new Redirect))
  loadReplay.valid := RegNext(io.memoryViolation.valid &&
    !io.memoryViolation.bits.robIdx.needFlush(stage2Redirect, flushReg),
    init = false.B
  )
  loadReplay.bits := RegEnable(io.memoryViolation.bits, io.memoryViolation.valid)
  io.frontend.fromFtq.getRedirectPcRead <> redirectGen.io.stage1PcRead
  io.frontend.fromFtq.getMemPredPcRead <> redirectGen.io.memPredPcRead
  redirectGen.io.exuMispredict <> exuRedirect
  redirectGen.io.loadReplay <> loadReplay
  redirectGen.io.flush := flushReg

  for(i <- 0 until CommitWidth){
    io.frontend.toFtq.rob_commits(i).valid := rob.io.commits.valid(i) && !rob.io.commits.isWalk
    io.frontend.toFtq.rob_commits(i).bits := rob.io.commits.info(i)
  }
  io.frontend.toFtq.stage2Redirect <> stage2Redirect
  io.frontend.toFtq.robFlush <> RegNext(rob.io.flushOut)

  val robPcRead = io.frontend.fromFtq.getRobFlushPcRead
  val flushPC = robPcRead(rob.io.flushOut.bits.ftqIdx, rob.io.flushOut.bits.ftqOffset)

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := flushReg
  flushRedirect.bits := DontCare
  flushRedirect.bits.ftqIdx := RegEnable(rob.io.flushOut.bits.ftqIdx, flush)
  flushRedirect.bits.interrupt := true.B
  flushRedirect.bits.cfiUpdate.target := Mux(io.robio.toCSR.isXRet || rob.io.exception.valid,
    io.robio.toCSR.trapTarget,
    Mux(RegEnable(rob.io.flushOut.bits.replayInst, flush),
      flushPC, // replay inst
      flushPC + 4.U // flush pipe
    )
  )
  when (flushRedirect.valid && RegEnable(rob.io.flushOut.bits.replayInst, flush)) {
    XSDebug("replay inst (%x) from rob\n", flushPC);
  }
  val flushRedirectReg = Wire(Valid(new Redirect))
  flushRedirectReg.valid := RegNext(flushRedirect.valid, init = false.B)
  flushRedirectReg.bits := RegEnable(flushRedirect.bits, enable = flushRedirect.valid)

  io.frontend.toFtq.stage3Redirect := Mux(flushRedirectReg.valid, flushRedirectReg, stage3Redirect)

  decode.io.in <> io.frontend.cfVec
  // currently, we only update wait table when isReplay
  decode.io.memPredUpdate(0) <> RegNext(redirectGen.io.memPredUpdate)
  decode.io.memPredUpdate(1) := DontCare
  decode.io.memPredUpdate(1).valid := false.B
  decode.io.csrCtrl := RegNext(io.csrCtrl)

  rat.io.flush := flushReg
  rat.io.robCommits := rob.io.commits
  for ((r, i) <- rat.io.intReadPorts.zipWithIndex) {
    val raddr = decode.io.out(i).bits.ctrl.lsrc.take(2) :+ decode.io.out(i).bits.ctrl.ldest
    r.map(_.addr).zip(raddr).foreach(x => x._1 := x._2)
    rename.io.intReadPorts(i) := r.map(_.data)
    r.foreach(_.hold := !rename.io.in(i).ready)
  }
  rat.io.intRenamePorts := rename.io.intRenamePorts
  for ((r, i) <- rat.io.fpReadPorts.zipWithIndex) {
    val raddr = decode.io.out(i).bits.ctrl.lsrc.take(3) :+ decode.io.out(i).bits.ctrl.ldest
    r.map(_.addr).zip(raddr).foreach(x => x._1 := x._2)
    rename.io.fpReadPorts(i) := r.map(_.data)
    r.foreach(_.hold := !rename.io.in(i).ready)
  }
  rat.io.fpRenamePorts := rename.io.fpRenamePorts
  rat.io.debug_int_rat <> io.debug_int_rat
  rat.io.debug_fp_rat <> io.debug_fp_rat

  // pipeline between decode and rename
  val redirectValid = stage2Redirect.valid || flushReg
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      flushReg || io.frontend.toFtq.stage3Redirect.valid)
  }

  rename.io.redirect <> stage2Redirect
  rename.io.flush := flushReg
  rename.io.robCommits <> rob.io.commits

  // pipeline between rename and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(rename.io.out(i), dispatch.io.fromRename(i), dispatch.io.recv(i), redirectValid)
  }
  dispatch.io.renameBypass := RegEnable(rename.io.renameBypass, rename.io.out(0).fire)
  dispatch.io.preDpInfo := RegEnable(rename.io.dispatchInfo, rename.io.out(0).fire)

  dispatch.io.flush <> flushReg
  dispatch.io.redirect <> stage2Redirect
  dispatch.io.enqRob <> rob.io.enq
  dispatch.io.enqLsq <> io.enqLsq
  dispatch.io.toIntDq <> intDq.io.enq
  dispatch.io.toFpDq <> fpDq.io.enq
  dispatch.io.toLsDq <> lsDq.io.enq
  dispatch.io.allocPregs <> io.allocPregs
  dispatch.io.csrCtrl <> io.csrCtrl
  dispatch.io.storeIssue <> io.stIn
  dispatch.io.singleStep := false.B

  intDq.io.redirect <> stage2Redirect
  intDq.io.flush <> flushReg
  fpDq.io.redirect <> stage2Redirect
  fpDq.io.flush <> flushReg
  lsDq.io.redirect <> stage2Redirect
  lsDq.io.flush <> flushReg

  io.dispatch <> intDq.io.deq ++ lsDq.io.deq ++ fpDq.io.deq

  val jumpInst = io.dispatch(0).bits
  val jumpPcRead = io.frontend.fromFtq.getJumpPcRead
  io.jumpPc := jumpPcRead(jumpInst.cf.ftqPtr, jumpInst.cf.ftqOffset)
  val jumpTargetRead = io.frontend.fromFtq.target_read
  io.jalr_target := jumpTargetRead(jumpInst.cf.ftqPtr, jumpInst.cf.ftqOffset)

  rob.io.redirect <> stage2Redirect
  val exeWbResults = VecInit(io.writeback ++ io.stOut)
  val timer = GTimer()
  for((rob_wb, wb) <- rob.io.exeWbResults.zip(exeWbResults)) {
    rob_wb.valid := RegNext(wb.valid && !wb.bits.uop.robIdx.needFlush(stage2Redirect, flushReg))
    rob_wb.bits := RegNext(wb.bits)
    rob_wb.bits.uop.debugInfo.writebackTime := timer
  }

  io.redirect <> stage2Redirect
  io.flush <> flushReg

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

  val hpmEvents = Wire(new PerfEventsBundle(numPCntCtrl))
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := io.csrCtrl.distribute_csr
  for(i <- 0 until numPCntCtrl ) {
    hpmEvents.PerfEvents(i).incr_step := DontCare
  }

  hpmEvents.PerfEvents( 0).incr_step   := decode.io.perfEvents.PerfEvents( 0).incr_step   
  hpmEvents.PerfEvents( 1).incr_step   := decode.io.perfEvents.PerfEvents( 1).incr_step   
  hpmEvents.PerfEvents( 2).incr_step   := decode.io.perfEvents.PerfEvents( 2).incr_step   
  hpmEvents.PerfEvents( 3).incr_step   := decode.io.perfEvents.PerfEvents( 3).incr_step   
  hpmEvents.PerfEvents( 4).incr_step   := decode.io.perfEvents.PerfEvents( 4).incr_step   
  hpmEvents.PerfEvents( 5).incr_step   := decode.io.perfEvents.PerfEvents( 5).incr_step   
  hpmEvents.PerfEvents( 6).incr_step   := rename.io.perfEvents.PerfEvents( 6).incr_step   
  hpmEvents.PerfEvents( 7).incr_step   := rename.io.perfEvents.PerfEvents( 7).incr_step   
  hpmEvents.PerfEvents( 8).incr_step   := rename.io.perfEvents.PerfEvents( 8).incr_step   
  hpmEvents.PerfEvents( 9).incr_step   := rename.io.perfEvents.PerfEvents( 9).incr_step   
  hpmEvents.PerfEvents(10).incr_step   := rename.io.perfEvents.PerfEvents(10).incr_step   
  hpmEvents.PerfEvents(11).incr_step   := rename.io.perfEvents.PerfEvents(11).incr_step   
  hpmEvents.PerfEvents(12).incr_step   := rename.io.perfEvents.PerfEvents(12).incr_step   
  hpmEvents.PerfEvents(13).incr_step   := rename.io.perfEvents.PerfEvents(13).incr_step   
  hpmEvents.PerfEvents(14).incr_step   := rename.io.perfEvents.PerfEvents(14).incr_step   
  hpmEvents.PerfEvents(15).incr_step   := rename.io.perfEvents.PerfEvents(15).incr_step   
  hpmEvents.PerfEvents(16).incr_step   := rename.io.perfEvents.PerfEvents(16).incr_step   
  hpmEvents.PerfEvents(17).incr_step   := rename.io.perfEvents.PerfEvents(17).incr_step   
  hpmEvents.PerfEvents(18).incr_step   := rename.io.perfEvents.PerfEvents(18).incr_step   
  hpmEvents.PerfEvents(19).incr_step   := rename.io.perfEvents.PerfEvents(19).incr_step   
  hpmEvents.PerfEvents(20).incr_step   := dispatch.io.perfEvents.PerfEvents(20).incr_step   
  hpmEvents.PerfEvents(21).incr_step   := dispatch.io.perfEvents.PerfEvents(21).incr_step   
  hpmEvents.PerfEvents(22).incr_step   := dispatch.io.perfEvents.PerfEvents(22).incr_step   
  hpmEvents.PerfEvents(23).incr_step   := dispatch.io.perfEvents.PerfEvents(23).incr_step   
  hpmEvents.PerfEvents(24).incr_step   := dispatch.io.perfEvents.PerfEvents(24).incr_step   
  hpmEvents.PerfEvents(25).incr_step   := dispatch.io.perfEvents.PerfEvents(25).incr_step   
  hpmEvents.PerfEvents(26).incr_step   := dispatch.io.perfEvents.PerfEvents(26).incr_step   
  hpmEvents.PerfEvents(27).incr_step   := dispatch.io.perfEvents.PerfEvents(27).incr_step   
  hpmEvents.PerfEvents(28).incr_step   := dispatch.io.perfEvents.PerfEvents(28).incr_step   
  hpmEvents.PerfEvents(29).incr_step   := intDq.io.perfEvents.PerfEvents(0).incr_step   
  hpmEvents.PerfEvents(30).incr_step   := intDq.io.perfEvents.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(31).incr_step   := intDq.io.perfEvents.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(32).incr_step   := intDq.io.perfEvents.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(33).incr_step   := intDq.io.perfEvents.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(34).incr_step   := intDq.io.perfEvents.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(35).incr_step   := intDq.io.perfEvents.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(36).incr_step   := intDq.io.perfEvents.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(37).incr_step   := intDq.io.perfEvents.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(38).incr_step   := fpDq.io.perfEvents.PerfEvents(0).incr_step   
  hpmEvents.PerfEvents(39).incr_step   := fpDq.io.perfEvents.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(40).incr_step   := fpDq.io.perfEvents.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(41).incr_step   := fpDq.io.perfEvents.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(42).incr_step   := fpDq.io.perfEvents.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(43).incr_step   := fpDq.io.perfEvents.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(44).incr_step   := fpDq.io.perfEvents.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(45).incr_step   := fpDq.io.perfEvents.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(46).incr_step   := fpDq.io.perfEvents.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(38).incr_step   := lsDq.io.perfEvents.PerfEvents(0).incr_step   
  hpmEvents.PerfEvents(39).incr_step   := lsDq.io.perfEvents.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(40).incr_step   := lsDq.io.perfEvents.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(41).incr_step   := lsDq.io.perfEvents.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(42).incr_step   := lsDq.io.perfEvents.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(43).incr_step   := lsDq.io.perfEvents.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(44).incr_step   := lsDq.io.perfEvents.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(45).incr_step   := lsDq.io.perfEvents.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(46).incr_step   := lsDq.io.perfEvents.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(47).incr_step   := rob.io.perfEvents.PerfEvents(39).incr_step   
  hpmEvents.PerfEvents(48).incr_step   := rob.io.perfEvents.PerfEvents(40).incr_step   
  hpmEvents.PerfEvents(49).incr_step   := rob.io.perfEvents.PerfEvents(41).incr_step   
  hpmEvents.PerfEvents(50).incr_step   := rob.io.perfEvents.PerfEvents(42).incr_step   
  hpmEvents.PerfEvents(51).incr_step   := rob.io.perfEvents.PerfEvents(43).incr_step   
  hpmEvents.PerfEvents(52).incr_step   := rob.io.perfEvents.PerfEvents(44).incr_step   
  hpmEvents.PerfEvents(53).incr_step   := rob.io.perfEvents.PerfEvents(45).incr_step   
  hpmEvents.PerfEvents(54).incr_step   := rob.io.perfEvents.PerfEvents(46).incr_step   
  hpmEvents.PerfEvents(55).incr_step   := rob.io.perfEvents.PerfEvents(47).incr_step   
  hpmEvents.PerfEvents(56).incr_step   := rob.io.perfEvents.PerfEvents(48).incr_step   
  hpmEvents.PerfEvents(57).incr_step   := rob.io.perfEvents.PerfEvents(49).incr_step   
  hpmEvents.PerfEvents(58).incr_step   := rob.io.perfEvents.PerfEvents(50).incr_step   
  hpmEvents.PerfEvents(59).incr_step   := rob.io.perfEvents.PerfEvents(51).incr_step   
  hpmEvents.PerfEvents(60).incr_step   := rob.io.perfEvents.PerfEvents(52).incr_step   
  hpmEvents.PerfEvents(61).incr_step   := rob.io.perfEvents.PerfEvents(53).incr_step   
  hpmEvents.PerfEvents(62).incr_step   := rob.io.perfEvents.PerfEvents(54).incr_step   
  hpmEvents.PerfEvents(63).incr_step   := rob.io.perfEvents.PerfEvents(55).incr_step   
  hpmEvents.PerfEvents(64).incr_step   := io.perfEventsEu0.PerfEvents(0).incr_step   
  hpmEvents.PerfEvents(65).incr_step   := io.perfEventsEu0.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(66).incr_step   := io.perfEventsEu0.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(67).incr_step   := io.perfEventsEu0.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(68).incr_step   := io.perfEventsEu0.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(69).incr_step   := io.perfEventsEu0.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(70).incr_step   := io.perfEventsEu0.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(71).incr_step   := io.perfEventsEu0.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(72).incr_step   := io.perfEventsEu0.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(73).incr_step   := io.perfEventsEu0.PerfEvents(9).incr_step   
  hpmEvents.PerfEvents(74).incr_step   := io.perfEventsEu1.PerfEvents(0).incr_step   
  hpmEvents.PerfEvents(75).incr_step   := io.perfEventsEu1.PerfEvents(1).incr_step   
  hpmEvents.PerfEvents(76).incr_step   := io.perfEventsEu1.PerfEvents(2).incr_step   
  hpmEvents.PerfEvents(77).incr_step   := io.perfEventsEu1.PerfEvents(3).incr_step   
  hpmEvents.PerfEvents(78).incr_step   := io.perfEventsEu1.PerfEvents(4).incr_step   
  hpmEvents.PerfEvents(79).incr_step   := io.perfEventsEu1.PerfEvents(5).incr_step   
  hpmEvents.PerfEvents(80).incr_step   := io.perfEventsEu1.PerfEvents(6).incr_step   
  hpmEvents.PerfEvents(81).incr_step   := io.perfEventsEu1.PerfEvents(7).incr_step   
  hpmEvents.PerfEvents(82).incr_step   := io.perfEventsEu1.PerfEvents(8).incr_step   
  hpmEvents.PerfEvents(83).incr_step   := io.perfEventsEu1.PerfEvents(9).incr_step   

  val hpm_ctrl = Module(new HPerfmonitor(numPCntCtrl,numCSRPCntCtrl))
  hpm_ctrl.io.Events_sets := hpmEvents
  hpm_ctrl.io.HPMEvent(0) := pfevent.io.HPMEvent(8)
  hpm_ctrl.io.HPMEvent(1) := pfevent.io.HPMEvent(9)
  hpm_ctrl.io.HPMEvent(2) := pfevent.io.HPMEvent(10)
  hpm_ctrl.io.HPMEvent(3) := pfevent.io.HPMEvent(11)
  hpm_ctrl.io.HPMEvent(4) := pfevent.io.HPMEvent(12)
  hpm_ctrl.io.HPMEvent(5) := pfevent.io.HPMEvent(13)
  hpm_ctrl.io.HPMEvent(6) := pfevent.io.HPMEvent(14)
  hpm_ctrl.io.HPMEvent(7) := pfevent.io.HPMEvent(15)
  for(i <- 0 until numCSRPCntCtrl ) {                
   // io.perfEvents.PerfEvents(i).incr_step := DontCare
    io.perfEvents.PerfEvents(i) := hpm_ctrl.io.Events_selected.PerfEvents(i)
  }
}
