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
import xiangshan.mem.mdp.{SSIT, LFST, WaitTable}
import xiangshan.frontend.{FtqPtr, FtqRead}
import xiangshan.mem.{LsqEnqIO}
import difftest._

class CtrlToFtqIO(implicit p: Parameters) extends XSBundle {
  val rob_commits = Vec(CommitWidth, Valid(new RobCommitInfo))
  val stage2Redirect = Valid(new Redirect)
  val stage3Redirect = ValidIO(new Redirect)
  val robFlush = ValidIO(new Redirect)
}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
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
    runahead_redirect.io.coreid := io.hartId
    runahead_redirect.io.valid := io.stage3Redirect.valid
    runahead_redirect.io.pc :=  s2_pc // for debug only
    runahead_redirect.io.target_pc := s2_target // for debug only
    runahead_redirect.io.checkpoint_id := io.stage3Redirect.bits.debug_runahead_checkpoint_id // make sure it is right
  }
}

class CtrlBlock(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))
    val frontend = Flipped(new FrontendToCtrlIO)
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    val dispatch = Vec(3*dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp))
    // from int block
    val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val stOut = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuOutput)))
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
    val writeback = Vec(NRIntWritePorts + NRFpWritePorts - exuParameters.LduCnt, Flipped(ValidIO(new ExuOutput)))
    // redirect out
    val redirect = ValidIO(new Redirect)
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  val decode = Module(new DecodeStage)
  val rat = Module(new RenameTableWrapper)
  val ssit = Module(new SSIT)
  val waittable = Module(new WaitTable)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth, "int"))
  val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth, "fp"))
  val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth, "ls"))
  val redirectGen = Module(new RedirectGenerator)

  val robWbSize = io.writeback.length + io.stOut.length
  val rob = Module(new Rob(robWbSize))

  val robPcRead = io.frontend.fromFtq.getRobFlushPcRead
  val flushPC = robPcRead(rob.io.flushOut.bits.ftqIdx, rob.io.flushOut.bits.ftqOffset)

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := RegNext(rob.io.flushOut.valid)
  flushRedirect.bits := RegEnable(rob.io.flushOut.bits, rob.io.flushOut.valid)
  flushRedirect.bits.cfiUpdate.target := Mux(io.robio.toCSR.isXRet || rob.io.exception.valid,
    io.robio.toCSR.trapTarget,
    Mux(flushRedirect.bits.flushItself(),
      flushPC, // replay inst
      flushPC + 4.U // flush pipe
    )
  )

  val flushRedirectReg = Wire(Valid(new Redirect))
  flushRedirectReg.valid := RegNext(flushRedirect.valid, init = false.B)
  flushRedirectReg.bits := RegEnable(flushRedirect.bits, enable = flushRedirect.valid)

  val stage2Redirect = Mux(flushRedirect.valid, flushRedirect, redirectGen.io.stage2Redirect)
  val stage3Redirect = Mux(flushRedirectReg.valid, flushRedirectReg, redirectGen.io.stage3Redirect)

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
  redirectGen.io.flush := RegNext(rob.io.flushOut.valid)

  for(i <- 0 until CommitWidth){
    io.frontend.toFtq.rob_commits(i).valid := rob.io.commits.valid(i) && !rob.io.commits.isWalk
    io.frontend.toFtq.rob_commits(i).bits := rob.io.commits.info(i)
  }
  io.frontend.toFtq.stage2Redirect <> stage2Redirect
  io.frontend.toFtq.robFlush <> RegNext(rob.io.flushOut)
  io.frontend.toFtq.stage3Redirect := stage3Redirect

  decode.io.in <> io.frontend.cfVec
  decode.io.csrCtrl := io.csrCtrl

  // memory dependency predict
  // when decode, send fold pc to mdp
  for (i <- 0 until DecodeWidth) {
    val mdp_foldpc = Mux(
      decode.io.out(i).fire(),
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
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      stage2Redirect.valid || stage3Redirect.valid)
  }

  rename.io.redirect <> stage2Redirect
  rename.io.robCommits <> rob.io.commits
  rename.io.ssit <> ssit.io.rdata
  rename.io.waittable <> RegNext(waittable.io.rdata)

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
  dispatch.io.singleStep := false.B

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
  rob.io.redirect <> stage2Redirect
  val exeWbResults = VecInit(io.writeback ++ io.stOut)
  val timer = GTimer()
  for((rob_wb, wb) <- rob.io.exeWbResults.zip(exeWbResults)) {
    rob_wb.valid := RegNext(wb.valid && !wb.bits.uop.robIdx.needFlush(stage2Redirect))
    rob_wb.bits := RegNext(wb.bits)
    rob_wb.bits.uop.debugInfo.writebackTime := timer
  }

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
  val csrevents = pfevent.io.hpmevent.slice(8,16)
  val perfinfo = IO(new Bundle(){
    val perfEvents        = Output(new PerfEventsBundle(csrevents.length))
    val perfEventsRs      = Input(new PerfEventsBundle(NumRs))
    val perfEventsEu0     = Input(new PerfEventsBundle(10))
    val perfEventsEu1     = Input(new PerfEventsBundle(10))
  })

  if(print_perfcounter){
    val decode_perf     = decode.perfEvents.map(_._1).zip(decode.perfinfo.perfEvents.perf_events)
    val rename_perf     = rename.perfEvents.map(_._1).zip(rename.perfinfo.perfEvents.perf_events)
    val dispat_perf     = dispatch.perfEvents.map(_._1).zip(dispatch.perfinfo.perfEvents.perf_events)
    val intdq_perf      = intDq.perfEvents.map(_._1).zip(intDq.perfinfo.perfEvents.perf_events)
    val fpdq_perf       = fpDq.perfEvents.map(_._1).zip(fpDq.perfinfo.perfEvents.perf_events)
    val lsdq_perf       = lsDq.perfEvents.map(_._1).zip(lsDq.perfinfo.perfEvents.perf_events)
    val rob_perf        = rob.perfEvents.map(_._1).zip(rob.perfinfo.perfEvents.perf_events)
    val perfEvents =  decode_perf ++ rename_perf ++ dispat_perf ++ intdq_perf ++ fpdq_perf ++ lsdq_perf ++ rob_perf

    for (((perf_name,perf),i) <- perfEvents.zipWithIndex) {
      println(s"ctrl perf $i: $perf_name")
    }
  }

  val hpmEvents = decode.perfinfo.perfEvents.perf_events ++ rename.perfinfo.perfEvents.perf_events ++ 
                  dispatch.perfinfo.perfEvents.perf_events ++ 
                  intDq.perfinfo.perfEvents.perf_events ++ fpDq.perfinfo.perfEvents.perf_events ++
                  lsDq.perfinfo.perfEvents.perf_events ++ rob.perfinfo.perfEvents.perf_events ++
                  perfinfo.perfEventsEu0.perf_events ++ perfinfo.perfEventsEu1.perf_events ++ 
                  perfinfo.perfEventsRs.perf_events

  val perf_length = hpmEvents.length
  val hpm_ctrl = Module(new HPerfmonitor(perf_length,csrevents.length))
  hpm_ctrl.io.hpm_event := csrevents
  hpm_ctrl.io.events_sets.perf_events := hpmEvents
  perfinfo.perfEvents := RegNext(hpm_ctrl.io.events_selected)
  pfevent.io.distribute_csr := RegNext(io.csrCtrl.distribute_csr)
}
