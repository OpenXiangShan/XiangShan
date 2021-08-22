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
import xiangshan.backend.rename.{BusyTable, Rename}
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.exu._
import xiangshan.backend.ftq.{Ftq, FtqRead, HasFtqHelper}
import xiangshan.backend.roq.{Roq, RoqCSRIO, RoqLsqIO, RoqPtr}
import xiangshan.mem.LsqEnqIO

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper with HasFtqHelper {
  val numRedirect = exuParameters.JmpCnt + exuParameters.AluCnt
  val io = IO(new Bundle() {
    val exuMispredict = Vec(numRedirect, Flipped(ValidIO(new ExuOutput)))
    val loadReplay = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    val stage1FtqRead = Vec(numRedirect + 1, new FtqRead)
    val stage2FtqRead = new FtqRead
    val stage2Redirect = ValidIO(new Redirect)
    val stage3Redirect = ValidIO(new Redirect)
    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredFtqRead = new FtqRead // read req send form stage 2
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
    val compareVec = (0 until xs.length).map(i => (0 until i).map(j => isAfter(xs(j).bits.roqIdx, xs(i).bits.roqIdx)))
    val resultOnehot = VecInit((0 until xs.length).map(i => Cat((0 until xs.length).map(j =>
      (if (j < i) !xs(j).valid || compareVec(i)(j)
      else if (j == i) xs(i).valid
      else !xs(j).valid || !compareVec(j)(i))
    )).andR))
    resultOnehot
  }

  for((ptr, redirect) <- io.stage1FtqRead.map(_.ptr).zip(
    io.exuMispredict.map(_.bits.redirect) :+ io.loadReplay.bits
  )){ ptr := redirect.ftqIdx }

  def getRedirect(exuOut: Valid[ExuOutput]): ValidIO[Redirect] = {
    val redirect = Wire(Valid(new Redirect))
    redirect.valid := exuOut.valid && exuOut.bits.redirect.cfiUpdate.isMisPred
    redirect.bits := exuOut.bits.redirect
    redirect
  }

  val jumpOut = io.exuMispredict.head
  val allRedirect = VecInit(io.exuMispredict.map(x => getRedirect(x)) :+ io.loadReplay)
  val oldestOneHot = selectOldestRedirect(allRedirect)
  val needFlushVec = VecInit(allRedirect.map(_.bits.roqIdx.needFlush(io.stage2Redirect, io.flush)))
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
  // at stage2, we read ftq to get pc
  io.stage2FtqRead.ptr := s1_redirect_bits_reg.ftqIdx

  val s1_isReplay = s1_redirect_onehot.last
  val s1_isJump = s1_redirect_onehot.head
  val ftqRead = Mux1H(s1_redirect_onehot, io.stage1FtqRead).entry
  val cfiUpdate_pc = Cat(
    ftqRead.ftqPC.head(VAddrBits - s1_redirect_bits_reg.ftqOffset.getWidth - instOffsetBits),
    s1_redirect_bits_reg.ftqOffset,
    0.U(instOffsetBits.W)
  )
  val real_pc = GetPcByFtq(
    ftqRead.ftqPC, s1_redirect_bits_reg.ftqOffset,
    ftqRead.lastPacketPC.valid,
    ftqRead.lastPacketPC.bits
  )
  val brTarget = real_pc + SignExt(ImmUnion.B.toImm32(s1_imm12_reg), XLEN)
  val snpc = real_pc + Mux(s1_pd.isRVC, 2.U, 4.U)
  val target = Mux(s1_isReplay,
    real_pc, // repaly from itself
    Mux(s1_redirect_bits_reg.cfiUpdate.taken,
      Mux(s1_isJump, s1_jumpTarget, brTarget),
      snpc
    )
  )

  // get pc from ftq
  io.memPredFtqRead.ptr := s1_redirect_bits_reg.stFtqIdx
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  val memPredFtqRead = io.memPredFtqRead.entry
  val store_pc = GetPcByFtq(memPredFtqRead.ftqPC, RegNext(s1_redirect_bits_reg).stFtqOffset,
    memPredFtqRead.lastPacketPC.valid,
    memPredFtqRead.lastPacketPC.bits
  )

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := RegNext(s1_isReplay && s1_redirect_valid_reg, init = false.B)
  // update wait table
  io.memPredUpdate.waddr := RegNext(XORFold(real_pc(VAddrBits-1, 1), MemPredPCWidth))
  io.memPredUpdate.wdata := true.B
  // update store set
  io.memPredUpdate.ldpc := RegNext(XORFold(real_pc(VAddrBits-1, 1), MemPredPCWidth))
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.stpc := XORFold(store_pc(VAddrBits-1, 1), MemPredPCWidth)


  val s2_br_mask = RegEnable(ftqRead.br_mask, enable = s1_redirect_valid_reg)
  val s2_sawNotTakenBranch = RegEnable(VecInit((0 until PredictWidth).map{ i =>
      if(i == 0) false.B else Cat(ftqRead.br_mask.take(i)).orR()
    })(s1_redirect_bits_reg.ftqOffset), enable = s1_redirect_valid_reg)
  val s2_hist = RegEnable(ftqRead.hist, enable = s1_redirect_valid_reg)
  val s2_target = RegEnable(target, enable = s1_redirect_valid_reg)
  val s2_pd = RegEnable(s1_pd, enable = s1_redirect_valid_reg)
  val s2_cfiUpdata_pc = RegEnable(cfiUpdate_pc, enable = s1_redirect_valid_reg)
  val s2_redirect_bits_reg = RegEnable(s1_redirect_bits_reg, enable = s1_redirect_valid_reg)
  val s2_redirect_valid_reg = RegNext(s1_redirect_valid_reg && !io.flush, init = false.B)
  val s2_ftqRead = io.stage2FtqRead.entry

  io.stage3Redirect.valid := s2_redirect_valid_reg
  io.stage3Redirect.bits := s2_redirect_bits_reg
  val stage3CfiUpdate = io.stage3Redirect.bits.cfiUpdate
  stage3CfiUpdate.pc := s2_cfiUpdata_pc
  stage3CfiUpdate.pd := s2_pd
  stage3CfiUpdate.rasSp := s2_ftqRead.rasSp
  stage3CfiUpdate.rasEntry := s2_ftqRead.rasTop
  stage3CfiUpdate.predHist := s2_ftqRead.predHist
  stage3CfiUpdate.specCnt := s2_ftqRead.specCnt
  stage3CfiUpdate.hist := s2_hist
  stage3CfiUpdate.predTaken := s2_redirect_bits_reg.cfiUpdate.predTaken
  stage3CfiUpdate.sawNotTakenBranch := s2_sawNotTakenBranch
  stage3CfiUpdate.target := s2_target
  stage3CfiUpdate.taken := s2_redirect_bits_reg.cfiUpdate.taken
  stage3CfiUpdate.isMisPred := s2_redirect_bits_reg.cfiUpdate.isMisPred
}

class CtrlBlock(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper with HasFtqHelper {
  val io = IO(new Bundle {
    val frontend = Flipped(new FrontendToBackendIO)
    val enqIQ = Vec(exuParameters.CriticalExuCnt, DecoupledIO(new MicroOp))
    // from int block
    val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val stOut = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuOutput)))
    val memoryViolation = Flipped(ValidIO(new Redirect))
    val enqLsq = Flipped(new LsqEnqIO)
    val jumpPc = Output(UInt(VAddrBits.W))
    val jalr_target = Output(UInt(VAddrBits.W))
    val roqio = new Bundle {
      // to int block
      val toCSR = new RoqCSRIO
      val exception = ValidIO(new ExceptionInfo)
      // to mem block
      val lsq = new RoqLsqIO
    }
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val perfInfo = Output(new Bundle{
      val ctrlInfo = new Bundle {
        val roqFull   = Input(Bool())
        val intdqFull = Input(Bool())
        val fpdqFull  = Input(Bool())
        val lsdqFull  = Input(Bool())
      }
      val bpuInfo = new Bundle {
        val bpRight = Output(UInt(XLEN.W))
        val bpWrong = Output(UInt(XLEN.W))
      }
    })
    val writeback = Vec(NRIntWritePorts + NRFpWritePorts, Flipped(ValidIO(new ExuOutput)))
    // redirect out
    val redirect = ValidIO(new Redirect)
    val flush = Output(Bool())
    val readIntRf = Vec(NRIntReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readFpRf = Vec(NRFpReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  val ftq = Module(new Ftq)

  val decode = Module(new DecodeStage)
  val rename = Module(new Rename)
  val dispatch = Module(new Dispatch)
  val intBusyTable = Module(new BusyTable(NRIntReadPorts, NRIntWritePorts))
  val fpBusyTable = Module(new BusyTable(NRFpReadPorts, NRFpWritePorts))
  val redirectGen = Module(new RedirectGenerator)

  val roqWbSize = NRIntWritePorts + NRFpWritePorts + exuParameters.StuCnt
  val roq = Module(new Roq(roqWbSize))

  val backendRedirect = redirectGen.io.stage2Redirect
  val frontendRedirect = redirectGen.io.stage3Redirect
  val flush = roq.io.flushOut.valid
  val flushReg = RegNext(flush)

  val exuRedirect = io.exuRedirect.map(x => {
    val valid = x.valid && x.bits.redirectValid
    val killedByOlder = x.bits.uop.roqIdx.needFlush(backendRedirect, flushReg)
    val delayed = Wire(Valid(new ExuOutput))
    delayed.valid := RegNext(valid && !killedByOlder, init = false.B)
    delayed.bits := RegEnable(x.bits, x.valid)
    delayed
  })
  val loadReplay = Wire(Valid(new Redirect))
  loadReplay.valid := RegNext(io.memoryViolation.valid &&
    !io.memoryViolation.bits.roqIdx.needFlush(backendRedirect, flushReg),
    init = false.B
  )
  loadReplay.bits := RegEnable(io.memoryViolation.bits, io.memoryViolation.valid)
  VecInit(ftq.io.ftqRead.tail.dropRight(2)) <> redirectGen.io.stage1FtqRead
  ftq.io.ftqRead.dropRight(1).last <> redirectGen.io.memPredFtqRead
  ftq.io.cfiRead <> redirectGen.io.stage2FtqRead
  redirectGen.io.exuMispredict <> exuRedirect
  redirectGen.io.loadReplay <> loadReplay
  redirectGen.io.flush := flushReg

  ftq.io.enq <> io.frontend.fetchInfo
  for(i <- 0 until CommitWidth){
    ftq.io.roq_commits(i).valid := roq.io.commits.valid(i) && !roq.io.commits.isWalk
    ftq.io.roq_commits(i).bits := roq.io.commits.info(i)
  }
  ftq.io.redirect <> backendRedirect
  ftq.io.flush := flushReg
  ftq.io.flushIdx := RegNext(roq.io.flushOut.bits.ftqIdx)
  ftq.io.flushOffset := RegNext(roq.io.flushOut.bits.ftqOffset)
  ftq.io.frontendRedirect <> frontendRedirect
  ftq.io.exuWriteback <> exuRedirect

  ftq.io.ftqRead.last.ptr := roq.io.flushOut.bits.ftqIdx
  val flushPC = GetPcByFtq(
    ftq.io.ftqRead.last.entry.ftqPC,
    RegEnable(roq.io.flushOut.bits.ftqOffset, roq.io.flushOut.valid),
    ftq.io.ftqRead.last.entry.lastPacketPC.valid,
    ftq.io.ftqRead.last.entry.lastPacketPC.bits
  )

  val flushRedirect = Wire(Valid(new Redirect))
  flushRedirect.valid := flushReg
  flushRedirect.bits := DontCare
  flushRedirect.bits.ftqIdx := RegEnable(roq.io.flushOut.bits.ftqIdx, flush)
  flushRedirect.bits.interrupt := true.B
  flushRedirect.bits.cfiUpdate.target := Mux(io.roqio.toCSR.isXRet || roq.io.exception.valid,
    io.roqio.toCSR.trapTarget,
    flushPC + 4.U // flush pipe
  )
  val flushRedirectReg = Wire(Valid(new Redirect))
  flushRedirectReg.valid := RegNext(flushRedirect.valid, init = false.B)
  flushRedirectReg.bits := RegEnable(flushRedirect.bits, enable = flushRedirect.valid)

  io.frontend.redirect_cfiUpdate := Mux(flushRedirectReg.valid, flushRedirectReg, frontendRedirect)
  io.frontend.commit_cfiUpdate := ftq.io.commit_ftqEntry
  io.frontend.ftqEnqPtr := ftq.io.enqPtr
  io.frontend.ftqLeftOne := ftq.io.leftOne

  decode.io.in <> io.frontend.cfVec
  // currently, we only update wait table when isReplay
  decode.io.memPredUpdate(0) <> RegNext(redirectGen.io.memPredUpdate)
  decode.io.memPredUpdate(1) := DontCare
  decode.io.memPredUpdate(1).valid := false.B
  // decode.io.memPredUpdate <> io.toLsBlock.memPredUpdate
  decode.io.csrCtrl := RegNext(io.csrCtrl)


  val jumpInst = dispatch.io.enqIQCtrl(0).bits
  val ftqOffsetReg = Reg(UInt(log2Up(PredictWidth).W))
  ftqOffsetReg := jumpInst.cf.ftqOffset
  ftq.io.ftqRead(0).ptr := jumpInst.cf.ftqPtr // jump
  io.jumpPc := GetPcByFtq(
    ftq.io.ftqRead(0).entry.ftqPC, ftqOffsetReg,
    ftq.io.ftqRead(0).entry.lastPacketPC.valid,
    ftq.io.ftqRead(0).entry.lastPacketPC.bits
  )
  io.jalr_target := ftq.io.ftqRead(0).entry.target

  // pipeline between decode and dispatch
  for (i <- 0 until RenameWidth) {
    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
      flushReg || io.frontend.redirect_cfiUpdate.valid)
  }

  rename.io.redirect <> backendRedirect
  rename.io.flush := flushReg
  rename.io.roqCommits <> roq.io.commits
  rename.io.out <> dispatch.io.fromRename
  rename.io.renameBypass <> dispatch.io.renameBypass
  rename.io.dispatchInfo <> dispatch.io.preDpInfo

  dispatch.io.redirect <> backendRedirect
  dispatch.io.flush := flushReg
  dispatch.io.enqRoq <> roq.io.enq
  dispatch.io.enqLsq <> io.enqLsq
  dispatch.io.singleStep := false.B
  dispatch.io.allocPregs.zipWithIndex.foreach { case (preg, i) =>
    intBusyTable.io.allocPregs(i).valid := preg.isInt
    fpBusyTable.io.allocPregs(i).valid := preg.isFp
    intBusyTable.io.allocPregs(i).bits := preg.preg
    fpBusyTable.io.allocPregs(i).bits := preg.preg
  }
  dispatch.io.enqIQCtrl := DontCare
  io.enqIQ <> dispatch.io.enqIQCtrl
  dispatch.io.csrCtrl <> io.csrCtrl
  dispatch.io.storeIssue <> io.stIn
  dispatch.io.readIntRf <> io.readIntRf
  dispatch.io.readFpRf <> io.readFpRf

  fpBusyTable.io.flush := flushReg
  intBusyTable.io.flush := flushReg
  for((wb, setPhyRegRdy) <- io.writeback.take(NRIntWritePorts).zip(intBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.rfWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  for((wb, setPhyRegRdy) <- io.writeback.drop(NRIntWritePorts).zip(fpBusyTable.io.wbPregs)){
    setPhyRegRdy.valid := wb.valid && wb.bits.uop.ctrl.fpWen
    setPhyRegRdy.bits := wb.bits.uop.pdest
  }
  intBusyTable.io.read <> dispatch.io.readIntState
  fpBusyTable.io.read <> dispatch.io.readFpState

  roq.io.redirect <> backendRedirect
  val exeWbResults = VecInit(io.writeback ++ io.stOut)
  for((roq_wb, wb) <- roq.io.exeWbResults.zip(exeWbResults)) {
    roq_wb.valid := RegNext(wb.valid && !wb.bits.uop.roqIdx.needFlush(backendRedirect, flushReg))
    roq_wb.bits := RegNext(wb.bits)
  }

  // TODO: is 'backendRedirect' necesscary?
  io.redirect <> backendRedirect
  io.flush <> flushReg
  io.debug_int_rat <> rename.io.debug_int_rat
  io.debug_fp_rat <> rename.io.debug_fp_rat

//  dispatch.io.readPortIndex.intIndex <> io.toIntBlock.readPortIndex
//  dispatch.io.readPortIndex.fpIndex <> io.toFpBlock.readPortIndex

  // roq to int block
  io.roqio.toCSR <> roq.io.csr
  io.roqio.toCSR.perfinfo.retiredInstr <> RegNext(roq.io.csr.perfinfo.retiredInstr)
  io.roqio.exception := roq.io.exception
  io.roqio.exception.bits.uop.cf.pc := flushPC
  // roq to mem block
  io.roqio.lsq <> roq.io.lsq

  io.perfInfo.ctrlInfo.roqFull := RegNext(roq.io.roqFull)
  io.perfInfo.ctrlInfo.intdqFull := RegNext(dispatch.io.ctrlInfo.intdqFull)
  io.perfInfo.ctrlInfo.fpdqFull := RegNext(dispatch.io.ctrlInfo.fpdqFull)
  io.perfInfo.ctrlInfo.lsdqFull := RegNext(dispatch.io.ctrlInfo.lsdqFull)
  io.perfInfo.bpuInfo <> RegNext(ftq.io.bpuInfo)
}
