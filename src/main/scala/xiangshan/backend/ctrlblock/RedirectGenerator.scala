package xiangshan.backend.ctrlblock

import org.chipsalliance.cde.config.Parameters
import chisel3.util._
import chisel3._
import utility.{HasCircularQueuePtrHelper, XORFold, GatedValidRegNext}
import xiangshan.frontend.{FtqRead, PreDecodeInfo}
import xiangshan.{MemPredUpdateReq, Redirect, XSBundle, XSModule}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class RedirectGeneratorIO(implicit p: Parameters) extends XSBundle {
    def numRedirect = backendParams.numRedirect

    val hartId = Input(UInt(8.W))
    val oldestExuRedirect = Flipped(ValidIO(new Redirect))
    val oldestExuOutPredecode = Input(new PreDecodeInfo) // guarded by exuRedirect.valid
    val loadReplay = Flipped(ValidIO(new Redirect))
    val robFlush = Flipped(ValidIO(new Redirect))
    val redirectPcRead = new FtqRead(UInt(VAddrBits.W))
    val stage2Redirect = ValidIO(new Redirect)

    val memPredUpdate = Output(new MemPredUpdateReq)
    val memPredPcRead = new FtqRead(UInt(VAddrBits.W)) // read req send form stage 2
    val stage2oldestOH = Output(UInt((NumRedirect + 1).W))
  }

  val io = IO(new RedirectGeneratorIO)

  val robFlush = io.robFlush
  val allRedirect: Vec[ValidIO[Redirect]] = VecInit(io.oldestExuRedirect, io.loadReplay)
  val oldestOneHot = Redirect.selectOldestRedirect(allRedirect)
  val flushAfter = Reg(ValidIO(new Redirect))
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(flushAfter) || robFlush.valid))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map { case (v, f) => v && !f }).asUInt.orR
  val oldestExuRedirect = io.oldestExuRedirect
  val oldestExuPredecode = io.oldestExuOutPredecode
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)
  io.redirectPcRead.vld := io.loadReplay.valid
  io.redirectPcRead.ptr := io.loadReplay.bits.ftqIdx
  io.redirectPcRead.offset := io.loadReplay.bits.ftqOffset

  val s1_exuTarget = RegEnable(oldestExuRedirect.bits.cfiUpdate.target, oldestExuRedirect.valid)
  val s1_pd = RegNext(oldestExuPredecode)
  val s1_redirect_bits_reg = RegEnable(oldestRedirect.bits, oldestValid)
  val s1_redirect_valid_reg = GatedValidRegNext(oldestValid)
  val s1_redirect_onehot = VecInit(oldestOneHot.map(x => GatedValidRegNext(x)))

  if (backendParams.debugEn){
    dontTouch(oldestValid)
    dontTouch(needFlushVec)
  }
  val flushAfterCounter = Reg(UInt(3.W))
  val robFlushOrExuFlushValid = oldestValid || robFlush.valid
  when(robFlushOrExuFlushValid) {
    flushAfter.valid := true.B
    flushAfter.bits := Mux(robFlush.valid, robFlush.bits, oldestRedirect.bits)
  }.elsewhen(!flushAfterCounter(0)) {
    flushAfter.valid := false.B
  }
  when(robFlushOrExuFlushValid) {
    flushAfterCounter := "b111".U
  }.elsewhen(flushAfterCounter(0)){
    flushAfterCounter := flushAfterCounter >> 1
  }
  // stage1 -> stage2
  io.stage2Redirect.valid := s1_redirect_valid_reg && !robFlush.valid
  io.stage2Redirect.bits := s1_redirect_bits_reg
  io.stage2oldestOH := s1_redirect_onehot.asUInt

  val s1_isReplay = s1_redirect_onehot.last
  val s1_isJump = s1_redirect_onehot.head
  val real_pc = io.redirectPcRead.data
  val snpc = real_pc + Mux(s1_pd.isRVC, 2.U, 4.U)
  val target = Mux(
    s1_isReplay,
    real_pc + Mux(s1_redirect_bits_reg.flushItself(), 0.U, Mux(s1_redirect_bits_reg.isRVC, 2.U, 4.U)),
    s1_exuTarget
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
  val s2_redirect_valid_reg = GatedValidRegNext(s1_redirect_valid_reg && !robFlush.valid, init = false.B)

  // get pc from ftq
  // valid only if redirect is caused by load violation
  // store_pc is used to update store set
  val store_pc = io.memPredPcRead(s1_redirect_valid_reg, s1_redirect_bits_reg.stFtqIdx, s1_redirect_bits_reg.stFtqOffset)

  // update load violation predictor if load violation redirect triggered
  io.memPredUpdate.valid := GatedValidRegNext(s1_isReplay && s1_redirect_valid_reg && s2_redirect_bits_reg.flushItself(), init = false.B)
  // update wait table
  io.memPredUpdate.waddr := RegEnable(XORFold(real_pc(VAddrBits - 1, 1), MemPredPCWidth), s1_isReplay && s1_redirect_valid_reg)
  io.memPredUpdate.wdata := true.B
  // update store set
  io.memPredUpdate.ldpc := RegEnable(XORFold(real_pc(VAddrBits - 1, 1), MemPredPCWidth), s1_isReplay && s1_redirect_valid_reg)
  // store pc is ready 1 cycle after s1_isReplay is judged
  io.memPredUpdate.stpc := XORFold(store_pc(VAddrBits - 1, 1), MemPredPCWidth)

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
