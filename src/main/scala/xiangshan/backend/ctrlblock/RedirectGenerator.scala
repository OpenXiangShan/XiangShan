package xiangshan.backend.ctrlblock

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.{HasCircularQueuePtrHelper, SignExt, XORFold}
import xiangshan.{ExuOutput, MemPredUpdateReq, Redirect, XSBundle, XSModule}
import xiangshan.backend.decode.ImmUnion
import xiangshan.frontend.FtqRead


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
  stage2CfiUpdate.target := target

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
}