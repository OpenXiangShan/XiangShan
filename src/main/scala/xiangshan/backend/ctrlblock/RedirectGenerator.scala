package xiangshan.backend.ctrlblock

import org.chipsalliance.cde.config.Parameters
import chisel3.util._
import chisel3._
import utility.{HasCircularQueuePtrHelper, XORFold, GatedValidRegNext}
import xiangshan.frontend.ftq.FtqRead
import xiangshan.{MemPredUpdateReq, Redirect, XSBundle, XSModule, AddrTransType}

class RedirectGenerator(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {

  class RedirectGeneratorIO(implicit p: Parameters) extends XSBundle {
    def numRedirect = backendParams.numRedirect

    val hartId = Input(UInt(8.W))
    val oldestExuRedirect = Flipped(ValidIO(new Redirect))
    val loadReplay = Flipped(ValidIO(new Redirect))
    val robFlush = Flipped(ValidIO(new Redirect))
    val stage2Redirect = ValidIO(new Redirect)

    val stage2oldestOH = Output(UInt((1 + 1).W))
  }

  val io = IO(new RedirectGeneratorIO)

  val loadRedirect = io.loadReplay
  val robFlush = io.robFlush
  val oldestExuRedirect = Wire(chiselTypeOf(io.oldestExuRedirect))
  oldestExuRedirect := io.oldestExuRedirect
  val allRedirect: Vec[ValidIO[Redirect]] = VecInit(oldestExuRedirect, loadRedirect)
  val oldestOneHot = Redirect.selectOldestRedirect(allRedirect)
  val flushAfter = RegInit(0.U.asTypeOf(ValidIO(new Redirect)))
  val needFlushVec = VecInit(allRedirect.map(_.bits.robIdx.needFlush(flushAfter) || robFlush.valid))
  val oldestValid = VecInit(oldestOneHot.zip(needFlushVec).map { case (v, f) => v && !f }).asUInt.orR
  val oldestRedirect = Mux1H(oldestOneHot, allRedirect)
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


}
