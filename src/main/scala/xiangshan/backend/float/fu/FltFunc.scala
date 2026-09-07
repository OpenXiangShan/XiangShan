package xiangshan.backend.vector.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{MuxCase, MuxLookup, Valid}
import xiangshan._
import xiangshan.backend.datapath.DataConfig.V0Data
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.vector.VecIssueQueue.{BypassDelay, WakeUpBundle}
import xiangshan.backend.vector.WbFuBusyTable
import xiangshan.backend.vector.fu.VecFuConfig.VialuCfg
import xiangshan.backend.float.FltIssueQueue.FltWakeUpBundle

trait FltFuncAlias { this: Func =>
  protected def ex0ctrl = in.ex.head.bits.ctrl
  protected def ex0data = in.ex.head.bits.data
  protected def ex0src0 = ex0data.src(0)
  protected def ex0src1 = ex0data.src(1)
  protected def ex0src2 = ex0data.src(2)
  protected def ex0uopIdx = ex0ctrl.uopIdx
}


class FltFixLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) with FltFuncAlias {
}


class FltNonFixedLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) with FltFuncAlias {
  val outFuWakeUp = IO(Output(new FltWakeUpBundle(backendParams.fpPregParams)))
  val outFuBusy = IO(Output(Bool()))

  protected val nonFixedLatOutCtrl = RegInit(0.U.asTypeOf(new Func.OutCtrl(cfg)))
  protected val nonFixedLatOutDebug = ex(0).bits.debug.map(debug => Reg(chiselTypeOf(debug)))

  protected val nonFixedLatOutCtrlNext = Wire(new Func.OutCtrl(cfg))
  nonFixedLatOutCtrlNext.robIdx := ex0ctrl.robIdx
  nonFixedLatOutCtrlNext.pdest := ex0ctrl.pdest
  nonFixedLatOutCtrlNext.pdestV0.zip(ex0ctrl.pdestV0).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.pdestVl.zip(ex0ctrl.pdestVl).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.rfWen.zip(ex0ctrl.rfWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.fpWen.zip(ex0ctrl.fpWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.vecWen.zip(ex0ctrl.vecWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.v0Wen.zip(ex0ctrl.v0Wen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.vlWen.zip(ex0ctrl.vlWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.exceptionVec.zeroInit()
  nonFixedLatOutCtrlNext.flushPipe.zip(ex0ctrl.flushPipe).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.replay.foreach(_ := false.B)
  nonFixedLatOutCtrlNext.isRVC.foreach(_ := false.B)
  nonFixedLatOutCtrlNext.fflagsWen.zip(ex0ctrl.fflagsWen).foreach { case (sink, source) => sink := source }
  dontTouch(outFuWakeUp)
}
