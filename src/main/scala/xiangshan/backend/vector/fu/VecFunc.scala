package xiangshan.backend.vector.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.{MuxCase, MuxLookup, Valid}
import xiangshan._
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.backend.datapath.DataConfig.V0Data
import xiangshan.backend.decode.opcode.Opcode.VIAluOpcodes
import xiangshan.backend.vector.VecIssueQueue.{BypassDelay, WakeUpBundle}
import xiangshan.backend.vector.WbFuBusyTable
import xiangshan.backend.vector.fu.VecFuConfig.VialuCfg


trait VecFuncAlias { this: Func =>
  protected def ex0ctrl = in.ex.head.bits.ctrl
  protected def ex0data = in.ex.head.bits.data
  protected def ex0vs1 = ex0data.src(0)
  protected def ex0vs2 = ex0data.src(1)
  protected def ex0oldVd = ex0data.src(2)
  protected def ex0vl = ex0data.vl.get

  protected def vxrm = in.vxrm.get

  protected def ex0vma = ex0ctrl.vtype.get.vma
  protected def ex0vta = ex0ctrl.vtype.get.vta
  protected def ex0vm = ex0ctrl.vm.get
  
  protected def ex0uopIdx = ex0ctrl.uopIdx

  protected def vstart = 0.U

  protected def allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
  protected def allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt

  protected def vs1Ex: Seq[UInt] = in.ex.map(_.bits.data.src(0))
  protected def vs2Ex: Seq[UInt] = in.ex.map(_.bits.data.src(1))
  protected def vs3Ex: Seq[UInt] = in.ex.map(_.bits.data.src(2))
  protected def oldVdEx: Seq[UInt] = in.ex.map(_.bits.data.src(2))
}

class VecFixLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) with VecFuncAlias {
  protected val needClearMask: Bool = false.B
  protected val srcMask: UInt =
    MuxCase(ex0data.v0.get, Seq(
      needClearMask -> allMaskFalse,
      ex0vm -> allMaskTrue
    ))
}


class VecNonFixedLatFunc(cfg: VecFuConfig)(implicit p: Parameters) extends Func(cfg) with VecFuncAlias {
  val outFuLat = IO(Output(Valid(UInt(WbFuBusyTable.NonFixedLatencyWidth.W))))
  val outFuWakeUp = IO(Output(new WakeUpBundle(backendParams.vpPregParams)))

  outFuWakeUp.wen := false.B
  outFuWakeUp.pdest := 0.U
  outFuWakeUp.delay := BypassDelay.delay3

  protected val nonFixedLatOutCtrl = RegInit(0.U.asTypeOf(new Func.OutCtrl(cfg)))
  protected val nonFixedLatOutDebug = ex(0).bits.debug.map(debug => Reg(chiselTypeOf(debug)))

  private val nonFixedLatOutCtrlNext = Wire(new Func.OutCtrl(cfg))
  nonFixedLatOutCtrlNext.robIdx := ex0ctrl.robIdx
  nonFixedLatOutCtrlNext.pdest := ex0ctrl.pdest
  nonFixedLatOutCtrlNext.pdestV0.zip(ex0ctrl.pdestV0).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.pdestVl.zip(ex0ctrl.pdestVl).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.rfWen.zip(ex0ctrl.rfWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.fpWen.zip(ex0ctrl.fpWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.vecWen.zip(ex0ctrl.vecWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.v0Wen.zip(ex0ctrl.v0Wen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.vlWen.zip(ex0ctrl.vlWen).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.exceptionVec.foreach { sink => sink := 0.U.asTypeOf(sink) }
  nonFixedLatOutCtrlNext.flushPipe.zip(ex0ctrl.flushPipe).foreach { case (sink, source) => sink := source }
  nonFixedLatOutCtrlNext.replay.foreach(_ := false.B)
  nonFixedLatOutCtrlNext.isRVC.foreach(_ := false.B)
  nonFixedLatOutCtrlNext.fflagsWen.zip(ex0ctrl.fflagsWen).foreach { case (sink, source) => sink := source }

  out.ex(0).bits.ctrl := nonFixedLatOutCtrl
  out.ex(0).bits.debug.zip(nonFixedLatOutDebug).foreach { case (sink, source) =>
    sink := source
  }

  protected def latchNonFixedLatOutCtrl(fire: Bool): Unit = {
    when(fire) {
      nonFixedLatOutCtrl := nonFixedLatOutCtrlNext
      nonFixedLatOutDebug.zip(ex(0).bits.debug).foreach { case (sink, source) =>
        sink := source
      }
    }
  }

  protected def connectNonFixedLatWakeUp(latency: Valid[UInt], flush: Bool, resultValid: Bool): Unit = {
    val wakeUpCountdown = RegInit(0.U(WbFuBusyTable.NonFixedLatencyWidth.W))
    val wakeUpPending = RegInit(false.B)
    val wakeUpPdest = RegInit(0.U.asTypeOf(nonFixedLatOutCtrl.pdest))
    val wakeUpWen = RegInit(false.B)

    val currentWen = nonFixedLatOutCtrl.vecWen.getOrElse(false.B)
    val directWakeUp = latency.valid && !flush && latency.bits <= 3.U
    val delayedWakeUp = wakeUpPending && !flush && wakeUpCountdown === 4.U
    val startDelayedWakeUp = latency.valid && !flush && latency.bits > 3.U

    outFuWakeUp.wen := (directWakeUp && currentWen) || (delayedWakeUp && wakeUpWen)
    outFuWakeUp.pdest := Mux(directWakeUp, nonFixedLatOutCtrl.pdest, wakeUpPdest)
    outFuWakeUp.delay := Mux(
      directWakeUp,
      MuxLookup(latency.bits, BypassDelay.delay0)(Seq(
        0.U -> BypassDelay.delay3,
        1.U -> BypassDelay.delay2,
        2.U -> BypassDelay.delay1,
        3.U -> BypassDelay.delay0,
      )),
      BypassDelay.delay0
    )

    when(flush || resultValid) {
      wakeUpPending := false.B
      wakeUpCountdown := 0.U
      wakeUpWen := false.B
    }.elsewhen(startDelayedWakeUp) {
      wakeUpPending := true.B
      wakeUpCountdown := latency.bits
      wakeUpPdest := nonFixedLatOutCtrl.pdest
      wakeUpWen := currentWen
    }.elsewhen(delayedWakeUp) {
      wakeUpPending := false.B
      wakeUpCountdown := 0.U
      wakeUpWen := false.B
    }.elsewhen(wakeUpPending) {
      wakeUpCountdown := wakeUpCountdown - 1.U
    }
  }
}
