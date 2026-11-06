package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

/** Mark architectural instructions before uop splitting and ROB compression. */
class DecodeDebug(implicit p: Parameters) extends XSModule {
  import DecodeDebug._

  val in = IO(new In)
  val out = IO(new Out)

  // DRET starts with an empty pipeline. Allow one machine instruction, then
  // mark every following instruction, including those in the same decode batch.
  // A branch redirect preserves consumption; a replay of the only executable
  // instruction must let that instruction retry. A taken trap consumes the
  // step even when an interrupt precedes the first decoded instruction.
  // CSR's step level can arrive before the execution redirect has propagated
  // to decode. Do not spend the permission on the old path in that interval.
  private val awaitingResumeFlush = RegInit(true.B)
  when(!in.singleStep || in.debugMode) {
    awaitingResumeFlush := true.B
  }.elsewhen(in.redirect || in.trapTrigger.valid) {
    awaitingResumeFlush := false.B
  }
  private val stepConsumed = RegInit(false.B)
  when(!in.singleStep || in.debugMode) {
    stepConsumed := false.B
  }.elsewhen(in.trapTrigger.valid) {
    stepConsumed := true.B
  }.elsewhen(awaitingResumeFlush || in.replay) {
    stepConsumed := false.B
  }.elsewhen(in.mops.exists(_.fire) && !in.redirect) {
    stepConsumed := true.B
  }

  // None acknowledges a trap without another matching trap trigger. All
  // following MOPs are marked; the oldest one traps and flushes the younger
  // markers. Split uops inherit the selected action through MopCtrlBundle.
  private val trapAction = RegInit(TriggerAction.None)
  private val currentTrapAction = Mux(in.trapTrigger.valid, in.trapTrigger.bits, trapAction)
  trapAction := Mux(in.debugMode, TriggerAction.None, currentTrapAction)
  private val trapRequest = currentTrapAction === TriggerAction.OtherTrigger ||
    TriggerAction.isDmode(currentTrapAction)

  for (i <- 0 until DecodeWidth) {
    val mop = in.mops(i)
    val active = mop.valid && !in.redirect && !in.debugMode
    val trap = active && trapRequest
    val earlierMop = in.mops.take(i).map(_.valid).foldLeft(false.B)(_ || _)
    val step = active && in.singleStep &&
      (awaitingResumeFlush || stepConsumed || in.trapTrigger.valid || earlierMop)
    // The frontend supplies the action; DecodeStage derives the BP bit after
    // splitting, so that bit is not yet available at this boundary.
    val dmodeTrigger = active && (TriggerAction.isDmode(mop.triggerAction) ||
      TriggerAction.isDmode(currentTrapAction))
    out.triggerAction(i) := MuxCase(mop.triggerAction, Seq(
      dmodeTrigger -> TriggerAction.DebugMode,
      step -> TriggerAction.SingleStep,
      trap -> currentTrapAction,
    ))
  }
}

object DecodeDebug {
  class Mop extends Bundle {
    val valid = Bool()
    val fire = Bool()
    val triggerAction = TriggerAction()
  }

  class In(implicit p: Parameters) extends XSBundle {
    val singleStep = Input(Bool())
    val trapTrigger = Flipped(Valid(TriggerAction()))
    val debugMode = Input(Bool())
    val redirect = Input(Bool())
    val replay = Input(Bool())
    val mops = Input(Vec(DecodeWidth, new Mop))
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val triggerAction = Output(Vec(DecodeWidth, TriggerAction()))
  }
}
