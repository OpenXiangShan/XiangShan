package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.fu.NewCSR.{IcountEvent, IcountResponse}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.Bundles.ExceptionInfo

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
  }.elsewhen(in.redirect.valid || in.trapTrigger.valid) {
    awaitingResumeFlush := false.B
  }
  private val stepConsumed = RegInit(false.B)
  when(!in.singleStep || in.debugMode) {
    stepConsumed := false.B
  }.elsewhen(in.trapTrigger.valid) {
    stepConsumed := true.B
  }.elsewhen(awaitingResumeFlush || in.replay) {
    stepConsumed := false.B
  }.elsewhen(in.mops.head.fire && !in.redirect.valid) {
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

  // Track one architectural instruction from serial admission until its new
  // CSR status is visible or a redirect cancels it.
  private object IcountState extends ChiselEnum {
    val Idle, WaitDispatch, WaitCompletion, WaitCSR = Value
  }
  private val state = RegInit(IcountState.Idle)
  private val selectedIcount = Reg(Bool())
  out.serial := in.icount.status.serial
  // IBuffer and CtrlBlock's decode buffer pack valid MOPs from slot zero.
  // Serial admission therefore needs no oldest-valid priority encoder.
  when(out.serial) {
    assert(!in.mops.tail.map(_.valid).reduce(_ || _) || in.mops.head.valid,
      "serial Decode input must start at MOP slot zero")
  }

  private val stepHead = awaitingResumeFlush || stepConsumed || in.trapTrigger.valid
  for (i <- 0 until DecodeWidth) {
    val mop = in.mops(i)
    val active = mop.valid && !in.redirect.valid && !in.debugMode
    val trap = active && trapRequest
    val step = active && in.singleStep && (if (i == 0) stepHead else true.B)
    val dmodeTrigger = active && (TriggerAction.isDmode(mop.triggerAction) ||
      TriggerAction.isDmode(currentTrapAction))
    // A previously matched trap or fetch Debug trigger retains its action
    // source. Equal action encodings must not consume icount pending.
    // Pending implies serial admission, so only slot zero can carry icount.
    // Its split uops inherit this decision through MopCtrlBundle.
    val icount = if (i == 0) active && in.icount.status.pending && !trapRequest && !dmodeTrigger else false.B
    out.allow(i) := !in.redirect.valid && state === IcountState.Idle && (!out.serial || (i == 0).B)
    out.icountSelected(i) := icount
    out.triggerAction(i) := MuxCase(mop.triggerAction, Seq(
      dmodeTrigger -> TriggerAction.DebugMode,
      icount -> in.icount.status.action,
      step -> TriggerAction.SingleStep,
      trap -> currentTrapAction,
    ))
  }

  private val admitted = out.serial && in.mops.head.fire
  when(admitted) {
    assert(state === IcountState.Idle && !in.mops.tail.map(_.fire).reduce(_ || _),
      "icount decode admits one architectural instruction")
    selectedIcount := out.icountSelected.head
  }

  // Configuration CSR/xRET instructions already use waitForward/blockBackward.
  // Before the target Dispatch handshake, their completion is ignored; after
  // it, this is the only architectural instruction and retires in slot zero.
  private val retired = state === IcountState.WaitCompletion && in.commit.valid
  private val trapped = state === IcountState.WaitCompletion && in.exception.valid
  private val completed = retired || trapped

  // Report raw completion facts once. CSR owns their counting semantics and
  // returns done with the resulting status through the same response path.
  private val update = Reg(new IcountEvent)
  when(completed) {
    // CSR uses the completed pointer only to exclude the configuration write.
    update.robIdx := Mux(trapped, in.redirect.bits.robIdx, in.commit.bits)
    update.trapped := trapped
    update.icountSelected := selectedIcount
  }
  out.icountEvent.valid := RegNext(completed, false.B)
  out.icountEvent.bits := update

  switch(state) {
    is(IcountState.Idle) {
      when(admitted) {
        state := IcountState.WaitDispatch
      }
    }
    is(IcountState.WaitDispatch) {
      // Any redirect cancels the target before it reaches Dispatch.
      when(in.redirect.valid) {
        state := IcountState.Idle
      }.elsewhen(in.dispatched) {
        state := IcountState.WaitCompletion
      }
    }
    is(IcountState.WaitCompletion) {
      // Capture a trap before its redirect can discard the record. An
      // ordinary flushAfter preserves it; replay/flushItself cancels it.
      when(completed) {
        state := IcountState.WaitCSR
      }.elsewhen(in.redirect.valid && in.redirect.bits.flushItself()) {
        state := IcountState.Idle
      }
    }
    is(IcountState.WaitCSR) {
      when(in.icount.done) {
        state := IcountState.Idle
      }
    }
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
    val redirect = Flipped(Valid(new Redirect))
    val replay = Input(Bool())
    val mops = Input(Vec(DecodeWidth, new Mop))
    val icount = Input(new IcountResponse)
    val dispatched = Input(Bool())
    val commit = Flipped(Valid(new RobPtr))
    val exception = Flipped(Valid(new ExceptionInfo))
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val icountEvent = Valid(new IcountEvent)
    val allow = Output(Vec(DecodeWidth, Bool()))
    val icountSelected = Output(Vec(DecodeWidth, Bool()))
    val serial = Output(Bool())
    val triggerAction = Output(Vec(DecodeWidth, TriggerAction()))
  }
}
