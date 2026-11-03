package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.rob.RobPtr

/** Track halt requests and single-step sessions, and select the final BP action. */
class DispatchDebug(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val singleStep = Input(Bool())
    val debugIntr = Input(Bool())
    val debugIntrDisable = Input(Bool())
    val debugMode = Input(Bool())
    val redirect = Flipped(Valid(new Redirect))
    val uops = Input(Vec(RenameWidth, new Bundle {
      val valid = Bool()
      val fire = Bool()
      val firstUop = Bool()
      val robIdx = new RobPtr
      val breakpoint = Bool()
      val triggerAction = TriggerAction()
    }))
    val triggerAction = Output(Vec(RenameWidth, TriggerAction()))
    val injectBreakpoint = Output(Vec(RenameWidth, Bool()))
    val pending = Output(Bool())
  })

  private def oldestUop(eligible: Seq[Bool]) =
    Mux1H(PriorityEncoderOH(VecInit(eligible).asUInt), io.uops)

  // Allow one ROB entry per single-step session, including all its split uops.
  // After a redirect, the branch target must stop before it executes.
  private val stepEntryValid = RegInit(false.B)
  private val stepRedirected = RegInit(false.B)
  private val stepEntry = Reg(new RobPtr)
  private val oldest = oldestUop(io.uops.map(_.valid))
  private val startStepEntry = io.singleStep && !stepEntryValid && oldest.fire
  private val allowedStepEntry = Mux(stepEntryValid, stepEntry, oldest.robIdx)

  stepEntryValid := io.singleStep && (stepEntryValid || oldest.fire)
  stepRedirected := io.singleStep && (stepRedirected || stepEntryValid && io.redirect.valid)
  when(startStepEntry) {
    stepEntry := oldest.robIdx
  }

  private val pending = RegInit(false.B)
  private val assigned = RegInit(false.B)
  private val assignedPtr = Reg(new RobPtr)
  // DM halt requests may be held until a later DMI write clears them. Consume
  // each assertion once, including requests that arrive while debug is masked.
  private val previousDebugIntr = RegNext(io.debugIntr, false.B)
  private val debugIntrRise = io.debugIntr && !previousDebugIntr
  private val acceptPulse = debugIntrRise && !io.debugIntrDisable && !io.debugMode
  private val request = pending || acceptPulse
  // Begin at a fresh ROB entry: an older split instruction may already have executed uops.
  private val candidate = oldestUop(io.uops.map(uop => uop.valid && uop.firstUop))
  private val targetPtr = Mux(assigned, assignedPtr, candidate.robIdx)
  private val haltEntryValid = !io.redirect.valid && !io.debugMode &&
    (assigned || request && !io.debugIntrDisable && candidate.valid)

  io.pending := request && !io.debugMode
  private val haltFired = io.uops.zipWithIndex.map { case (uop, i) =>
    // Mark every uop of the selected entry, including a compressed latter slot.
    val halt = haltEntryValid && uop.valid && uop.robIdx.isSameEntry(targetPtr)
    val step = io.singleStep && (stepRedirected || !uop.robIdx.isSameEntry(allowedStepEntry))
    val dmodeTrigger = uop.breakpoint && TriggerAction.isDmode(uop.triggerAction)
    io.injectBreakpoint(i) := halt || step
    // Halt outranks a Dmode trigger; a valid Dmode trigger outranks single-step.
    io.triggerAction(i) := Mux(halt, TriggerAction.DebugIntr,
      Mux(step && !dmodeTrigger, TriggerAction.SingleStep, uop.triggerAction))
    halt && uop.fire
  }.reduce(_ || _)

  // Entry to Dmode acknowledges the request. A flush before entry only clears
  // its ROB assignment, retaining pending so it can be attached again.
  pending := io.pending
  assigned := !io.debugMode && (haltFired || assigned && !assignedPtr.needFlush(io.redirect))
  when(haltFired) {
    assignedPtr := targetPtr
  }
}
