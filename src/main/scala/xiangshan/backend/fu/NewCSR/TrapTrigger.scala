package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{HasXSParameter, TriggerAction, XSBundle}
import xiangshan.backend.fu.NewCSR.CSRBundles.{CauseBundle, PrivState}
import xiangshan.backend.fu.NewCSR.CSRConfig._

/** Match only an accepted architectural trap, before its source context changes.
  * The caller supplies the final handler-visible cause (including VS remapping).
  * Actions are returned to the caller, never substituted for the original trap.
  */
class TrapTriggerMatcher(implicit val p: Parameters) extends Module with HasXSParameter {
  import TrapTriggerMatcher._

  val in = IO(new In)
  val out = IO(new Out)

  val currentAsid = Mux(in.privState.isVirtual, in.vsatpAsid, in.satpAsid)
  val cause = in.trap.bits.ExceptionCode.asUInt
  val causeInBitmap = cause < XLEN.U
  val actions = VecInit(in.tdata1.map(_.getTriggerAction.asUInt))

  for (idx <- 0 until TriggerNum) {
    val tdata1 = in.tdata1(idx)
    val itrigger = Wire(new Itrigger)
    itrigger := tdata1.DATA.asUInt
    val contextMatches = TriggerUtil.textraMatch(
      in.tdata3(idx), in.scontext, currentAsid, in.mhcontext, in.vmid
    )
    // Check the full cause before using its low bits for the bitmap index.
    val bitmapMatches = causeInBitmap && in.tdata2(idx)(cause(log2Ceil(XLEN) - 1, 0))
    val interruptMatches = tdata1.TYPE.isOneOf(Tdata1Type.Itrigger) &&
      in.trap.bits.Interrupt.asBool && Mux(in.isNmi, itrigger.NMI.asBool, bitmapMatches)
    val exceptionMatches = tdata1.TYPE.isOneOf(Tdata1Type.Etrigger) &&
      !in.trap.bits.Interrupt.asBool && !in.isNmi && bitmapMatches
    val actionAllowed = Mux(actions(idx) === TriggerAction.DebugMode,
      tdata1.DMODE.asBool, in.canRaiseBreakpoint)
    out.matches(idx) := in.trap.valid && !in.debugMode && contextMatches &&
      itrigger.modeEnabled(in.privState) && (interruptMatches || exceptionMatches) && actionAllowed
  }

  // These are architectural tdata1.action values. Pipeline breakpoint actions
  // (OtherTrigger / FetchTrigger / MemTrigger) are selected by the consumer.
  val debugMatch = TriggerUtil.triggerActionMatchVec(out.matches, actions, TrigAction.DebugMode.asUInt)
  val breakpointMatch = TriggerUtil.triggerActionMatchVec(out.matches, actions, TrigAction.BreakpointExp.asUInt)
  out.action := MuxCase(TriggerAction.None, Seq(
    debugMatch.asUInt.orR -> TrigAction.DebugMode.asUInt,
    breakpointMatch.asUInt.orR -> TrigAction.BreakpointExp.asUInt,
  ))
}

object TrapTriggerMatcher {
  class In(implicit p: Parameters) extends XSBundle {
    val trap = Input(ValidIO(new CauseBundle))
    val isNmi = Input(Bool())
    val privState = Input(new PrivState)
    val debugMode = Input(Bool())
    val canRaiseBreakpoint = Input(Bool())
    val tdata1 = Input(Vec(TriggerNum, new Tdata1Bundle))
    val tdata2 = Input(Vec(TriggerNum, UInt(XLEN.W)))
    val tdata3 = Input(Vec(TriggerNum, new Tdata3Bundle))
    val scontext = Input(UInt(32.W))
    val mhcontext = Input(UInt(14.W))
    val satpAsid = Input(UInt(ASIDLEN.W))
    val vsatpAsid = Input(UInt(ASIDLEN.W))
    val vmid = Input(UInt(VMIDLEN.W))
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val action = Output(TriggerAction())
    val matches = Output(Vec(TriggerNum, Bool()))
  }
}
