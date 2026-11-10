package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{TriggerAction, XSBundle}
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState
import xiangshan.backend.rob.RobPtr

// One event pulse represents one architectural instruction execution. A trap
// and a retirement must never be reported separately for the same execution.
class IcountEvent(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val trapped = Bool()
  val icountSelected = Bool()
}

class IcountTrapResult(implicit p: Parameters) extends XSBundle {
  val isInterrupt = Bool()
  val breakpoint = Bool()
  val trigger = TriggerAction()
  val toDebug = Bool()
}

class IcountStatus(implicit p: Parameters) extends XSBundle {
  val serial = Bool()
  val pending = Bool()
  val action = TriggerAction()
}

class IcountResponse(implicit p: Parameters) extends XSBundle {
  val status = new IcountStatus
  val done = Bool()
}

class IcountController(implicit p: Parameters) extends Module {
  import IcountController._

  val in = IO(new In)
  val out = IO(new Out)

  val count = Wire(new Icount)
  count := in.tdata1.DATA.asUInt
  val enabled = in.tdata1.TYPE === Tdata1Type.Icount
  val modeMatch = enabled && count.modeEnabled(in.priv)
  // Decode's completion event precedes the CSR trap path. Keep at most one
  // trapped event until CSR has made its architectural trap/Debug decision.
  val waitingTrap = RegInit(false.B)
  val pendingSelected = Reg(Bool())
  val pendingIgnored = Reg(Bool())
  val receivedTrap = in.event.valid && in.event.bits.trapped
  val completeTrap = (waitingTrap || receivedTrap) && in.trap.valid
  val completeRetirement = in.event.valid && !in.event.bits.trapped
  val processed = completeRetirement || completeTrap
  val ignored = Mux(waitingTrap, pendingIgnored, in.ignoreEvent || in.modifyIcount)
  val selected = Mux(waitingTrap, pendingSelected, in.event.bits.icountSelected)
  when(receivedTrap && !in.trap.valid) {
    waitingTrap := true.B
    pendingSelected := in.event.bits.icountSelected
    pendingIgnored := in.ignoreEvent || in.modifyIcount
  }
  when(waitingTrap && in.modifyIcount) {
    pendingIgnored := true.B
  }
  when(completeTrap) {
    waitingTrap := false.B
  }
  when(waitingTrap) {
    assert(!in.event.valid, "one outstanding icount completion")
  }
  // Only an instruction admitted under serial (and hence eligible) sends an
  // event. RET/context/xIE writes must retain that source qualification.
  val matched = completeRetirement || completeTrap && !in.trap.bits.toDebug
  val fired = completeTrap && selected && !in.trap.bits.isInterrupt &&
    in.trap.bits.breakpoint && in.trap.bits.trigger === out.response.status.action
  val matchEvent = processed && matched && enabled

  val eligible = modeMatch && !in.debugMode && in.contextMatch &&
    (count.ACTION.asUInt =/= TrigAction.BreakpointExp.asUInt || in.canRaiseBreakpoint)
  out.response.status.serial := eligible && (count.COUNT.asUInt =/= 0.U || count.PENDING.asBool)
  out.response.status.pending := eligible && count.PENDING.asBool
  out.response.status.action := Mux(count.ACTION.asUInt === TrigAction.DebugMode.asUInt, TriggerAction.DebugMode, TriggerAction.OtherTrigger)
  // The parent applies the update at this edge. Done accompanies the resulting
  // status, including events ignored or displaced by a software write.
  out.response.done := RegNext(processed, false.B)

  // The completion is a one-cycle pulse. A simultaneous software write wins;
  // the displaced hardware update is not retried against the new CSR value.
  out.update.valid := enabled && processed && !ignored && !in.modifyIcount &&
    (matched || fired)
  out.update.bits := count.asUInt
  when(fired) {
    out.update.bits.PENDING := 0.U
  }
  when(matchEvent && count.COUNT.asUInt =/= 0.U) {
    out.update.bits.COUNT := count.COUNT.asUInt - 1.U
    when(count.COUNT.asUInt === 1.U) {
      out.update.bits.PENDING := 1.U
    }
  }
}

object IcountController {
  class In(implicit p: Parameters) extends XSBundle {
    val tdata1 = Input(new Tdata1Bundle)
    val priv = Input(new PrivState)
    val contextMatch = Input(Bool())
    val debugMode = Input(Bool())
    val canRaiseBreakpoint = Input(Bool())
    val modifyIcount = Input(Bool())
    val ignoreEvent = Input(Bool())
    val event = Input(Valid(new IcountEvent))
    val trap = Input(Valid(new IcountTrapResult))
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val response = Output(new IcountResponse)
    val update = Output(Valid(new Icount))
  }
}
