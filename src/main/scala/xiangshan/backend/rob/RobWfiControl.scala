package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

/** Own WFI state with the original wake, enqueue, then disable priority. */
final class RobWfiControl(
  admittedWfi: Seq[Bool], wakeEvent: Bool, flush: Bool, enabled: Bool,
  safeFromMem: Bool, safeFromFrontend: Bool
)(implicit val p: Parameters) extends HasXSParameter {
  val waiting = RegInit(false.B)
  private val cycles = RegInit(0.U(20.W))
  if (wfiResume) {
    when(waiting) {
      cycles := cycles + 1.U
    }.elsewhen(!waiting && RegNext(waiting)) {
      cycles := 0.U
    }
  }
  private val timeout = cycles.andR
  when(RegNext(RegNext(wakeEvent)) || flush || timeout) { waiting := false.B }
  admittedWfi.foreach { accepted =>
    when(accepted) { waiting := true.B }
  }
  when(!enabled) { waiting := false.B }
  val request = waiting
  val safeToIdle = waiting && safeFromMem && safeFromFrontend
}
