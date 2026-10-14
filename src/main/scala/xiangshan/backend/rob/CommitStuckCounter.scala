package xiangshan.backend.rob

import chisel3._
import xiangshan.backend.rob.CommitStuckCounter._

class CommitStuckCounter(width: Int, forceEnable: Bool) extends Module {
  val in = IO(new In)
  val out = IO(new Out(width))

  private val count = RegInit(0.U(width.W))
  private val effectiveEnable = in.runtimeEnable || forceEnable

  when (!effectiveEnable || !in.stuck) {
    count := 0.U
  }.otherwise {
    count := count + 1.U
  }

  out.count := count
  out.overflow := count.andR && in.overflowEnabled
}

object CommitStuckCounter {
  class In extends Bundle {
    // Commit progress observation.
    val stuck = Input(Bool())

    // Runtime counter control.
    val runtimeEnable = Input(Bool())

    // Overflow qualification.
    val overflowEnabled = Input(Bool())
  }

  class Out(width: Int) extends Bundle {
    // Stuck cycle count.
    val count = Output(UInt(width.W))

    // Qualified overflow notification.
    val overflow = Output(Bool())
  }
}
