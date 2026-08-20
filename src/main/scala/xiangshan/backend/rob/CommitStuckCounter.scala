package xiangshan.backend.rob

import chisel3._

class CommitStuckCounter(width: Int, forceEnable: Bool) extends Module {
  val io = IO(new Bundle {
    val stuck = Input(Bool())
    val runtimeEnable = Input(Bool())
    val overflowEnabled = Input(Bool())
    val count = Output(UInt(width.W))
    val overflow = Output(Bool())
  })

  private val count = RegInit(0.U(width.W))
  private val effectiveEnable = io.runtimeEnable || forceEnable

  when(!effectiveEnable || !io.stuck) {
    count := 0.U
  }.otherwise {
    count := count + 1.U
  }

  io.count := count
  io.overflow := count.andR && io.overflowEnabled
}
