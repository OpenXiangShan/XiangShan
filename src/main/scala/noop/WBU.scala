package noop

import chisel3._
import chisel3.util._

class WBU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new PcCtrlDataIO))
    val wb = new WriteBackIO
    val brIn = Flipped(new BranchIO)
    val brOut = new BranchIO
    val writeback = Output(Bool())
  })

  io.wb.rfWen := io.in.bits.ctrl.rfWen && io.in.valid
  io.wb.rfDest := io.in.bits.ctrl.rfDest
  io.wb.rfWdata := io.in.bits.data.dest
  io.in.ready := true.B

  io.brOut <> RegNext(io.brIn)

  io.writeback := io.in.valid
}
