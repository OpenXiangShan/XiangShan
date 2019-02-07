package core

import chisel3._
import chisel3.util._

import Decode._

class WBU extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new PcCtrlDataIO)
    val wb = new WriteBackIO
    val brIn = Flipped(new BranchIO)
    val brOut = new BranchIO
  })

  io.wb.rfWen := io.in.ctrl.rfWen
  io.wb.rfDest := io.in.ctrl.rfDest
  io.wb.rfWdata := io.in.data.dest

  io.brOut <> io.brIn
}
