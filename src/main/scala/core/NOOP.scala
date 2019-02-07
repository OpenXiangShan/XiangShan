package core

import chisel3._
import chisel3.util._

class NOOP extends Module {
  val io = IO(new Bundle {
    val imem = new MemIO
    val trap = Output(UInt(2.W))
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU)
  val wbu = Module(new WBU)

  io.imem <> ifu.io.imem
  idu.io.in <> ifu.io.out
  isu.io.in <> idu.io.out
  exu.io.in <> isu.io.out
  wbu.io.in <> exu.io.out
  isu.io.wb <> wbu.io.wb

  io.trap := isu.io.trap
}
