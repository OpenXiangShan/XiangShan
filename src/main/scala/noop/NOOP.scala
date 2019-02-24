package noop

import chisel3._
import chisel3.util._

import memory.MemIO

trait NOOPConfig {
  val HasIcache = true
}

class NOOP extends Module with NOOPConfig {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val trap = Output(UInt(2.W))
  })

  val ifu = Module(new IFU)
  val idu = Module(new IDU)
  val isu = Module(new ISU)
  val exu = Module(new EXU)
  val wbu = Module(new WBU)

  if (HasIcache) {
    val icache = Module(new ICache)
    icache.io.in <> ifu.io.imem
    io.imem <> icache.io.out
  }
  else {
    io.imem <> ifu.io.imem
  }

  idu.io.in <> ifu.io.out
  isu.io.in <> idu.io.out
  exu.io.in <> isu.io.out
  io.dmem <> exu.io.dmem
  wbu.io.in <> exu.io.out
  wbu.io.brIn <> exu.io.br
  isu.io.wb <> wbu.io.wb
  ifu.io.br <> wbu.io.brOut
  ifu.io.writeback := wbu.io.writeback

  exu.io.csrCtrl.instrCommit := wbu.io.writeback

  io.trap := isu.io.trap
}
