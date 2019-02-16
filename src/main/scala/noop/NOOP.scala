package noop

import chisel3._
import chisel3.util._

import memory.MemIO
import gpu.GPU

trait NOOPConfig {
  val HasGPU = false
}

class NOOP extends Module with NOOPConfig {
  val io = IO(new Bundle {
    val imem = new MemIO
    val dmem = new MemIO
    val trap = Output(UInt(2.W))

    val gpuStart = Input(Bool())
    val gmem = new MemIO(256)
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
  io.dmem <> exu.io.dmem
  wbu.io.in <> exu.io.out
  wbu.io.brIn <> exu.io.br
  isu.io.wb <> wbu.io.wb
  ifu.io.br <> wbu.io.brOut

  io.trap := isu.io.trap

  if (HasGPU) {
    val gpu = Module(new GPU)
    gpu.io.start := io.gpuStart
    io.gmem <> gpu.io.out
  }
  else {
    io.gmem := DontCare
  }
}
