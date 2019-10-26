package top

import system._
import noop.NOOPConfig

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import device.AXI4RAM

class DiffTestIO extends Bundle {
  val r = Output(Vec(32, UInt(64.W)))
  val commit = Output(Bool())
  val thisPC = Output(UInt(64.W))
  val isMMIO = Output(Bool())
  val intrNO = Output(UInt(64.W))
}

/*
class NOOPSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
  })

  lazy val config = NOOPConfig(FPGAPlatform = false)
  val soc = Module(new NOOPSoC()(config))
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val memdelay = Module(new AXI4Delayer(0))
  val mmio = Module(new SimMMIO)

  memdelay.io.in <> soc.io.mem
  mem.io.in <> memdelay.io.out

  mmio.io.rw <> soc.io.mmio
  soc.io.mtip := mmio.io.mtip

  soc.io.meip := Counter(true.B, 9973)._2  // use prime here to not overlapped by mtip

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.commit, "difftestCommit")
  BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
  BoringUtils.addSink(difftest.isMMIO, "difftestIsMMIO")
  BoringUtils.addSink(difftest.intrNO, "difftestIntrNO")
  BoringUtils.addSink(difftest.r, "difftestRegs")
  io.difftest := difftest
}

object TestMain extends App {
  chisel3.Driver.execute(args, () => new NOOPSimTop)
}
*/
