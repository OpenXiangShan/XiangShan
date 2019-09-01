package top

import system._
import noop.NOOPConfig

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import device.AXI4RAM
import utils.DiffTestIO

class NOOPSimTop(memInitFile: String = "") extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
  })

  val noop = Module(new NOOPSoC()(NOOPConfig(FPGAPlatform = false)))
  val imem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val dmem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val imemdelay = Module(new AXI4Delayer(20))
  val dmemdelay = Module(new AXI4Delayer(20))
  val mmio = Module(new SimMMIO)

  imemdelay.io.in <> noop.io.imem
  imem.io.in <> imemdelay.io.out
  dmemdelay.io.in <> noop.io.dmem
  dmem.io.in <> dmemdelay.io.out

  mmio.io.rw <> noop.io.mmio

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.commit, "difftestCommit")
  BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
  BoringUtils.addSink(difftest.isMMIO, "difftestIsMMIO")
  BoringUtils.addSink(difftest.r, "difftestRegs")
  io.difftest := difftest
}
