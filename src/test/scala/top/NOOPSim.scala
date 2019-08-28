package top

import noop._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import bus.axi4._
import device.AXI4RAM
import bus.simplebus.SimpleBus2AXI4Converter
import utils.DiffTestIO

class NOOPSimTop(memInitFile: String = "") extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
  })

  val noop = Module(new NOOP()(NOOPConfig(FPGAPlatform = false)))
  val imem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val dmem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val imemdelay = Module(new AXI4Delayer(40))
  val dmemdelay = Module(new AXI4Delayer(40))
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

//  noop.io.uncacheMem := DontCare
}
