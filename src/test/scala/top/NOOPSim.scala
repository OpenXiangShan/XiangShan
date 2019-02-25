package top

import noop._

import chisel3._
import chisel3.util._

import bus.axi4._
import bus.simplebus.SimpleBus2AXI4Converter

class NOOPSimTop(memInitFile: String = "") extends Module {
  val io = IO(new Bundle{
    val trap = Output(UInt((3 + 1 + 4 + 32 + 32 + 2).W))
    val mmioRdata = Input(UInt(32.W))
    val trapInfo = new PcInstrIO
    val sim = new Bundle {
      val cycleCnt = Output(UInt(32.W))
      val instrCnt = Output(UInt(32.W))
    }
  })

  val noop = Module(new NOOP)
  val imem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val dmem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val imem2axi = Module(new SimpleBus2AXI4Converter)
  val dmem2axi = Module(new SimpleBus2AXI4Converter)
  val imemdelay = Module(new AXI4Delayer(0.5))
  val dmemdelay = Module(new AXI4Delayer(0.5))
  val mmio = Module(new SimMMIO)

  imem2axi.io.in <> noop.io.imem
  imemdelay.io.in <> imem2axi.io.out
  imem.io.in <> imemdelay.io.out
  dmem2axi.io.in <> noop.io.dmem
  dmemdelay.io.in <> dmem2axi.io.out
  dmem.io.in <> dmemdelay.io.out

  io.trap := Cat(mmio.io.mmioTrap.cmd, mmio.io.mmioTrap.valid, noop.io.dmem.req.bits.wmask,
    noop.io.dmem.req.bits.addr, noop.io.dmem.req.bits.wdata, noop.io.trap)


  noop.io.dmem.req.ready       := Mux(mmio.io.mmioTrap.valid, mmio.io.rw.req.ready, dmem2axi.io.in.req.ready)
  noop.io.dmem.resp.bits.rdata := Mux(mmio.io.mmioTrap.valid, io.mmioRdata, dmem2axi.io.in.resp.bits.rdata)
  noop.io.dmem.resp.valid      := Mux(mmio.io.mmioTrap.valid, mmio.io.rw.resp.valid, dmem2axi.io.in.resp.valid)
  dmem2axi.io.in.req.valid     := Mux(mmio.io.mmioTrap.valid, false.B, noop.io.dmem.req.valid)
  dmem2axi.io.in.req.bits.wen  := Mux(mmio.io.mmioTrap.valid, false.B, noop.io.dmem.req.bits.wen)

  mmio.io.rw.req.bits  := noop.io.dmem.req.bits
  mmio.io.rw.req.valid := noop.io.dmem.req.valid
  mmio.io.rw.resp.ready := true.B

  io.trapInfo.pc := noop.io.imem.req.bits.addr
  io.trapInfo.instr := noop.io.imem.resp.bits.rdata
  mmio.io.mmioTrap.rdata := io.mmioRdata

  io.sim <> noop.io.sim
}
