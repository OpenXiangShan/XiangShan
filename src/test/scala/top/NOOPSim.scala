package top

import noop._

import chisel3._
import chisel3.util._

import memory.DistributedMem
import memory.{AHBRAM, AHBParameters, MemIO2AHBLiteConverter}
import memory.{AXI4RAM, AXI4Parameters, MemIO2AXI4Converter, AXI4Delayer}

class NOOPSimTop(memInitFile: String = "") extends Module {
  val io = IO(new Bundle{
    val trap = Output(UInt((3 + 1 + 4 + 32 + 32 + 2).W))
    val mmioRdata = Input(UInt(32.W))
    val trapInfo = new PcInstrIO
    val cycleCnt = Output(UInt(32.W))
    val instrCnt = Output(UInt(32.W))
  })

  val noop = Module(new NOOP)
  val mem = Module(new DistributedMem(memByte = 128 * 1024 * 1024, dualPort = true, dataFile = memInitFile))
  val mem2axi = Module(new MemIO2AXI4Converter)
  val delay = Module(new AXI4Delayer(0))
  val aximem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, dataFile = memInitFile))
  val mmio = Module(new SimMMIO)

  noop.io.imem <> mem.io.ro
  mem2axi.io.in <> noop.io.dmem
  delay.io.in <> mem2axi.io.out
  aximem.io.in <> delay.io.out
  mem.io.rw := DontCare

  io.trap := Cat(mmio.io.mmioTrap.cmd, mmio.io.mmioTrap.valid, noop.io.dmem.w.bits.mask,
    noop.io.dmem.a.bits.addr, noop.io.dmem.w.bits.data, noop.io.trap)

  noop.io.dmem.a.ready     := Mux(mmio.io.mmioTrap.valid, mmio.io.rw.a.ready, mem2axi.io.in.a.ready)
  noop.io.dmem.r.bits.data := Mux(mmio.io.mmioTrap.valid, io.mmioRdata, mem2axi.io.in.r.bits.data)
  noop.io.dmem.r.valid     := Mux(mmio.io.mmioTrap.valid, mmio.io.rw.r.valid, mem2axi.io.in.r.valid)
  mem2axi.io.in.a.valid    := Mux(mmio.io.mmioTrap.valid, false.B, noop.io.dmem.a.valid)
  mem2axi.io.in.w.valid    := Mux(mmio.io.mmioTrap.valid, false.B, noop.io.dmem.w.valid)

  mmio.io.rw.a.bits  := noop.io.dmem.a.bits
  mmio.io.rw.a.valid := noop.io.dmem.a.valid
  mmio.io.rw.w       := noop.io.dmem.w
  mmio.io.rw.r.ready := true.B

  io.trapInfo.pc := noop.io.imem.a.bits.addr
  io.trapInfo.instr := noop.io.imem.r.bits.data
  mmio.io.mmioTrap.rdata := io.mmioRdata

  noop.io.gmem := DontCare
  noop.io.gpuStart := DontCare

  io.instrCnt := Counter(mem.io.ro.r.fire(), 0x7fffffff)._1
  io.cycleCnt := Counter(true.B, 0x7fffffff)._1
}
