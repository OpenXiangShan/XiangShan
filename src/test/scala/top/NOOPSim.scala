package top

import noop._

import chisel3._
import chisel3.util._

import memory.DistributedMem

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
  val mmio = Module(new SimMMIO)

  noop.io.imem <> mem.io.ro
  noop.io.dmem <> mem.io.rw

  io.trap := Cat(mmio.io.mmioTrap.cmd, mmio.io.mmioTrap.valid, noop.io.dmem.w.bits.mask,
    noop.io.dmem.a.bits.addr, noop.io.dmem.w.bits.data, noop.io.trap)

  noop.io.dmem.r.bits.data := Mux(mmio.io.mmioTrap.valid, io.mmioRdata, mem.io.rw.r.bits.data)
  mmio.io.rw.a.bits := mem.io.rw.a.bits
  mmio.io.rw.a.valid := mem.io.rw.a.valid
  mmio.io.rw.w := mem.io.rw.w
  mmio.io.rw.r.ready := true.B

  io.trapInfo.pc := noop.io.imem.a.bits.addr
  io.trapInfo.instr := noop.io.imem.r.bits.data
  mmio.io.mmioTrap.rdata := io.mmioRdata

  noop.io.gmem := DontCare
  noop.io.gpuStart := DontCare

  io.instrCnt := Counter(mem.io.ro.r.fire(), 0x7fffffff)._1
  io.cycleCnt := Counter(true.B, 0x7fffffff)._1
}
