package device.lvna

import chisel3._
import freechips.rocketchip.config._
import freechips.rocketchip.util._
import xiangshan._
import chisel3.util.MuxCase



class NohypeMapper(addrWidth: Int = 64)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inAddr = Input(UInt(addrWidth.W))
    val outAddr = Output(UInt(addrWidth.W))
    val memOffset = Input(UInt(64.W))
    val ioOffset = Input(UInt(64.W))
  })

  val is_mmio = io.inAddr < 0x80000000L.U
  val is_fix_mmio = io.inAddr < 0x10000000L.U

  val useMemoryOffsetMap = !is_mmio
  val useIOOffsetMap = is_mmio && !is_fix_mmio

  io.outAddr := MuxCase(io.inAddr, Seq(
    (useMemoryOffsetMap) -> (io.inAddr + io.memOffset),
    (useIOOffsetMap) -> (io.inAddr + io.ioOffset),
  ))
}

class NohypeUnMapper(addrWidth: Int = 64)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inAddr = Input(UInt(addrWidth.W))
    val outAddr = Output(UInt(addrWidth.W))
    val memOffset = Input(UInt(64.W))
    val ioOffset = Input(UInt(64.W))
  })

  val is_mmio = io.inAddr < 0x80000000L.U
  val is_fix_mmio = io.inAddr < 0x10000000L.U

  val useMemoryOffsetMap = !is_mmio
  val useIOOffsetMap = is_mmio && !is_fix_mmio

  io.outAddr := MuxCase(io.inAddr, Seq(
    (useMemoryOffsetMap) -> (io.inAddr - io.memOffset),
    (useIOOffsetMap) -> (io.inAddr - io.ioOffset),
  ))
}
