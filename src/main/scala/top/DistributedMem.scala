package top

import noop.MemIO

import chisel3._
import chisel3.util._

class DistributedMem extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new MemIO)
    val dmem = Flipped(new MemIO)
  })

  val memSize = 4096 // bytes
  val wordNum = memSize / 4
  val memAddrBits = log2Up(wordNum)
  def Index(addr: UInt): UInt = addr(memAddrBits + 2 - 1, 2)

  val mem = List.fill(4)(Mem(wordNum, UInt(8.W)))

  val imemIdx = Index(io.imem.a.bits.addr)
  val dmemIdx = Index(io.dmem.a.bits.addr)

  val wen = io.dmem.a.valid && io.dmem.w.valid
  io.imem.r.bits.data := Cat(mem.reverseMap(_(imemIdx)))
  io.dmem.r.bits.data := Cat(mem.reverseMap(_(dmemIdx)))
  io.imem.r.valid := true.B
  io.dmem.r.valid := true.B

  when (wen) { mem.zipWithIndex.map { case (m, i) =>
      when (io.dmem.w.bits.mask(i)) {
        m(dmemIdx) := io.dmem.w.bits.data(i * 8 + 7, i * 8)
      }
  }}
}
