package top

import chisel3._
import chisel3.util._

import memory.MemIO

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new MemIO)
    val mmioTrap = new Bundle {
      val valid = Output(Bool())
      val cmd = Output(UInt(3.W))
      val rdata = Input(UInt(32.W))
    }
  })

  val wen = io.rw.a.valid && io.rw.w.valid
  val wdataVec = VecInit.tabulate(4) { i => io.rw.w.bits.data(8 * (i + 1) - 1, 8 * i) }
  val wmask = VecInit.tabulate(4) { i => io.rw.w.bits.mask(i).toBool }

  io.mmioTrap.valid := false.B
  io.mmioTrap.cmd := 0.U

  when (io.rw.a.valid) {
    switch (io.rw.a.bits.addr) {
      is (0x43f8.U) {
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 6.U
        when (wen) { printf("%c", wdataVec(0)) }
      }
      is (0x4048.U) {
        // read RTC
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 0.U
      }
      is (0x4060.U) {
        // read key
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 1.U
      }
      is (0x4100.U) {
        // read screen size
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 2.U
      }
      is (0x4104.U) {
        // write vga sync
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 4.U
      }
    }

    when (io.rw.a.bits.addr >= 0x40000.U && io.rw.a.bits.addr < 0xc0000.U && wen) {
      // write to vmem
      io.mmioTrap.valid := true.B
      io.mmioTrap.cmd := 5.U
    }
  }

  io.rw.a.ready := true.B
  io.rw.r.bits.data := io.mmioTrap.rdata
  io.rw.r.valid := io.mmioTrap.valid
}
