package xiangshan.backend.rename.refcnt

import chisel3._
import chisel3.util._

class Adder extends Module {
  val io = IO(new Bundle() {
    val inc = Input(Bool())
    val dec = Input(Bool())

    val cnt = Input(UInt(2.W))
    val nextCnt = Output(UInt(2.W))
  })

  val lowerBit = io.inc ^ io.dec
  val higherBit = (~io.inc) & io.dec

  io.nextCnt := io.cnt + Cat(higherBit, lowerBit)
}
