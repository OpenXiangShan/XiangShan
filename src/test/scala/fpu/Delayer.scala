package fpu

import chisel3._
import chisel3.util.Queue

/** Note:
  *   chiseltest will end simulation when 'expect' fail and
  *   return the value at the beginning of the current cycle,
  *   so we need to delay the dut's output to get the value
  *   after this cycle.
  */
class Delayer(dutGen: => FPUSubModule) extends FPUSubModule {
  val dut = Module(dutGen)
  dut.io.in <> io.in
  io.out <> Queue(dut.io.out, 1, pipe = true)
}
