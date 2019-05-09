package utils

import chisel3._
import chisel3.util._

class DiffTestIO extends Bundle {
  val r = Output(Vec(33, UInt(32.W)))
  val commit = Output(Bool())
}
