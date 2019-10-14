package utils

import chisel3._
import chisel3.util._

class DiffTestIO extends Bundle {
  val r = Output(Vec(32, UInt(64.W)))
  val commit = Output(Bool())
  val thisPC = Output(UInt(64.W))
  val isMMIO = Output(Bool())
  val isRVC = Output(Bool())
}
