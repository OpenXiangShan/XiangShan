package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan.{MicroOp, _}
import xiangshan.backend.exu.ExuParameters
import xiangshan.backend.fu._

class Mtest (implicit  p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val rs1_data = Input(Vec(2, UInt(XLEN.W)))
    val rs2_data = Input(Vec(2, UInt(XLEN.W)))
    val valid_in = Input(Bool())
    val rd_data = Output(Vec(2, UInt(XLEN.W)))
    val valid_out = Output(Bool())
    val ready_out = Output(Bool())
  })

  io.valid_out := io.valid_in
  io.ready_out := true.B
  for (i <- 0 until 2) {
    io.rd_data(i) := io.rs1_data(i) + io.rs2_data(i)
  }
}