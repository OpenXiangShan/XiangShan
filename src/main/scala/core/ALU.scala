package core

import chisel3._
import chisel3.util._

object ALU {
  def FN_ADD  = "b0000".U
  def FN_SLL  = "b0001".U
  def FN_SLT  = "b0010".U
  def FN_SLTU = "b0011".U
  def FN_XOR  = "b0100".U
  def FN_SLR  = "b0101".U
  def FN_OR   = "b0110".U
  def FN_AND  = "b0111".U
  def FN_SUB  = FN_ADD | "b1000".U
  def FN_SAR  = FN_SLR | "b1000".U
}

import ALU._

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val func = Input(UInt(4.W))
    val out = Output(UInt(32.W))
  })

  io.out := MuxLookup(io.func, 0.U, List(
    (FN_ADD , io.a  +  io.b),
    (FN_SLL , io.a  << io.b(4, 0)),
    (FN_SLT , Cat(0.U(31.W), (io.a.asSInt < io.b.asSInt))),
    (FN_SLTU, Cat(0.U(31.W), (io.a < io.b))),
    (FN_XOR , io.a  ^  io.b),
    (FN_SLR , io.a  >> io.b(4, 0)),
    (FN_OR  , io.a  |  io.b),
    (FN_AND , io.a  &  io.b),
    (FN_SUB , io.a  -  io.b),
    (FN_SAR , (io.a.asSInt >> io.b(4, 0)).asUInt)
  ))
}
