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

  val useMuxTree = true
}

import ALU._

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val func = Input(UInt(4.W))
    val out = Output(UInt(32.W))
  })

  val shamt = io.b(4, 0)
  val funcList = List(
    (FN_ADD , io.a  +  io.b),
    (FN_SLL , (io.a  << shamt)(31, 0)),
    (FN_SLT , (io.a.asSInt < io.b.asSInt).asUInt),
    (FN_SLTU, (io.a < io.b).asUInt),
    (FN_XOR , io.a  ^  io.b),
    (FN_SLR , io.a  >> shamt),
    (FN_OR  , io.a  |  io.b),
    (FN_AND , io.a  &  io.b),
    (FN_SUB , io.a  -  io.b),
    (FN_SAR , (io.a.asSInt >> shamt).asUInt)
  )

  io.out := (if (useMuxTree) Mux1H(funcList.map { case (func, res) => (func === io.func, res) })
             else MuxLookup(io.func, 0.U, funcList))
}
