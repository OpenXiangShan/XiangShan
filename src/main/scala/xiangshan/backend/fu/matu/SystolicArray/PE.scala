package matu.SystolicArray

import chisel3._


class PE_Control extends Bundle {
  val ctrl_sa_send_data = Bool()
  // TODO add bias preload control signals
}

class PE(val IN_WIDTH: Int, val C_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val in_control = Input(new PE_Control)
    val in_a = Input(SInt(IN_WIDTH.W))
    val in_b = Input(SInt(IN_WIDTH.W))
    val in_c = Input(SInt(C_WIDTH.W))

    val out_control = Output(new PE_Control)
    val out_a = Output(SInt(IN_WIDTH.W))
    val out_b = Output(SInt(IN_WIDTH.W))
    val out_c = Output(SInt(C_WIDTH.W))
  })

  val a_reg = RegInit(0.S(IN_WIDTH.W))
  val b_reg = RegInit(0.S(IN_WIDTH.W))
  val c_reg = RegInit(0.S(C_WIDTH.W))

  val mac = Module(new MacUnit(IN_WIDTH, C_WIDTH))
  mac.io.in_a := io.in_a
  mac.io.in_b := io.in_b
  mac.io.in_c := c_reg

  a_reg := io.in_a
  b_reg := io.in_b

  c_reg := Mux(io.in_control.ctrl_sa_send_data, io.in_c, mac.io.out_c)

  io.out_a := a_reg
  io.out_b := b_reg
  io.out_c := c_reg

  io.out_control := io.in_control
}