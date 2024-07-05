package matu.SystolicArray

import chisel3._


// when no bias: C = A * B
// when need bias: C = A * B + D    D is preloaded into SA before A * B
// for example: A: INT8  B: INT8  C: INT32  D: INT32
class SystolicArray(val IN_WIDTH: Int, val C_WIDTH: Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
  val io = IO(new Bundle {
    val in_control = Input(Vec(SA_COLS, new PE_Control))
    val in_a = Input(Vec(SA_ROWS, SInt(IN_WIDTH.W)))
    val in_b = Input(Vec(SA_COLS, SInt(IN_WIDTH.W)))
    val in_c = Input(Vec(SA_COLS, SInt(C_WIDTH.W)))

    val out_control = Output(Vec(SA_COLS, new PE_Control))
    val out_a = Output(Vec(SA_ROWS, SInt(IN_WIDTH.W)))
    val out_b = Output(Vec(SA_COLS, SInt(IN_WIDTH.W)))
    val out_c = Output(Vec(SA_COLS, SInt(C_WIDTH.W)))
  })

  val sa = Seq.fill(SA_ROWS, SA_COLS)(Module(new PE(IN_WIDTH, C_WIDTH)))
  val sa_t = sa.transpose

  for (r <- 0 until SA_ROWS) {
    sa(r).foldLeft(io.in_a(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  for (c <- 0 until SA_COLS) {
    sa_t(c).foldLeft(io.in_b(c)) {
      case (in_b, pe) =>
        pe.io.in_b := in_b
        pe.io.out_b
    }
  }

  for (c <- 0 until SA_COLS) {
    sa_t(c).foldLeft(io.in_c(c)) {
      case (in_c, pe) =>
        pe.io.in_c := in_c
        pe.io.out_c
    }
  }


  for (c <- 0 until SA_COLS) {
    sa_t(c).foldLeft(io.in_control(c)) {
      case (in_ctrl, pe) =>
        pe.io.in_control := in_ctrl
        pe.io.out_control
    }
  }

  for (c <- 0 until SA_COLS) {
    io.out_b(c) := sa(SA_ROWS - 1)(c).io.out_b
    io.out_c(c) := sa(SA_ROWS - 1)(c).io.out_c
    io.out_control(c) := sa(SA_ROWS-1)(c).io.out_control
  }

  for (r <- 0 until SA_ROWS) {
    io.out_a(r) := sa(r)(SA_COLS-1).io.out_a
  }

}

