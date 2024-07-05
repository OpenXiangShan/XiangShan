package xiangshan.backend.fu.matu

import chisel3._
import chisel3.util._


class GlobalCounter(val maxCount:Int) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val tick = Output(Bool())
  })

  val count = RegInit(0.U(log2Ceil(maxCount+1).W))
  val tick = count === maxCount.asUInt

  val enable = RegInit(false.B)
  when(io.start) {
    enable := true.B
  }.elsewhen(tick) {
    enable := false.B
  }

  when(enable) {
    when(count =/= maxCount.asUInt) {
      count := count + 1.U
    }.otherwise {
      count := 0.U
    }
  }

  io.tick := tick
}

class Controller(val INA_ROWS:Int, val INA_COLS:Int, val SA_ROWS: Int, val SA_COLS: Int) extends Module {
  val io = IO(new Bundle {
    val ibh_data_in_done  = Input(Bool())
    val ibv_data_in_done  = Input(Bool())
    val ob_empty          = Input(Bool())

    val ctrl_ib_data_out  = Output(Bool())
    val ctrl_ob_data_in   = Output(Bool())
    val ctrl_sa_send_data = Output(Bool())  // sa send data to OutputBuffer
    val ctrl_sa_isIdle    = Output(Bool())
  })

  // generate a pulse to inform InputBuffer
  val ctrl_ib_data_out = WireInit(false.B)
  val delay_ctrl_ib_data_out = RegInit(false.B)
  val ctrl_ib_data_out_edge = WireInit(false.B)
  delay_ctrl_ib_data_out := ctrl_ib_data_out
  ctrl_ib_data_out_edge := !delay_ctrl_ib_data_out & ctrl_ib_data_out
  io.ctrl_ib_data_out := ctrl_ib_data_out_edge

  val cal_done = RegInit(false.B)
  val out_done = RegInit(false.B)
  val cal_gc_start = WireInit(false.B)

  val isStall = RegInit(false.B)

  // generate cal_done, meaning that calculation is done
  val cal_gc       = Module(new GlobalCounter(INA_COLS+INA_ROWS+SA_COLS-1))
  cal_gc.io.start := cal_gc_start
  when(cal_gc.io.tick) {
    cal_done := true.B
  }

  // generate out_done, meaning that data enter to output buffer done
  val out_gc       = Module(new GlobalCounter(SA_COLS-1))
  out_gc.io.start := cal_gc.io.tick & io.ob_empty | isStall & io.ob_empty
  when(out_gc.io.tick) {
    out_done := true.B
  }

  io.ctrl_ob_data_in := cal_done & !out_done & !isStall
  io.ctrl_sa_send_data := cal_done & !out_done & !isStall

  // FSM for systolic array
  // idle: sa wait for instruction and prepare data
  // compute: sa execute matrix multiplication
  // stall: when the OutputBuffer is full, the data is temporarily stored in sa
  // output: sa send data to OutputBuffer
  val idle :: compute :: stall :: output :: Nil = Enum(4)
  val state = RegInit(idle)

  when(state === idle) {
    when(io.ibh_data_in_done && io.ibv_data_in_done && !isStall) {
      state := compute
      isStall := false.B
      ctrl_ib_data_out := true.B
      cal_gc_start := true.B
    }
  }.elsewhen(state === compute) {
    when(cal_done && io.ob_empty) {
      state := output
    }.elsewhen(cal_gc.io.tick && !io.ob_empty) {
      state := stall
      isStall := true.B
    }
  }.elsewhen(state === stall) {
    when(io.ob_empty) {
      state := output
      isStall := false.B
    }
  }.elsewhen(state === output) {
    when(out_done) {
      state := idle
      cal_done := false.B
      out_done := false.B
    }
  }

  io.ctrl_sa_isIdle := state === idle

}