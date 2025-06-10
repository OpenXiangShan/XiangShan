package device

import chisel3.{BlackBox, Bool, Bundle, Input, Module, Output, UInt,_}
import chisel3.util.{HasBlackBoxResource, ValidIO}



class ClintAsyncCJ extends Module {
  val io = IO(new Bundle {
    val i_time = Input(ValidIO(UInt(64.W)))
    val o_time = Output(ValidIO(UInt(64.W)))
  })
  //instance sysc_ip
  val sync_inst = Module(new xh_sys_cnt_sync)
  sync_inst.io.clk := clock
  sync_inst.io.cpurst_b := (!reset.asBool).asAsyncReset
  sync_inst.io.i_cpu_sys_cnt := io.i_time.bits
  sync_inst.io.i_cpu_sys_cnt_updt := io.i_time.valid
  io.o_time.bits := sync_inst.io.cpu_sys_cnt
  io.o_time.valid := sync_inst.io.cpu_sys_cnt_updt_f
}

class xh_sys_cnt_sync extends BlackBox {
  val io= IO(new Bundle {
    val clk = Input(Clock())
    val cpurst_b = Input(Reset()) //active low
    val i_cpu_sys_cnt = Input(UInt(64.W))
    val i_cpu_sys_cnt_updt = Input(Bool())
    val cpu_sys_cnt = Output(UInt(64.W))
    val cpu_sys_cnt_updt_f = Output(Bool())
  })
}
