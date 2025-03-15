package device

import chisel3.{BlackBox, Bool, Bundle, Input, Module, Output, UInt,_}
import chisel3.util.{HasBlackBoxResource, ValidIO}



class ClintAsyncSPMT extends Module {
  val io = IO(new Bundle {
    val i_time = Input(ValidIO(UInt(64.W)))
    val o_time = Output(ValidIO(UInt(64.W)))
    val time = Output(ValidIO(UInt(64.W)))
  })
  //instance sysc_ip
  val sync_inst = Module(new xh_sys_cnt_sync)
  sync_inst.i_cpu_sys_cnt := io.i_time.bits
  sync_inst.i_cpu_sys_cnt_updt := io.i_time.valid
  io.o_time.bits := sync_inst.cpu_sys_cnt
  io.o_time.valid := sync_inst.cpu_sys_cnt_updt_f
}

class xh_sys_cnt_sync extends BlackBox with HasBlackBoxResource {
  val i_cpu_sys_cnt = IO(Input(UInt(64.W)))
  val i_cpu_sys_cnt_updt = IO(Input(Bool()))
  val cpu_sys_cnt = IO(Output(UInt(64.W)))
  val cpu_sys_cnt_updt_f = IO(Output(Bool()))
  addResource("/aia/src/rtl/imsic/xh_sys_cnt_sync.v")
}