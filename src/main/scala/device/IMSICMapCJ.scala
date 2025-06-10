package device

import chisel3.{BlackBox, Bool, Bundle, Input, Module, Output, UInt, _}
import chisel3.util.{HasBlackBoxResource, ValidIO}




class IMSICMapCJ extends Module{
  val io = IO(new Bundle {
    val i_msi = Input(ValidIO(UInt(12.W)))
    val o_msi = Output(ValidIO(UInt(12.W)))
  })
  //instance sysc_ip
  val imsicmap_inst = Module(new imsic_map)
  imsicmap_inst.io.clock := clock
  imsicmap_inst.io.reset := reset.asAsyncReset
  imsicmap_inst.io.msi_info_spmt := io.i_msi.bits
  imsicmap_inst.io.msi_vld_spmt := io.i_msi.valid
  io.o_msi.bits := imsicmap_inst.io.msi_info_bosc
  io.o_msi.valid := imsicmap_inst.io.msi_vld_bosc
}

class imsic_map extends BlackBox {
  val io= IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset()) //active high
    val msi_info_spmt = Input(UInt(12.W))
    val msi_vld_spmt= Input(Bool())
    val msi_info_bosc = Output(UInt(12.W))
    val msi_vld_bosc = Output(Bool())
  })
}