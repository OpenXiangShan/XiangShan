package device.lvna

import chisel3._

//object AutoCatConstants {
//  val resetBinPowerWidth = 6 // 2^0 ~ 2^63 is enough.
//  val nrL2Ways = 16 // Currently fixed.
//}
trait HasAutoCatParameters {
  val resetBinPowerWidth = 6 // 2^0 ~ 2^63 is enough.
  val nrL2Ways = 16 // Currently fixed.
}

class AutoCatIOInternal extends Bundle with HasAutoCatParameters {
  val access_valid_in = Input(Bool())
  // 2's power of reset limit, say, update suggested waymask per 2^reset_bin_power cycles.
  val reset_bin_power = Input(UInt(resetBinPowerWidth.W))
  val allowed_gap = Input(UInt(32.W))
  val hit_vec_in = Input(UInt(nrL2Ways.W))
  val suggested_waymask_out = Output(UInt(nrL2Ways.W))
}

class autocat extends BlackBox {
  val io = IO(new AutoCatIOInternal {
    val clk_in = Input(Clock())
    val reset_in = Input(Bool())
  })
}