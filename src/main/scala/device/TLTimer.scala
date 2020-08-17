package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config._
import chisel3.util.experimental.BoringUtils
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegWriteFn}
import utils.{GTimer, HoldUnless, MaskExpand, RegMap}

class TLTimer(address: Seq[AddressSet], sim: Boolean)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("clint", Seq("XiangShan", "clint"))
  val node = TLRegisterNode(address, device, beatBytes = 8)

  lazy val module = new LazyModuleImp(this){
    val mtip = IO(Output(Bool()))

    val mtime = RegInit(0.U(64.W))  // unit: us
    val mtimecmp = RegInit(0.U(64.W))

    val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 100)
    val freq = RegInit(clk.U(16.W))
    val inc = RegInit(1000.U(16.W))

    val cnt = RegInit(0.U(16.W))
    val nextCnt = cnt + 1.U
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)
    val tick = (nextCnt === freq)
    when (tick) { mtime := mtime + inc }

    if (sim) {
      val isWFI = WireInit(false.B)
      BoringUtils.addSink(isWFI, "isWFI")
      when (isWFI) { mtime := mtime + 100000.U }
    }

    node.regmap( mapping =
      0x4000 -> RegField.bytes(mtimecmp),
      0x8000 -> RegField.bytes(freq),
      0x8008 -> RegField.bytes(inc),
      0xbff8 -> RegField.bytes(mtime)
    )

//    val gtime = GTimer()
//    printf(p"[$gtime][Timer] mtime=$mtime cnt=$cnt freq=$freq\n")

    mtip := RegNext(mtime >= mtimecmp)
  }
}
