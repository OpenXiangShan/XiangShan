package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.RegField
import utils.{HasTLDump, XSDebug}
import xiangshan.HasXSLog

class TLTimer(address: Seq[AddressSet], sim: Boolean)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("clint", Seq("XiangShan", "clint"))
  val node = TLRegisterNode(address, device, beatBytes = 8)
  val NumCores = top.Parameters.get.socParameters.NumCores

  lazy val module = new LazyModuleImp(this) with HasXSLog with HasTLDump{
    val io = IO(new Bundle() {
      val mtip = Output(Vec(NumCores, Bool()))
      val msip = Output(Vec(NumCores, Bool()))
    })

    val mtime = RegInit(0.U(64.W))  // unit: us
    val mtimecmp = Seq.fill(NumCores)(RegInit(0.U(64.W)))
    val msip = Seq.fill(NumCores)(RegInit(0.U(32.W)))

    val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 1000000)
    val freq = RegInit(clk.U(64.W))
    val inc = RegInit(1.U(64.W))

    val cnt = RegInit(0.U(64.W))
    val nextCnt = cnt + 1.U
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)
    val tick = (nextCnt === freq)
    when (tick) { mtime := mtime + inc }

    var clintMapping = Seq(
      0x8000 -> RegField.bytes(freq),
      0x8008 -> RegField.bytes(inc),
      0xbff8 -> RegField.bytes(mtime))

    for (i <- 0 until NumCores) {
      clintMapping = clintMapping ++ Seq(
        0x0000 + i*4 -> RegField.bytes(msip(i)),
        0x4000 + i*8 -> RegField.bytes(mtimecmp(i))
      )
    }

    node.regmap( mapping = clintMapping:_* )

    val in = node.in.head._1
    when(in.a.valid){
      XSDebug("[A] channel valid ready=%d ", in.a.ready)
      in.a.bits.dump
    }

    for (i <- 0 until NumCores) {
      io.mtip(i) := RegNext(mtime >= mtimecmp(i))
      io.msip(i) := RegNext(msip(i) =/= 0.U)
    }
  }
}
