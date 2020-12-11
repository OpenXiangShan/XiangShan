package device

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._

class TimerIO extends Bundle {
  val mtip = Output(Bool())
}

class AXI4Timer
(
  sim: Boolean = false,
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new TimerIO)
{
  override lazy val module = new AXI4SlaveModuleImp[TimerIO](this){
    val mtime = RegInit(0.U(64.W))  // unit: us
    val mtimecmp = RegInit(0.U(64.W))

    val clk = (if (!sim) 40 /* 40MHz / 1000000 */ else 10000)
    val freq = RegInit(clk.U(16.W))
    val inc = RegInit(1000.U(16.W))

    val cnt = RegInit(0.U(16.W))
    val nextCnt = cnt + 1.U
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)
    val tick = (nextCnt === freq)
    when (tick) { mtime := mtime + inc }

    val mapping = Map(
      RegMap(0x4000, mtimecmp),
      RegMap(0x8000, freq),
      RegMap(0x8008, inc),
      RegMap(0xbff8, mtime)
    )
    def getOffset(addr: UInt) = addr(15,0)

    RegMap.generate(mapping, getOffset(raddr), in.r.bits.data,
      getOffset(waddr), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))

    io.extra.get.mtip := RegNext(mtime >= mtimecmp)
  }
}
