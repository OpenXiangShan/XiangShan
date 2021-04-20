package device

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._

class UARTIO extends Bundle {
  val out = new Bundle {
    val valid = Output(Bool())
    val ch = Output(UInt(8.W))
  }
  val in = new Bundle {
    val valid = Output(Bool())
    val ch = Input(UInt(8.W))
  }
}

class AXI4UART
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new UARTIO)
{
  override lazy val module = new AXI4SlaveModuleImp[UARTIO](this){
    val rxfifo = RegInit(0.U(32.W))
    val txfifo = Reg(UInt(32.W))
    val stat = RegInit(1.U(32.W))
    val ctrl = RegInit(0.U(32.W))

    io.extra.get.out.valid := (waddr(3,0) === 4.U && in.w.fire())
    io.extra.get.out.ch := in.w.bits.data(7,0)
    io.extra.get.in.valid := (raddr(3,0) === 0.U && in.r.fire())

    val mapping = Map(
      RegMap(0x0, io.extra.get.in.ch, RegMap.Unwritable),
      RegMap(0x4, txfifo),
      RegMap(0x8, stat),
      RegMap(0xc, ctrl)
    )

    RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
      waddr(3,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0))
    )
  }
}
