package top

import chisel3._
import chipsalliance.rocketchip.config
import device._
import freechips.rocketchip.amba.axi4.{AXI4EdgeParameters, AXI4MasterNode, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}

class SimMMIO(edge: AXI4EdgeParameters)(implicit p: config.Parameters) extends LazyModule {

  val node = AXI4MasterNode(List(edge.master))

  val flash = LazyModule(new AXI4Flash(Seq(AddressSet(0x10000000L, 0xfffffff))))
  val uart = LazyModule(new AXI4UART(Seq(AddressSet(0x40600000L, 0xf))))
  val vga = LazyModule(new AXI4VGA(
    sim = false,
    fbAddress = Seq(AddressSet(0x50000000L, 0x3fffffL)),
    ctrlAddress = Seq(AddressSet(0x40001000L, 0x7L))
  ))
  val sd = LazyModule(new AXI4DummySD(Seq(AddressSet(0x40002000L, 0xfff))))

  val axiBus = AXI4Xbar()

  uart.node := axiBus
  vga.node :*= axiBus
  flash.node := axiBus
  sd.node := axiBus

  axiBus := node

  val io_axi4 = InModuleBody {
    node.makeIOs()
  }

  def connectToSoC(soc: HaveAXI4PeripheralPort) = {
    io_axi4 <> soc.peripheral
  }

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle() {
      val uart = new UARTIO
    })
    io.uart <> uart.module.io.extra.get
  }

}
