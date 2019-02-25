package bus.simplebus

import chisel3._
import chisel3.util._

class SimpleBusAddrBundle extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(3.W))
}

class SimpleBusDataBundle(val dataBits: Int) extends Bundle {
  val data = Output(UInt(dataBits.W))
}

class SimpleBusMaskDataBundle(dataBits: Int) extends SimpleBusDataBundle(dataBits) {
  val mask = Output(UInt((dataBits / 8).W))
}

class SimpleBus(val dataBits: Int = 32) extends Bundle {
  val a = Decoupled(new SimpleBusAddrBundle)
  val r = Flipped(Decoupled(new SimpleBusDataBundle(dataBits)))
  val w = Valid(new SimpleBusMaskDataBundle(dataBits))

  def isRead (): Bool = a.valid && !w.valid
  def isWrite(): Bool = a.valid &&  w.valid

  def toAXI4() = {
    val mem2axi = Module(new SimpleBus2AXI4Converter)
    mem2axi.io.in <> this
    mem2axi.io.out
  }
}
