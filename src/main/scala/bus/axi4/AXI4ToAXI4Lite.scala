package bus.axi4

import chisel3._
import chisel3.experimental.DataMirror
import chisel3.util._

class AXI4ToAXI4Lite(inType: AXI4) extends MultiIOModule{
  val in = IO(Flipped(inType))
  val out = IO(new AXI4Lite)

  def connect(lite: Data, full: Data): Unit = {
    (lite, full) match {
      case (e1: Element, e2: Element) =>
        e1 <> e2
      case (r1: Record, r2: Record) =>
        r2 <> DontCare
        for((s, d) <- r1.elements){
          connect(d, r2.elements(s))
        }
    }
  }

  connect(out, in)
}

object AXI4ToAXI4Lite {
  def apply(in: AXI4): AXI4Lite = {
    val m = Module(new AXI4ToAXI4Lite(in.cloneType))
    m.in <> in
    m.out
  }
}
