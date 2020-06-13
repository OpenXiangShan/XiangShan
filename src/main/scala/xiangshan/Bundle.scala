package xiangshan

import chisel3._

class FetchPacket extends XSBundle {
  val instrs = Vec(FetchWidth, UInt(32.W))
  val mask = UInt(FetchWidth.W)
  val pc = UInt(VAddrBits.W) // the pc of first inst in the fetch group
}

class Redirect extends XSBundle {
  val target = UInt(VAddrBits.W)
}