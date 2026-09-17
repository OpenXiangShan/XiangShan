package xiangshan.cache

import chisel3._
import oceanus.compactchi._

/*
 * L1-only Compact CHI pieces that left the shared Flit.scala (now in XSCache).
 * Type3 64b DAT is a XiangShan shim until XSCache Type3 is 64b.
 */
object CompactCHIDatWidth {
  val Coherent = 256
  val Type3Uncache = 64
  def type3BeWidth = Type3Uncache / 8
}

object L1CCHINodeId {
  val DCacheSrcId: UInt = 0.U
  val ICacheSrcId: UInt = 1.U
  val PtwSrcId: UInt = 2.U
  val UncacheSrcId: UInt = 3.U
  val InstrUncacheSrcId: UInt = 4.U

  val L2TgtId: UInt = 0.U
}

class FlitDnDAT64 extends FlitDnDATWithoutData {
  val Data = UInt(CompactCHIDatWidth.Type3Uncache.W)
}

class FlitUpDAT64 extends FlitUpDATWithoutData {
  val Data = UInt(CompactCHIDatWidth.Type3Uncache.W)
  val BE = UInt(CompactCHIDatWidth.type3BeWidth.W)
}
