package xiangshan.cache

import chisel3._

/*
 * L1 Compact CHI node IDs for Type 1 / Type 4 ports.
 */
object L1CCHINodeId {
  val DCacheSrcId: UInt = 0.U
  val ICacheSrcId: UInt = 1.U
  val PtwSrcId: UInt = 2.U

  val L2TgtId: UInt = 0.U
}
