package oceanus.compactchi

import chisel3._

/*
 * Per-core L1 upstream Compact CHI SrcIDs.
 *
 * Each cacheable L1 client (D$, I$, PTW, …) is a distinct upstream node and must
 * use its own SrcID so L2 can route CompData / Snoop. TgtID is still a placeholder
 * until the L2 Compact CHI port is wired.
 */
object L1CCHINodeId {
  val DCacheSrcId: UInt = 0.U
  val ICacheSrcId: UInt = 1.U
  // val PtwSrcId: UInt = 2.U // reserved for PTW Type 4

  val L2TgtId: UInt = 0.U
}
