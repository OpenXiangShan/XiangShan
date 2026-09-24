package xiangshan.cache

import chisel3._
import oceanus.l2.{L2UpstreamPortType, L2UpstreamTable, L2UpstreamTableEntry}

/*
 * Factory for the tile Compact CHI upstream table stored on XSCoreParameters.cchiUpstream.
 * The same Scala object is passed to oceanus.l2.L2Top (L2Configuration.upstream) and
 * used by L1 D$/I$/PTW SrcIDs (HasXSParameter.cchiDcacheSrcId / cchiIcacheSrcId / cchiPtwSrcId).
 *
 * Type1 order is D$ channel index. DefaultConfig (2 D$ channels):
 *   Type1 nid 0 = D$ ch0, Type1 nid 1 = D$ ch1, Type4 nid 4 = I$, Type4 nid 5 = PTW.
 */
object L1CCHIUpstream {
  val ICacheNid = 4
  val PtwNid = 5

  def apply(nDcacheCh: Int): L2UpstreamTable = {
    require(nDcacheCh >= 1 && nDcacheCh <= 2,
      s"L1 CCHI Type1 count must be 1 or 2, got $nDcacheCh")
    val t1 = Seq.tabulate(nDcacheCh)(ch =>
      L2UpstreamTableEntry(L2UpstreamPortType.Type1, ch)
    )
    val t4 = Seq(
      L2UpstreamTableEntry(L2UpstreamPortType.Type4, ICacheNid),
      L2UpstreamTableEntry(L2UpstreamPortType.Type4, PtwNid)
    )
    new L2UpstreamTable(t1 ++ t4)
  }
}
