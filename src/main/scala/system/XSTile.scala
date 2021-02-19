package system

import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{ClockCrossingType, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalTreeNode
import freechips.rocketchip.interrupts.{IntIdentityNode, IntInwardNode, IntOutwardNode}
import freechips.rocketchip.prci.ClockSinkParameters
import freechips.rocketchip.rocket.{BTBParams, DCacheParams, ICacheParams, MulDivParams}
import freechips.rocketchip.tile.{BaseTile, BaseTileModuleImp, CoreParams, FPUParams, LookupByHartIdImpl, SinksExternalInterrupts, SourcesExternalNotifications, TileParams}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLInwardNode, TLOutwardNode}



case class XSCoreParameters
(
  bootFreqHz: BigInt = 0,
  useVM: Boolean = true,
  useSupervisor: Boolean = true,
  useDebug: Boolean = false,
  useAtomics: Boolean = true,
  useAtomicsOnlyForIO: Boolean = true,
  useSCIE: Boolean = true,
  useRVE: Boolean = true,
  nLocalInterrupts: Int = 0,
) extends CoreParams {
  val lrscCycles: Int = 80
  override val useUser: Boolean = true
  override val useCompressed: Boolean = true
  override val mulDiv: Option[MulDivParams] = Some(MulDivParams())
  override val fpu: Option[FPUParams] = Some(FPUParams())
  override val fetchWidth: Int = 8
  override val decodeWidth: Int = 6
  override val retireWidth: Int = 6
  override val instBits: Int = 16
  override val useNMI: Boolean = true
  override val nPMPs: Int = 0
  override val pmpGranularity: Int = 0
  override val nBreakpoints: Int = 0
  override val useBPWatch: Boolean = false
  override val mcontextWidth: Int = 64
  override val scontextWidth: Int = 64
  override val nPerfCounters: Int = 0
  override val haveBasicCounters: Boolean = false
  override val haveFSDirty: Boolean = false
  override val misaWritable: Boolean = false
  override val haveCFlush: Boolean = false
  override val nL2TLBEntries: Int = 512
  override val mtvecInit: Option[BigInt] = None
  override val mtvecWritable: Boolean = true
}

case class XSTileParameters
(
  core: XSCoreParameters = XSCoreParameters(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  btb: Option[BTBParams] = Some(BTBParams()),
  hartId: Int = 0,
  beuAddr: Option[BigInt] = None,
  blockerCtrlAddr: Option[BigInt] = None,
  name: Option[String] = Some("xs_tile"),
) extends TileParams {
  override val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
}

class XSTile
(
  val xsParams: XSTileParameters,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters,
  logicalTreeNode: LogicalTreeNode
) extends BaseTile(xsParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
{

  val intOutwardNode = IntIdentityNode()
  val slaveNode = TLIdentityNode()
  val masterNode = visibilityNode

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq())


  override def module: BaseTileModuleImp[BaseTile] = ???





}
