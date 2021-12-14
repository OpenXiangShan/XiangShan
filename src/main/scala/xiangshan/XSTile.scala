package xiangshan

import chisel3._
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy.{BundleBridgeSink, LazyModule, LazyModuleImp, LazyModuleImpLike}
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.GenericLogicalTreeNode
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortParameters, IntSinkPortSimple}
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink.{BankBinder, TLBuffer, TLIdentityNode, TLNode, TLTempNode, TLXbar}
import huancun.debug.TLLogger
import huancun.{HCCacheParamsKey, HuanCun}
import system.HasSoCParameter
import top.BusPerfMonitor
import utils.{ResetGen, TLClientsMerger, TLEdgeBuffer}

class L1CacheErrorInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val paddr = Valid(UInt(soc.PAddrBits.W))
  // for now, we only detect ecc
  val ecc_error = Valid(Bool())
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1CacheErrorInfo
  val dcache = new L1CacheErrorInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.paddr, s"IBUS", s"Icache bus error"),
      Some(icache.ecc_error, s"I_ECC", s"Icache ecc error"),
      Some(dcache.paddr, s"DBUS", s"Dcache bus error"),
      Some(dcache.ecc_error, s"D_ECC", s"Dcache ecc error")
    )
}

/**
  *   XSTileMisc contains every module except Core and L2 Cache
  */
class XSTileMisc()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  val l1_xbar = TLXbar()
  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3
  val memory_port = TLIdentityNode()
  val beu = LazyModule(new BusErrorUnit(
    new XSL1BusErrors(), BusErrorUnitParams(0x38010000), new GenericLogicalTreeNode
  ))
  val busPMU = BusPerfMonitor(enable = !debugOpts.FPGAPlatform)
  val l1d_logger = TLLogger(s"L2_L1D_${coreParams.HartId}", !debugOpts.FPGAPlatform)
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  val i_mmio_port = TLTempNode()
  val d_mmio_port = TLTempNode()

  busPMU := l1d_logger
  l1_xbar :=* busPMU

  l2_binder match {
    case Some(binder) =>
      memory_port := TLBuffer() := TLClientsMerger() := TLXbar() :=* binder
    case None =>
      memory_port := l1_xbar
  }

  mmio_xbar := TLBuffer.chainNode(2) := i_mmio_port
  mmio_xbar := TLBuffer.chainNode(2) := d_mmio_port
  beu.node := TLBuffer.chainNode(1) := mmio_xbar
  mmio_port := TLBuffer() := mmio_xbar

  lazy val module = new LazyModuleImp(this){
    val beu_errors = IO(Input(chiselTypeOf(beu.module.io.errors)))
    beu.module.io.errors <> beu_errors
  }
}

class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  private val core = LazyModule(new XSCore())
  private val misc = LazyModule(new XSTileMisc())
  private val l2cache = coreParams.L2CacheParamsOpt.map(l2param =>
    LazyModule(new HuanCun()(new Config((_, _, _) => {
      case HCCacheParamsKey => l2param
    })))
  )

  // public ports
  val memory_port = misc.memory_port
  val uncache = misc.mmio_port
  val clint_int_sink = core.clint_int_sink
  val plic_int_sink = core.plic_int_sink
  val debug_int_sink = core.debug_int_sink
  val beu_int_source = misc.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Bool()))

  if (coreParams.dcacheParametersOpt.nonEmpty) {
    misc.l1d_logger :=
      TLBuffer.chainNode(1, Some("L1D_to_L2_buffer")) :=
      core.memBlock.dcache.clientNode
  }
  misc.busPMU :=
    TLLogger(s"L2_L1I_${coreParams.HartId}", !debugOpts.FPGAPlatform) :=
    TLBuffer.chainNode(1, Some("L1I_to_L2_buffer")) :=
    core.frontend.icache.clientNode

  if (!coreParams.softPTW) {
    misc.busPMU :=
      TLLogger(s"L2_PTW_${coreParams.HartId}", !debugOpts.FPGAPlatform) :=
      TLBuffer.chainNode(3, Some("PTW_to_L2_buffer")) :=
      core.ptw.node
  }
  l2cache match {
    case Some(l2) =>
      misc.l2_binder.get :*= l2.node :*= TLBuffer() :*= misc.l1_xbar
    case None =>
  }

  misc.i_mmio_port := core.frontend.instrUncache.clientNode
  misc.d_mmio_port := core.memBlock.uncache.clientNode

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
    })

    dontTouch(io.hartId)

    val core_soft_rst = core_reset_sink.in.head._1

    core.module.io.hartId := io.hartId
    if(l2cache.isDefined){
      core.module.io.perfEvents.zip(l2cache.get.module.io.perfEvents.flatten).foreach(x => x._1.value := x._2)
    }
    else {
      core.module.io.perfEvents <> DontCare
    }

    misc.module.beu_errors <> core.module.io.beu_errors

    // Modules are reset one by one
    // io_reset ----
    //             |
    //             v
    // reset ----> OR_SYNC --> {Misc, L2 Cache, Cores}
    val l2cacheMod = if (l2cache.isDefined) Seq(l2cache.get.module) else Seq()
    val resetChain = Seq(
      Seq(misc.module, core.module) ++ l2cacheMod
    )
    ResetGen(resetChain, reset.asBool || core_soft_rst, !debugOpts.FPGAPlatform)
  }
}
