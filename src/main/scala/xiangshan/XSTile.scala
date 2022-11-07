package xiangshan

import chisel3._
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.experimental.hierarchy.{Definition, instantiable, public, Instance}
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import huancun.debug.TLLogger
import huancun.mbist.MBISTInterface
import huancun.{HCCacheParamsKey, HuanCun}
import huancun.utils.{ResetGen, DFTResetSignals}
import system.HasSoCParameter
import top.BusPerfMonitor
import utils.{IntBuffer, TLClientsMerger, TLEdgeBuffer}

class L1BusErrorUnitInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val ecc_error = Valid(UInt(soc.PAddrBits.W))
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1BusErrorUnitInfo
  val dcache = new L1BusErrorUnitInfo
  val l2 = new L1BusErrorUnitInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.ecc_error, "I_ECC", "Icache ecc error"),
      Some(dcache.ecc_error, "D_ECC", "Dcache ecc error"),
      Some(l2.ecc_error, "L2_ECC", "L2Cache ecc error")
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
    new XSL1BusErrors(), BusErrorUnitParams(0x38010000)
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
      memory_port := TLBuffer.chainNode(2) := TLClientsMerger() := TLXbar() :=* binder
    case None =>
      memory_port := l1_xbar
  }

  mmio_xbar := TLBuffer.chainNode(2) := i_mmio_port
  mmio_xbar := TLBuffer.chainNode(2) := d_mmio_port
  beu.node := TLBuffer.chainNode(3) := mmio_xbar
  mmio_port := TLBuffer.chainNode(3) := mmio_xbar

  lazy val module = new LazyModuleImp(this){
    val beu_errors = IO(Input(chiselTypeOf(beu.module.io.errors)))
    beu.module.io.errors <> beu_errors
  }
}

class XSTile(val parentName:String = "Unknown")(implicit p: Parameters) extends LazyHardenModule[XSTileImp]
//class XSTile(val parentName:String = "Unknown")(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {
  val core = LazyModule(new XSCore(parentName + "core_"))
  val misc = LazyModule(new XSTileMisc())
  val l2cache = coreParams.L2CacheParamsOpt.map(l2param =>
    LazyModule(new HuanCun(parentName = parentName + "L2_")(new Config((_, _, _) => {
      case HCCacheParamsKey => l2param.copy(enableTopDown = env.EnableTopDown)
    })))
  )

  // public ports
  val memory_port = misc.memory_port
  val uncache = misc.mmio_port
  val clint_int_sink = IntIdentityNode()
  val plic_int_sink = IntIdentityNode()
  val debug_int_sink = IntIdentityNode()
  val beu_int_source = IntIdentityNode()
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))

  core.clint_int_sink :*= IntBuffer() :*= clint_int_sink
  core.plic_int_sink :*= IntBuffer() :*= plic_int_sink
  core.debug_int_sink :*= IntBuffer() :*= debug_int_sink
  beu_int_source :*= IntBuffer() :*= misc.beu.intNode



  val l1d_to_l2_bufferOpt = coreParams.dcacheParametersOpt.map { _ =>
    val buffer = LazyModule(new TLBuffer)
    misc.l1d_logger := buffer.node := core.memBlock.dcache.clientNode
    buffer
  }

  def chainBuffer(depth: Int, n: String): (Seq[LazyModule], TLNode) = {
    val buffers = Seq.fill(depth){ LazyModule(new TLBuffer()) }
    buffers.zipWithIndex.foreach{ case (b, i) => {
      b.suggestName(s"${n}_${i}")
    }}
    val node = buffers.map(_.node.asInstanceOf[TLNode]).reduce(_ :*=* _)
    (buffers, node)
  }

  val (l1i_to_l2_buffers, l1i_to_l2_buf_node) = chainBuffer(3, "l1i_to_l2_buffer")
  misc.busPMU :=
    TLLogger(s"L2_L1I_${coreParams.HartId}", !debugOpts.FPGAPlatform) :=
    l1i_to_l2_buf_node :=
    core.frontend.icache.clientNode

  val ptw_to_l2_buffers = if (!coreParams.softPTW) {
    val (buffers, buf_node) = chainBuffer(5, "ptw_to_l2_buffer")
    misc.busPMU :=
      TLLogger(s"L2_PTW_${coreParams.HartId}", !debugOpts.FPGAPlatform) :=
      buf_node :=
      core.ptw_to_l2_buffer.node
    buffers
  } else Seq()

  l2cache match {
    case Some(l2) =>
      misc.l2_binder.get :*= l2.node :*= TLBuffer() :*= TLBuffer() :*= misc.l1_xbar
      l2.pf_recv_node.map(recv => {
        println("Connecting L1 prefetcher to L2!")
        recv := core.memBlock.pf_sender_opt.get
      })
    case None =>
  }

  misc.i_mmio_port := core.frontend.instrUncache.clientNode
  misc.d_mmio_port := core.memBlock.uncache.clientNode

  lazy val module = new XSTileImp(this)
}

@instantiable
class XSTileImp(outer: XSTile) extends LazyHardenModuleImp(outer) {
  @public val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val cpu_halt = Output(Bool())
    val dfx_reset = Input(new DFTResetSignals())
  })
  @public val ireset = reset
  dontTouch(io.hartId)

  val core_soft_rst = outer.core_reset_sink.in.head._1

  outer.core.module.io.hartId := io.hartId
  outer.core.module.io.dfx_reset := io.dfx_reset
  io.cpu_halt := outer.core.module.io.cpu_halt
  if(outer.l2cache.isDefined){
    outer.core.module.io.perfEvents.zip(outer.l2cache.get.module.io.perfEvents.flatten).foreach(x => x._1.value := x._2)
  }
  else {
    outer.core.module.io.perfEvents <> DontCare
  }

  outer.misc.module.beu_errors.icache <> outer.core.module.io.beu_errors.icache
  outer.misc.module.beu_errors.dcache <> outer.core.module.io.beu_errors.dcache
  if(outer.l2cache.isDefined){
    outer.misc.module.beu_errors.l2.ecc_error.valid := outer.l2cache.get.module.io.ecc_error.valid
    outer.misc.module.beu_errors.l2.ecc_error.bits := outer.l2cache.get.module.io.ecc_error.bits
  } else {
    outer.misc.module.beu_errors.l2 <> 0.U.asTypeOf(outer.misc.module.beu_errors.l2)
  }

  val coreMbistIntf = if(outer.coreParams.hasMbist && outer.coreParams.hasShareBus){
    val params = outer.core.module.mbistPipeline.get.bd.params
    val node = outer.core.module.mbistPipeline.get.node
    val intf = Some(Module(new MBISTInterface(
      params = Seq(params),
      ids = Seq(node.children.flatMap(_.array_id)),
      name = s"MBIST_intf_core",
      pipelineNum = 1
    )))
    intf.get.toPipeline.head <> outer.core.module.mbist.get
    outer.core.module.mbistPipeline.get.genCSV(intf.get.info, "MBIST_Core")
    intf.get.mbist := DontCare
    dontTouch(intf.get.mbist)
    //TODO: add mbist controller connections here
    intf
  } else {
    None
  }

  val l2MbistIntf = if(outer.l2cache.isDefined){
    if(p(XSCoreParamsKey).L2CacheParamsOpt.get.hasMbist && p(XSCoreParamsKey).L2CacheParamsOpt.get.hasShareBus){
      val params = outer.l2cache.get.module.l2TopPipeLine.get.bd.params
      val node = outer.l2cache.get.module.l2TopPipeLine.get.node
      val intf = Some(Module(new MBISTInterface(
        params = Seq(params),
        ids = Seq(node.children.flatMap(_.array_id)),
        name = s"MBIST_intf_l2",
        pipelineNum = 1
      )))
      intf.get.toPipeline.head <> outer.l2cache.get.module.l2pipePorts.get
      outer.l2cache.get.module.l2TopPipeLine.get.genCSV(intf.get.info, "MBIST_L2")
      intf.get.mbist := DontCare
      dontTouch(intf.get.mbist)
      //TODO: add mbist controller connections here
      intf
    } else {
      None
    }
  } else {
    None
  }

  val mbistBroadCastToCore = if(outer.coreParams.hasMbist) {
    val res = Some(Wire(new huancun.utils.BroadCastBundle))
    outer.core.module.dft.get := res.get
    res
  } else {
    None
  }
  val mbistBroadCastToL2 = if(outer.coreParams.L2CacheParamsOpt.isDefined) {
    if(outer.coreParams.L2CacheParamsOpt.get.hasMbist){
      val res = Some(Wire(new huancun.utils.BroadCastBundle))
      outer.core.module.dft.get := res.get
      res
    } else {
      None
    }
  } else {
    None
  }
  @public val dft = if(mbistBroadCastToCore.isDefined || mbistBroadCastToL2.isDefined){
    Some(IO(new huancun.utils.BroadCastBundle))
  } else {
    None
  }
  if(dft.isDefined){
    if(mbistBroadCastToCore.isDefined){
      mbistBroadCastToCore.get := dft.get
    }
    if(mbistBroadCastToL2.isDefined){
      mbistBroadCastToL2.get := dft.get
    }
  }
  // Modules are reset one by one
  // io_reset ----
  //             |
  //             v
  // reset ----> OR_SYNC --> {Misc, L2 Cache, Cores}
  val resetChain = Seq(
    Seq(outer.misc.module, outer.core.module) ++
      outer.l1i_to_l2_buffers.map(_.module.asInstanceOf[MultiIOModule]) ++
      outer.ptw_to_l2_buffers.map(_.module.asInstanceOf[MultiIOModule]) ++
      outer.l1d_to_l2_bufferOpt.map(_.module) ++
      outer.l2cache.map(_.module)
  )
  ResetGen(resetChain, reset, Some(io.dfx_reset), !outer.debugOpts.FPGAPlatform)
}
