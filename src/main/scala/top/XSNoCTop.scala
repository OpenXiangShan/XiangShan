/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package top

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import xiangshan._
import utils._
import utility._
import system._
import device._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import coupledL2.tl2chi.{CHIAsyncBridgeSink, PortIO}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.util.{AsyncQueueParams, AsyncQueueSource}
import chisel3.experimental.annotate
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation
import utility.sram.SramBroadcastBundle

import difftest.common.DifftestWiring
import difftest.util.Profile

class XSNoCTop()(implicit p: Parameters) extends BaseXSSoc with HasSoCParameter
{
  override lazy val desiredName: String = "XSTop"

  ResourceBinding {
    val width = ResourceInt(2)
    val model = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
  }

  require(enableCHI)

  // xstile
  val core_with_l2 = LazyModule(new XSTileWrap()(p.alter((site, here, up) => {
    case XSCoreParamsKey => tiles.head
    case PerfCounterOptionsKey => up(PerfCounterOptionsKey).copy(perfDBHartID = tiles.head.HartId)
  })))

  // imsic bus top
  val u_imsic_bus_top = LazyModule(new imsic_bus_top)

  // interrupts
  val clintIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 2))
  val debugIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))
  val plicIntNode = IntSourceNode(IntSourcePortSimple(1, 2, 1))
  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, 1, (new NonmaskableInterruptIO).elements.size))
  val beuIntNode = IntSinkNode(IntSinkPortSimple(1, 1))
  core_with_l2.clintIntNode := clintIntNode
  core_with_l2.debugIntNode := debugIntNode
  core_with_l2.plicIntNode :*= plicIntNode
  core_with_l2.nmiIntNode := nmiIntNode
  beuIntNode := core_with_l2.beuIntNode
  val clint = InModuleBody(clintIntNode.makeIOs())
  val debug = InModuleBody(debugIntNode.makeIOs())
  val plic = InModuleBody(plicIntNode.makeIOs())
  val nmi = InModuleBody(nmiIntNode.makeIOs())
  val beu = InModuleBody(beuIntNode.makeIOs())

  // seperate DebugModule bus
  val EnableDMAsync = EnableDMAsyncBridge.isDefined
  // asynchronous bridge sink node
  val dmAsyncSinkOpt = Option.when(SeperateDMBus && EnableDMAsync)(
    LazyModule(new TLAsyncCrossingSink(EnableDMAsyncBridge.get))
  )
  dmAsyncSinkOpt.foreach(_.node := core_with_l2.dmAsyncSourceOpt.get.node)
  // synchronous sink node
  val dmSyncSinkOpt = Option.when(SeperateDMBus && !EnableDMAsync)(TLTempNode())
  dmSyncSinkOpt.foreach(_ := core_with_l2.dmSyncSourceOpt.get)

  // The Manager Node is only used to make IO. Standalone DM should be used for XSNoCTopConfig
  val dm = Option.when(SeperateDMBus)(TLManagerNode(Seq(
    TLSlavePortParameters.v1(
      managers = Seq(
        TLSlaveParameters.v1(
          address = Seq(p(DebugModuleKey).get.address),
          regionType = RegionType.UNCACHED,
          supportsGet = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          supportsPutPartial = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          supportsPutFull = TransferSizes(1, p(SoCParamsKey).L3BlockSize),
          fifoId = Some(0)
        )
      ),
      beatBytes = 8
    )
  )))
  val dmXbar = Option.when(SeperateDMBus)(TLXbar())
  dmAsyncSinkOpt.foreach(sink => dmXbar.get := sink.node)
  dmSyncSinkOpt.foreach(sink => dmXbar.get := sink)
  dm.foreach(_ := dmXbar.get)
  // seperate debug module io
  val io_dm = dm.map(x => InModuleBody(x.makeIOs()))

  // reset nodes
  val core_rst_node = BundleBridgeSource(() => Reset())
  core_with_l2.tile.core_reset_sink := core_rst_node

  class XSNoCTopImp(wrapper: XSNoCTop) extends LazyRawModuleImp(wrapper) {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(this)(Seq(NestedPrefixModulesAnnotation(mod, prefix, true)))
    }
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val noc_clock = EnableCHIAsyncBridge.map(_ => IO(Input(Clock())))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_clock = IO(Input(Clock()))
    val soc_reset = IO(Input(AsyncReset()))
    private val hasMbist = tiles.head.hasMbist
    val io = IO(new Bundle {
      val hartId = Input(UInt(p(MaxHartIdBits).W))
      val riscv_halt = Output(Bool())
      val riscv_critical_error = Output(Bool())
      val hartResetReq = Input(Bool())
      val hartIsInReset = Output(Bool())
      val riscv_rst_vec = Input(UInt(soc.PAddrBits.W))
      val chi = new PortIO
      val nodeID = Input(UInt(soc.NodeIDWidthList(issue).W))
      val clintTime = Input(ValidIO(UInt(64.W)))
      val traceCoreInterface = new Bundle {
        val fromEncoder = Input(new Bundle {
          val enable = Bool()
          val stall  = Bool()
        })
        val toEncoder   = Output(new Bundle {
          val cause     = UInt(TraceCauseWidth.W)
          val tval      = UInt(TraceTvalWidth.W)
          val priv      = UInt(TracePrivWidth.W)
          val iaddr     = UInt((TraceTraceGroupNum * TraceIaddrWidth).W)
          val itype     = UInt((TraceTraceGroupNum * TraceItypeWidth).W)
          val iretire   = UInt((TraceTraceGroupNum * TraceIretireWidthCompressed).W)
          val ilastsize = UInt((TraceTraceGroupNum * TraceIlastsizeWidth).W)
        })
      }
      val dft = if(hasMbist) Some(Input(new SramBroadcastBundle)) else None
      val dft_reset = if(hasMbist) Some(Input(new DFTResetSignals())) else None
    })
    // imsic axi4 io
    val imsic_axi4 = wrapper.u_imsic_bus_top.axi4.map(x => IO(Flipped(new VerilogAXI4Record(x.elts.head.params.copy(addrBits = 32)))))
    // imsic tl io
    val imsic_m_tl = wrapper.u_imsic_bus_top.tl_m.map(x => IO(chiselTypeOf(x.getWrappedValue)))
    val imsic_s_tl = wrapper.u_imsic_bus_top.tl_s.map(x => IO(chiselTypeOf(x.getWrappedValue)))
    // imsic bare io
    val imsic = wrapper.u_imsic_bus_top.module.msi.map(x => IO(chiselTypeOf(x)))

    val noc_reset_sync = EnableCHIAsyncBridge.map(_ => withClockAndReset(noc_clock, noc_reset) { ResetGen(2, io.dft_reset) })
    val soc_reset_sync = withClockAndReset(soc_clock, soc_reset) { ResetGen(2, io.dft_reset) }
    wrapper.core_with_l2.module.io.dft.zip(io.dft).foreach({case(a, b) => a := b})
    wrapper.core_with_l2.module.io.dft_reset.zip(io.dft_reset).foreach({case(a, b) => a := b})
    // device clock and reset
    wrapper.u_imsic_bus_top.module.clock := soc_clock
    wrapper.u_imsic_bus_top.module.reset := soc_reset_sync

    // imsic axi4 io connection
    imsic_axi4.foreach(_.viewAs[AXI4Bundle] <> wrapper.u_imsic_bus_top.axi4.get.elements.head._2)
    // imsic tl io connection
    wrapper.u_imsic_bus_top.tl_m.foreach(_ <> imsic_m_tl.get)
    wrapper.u_imsic_bus_top.tl_s.foreach(_ <> imsic_s_tl.get)
    // imsic bare io connection
    wrapper.u_imsic_bus_top.module.msi.foreach(_ <> imsic.get)

    // input
    dontTouch(io)

    core_with_l2.module.clock := clock
    core_with_l2.module.reset := reset
    core_with_l2.module.noc_reset.foreach(_ := noc_reset.get)
    core_with_l2.module.soc_reset := soc_reset
    core_with_l2.module.io.hartId := io.hartId
    core_with_l2.module.io.nodeID.get := io.nodeID
    io.riscv_halt := core_with_l2.module.io.cpu_halt
    io.riscv_critical_error := core_with_l2.module.io.cpu_crtical_error
    core_with_l2.module.io.hartResetReq := io.hartResetReq
    io.hartIsInReset := core_with_l2.module.io.hartIsInReset
    core_with_l2.module.io.reset_vector := io.riscv_rst_vec
    // trace Interface
    val traceInterface = core_with_l2.module.io.traceCoreInterface
    traceInterface.fromEncoder := io.traceCoreInterface.fromEncoder
    io.traceCoreInterface.toEncoder.priv := traceInterface.toEncoder.priv
    io.traceCoreInterface.toEncoder.cause := traceInterface.toEncoder.trap.cause
    io.traceCoreInterface.toEncoder.tval := traceInterface.toEncoder.trap.tval
    io.traceCoreInterface.toEncoder.iaddr := VecInit(traceInterface.toEncoder.groups.map(_.bits.iaddr)).asUInt
    io.traceCoreInterface.toEncoder.itype := VecInit(traceInterface.toEncoder.groups.map(_.bits.itype)).asUInt
    io.traceCoreInterface.toEncoder.iretire := VecInit(traceInterface.toEncoder.groups.map(_.bits.iretire)).asUInt
    io.traceCoreInterface.toEncoder.ilastsize := VecInit(traceInterface.toEncoder.groups.map(_.bits.ilastsize)).asUInt

    EnableClintAsyncBridge match {
      case Some(param) =>
        withClockAndReset(soc_clock, soc_reset_sync) {
          val source = Module(new AsyncQueueSource(UInt(64.W), param))
          source.io.enq.valid := io.clintTime.valid
          source.io.enq.bits := io.clintTime.bits
          core_with_l2.module.io.clintTime <> source.io.async
        }
      case None =>
        core_with_l2.module.io.clintTime <> io.clintTime
    }

    EnableCHIAsyncBridge match {
      case Some(param) =>
        withClockAndReset(noc_clock.get, noc_reset_sync.get) {
          val sink = Module(new CHIAsyncBridgeSink(param))
          sink.io.async <> core_with_l2.module.io.chi
          io.chi <> sink.io.deq
        }
      case None =>
        io.chi <> core_with_l2.module.io.chi
    }

    // Seperate DebugModule TL Async Queue Sink
    if (SeperateDMBus && EnableDMAsync) {
      dmAsyncSinkOpt.get.module.clock := soc_clock
      dmAsyncSinkOpt.get.module.reset := soc_reset_sync
    }

    core_with_l2.module.io.msiInfo.valid := wrapper.u_imsic_bus_top.module.msiio.vld_req
    core_with_l2.module.io.msiInfo.bits := wrapper.u_imsic_bus_top.module.msiio.data
    wrapper.u_imsic_bus_top.module.msiio.vld_ack := core_with_l2.module.io.msiAck
    // tie off core soft reset
    core_rst_node.out.head._1 := false.B.asAsyncReset

    core_with_l2.module.io.debugTopDown.l3MissMatch := false.B
    core_with_l2.module.io.l3Miss := false.B
  }

  lazy val module = new XSNoCTopImp(this)
}

class XSNoCDiffTop(implicit p: Parameters) extends Module {
  override val desiredName: String = "XSDiffTop"
  val l_soc = LazyModule(new XSNoCTop())
  val soc = Module(l_soc.module)

  // Expose XSTop IOs outside, i.e. io
  def exposeIO(data: Data, name: String): Unit = {
    val dummy = IO(chiselTypeOf(data)).suggestName(name)
    dummy <> data
  }
  def exposeOptionIO(data: Option[Data], name: String): Unit = {
    if (data.isDefined) {
      val dummy = IO(chiselTypeOf(data.get)).suggestName(name)
      dummy <> data.get
    }
  }
  exposeIO(l_soc.clint, "clint")
  exposeIO(l_soc.debug, "debug")
  exposeIO(l_soc.plic, "plic")
  exposeIO(l_soc.beu, "beu")
  exposeIO(l_soc.nmi, "nmi")
  soc.clock := clock
  soc.reset := reset.asAsyncReset
  exposeIO(soc.soc_clock, "soc_clock")
  exposeIO(soc.soc_reset, "soc_reset")
  exposeIO(soc.io, "io")
  exposeOptionIO(soc.noc_clock, "noc_clock")
  exposeOptionIO(soc.noc_reset, "noc_reset")
  exposeOptionIO(soc.imsic_axi4, "imsic_axi4")
  exposeOptionIO(soc.imsic_m_tl, "imsic_m_tl")
  exposeOptionIO(soc.imsic_s_tl, "imsic_s_tl")
  exposeOptionIO(soc.imsic, "imsic")

  // TODO:
  // XSDiffTop is only part of DUT, we can not instantiate difftest here.
  // Temporarily we collect Performance counters for each DiffTop, need control signals passed from Difftest
  val timer = IO(Input(UInt(64.W)))
  val logEnable = IO(Input(Bool()))
  val clean = IO(Input(Bool()))
  val dump = IO(Input(Bool()))
  XSLog.collect(timer, logEnable, clean, dump)
  DifftestWiring.createAndConnectExtraIOs()
  Profile.generateJson("XiangShan")
  XSNoCDiffTopChecker()
}

// TODO:
// Currently we use two-step XiangShan-Difftest, generating XS(with Diff Interface only) and Difftest seperately
// To avoid potential interface problem between XS and Diff, we add Checker and CI(dual-core)
// We will try one-step XS-Diff later
object XSNoCDiffTopChecker {
  def apply(): Unit = {
    val verilog =
      """
        |`define CONFIG_XSCORE_NR 2
        |`include "gateway_interface.svh"
        |module XSDiffTopChecker(
        | input                                 cpu_clk,
        | input                                 cpu_rstn,
        | input                                 sys_clk,
        | input                                 sys_rstn
        |);
        |wire [63:0] timer;
        |wire logEnable;
        |wire clean;
        |wire dump;
        |// FIXME: use siganls from Difftest rather than default value
        |assign timer = 64'b0;
        |assign logEnable = 1'b0;
        |assign clean = 1'b0;
        |assign dump = 1'b0;
        |gateway_if gateway_if_i();
        |core_if core_if_o[`CONFIG_XSCORE_NR]();
        |generate
        |    genvar i;
        |    for (i = 0; i < `CONFIG_XSCORE_NR; i = i+1)
        |    begin: u_CPU_TOP
        |    // FIXME: add missing ports
        |    XSDiffTop u_XSTop (
        |        .clock                   (cpu_clk),
        |        .noc_clock               (sys_clk),
        |        .soc_clock               (sys_clk),
        |        .io_hartId               (6'h0 + i),
        |        .timer                   (timer),
        |        .logEnable               (logEnable),
        |        .clean                   (clean),
        |        .dump                    (dump),
        |        .gateway_out             (core_if_o[i])
        |    );
        |    end
        |endgenerate
        |    CoreToGateway u_CoreToGateway(
        |    .gateway_out (gateway_if_i.out),
        |    .core_in (core_if_o)
        |    );
        |    GatewayEndpoint u_GatewayEndpoint(
        |    .clock (sys_clk),
        |    .reset (sys_rstn),
        |    .gateway_in (gateway_if_i.in),
        |    .step ()
        |    );
        |
        |endmodule
      """.stripMargin
    FileRegisters.writeOutputFile("./build", "XSDiffTopChecker.sv", verilog)
  }
}
