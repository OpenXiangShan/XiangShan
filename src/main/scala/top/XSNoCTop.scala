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

import aia.{AXIRegIMSIC_WRAP, IMSICParams, MSITransBundle}
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import system._
import device._
import freechips.rocketchip.devices.debug.{DebugModuleKey, DebugTransportModuleJTAG, JtagDTMKeyDefault, SystemJTAGIO}
//import aia._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import coupledL2.tl2chi.{PortIO, CHIAsyncBridgeSink}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.util.{AsyncQueueSource, AsyncQueueParams}
import chisel3.experimental.{annotate, ChiselAnnotation}
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

import difftest.common.DifftestWiring
import difftest.util.Profile
import freechips.rocketchip.jtag.JTAGIO
import top.TopMain.config

class XSNoCTop()(implicit p: Parameters) extends BaseXSSoc with HasSoCParameter
{
  override lazy val desiredName: String = "XSTop"
  private val MSI_INFO_WIDTH = 11
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
  val u_imsic_bus_top = Option.when(!IMSICUseHalf)(LazyModule(new imsic_bus_top(
    useTL = soc.IMSICUseTL,
    baseAddress = (0x3A800000, 0x3B000000)
  )))
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

  // reset nodes
  val core_rst_node = BundleBridgeSource(() => Reset())
  core_with_l2.tile.core_reset_sink := core_rst_node

  class XSNoCTopImp(wrapper: XSNoCTop) extends LazyRawModuleImp(wrapper) {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(new ChiselAnnotation {
        def toFirrtl = NestedPrefixModulesAnnotation(mod, prefix, true)
      })
    }
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val noc_clock = EnableCHIAsyncBridge.map(_ => IO(Input(Clock())))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_clock = Option.when(!ClintAsyncFromSPMT)(IO(Input(Clock())))
    val soc_reset = Option.when(!ClintAsyncFromSPMT)(IO(Input(AsyncReset())))
    val i = Option.when(CHIAsyncFromSPMT)(IO(new Bundle {   //for spacemit
      val dft = Input(new Bundle{
        val icg_scan_en = Bool()
        val scan_enable = Bool()
      })
    }))
    val io = IO(new Bundle {
      val hartId = Input(UInt(p(MaxHartIdBits).W))
      val riscv_halt = Output(Bool())
      val riscv_critical_error = Output(Bool())
      val hartResetReq = Option.when(!UseDMInTop)(Input(Bool()))
      val hartIsInReset = Option.when(!UseDMInTop)(Output(Bool()))
      val dm_ndreset = Option.when(UseDMInTop)(Output(Bool()))
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
      val memcfg = Input(UInt(64.W))   // for memory adjust,such as rate,sequence,..
      // differentiate imsic version
      val msiinfo = Option.when(IMSICUseHalf)(new MSITransBundle(aia.IMSICParams()))
      val jtag = Option.when(UseDMInTop)(Flipped(new JTAGIO(hasTRSTn = true)))
    })
    // imsic axi4lite io
    val imsic_axi4lite = wrapper.u_imsic_bus_top.map(_.module.axi4lite.map(x => IO(chiselTypeOf(x))))
    // imsic tl io
    val imsic_m_tl = wrapper.u_imsic_bus_top.map(_.tl_m.map(x => IO(chiselTypeOf(x.getWrappedValue))))
    val imsic_s_tl = wrapper.u_imsic_bus_top.map(_.tl_s.map(x => IO(chiselTypeOf(x.getWrappedValue))))

    val noc_reset_sync = EnableCHIAsyncBridge.map(_ => withClockAndReset(noc_clock, noc_reset) { ResetGen() })
    val soc_reset_sync = withClockAndReset(soc_clock.get, soc_reset.get) { ResetGen() }
    val reset_sync = withClockAndReset(clock, reset) { ResetGen() }
    val jtag_reset = ~(io.jtag.flatMap(_.TRSTn).getOrElse(true.B)) //change since resetGen is active high

    val jtag_reset_sync = io.jtag.map(iojtag => withClockAndReset(iojtag.TCK, jtag_reset) { ResetGen() })
    // device clock and reset
    wrapper.u_imsic_bus_top.foreach(_.module.clock := soc_clock.get)
    wrapper.u_imsic_bus_top.foreach(_.module.reset := soc_reset_sync)

    // imsic axi4lite io connection
    wrapper.u_imsic_bus_top.foreach(_.module.axi4lite.foreach(_ <> imsic_axi4lite.get.get))

    // imsic tl io connection
    wrapper.u_imsic_bus_top.foreach(_.tl_m.foreach(_ <> imsic_m_tl.get.get))
    wrapper.u_imsic_bus_top.foreach(_.tl_s.foreach(_ <> imsic_s_tl.get.get))

    // temporary dontcare some io
    IMSICUseHalf match{
      case (true) =>
        io.msiinfo.foreach(_ <> core_with_l2.module.io.msiinfo)
      case (false) =>
        core_with_l2.module.io.msiinfo.vld_req := wrapper.u_imsic_bus_top.get.module.o_msi_info_vld
        core_with_l2.module.io.msiinfo.data := wrapper.u_imsic_bus_top.get.module.o_msi_info
    }

    io.jtag.foreach(_ <> DontCare)
    // TODO requirement from spacement: instanciated dtm to connected with debug,as other half module of debug module.
    //    def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {
    val c = new JtagDTMKeyDefault
    val dtm = Option.when(UseDMInTop)(Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, c)))
    //start TBD about JTAG zhaohong
    //      io.debugIO.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x }  // force TMS high when debug is disabled
    dtm.foreach { dtm =>
      dtm.io.jtag_clock := io.jtag.get.TCK
      dtm.io.jtag_reset := jtag_reset_sync.get
      dtm.io.jtag_mfr_id := 0.U
      dtm.io.jtag_part_number := 0.U
      dtm.io.jtag_version := 0.U
      dtm.rf_reset := 0.U
    }
         dtm.foreach(_.io.dmi<>core_with_l2.module.io.dm.get.dmi.get)  //wait core_with_l2 update dm interface
    //end TBD about JTAG zhaohong

    // input
    dontTouch(io)

    core_with_l2.module.clock := clock
    core_with_l2.module.reset := reset_sync
    core_with_l2.module.noc_reset.foreach(_ := noc_reset.get)
    core_with_l2.module.soc_reset.foreach(_ := soc_reset.get)
    core_with_l2.module.io.hartId := io.hartId
    core_with_l2.module.io.nodeID.get := io.nodeID
    io.riscv_halt := core_with_l2.module.io.cpu_halt
    io.riscv_critical_error := core_with_l2.module.io.cpu_crtical_error
    core_with_l2.module.io.hartResetReq.foreach(_ := io.hartResetReq.get)
    io.hartIsInReset.foreach(_ := core_with_l2.module.io.hartIsInReset.get)
    io.dm_ndreset.foreach(_ := core_with_l2.module.io.dm.get.ndreset)
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

    (EnableClintAsyncBridge,ClintAsyncFromSPMT) match {
      case (Some(param), false) =>
            withClockAndReset(soc_clock.get, soc_reset_sync) {
              val source = Module(new AsyncQueueSource(UInt(64.W), param))
              source.io.enq.valid := io.clintTime.valid
              source.io.enq.bits := io.clintTime.bits
              core_with_l2.module.io.clintTime <> source.io.async
            }
      case _ =>
        core_with_l2.module.io.clintTime <> io.clintTime
    }

    (EnableCHIAsyncBridge,CHIAsyncFromSPMT) match {
      case (Some(param), true) => // chiasync bridge can be provided by spacemit co.
            withClockAndReset(noc_clock.get, noc_reset_sync.get) {
              val sink = Module(new CHIAsyncICNSPMT())
              sink.i.dft.icg_scan_en := i.get.dft.icg_scan_en
              sink.i.dft.scan_enable := i.get.dft.scan_enable
              sink.io.cdb <> core_with_l2.module.io.chi
              io.chi <> sink.io.chi
            }
      case (Some(param), false) =>
            withClockAndReset(noc_clock.get, noc_reset_sync.get) {
              val sink = Module(new CHIAsyncBridgeSink(param))
              sink.io.async <> core_with_l2.module.io.chi
              io.chi <> sink.io.deq
            }
      case (None,_) =>
        io.chi <> core_with_l2.module.io.chi
    }

//    core_with_l2.module.io.msiInfo.valid := wrapper.u_imsic_bus_top.map(_.module.o_msi_info_vld).getOrElse(false.B)
//    core_with_l2.module.io.msiInfo.bits.info := wrapper.u_imsic_bus_top.map(_.module.o_msi_info).getOrElse(0.U)
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
  def exposeOption2IO(data: Option[Option[Data]], name: String): Unit = {
    if (data.isDefined && data.get.isDefined) {
      val dummy = IO(chiselTypeOf(data.get.get)).suggestName(name)
      dummy <> data.get.get
    }
  }
  exposeIO(l_soc.clint, "clint")
  exposeIO(l_soc.debug, "debug")
  exposeIO(l_soc.plic, "plic")
  exposeIO(l_soc.beu, "beu")
  exposeIO(l_soc.nmi, "nmi")
  soc.clock := clock
  soc.reset := reset.asAsyncReset
  exposeIO(soc.soc_clock.get, "soc_clock")
  exposeIO(soc.soc_reset.get, "soc_reset")
  exposeIO(soc.io, "io")
  exposeOptionIO(soc.noc_clock, "noc_clock")
  exposeOptionIO(soc.noc_reset, "noc_reset")
  exposeOption2IO(soc.imsic_axi4lite, "imsic_axi4lite")

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

//TODO:
//Currently we use two-step XiangShan-Difftest, generating XS(with Diff Interface only) and Difftest seperately
//To avoid potential interface problem between XS and Diff, we add Checker and CI(dual-core)
//We will try one-step XS-Diff later
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
