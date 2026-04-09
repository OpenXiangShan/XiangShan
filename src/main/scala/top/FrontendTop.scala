/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import system.{HasSoCParameter, SoCParamsKey}
import utility._
import xiangshan._
import xiangshan.frontend._


// ── BlackBox stub for the backend ──────────────────────────────────────────
// Matches real BackendIO.frontend = Flipped(new FrontendToCtrlIO)
class BlackBoxBackend(implicit p: Parameters) extends BlackBox {
  val io = IO(Flipped(new FrontendToCtrlIO))
}

// ── FrontendTop LazyModule ──────────────────────────────────────────────────
class FrontendTop()(implicit p: Parameters) extends LazyModule
    with HasXSParameter
    with HasSoCParameter {

  override def shouldBeInlined: Boolean = false

  val frontend = LazyModule(new Frontend())

  // Create TLManagerNode instances to act as "slave-side IO boundaries"
  // These provide the outward Diplomacy edges needed for makeIOs() to work.
  // The manager nodes accept all addresses and transfer sizes to be maximally permissive.
  val icacheIONode = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0, 0xFFFFFFFFFFFFL)), // full 64-bit address space
      regionType = RegionType.UNCACHED,
      executable = true,
      supportsGet = TransferSizes(1, soc.L3BlockSize),
      supportsPutPartial = TransferSizes(1, soc.L3BlockSize),
      supportsPutFull = TransferSizes(1, soc.L3BlockSize),
      fifoId = Some(0)
    )),
    beatBytes = soc.L3OuterBusWidth / 8
  )))

  val uncacheIONode = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0, 0xFFFFFFFFFFFFL)),
      regionType = RegionType.UNCACHED,
      executable = true,
      supportsGet = TransferSizes(1, soc.L3BlockSize),
      supportsPutPartial = TransferSizes(1, soc.L3BlockSize),
      supportsPutFull = TransferSizes(1, soc.L3BlockSize),
      fifoId = Some(0)
    )),
    beatBytes = soc.L3OuterBusWidth / 8
  )))

  // Connect the Frontend's client nodes to our manager IO nodes
  icacheIONode := frontend.inner.icache.clientNode
  uncacheIONode := frontend.inner.instrUncache.clientNode

  // ICache has an optional control unit with a TLRegisterNode (slave).
  // Connect it to a dummy TLClientNode so it has the required inward edge.
  frontend.inner.icache.ctrlUnitOpt.foreach { ctrlUnit =>
    val dummyCtrlClient = TLClientNode(Seq(TLMasterPortParameters.v1(
      Seq(TLMasterParameters.v1("icache_ctrl_dummy", IdRange(0, 1)))
    )))
    ctrlUnit.node := dummyCtrlClient
  }

  // Expose the manager nodes' inward bundles as module IO
  val icacheMemPort = InModuleBody(icacheIONode.makeIOs())
  val uncachePort = InModuleBody(uncacheIONode.makeIOs())

  lazy val module = new FrontendTopImp(this)
}

// ── FrontendTopImp ──────────────────────────────────────────────────────────
class FrontendTopImp(wrapper: FrontendTop) extends LazyRawModuleImp(wrapper) {
  override def localModulePrefix             = wrapper.soc.XSTopPrefix
  override def localModulePrefixUseSeparator = false

  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))

  val reset_sync = withClockAndReset(clock, reset) { ResetGen() }
  childClock := clock
  childReset := reset_sync

  // BlackBoxBackend connects directly to child module io.backend
  // Same pattern as real XSCore: backend.io.frontend <> frontend.module.io.backend
  val backend = Module(new BlackBoxBackend)
  backend.io <> wrapper.frontend.module.io.backend  // ✅ 子模块io，方向正确

  // Expose FrontendIO fields (excluding backend) as top-level ports
  val frontend_io = IO(new Bundle {
    val hartId          = chiselTypeOf(wrapper.frontend.module.io.hartId)
    val reset_vector    = chiselTypeOf(wrapper.frontend.module.io.reset_vector)
    val sfence          = chiselTypeOf(wrapper.frontend.module.io.sfence)
    val fencei          = chiselTypeOf(wrapper.frontend.module.io.fencei)
    val ptw             = chiselTypeOf(wrapper.frontend.module.io.ptw)
    // backend excluded — connected internally to BlackBoxBackend
    val error           = chiselTypeOf(wrapper.frontend.module.io.error)
    val tlbCsr          = chiselTypeOf(wrapper.frontend.module.io.tlbCsr)
    val csrCtrl         = chiselTypeOf(wrapper.frontend.module.io.csrCtrl)
    val resetInFrontend = chiselTypeOf(wrapper.frontend.module.io.resetInFrontend)
    val softPrefetch    = chiselTypeOf(wrapper.frontend.module.io.softPrefetch)
    val debugTopDown    = chiselTypeOf(wrapper.frontend.module.io.debugTopDown)
    val dft             = wrapper.frontend.module.io.dft.map(chiselTypeOf(_))
    val dft_reset       = wrapper.frontend.module.io.dft_reset.map(chiselTypeOf(_))
  })

  // Connect all FrontendIO fields (excluding backend)
  frontend_io.hartId          <> wrapper.frontend.module.io.hartId
  frontend_io.reset_vector    <> wrapper.frontend.module.io.reset_vector
  frontend_io.sfence          <> wrapper.frontend.module.io.sfence
  frontend_io.fencei          <> wrapper.frontend.module.io.fencei
  frontend_io.ptw             <> wrapper.frontend.module.io.ptw
  frontend_io.error           <> wrapper.frontend.module.io.error
  frontend_io.tlbCsr          <> wrapper.frontend.module.io.tlbCsr
  frontend_io.csrCtrl         <> wrapper.frontend.module.io.csrCtrl
  frontend_io.resetInFrontend <> wrapper.frontend.module.io.resetInFrontend
  frontend_io.softPrefetch    <> wrapper.frontend.module.io.softPrefetch
  frontend_io.debugTopDown    <> wrapper.frontend.module.io.debugTopDown
  wrapper.frontend.module.io.dft.zip(frontend_io.dft).foreach { case (m, f) => f <> m }
  wrapper.frontend.module.io.dft_reset.zip(frontend_io.dft_reset).foreach { case (m, f) => f <> m }
}

// ── Entry point ─────────────────────────────────────────────────────────────
object FrontendTopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableDifftest = config(DebugOptionsKey).EnableDifftest || config(DebugOptionsKey).AlwaysBasicDiff
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  // XSTileKey holds a Seq of per-tile params; HasXSParameter (used by Frontend)
  // requires XSCoreParamsKey to be set to a single-tile projection.
  // This mirrors how XSTileWrap / XSNoCTop inject the key per tile.
  val frontendConfig = config.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSTileKey).head
  })

  val soc = DisableMonitors(p => LazyModule(new FrontendTop()(p)))(frontendConfig)
  Generator.execute(firrtlOpts, soc.module, firtoolOpts)

  FileRegisters.write(fileDir = "./build-frontend", filePrefix = "FrontendTop.")
}
