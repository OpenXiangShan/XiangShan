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
import coupledL2.{IsKeywordField, IsKeywordKey, MemBackTypeMM, MemBackTypeMMField, MemPageTypeNC, MemPageTypeNCField, PrefetchRecv, VaddrKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleFieldBase, BundleKeyBase}
import huancun.{AliasField, AliasKey, PrefetchKey}
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import utility._
import xiangshan._
import xiangshan.backend.ctrlblock.DebugLSIO
import xiangshan.backend.rob.RobDebugRollingIO
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.cache.mmu.TlbPtwIO
import xiangshan.mem._

class BlackBoxMemBlockFrontend(implicit val p: Parameters) extends BlackBox with HasXSParameter {
  val io = IO(new Bundle {
    val ptw = new TlbPtwIO
    val softPrefetch = Vec(backendParams.LduCnt, Flipped(Valid(new SoftIfetchPrefetchBundle)))
    val resetInFrontend = Output(Bool())
  })
}

class BlackBoxMemBlockBackend(implicit val p: Parameters) extends BlackBox with HasXSParameter {
  val io = IO(new Bundle {
    val ooo_to_mem = Flipped(new ooo_to_mem)
    val mem_to_ooo = Flipped(new mem_to_ooo)
    val robHeadVaddr = Output(Valid(UInt(VAddrBits.W)))
    val debugTopDownFromMem = Input(new MemCoreTopDownIO)
    val debugRolling = new RobDebugRollingIO
    val traceCoreInterface = new TraceCoreInterface(hasOffset = true)
    val topDownInfo = new TopDownInfo
    val wfi = new WfiReqBundle
  })
}

class MemBlockTop()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {

  override def shouldBeInlined: Boolean = false

  val memBlock = LazyModule(new MemBlock())

  private def makeUncachedManagerNode(
    name: String,
    address: Seq[AddressSet],
    beatBytes: Int,
    executable: Boolean = false,
    requestKeys: Seq[BundleKeyBase] = Nil,
    responseFields: Seq[BundleFieldBase] = Nil
  ) = {
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = address.map(addr => TLSlaveParameters.v1(
        address = Seq(addr),
        regionType = RegionType.UNCACHED,
        executable = executable,
        supportsGet = TransferSizes(1, soc.L3BlockSize),
        supportsPutPartial = TransferSizes(1, soc.L3BlockSize),
        supportsPutFull = TransferSizes(1, soc.L3BlockSize),
        fifoId = Some(0)
      )),
      beatBytes = beatBytes,
      responseFields = responseFields,
      requestKeys = requestKeys
    )))(ValName(name))
  }

  private def makeCachedManagerNode(
    name: String,
    beatBytes: Int,
    requestKeys: Seq[BundleKeyBase] = Nil,
    responseFields: Seq[BundleFieldBase] = Nil,
    endSinkId: Int = 1024
  ) = {
    TLManagerNode(Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0x0, 0xFFFFFFFFFFFFL)),
        regionType = RegionType.TRACKED,
        supportsGet = TransferSizes(1, soc.L3BlockSize),
        supportsPutPartial = TransferSizes(1, soc.L3BlockSize),
        supportsPutFull = TransferSizes(1, soc.L3BlockSize),
        supportsAcquireB = TransferSizes(soc.L3BlockSize),
        supportsAcquireT = TransferSizes(soc.L3BlockSize),
        fifoId = Some(0)
      )),
      beatBytes = beatBytes,
      endSinkId = endSinkId,
      responseFields = responseFields,
      requestKeys = requestKeys
    )))(ValName(name))
  }

  private val frontendICacheParams = coreParams.icacheParameters
  private val frontendICacheAliasBits = {
    val untagBits = log2Ceil(frontendICacheParams.nSets) + log2Ceil(frontendICacheParams.blockBytes)
    Option.when(untagBits > 12)(untagBits - 12)
  }

  private val frontendICacheClientNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "frontend_icache_client",
      sourceId = IdRange(0, frontendICacheParams.nFetchMshr + frontendICacheParams.nPrefetchMshr + 1)
    )),
    requestFields = Seq(
      ReqSourceField(),
      MemBackTypeMMField()
    ) ++ frontendICacheAliasBits.map(AliasField).toSeq
  )))(ValName("frontend_icache_client"))

  private val frontendInstrUncacheClientNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "frontend_instr_uncache_client",
      sourceId = IdRange(0, 1)
    )),
    requestFields = Seq(
      MemBackTypeMMField(),
      MemPageTypeNCField()
    )
  )))(ValName("frontend_instr_uncache_client"))

  private val frontendICacheCtrlFrontendNode = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0x0, 0xFFFFFFFFFFFFL)),
      regionType = RegionType.UNCACHED,
      executable = false,
      supportsGet = TransferSizes(1, 8),
      supportsPutPartial = TransferSizes(1, 8),
      supportsPutFull = TransferSizes(1, 8),
      fifoId = Some(0)
    )),
    beatBytes = 8
  )))(ValName("frontend_icachectrl_frontend"))

  val frontendICacheIONode =
    makeCachedManagerNode(
      "frontend_icache_mem",
      soc.L3OuterBusWidth / 8,
      requestKeys = Seq(ReqSourceKey, MemBackTypeMM, AliasKey)
    )
  val frontendInstrUncacheIONode =
    makeUncachedManagerNode(
      "frontend_instr_uncache_mem",
      Seq(AddressSet(0x0, 0xFFFFFFFFFFFFL)),
      8,
      executable = true,
      requestKeys = Seq(MemBackTypeMM, MemPageTypeNC)
    )
  val frontendICacheCtrlIONode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1("frontend_icachectrl_mem", IdRange(0, 32)))
  )))(ValName("frontend_icachectrl_mem"))

  private val dcacheIONode = Option.when(coreParams.dcacheParametersOpt.nonEmpty) {
    makeCachedManagerNode(
      "dcache_mem",
      soc.L3OuterBusWidth / 8,
      requestKeys = Seq(PrefetchKey, ReqSourceKey, VaddrKey, MemBackTypeMM, MemPageTypeNC, AliasKey),
      responseFields = Seq(IsKeywordField())
    )
  }
  private val uncacheExternalAddress = {
    val fullAddress = AddressSet(0x0, 0xFFFFFFFFFFFFL)
    val internalMmio = Seq(AddressSet(0x38022000L, 0x7fL))
    internalMmio.foldLeft(Seq(fullAddress)) { case (acc, addr) =>
      acc.flatMap(_.subtract(addr))
    }
  }
  private val uncacheIONode =
    makeUncachedManagerNode(
      "uncache_mem",
      uncacheExternalAddress,
      8,
      requestKeys = Seq(MemBackTypeMM, MemPageTypeNC)
    )
  private val ptwIONode = Option.when(!coreParams.softPTW) {
    makeCachedManagerNode(
      "ptw_mem",
      soc.L3OuterBusWidth / 8,
      requestKeys = Seq(ReqSourceKey)
    )
  }

  memBlock.inner.frontendBridge.icache_node := frontendICacheClientNode
  memBlock.inner.frontendBridge.instr_uncache_node := frontendInstrUncacheClientNode
  frontendICacheCtrlFrontendNode := memBlock.inner.frontendBridge.icachectrl_node
  frontendICacheIONode := memBlock.inner.frontendBridge.icache_node
  frontendInstrUncacheIONode := memBlock.inner.frontendBridge.instr_uncache_node
  memBlock.inner.frontendBridge.icachectrl_node := frontendICacheCtrlIONode
  dcacheIONode.foreach(_ := memBlock.inner.dcache_port := memBlock.inner.l1d_to_l2_buffer.node := memBlock.inner.dcache.clientNode)
  uncacheIONode := memBlock.inner.uncache_port
  ptwIONode.foreach(_ := memBlock.inner.ptw_to_l2_buffer.node)

  val l2PfRecvSink = memBlock.inner.l2_pf_sender_opt.map { sender =>
    val sink = BundleBridgeSink(Some(() => new PrefetchRecv))
    sink := sender
    sink
  }
  val l3PfRecvSink = memBlock.inner.l3_pf_sender_opt.map { sender =>
    val sink = BundleBridgeSink(Some(() => new huancun.PrefetchRecv))
    sink := sender
    sink
  }

  val frontendICacheMemPort = InModuleBody(frontendICacheIONode.makeIOs())
  val frontendInstrUncacheMemPort = InModuleBody(frontendInstrUncacheIONode.makeIOs())
  val frontendICacheCtrlMemPort = InModuleBody(frontendICacheCtrlIONode.makeIOs())
  val frontendICacheFrontendPort = InModuleBody(frontendICacheClientNode.makeIOs())
  val frontendInstrUncacheFrontendPort = InModuleBody(frontendInstrUncacheClientNode.makeIOs())
  val frontendICacheCtrlFrontendPort = InModuleBody(frontendICacheCtrlFrontendNode.makeIOs())
  val dcacheMemPort = dcacheIONode.map(node => InModuleBody(node.makeIOs()))
  val uncachePort = InModuleBody(uncacheIONode.makeIOs())
  val ptwMemPort = ptwIONode.map(node => InModuleBody(node.makeIOs()))

  val clintIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 2))
  val debugIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))
  val plicIntNode = IntSourceNode(IntSourcePortSimple(1, 2, 1))
  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, 1, (new NonmaskableInterruptIO).elements.size))
  val beuIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))

  memBlock.inner.clint_int_sink := IntBuffer() := clintIntNode
  memBlock.inner.plic_int_sink :*= IntBuffer() :*= plicIntNode
  memBlock.inner.debug_int_sink := IntBuffer() := debugIntNode
  memBlock.inner.nmi_int_sink := IntBuffer() := nmiIntNode
  memBlock.inner.beu_local_int_sink := IntBuffer() := beuIntNode

  val clint = InModuleBody(clintIntNode.makeIOs())
  val debug = InModuleBody(debugIntNode.makeIOs())
  val plic = InModuleBody(plicIntNode.makeIOs())
  val nmi = InModuleBody(nmiIntNode.makeIOs())
  val beu = InModuleBody(beuIntNode.makeIOs())

  lazy val module = new MemBlockTopImp(this)
}

class MemBlockTopImp(wrapper: MemBlockTop) extends LazyRawModuleImp(wrapper) {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(AsyncReset()))

  val reset_sync = withClockAndReset(clock, reset) { ResetGen() }
  childClock := clock
  childReset := reset_sync

  val frontend = Module(new BlackBoxMemBlockFrontend)
  val backend = Module(new BlackBoxMemBlockBackend)

  frontend.io.ptw <> wrapper.memBlock.module.io.fetch_to_mem.itlb
  frontend.io.softPrefetch <> wrapper.memBlock.module.io.ifetchPrefetch
  wrapper.memBlock.module.io.resetInFrontendBypass.fromFrontend := frontend.io.resetInFrontend

  backend.io.ooo_to_mem <> wrapper.memBlock.module.io.ooo_to_mem
  backend.io.mem_to_ooo <> wrapper.memBlock.module.io.mem_to_ooo
  backend.io.robHeadVaddr <> wrapper.memBlock.module.io.debugTopDown.robHeadVaddr
  backend.io.debugTopDownFromMem <> wrapper.memBlock.module.io.debugTopDown.toCore
  wrapper.memBlock.module.io.debugRolling <> backend.io.debugRolling
  wrapper.memBlock.module.io.traceCoreInterfaceBypass.fromBackend <> backend.io.traceCoreInterface
  backend.io.topDownInfo.lqEmpty <> wrapper.memBlock.module.io.topDownInfo.toBackend.lqEmpty
  backend.io.topDownInfo.sqEmpty <> wrapper.memBlock.module.io.topDownInfo.toBackend.sqEmpty
  backend.io.topDownInfo.l1Miss <> wrapper.memBlock.module.io.topDownInfo.toBackend.l1Miss
  wrapper.memBlock.module.io.topDownInfo.toBackend.noUopsIssued := backend.io.topDownInfo.noUopsIssued
  backend.io.topDownInfo.l2TopMiss.l2Miss <> wrapper.memBlock.module.io.topDownInfo.toBackend.l2TopMiss.l2Miss
  backend.io.topDownInfo.l2TopMiss.l3Miss <> wrapper.memBlock.module.io.topDownInfo.toBackend.l2TopMiss.l3Miss
  wrapper.memBlock.module.io.wfi <> backend.io.wfi

  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.pf_ctrl.l1I_pf_enable)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.pf_ctrl.l2_pf_store_only)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.bp_ctrl.ubtb_enable)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.bp_ctrl.btb_enable)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.bp_ctrl.tage_enable)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.bp_ctrl.sc_enable)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.frontend_trigger)
  dontTouch(wrapper.memBlock.module.io.ooo_to_mem.csrCtrl.fsIsOff)
  wrapper.l2PfRecvSink.foreach(sink => dontTouch(sink.in.head._1.l2_pf_en))
  wrapper.l3PfRecvSink.foreach(sink => dontTouch(sink.in.head._1.l2_pf_en))

  val hartId = IO(chiselTypeOf(wrapper.memBlock.module.io.hartId))
  val redirect = IO(chiselTypeOf(wrapper.memBlock.module.io.redirect))
  val dcacheError = IO(chiselTypeOf(wrapper.memBlock.module.io.dcacheError))
  val uncacheError = IO(chiselTypeOf(wrapper.memBlock.module.io.uncacheError))
  val memInfo = IO(chiselTypeOf(wrapper.memBlock.module.io.memInfo))
  val debug_ls = IO(chiselTypeOf(wrapper.memBlock.module.io.debug_ls))
  val l2_hint = IO(chiselTypeOf(wrapper.memBlock.module.io.l2_hint))
  val l2PfqBusy = IO(chiselTypeOf(wrapper.memBlock.module.io.l2PfqBusy))
  val l2_tlb_req = IO(chiselTypeOf(wrapper.memBlock.module.io.l2_tlb_req))
  val l2_pmp_resp = IO(chiselTypeOf(wrapper.memBlock.module.io.l2_pmp_resp))
  val l2_flush_done = IO(chiselTypeOf(wrapper.memBlock.module.io.l2_flush_done))
  val fromTopToBackend = IO(chiselTypeOf(wrapper.memBlock.module.io.fromTopToBackend))
  val inner_hartId = IO(chiselTypeOf(wrapper.memBlock.module.io.inner_hartId))
  val inner_reset_vector = IO(chiselTypeOf(wrapper.memBlock.module.io.inner_reset_vector))
  val outer_reset_vector = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_reset_vector))
  val outer_cpu_halt = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_cpu_halt))
  val outer_l2_flush_en = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_l2_flush_en))
  val outer_power_down_en = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_power_down_en))
  val outer_cpu_critical_error = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_cpu_critical_error))
  val outer_msi_ack = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_msi_ack))
  val outer_teemsi_ack = wrapper.memBlock.module.io.outer_teemsi_ack.map(x => IO(chiselTypeOf(x)))
  val inner_beu_errors_icache = IO(chiselTypeOf(wrapper.memBlock.module.io.inner_beu_errors_icache))
  val outer_beu_errors_icache = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_beu_errors_icache))
  val inner_hc_perfEvents = IO(chiselTypeOf(wrapper.memBlock.module.io.inner_hc_perfEvents))
  val outer_hc_perfEvents = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_hc_perfEvents))
  val outer_l2PfCtrl = IO(chiselTypeOf(wrapper.memBlock.module.io.outer_l2PfCtrl))
  val reset_backend = IO(Output(Bool()))
  val resetInFrontendToL2Top = IO(chiselTypeOf(wrapper.memBlock.module.io.resetInFrontendBypass.toL2Top))
  val traceCoreInterfaceToL2Top = IO(chiselTypeOf(wrapper.memBlock.module.io.traceCoreInterfaceBypass.toL2Top))
  val topDownFromL2Top = IO(chiselTypeOf(wrapper.memBlock.module.io.topDownInfo.fromL2Top))
  val dft = wrapper.memBlock.module.io.dft.map(x => IO(chiselTypeOf(x)))
  val dft_reset = wrapper.memBlock.module.io.dft_reset.map(x => IO(chiselTypeOf(x)))
  val dft_frnt_ram_hold = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_hold))))
  val dft_frnt_ram_bypass = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_bypass))))
  val dft_frnt_ram_bp_clken = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_bp_clken))))
  val dft_frnt_ram_aux_clk = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_aux_clk))))
  val dft_frnt_ram_aux_ckbp = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_aux_ckbp))))
  val dft_frnt_ram_mcp_hold = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.ram_mcp_hold))))
  val dft_frnt_cgen = wrapper.memBlock.module.io.dft_frnt.map(x => IO(Output(chiselTypeOf(x.cgen))))
  val dft_reset_frnt_lgc_rst_n = wrapper.memBlock.module.io.dft_reset_frnt.map(x => IO(Output(chiselTypeOf(x.lgc_rst_n))))
  val dft_reset_frnt_mode = wrapper.memBlock.module.io.dft_reset_frnt.map(x => IO(Output(chiselTypeOf(x.mode))))
  val dft_reset_frnt_scan_mode = wrapper.memBlock.module.io.dft_reset_frnt.map(x => IO(Output(chiselTypeOf(x.scan_mode))))
  val dft_bcknd_cgen = wrapper.memBlock.module.io.dft_bcknd.map(x => IO(Output(chiselTypeOf(x.cgen))))
  val dft_reset_bcknd_lgc_rst_n = wrapper.memBlock.module.io.dft_reset_bcknd.map(x => IO(Output(chiselTypeOf(x.lgc_rst_n))))
  val dft_reset_bcknd_mode = wrapper.memBlock.module.io.dft_reset_bcknd.map(x => IO(Output(chiselTypeOf(x.mode))))
  val dft_reset_bcknd_scan_mode = wrapper.memBlock.module.io.dft_reset_bcknd.map(x => IO(Output(chiselTypeOf(x.scan_mode))))
  val perf = wrapper.memBlock.module.io_perf.zipWithIndex.map { case (x, _) => IO(Output(chiselTypeOf(x.value))) }
  val l2_prefetch = wrapper.l2PfRecvSink.map(sink => IO(Output(chiselTypeOf(sink.in.head._1))))
  val l3_prefetch = wrapper.l3PfRecvSink.map(sink => IO(Output(chiselTypeOf(sink.in.head._1))))

  wrapper.memBlock.module.io.hartId <> hartId
  wrapper.memBlock.module.io.redirect <> redirect
  dcacheError <> wrapper.memBlock.module.io.dcacheError
  uncacheError <> wrapper.memBlock.module.io.uncacheError
  memInfo <> wrapper.memBlock.module.io.memInfo
  debug_ls <> wrapper.memBlock.module.io.debug_ls
  wrapper.memBlock.module.io.l2_hint <> l2_hint
  wrapper.memBlock.module.io.l2PfqBusy <> l2PfqBusy
  wrapper.memBlock.module.io.l2_tlb_req <> l2_tlb_req
  wrapper.memBlock.module.io.l2_pmp_resp <> l2_pmp_resp
  wrapper.memBlock.module.io.l2_flush_done <> l2_flush_done
  wrapper.memBlock.module.io.fromTopToBackend <> fromTopToBackend
  inner_hartId <> wrapper.memBlock.module.io.inner_hartId
  inner_reset_vector <> wrapper.memBlock.module.io.inner_reset_vector
  wrapper.memBlock.module.io.outer_reset_vector <> outer_reset_vector
  outer_cpu_halt <> wrapper.memBlock.module.io.outer_cpu_halt
  outer_l2_flush_en <> wrapper.memBlock.module.io.outer_l2_flush_en
  outer_power_down_en <> wrapper.memBlock.module.io.outer_power_down_en
  outer_cpu_critical_error <> wrapper.memBlock.module.io.outer_cpu_critical_error
  outer_msi_ack <> wrapper.memBlock.module.io.outer_msi_ack
  wrapper.memBlock.module.io.outer_teemsi_ack.zip(outer_teemsi_ack).foreach { case (m, io) => io <> m }
  wrapper.memBlock.module.io.inner_beu_errors_icache <> inner_beu_errors_icache
  outer_beu_errors_icache <> wrapper.memBlock.module.io.outer_beu_errors_icache
  inner_hc_perfEvents <> wrapper.memBlock.module.io.inner_hc_perfEvents
  wrapper.memBlock.module.io.outer_hc_perfEvents <> outer_hc_perfEvents
  outer_l2PfCtrl <> wrapper.memBlock.module.io.outer_l2PfCtrl
  reset_backend := wrapper.memBlock.module.io.reset_backend.asBool
  resetInFrontendToL2Top <> wrapper.memBlock.module.io.resetInFrontendBypass.toL2Top
  traceCoreInterfaceToL2Top <> wrapper.memBlock.module.io.traceCoreInterfaceBypass.toL2Top
  wrapper.memBlock.module.io.topDownInfo.fromL2Top <> topDownFromL2Top
  wrapper.memBlock.module.io.dft.zip(dft).foreach { case (m, io) => m <> io }
  wrapper.memBlock.module.io.dft_reset.zip(dft_reset).foreach { case (m, io) => m <> io }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_hold).foreach { case (m, io) => io := m.ram_hold }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_bypass).foreach { case (m, io) => io := m.ram_bypass }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_bp_clken).foreach { case (m, io) => io := m.ram_bp_clken }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_aux_clk).foreach { case (m, io) => io := m.ram_aux_clk }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_aux_ckbp).foreach { case (m, io) => io := m.ram_aux_ckbp }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_ram_mcp_hold).foreach { case (m, io) => io := m.ram_mcp_hold }
  wrapper.memBlock.module.io.dft_frnt.zip(dft_frnt_cgen).foreach { case (m, io) => io := m.cgen }
  wrapper.memBlock.module.io.dft_reset_frnt.zip(dft_reset_frnt_lgc_rst_n).foreach { case (m, io) => io := m.lgc_rst_n }
  wrapper.memBlock.module.io.dft_reset_frnt.zip(dft_reset_frnt_mode).foreach { case (m, io) => io := m.mode }
  wrapper.memBlock.module.io.dft_reset_frnt.zip(dft_reset_frnt_scan_mode).foreach { case (m, io) => io := m.scan_mode }
  wrapper.memBlock.module.io.dft_bcknd.zip(dft_bcknd_cgen).foreach { case (m, io) => io := m.cgen }
  wrapper.memBlock.module.io.dft_reset_bcknd.zip(dft_reset_bcknd_lgc_rst_n).foreach { case (m, io) => io := m.lgc_rst_n }
  wrapper.memBlock.module.io.dft_reset_bcknd.zip(dft_reset_bcknd_mode).foreach { case (m, io) => io := m.mode }
  wrapper.memBlock.module.io.dft_reset_bcknd.zip(dft_reset_bcknd_scan_mode).foreach { case (m, io) => io := m.scan_mode }
  wrapper.memBlock.module.io_perf.zip(perf).foreach { case (m, io) => io := m.value }
  wrapper.l2PfRecvSink.zip(l2_prefetch).foreach { case (sink, io) => io := sink.in.head._1 }
  wrapper.l3PfRecvSink.zip(l3_prefetch).foreach { case (sink, io) => io := sink.in.head._1 }
}

object MemBlockTopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  val boundCoreParams = config(XSTileKey).head
  val memBlockConfig = config.alter((site, here, up) => {
    case XSCoreParamsKey => boundCoreParams
  })

  private val backendParams = memBlockConfig(XSCoreParamsKey).backendParams
  for ((exuCfg, i) <- backendParams.allExuParams.zipWithIndex) {
    exuCfg.bindBackendParam(backendParams)
    exuCfg.updateIQWakeUpConfigs(backendParams.iqWakeUpParams)
    exuCfg.updateExuIdx(i)
  }
  backendParams.configChecks
  for (schdCfg <- backendParams.allSchdParams) {
    schdCfg.bindBackendParam(backendParams)
    for (iqCfg <- schdCfg.issueBlockParams) {
      iqCfg.bindBackendParam(backendParams)
    }
  }

  val soc = DisableMonitors(p => LazyModule(new MemBlockTop()(p)))(memBlockConfig)
  Generator.execute(firrtlOpts, soc.module, firtoolOpts)

  FileRegisters.write(fileDir = "./build_memblock", filePrefix = "MemBlockTop.")
}
