/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import xscache.coupledL2.{L2ToL1PfCtrl, MemBackTypeMMField, MemPageTypeNCField, PrefetchCtrlFromCore, PrefetchRecv}
import xscache.chi.{ChannelIO, DecoupledPortIO, PortIO}
import oceanus.chi.EnumCHIChannel
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP, CHIBundleSNP}
import oceanus.chi.link.OceanusChannelAdapter
import oceanus.l2.{L2Configuration, L2Top}
import system.HasSoCParameter
import utility._
import utility.sram.{SramBroadcastBundle, SramHelper}
import utility.mbist.{MbistInterface, MbistPipeline}
import xiangshan.cache.{CCHIConnect, CCHIType1Port, CCHIType4Port}
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.trace.{Itype, TraceCoreInterface}
import xiangshan.cache.axi.{AXI4PMAUserAdapter, BusErrorUnitAXI}

class L1BusErrorUnitInfo(implicit val p: Parameters) extends Bundle with HasSoCParameter {
  val ecc_error = Valid(UInt(soc.PAddrBits.W))
}

class XSL1BusErrors()(implicit val p: Parameters) extends BusErrors {
  val icache = new L1BusErrorUnitInfo
  val dcache = new L1BusErrorUnitInfo
  val uncache = new L1BusErrorUnitInfo
  val l2 = new L1BusErrorUnitInfo

  override def toErrorList: List[Option[(ValidIO[UInt], String, String)]] =
    List(
      Some(icache.ecc_error, "I_ECC", "Icache ecc error"),
      Some(dcache.ecc_error, "D_ECC", "Dcache ecc error"),
      Some(uncache.ecc_error, "U_ECC", "Uncache ecc error"),
      Some(l2.ecc_error, "L2_ECC", "L2Cache ecc error")
    )
}

/**
  * Tile-side wrapper around oceanus.l2.L2Top
  */
class L2TopWrapperInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = true

  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Components ============
  val mmio_xbar = AXI4Xbar()
  val mmio_port = AXI4IdentityNode() // to soc_xbar (step 4a)
  val memory_port = if (enableL2) None else Some(TLIdentityNode())
  val beu = LazyModule(new BusErrorUnitAXI(
    new XSL1BusErrors(),
    BusErrorUnitParams(soc.BEURange.base, soc.BEURange.mask.toInt + 1)
  ))

  val i_mmio_port = AXI4IdentityNode()
  val d_mmio_port = AXI4IdentityNode()
  val sep_tl_port_opt = Option.when(SeperateBus != top.SeperatedBusType.NONE)(TLTempNode())
  val i_mmio_buffer = LazyModule(new AXI4Buffer())

  val clint_int_node = IntIdentityNode()
  val debug_int_node = IntIdentityNode()
  val plic_int_node = IntIdentityNode()
  val nmi_int_node = IntIdentityNode()
  val beu_local_int_source = IntSourceNode(IntSourcePortSimple())
  val beu_local_int_source_buffer = IntBuffer()

  // Reserved L2 prefetch Diplomacy sinks. Oceanus L2 has no prefetcher yet;
  // keep the L1 sender endpoints so they can be wired later.
  val pf_recv_node: Option[BundleBridgeSink[PrefetchRecv]] =
    Option.when(coreParams.prefetcher.nonEmpty)(BundleBridgeSink(Some(() => new PrefetchRecv)))
  val l3_pf_recv_node: Option[BundleBridgeSink[PrefetchRecv]] =
    Option.when(coreParams.prefetcher.nonEmpty)(BundleBridgeSink(Some(() => new PrefetchRecv)))

  // =========== Connection ============
  mmio_xbar := AXI4Buffer() := AXI4Buffer() := i_mmio_port
  mmio_xbar := AXI4Buffer() := AXI4Buffer() := d_mmio_port
  beu.node := AXI4Buffer() := AXI4PMAUserAdapter(stripUser = true) := mmio_xbar
  if (SeperateBus != top.SeperatedBusType.NONE) {
    sep_tl_port_opt.get := AXI4ToTL() := AXI4Buffer() := AXI4PMAUserAdapter(stripUser = true) := mmio_xbar
  }

  // Filter out in-core addresses before they appear on mmio_port / soc_xbar.
  // AXI4Filter only has Smask (intersect); Ssubtract matches TLFilter.mSubtract.
  private def icacheCtrlAddressOpt: Option[AddressSet] = Option.when(icacheCtrlEnabled)(icacheCtrlAddress)
  private def dcacheCtrlAddressOpt: Option[AddressSet] = dcacheParameters.cacheCtrlAddressOpt
  private def cacheAddressSet: Seq[AddressSet] = (icacheCtrlAddressOpt ++ dcacheCtrlAddressOpt).toSeq
  private def mmioFilters = (if(SeperateBus != top.SeperatedBusType.NONE) (SeperateBusRanges ++ cacheAddressSet) else cacheAddressSet) :+ soc.BEURange
  private def axi4Ssubtract(excepts: Seq[AddressSet]): AXI4SlaveParameters => Option[AXI4SlaveParameters] = { s =>
    val filtered = excepts.foldLeft(s.address) { (addr, e) => addr.flatMap(_.subtract(e)) }
    if (filtered.isEmpty) {
      None
    } else {
      val alignment = filtered.map(_.alignment).min
      val maxTransfer = 1 << 30
      val capTransfer = if (alignment == 0 || alignment > maxTransfer) maxTransfer else alignment.toInt
      val cap = TransferSizes(1, capTransfer)
      Some(s.copy(
        address = filtered,
        supportsWrite = s.supportsWrite.intersect(cap),
        supportsRead = s.supportsRead.intersect(cap)
      ))
    }
  }
  // Tile MMIO AXI is 8B (Uncache / BEU / CtrlUnit). soc_xbar is 32B (NCB).
  // Convert only on the egress so in-core masters keep a 64-bit AXI bundle.
  private val mmioBusBytes = 8
  private val mmioToSocXbar = TLXbar()
  // Distinct from SoC TLError at 0x1000000000000, which is still visible
  // through TLToAXI4 and would overlap on AXI4ToTL's AXI slave port.
  private val mmioToSocError = LazyModule(new TLError(
    params = DevNullParams(
      address = Seq(AddressSet(0x2000000000000L, 0xffffffffffffL)),
      maxAtomic = 8,
      maxTransfer = 4096
    ),
    beatBytes = mmioBusBytes
  ))
  mmioToSocError.node := mmioToSocXbar
  mmio_port :=
    AXI4Filter(axi4Ssubtract(mmioFilters)) :=
    AXI4Buffer() :=
    TLToAXI4(wcorrupt = false) :=
    TLWidthWidget(mmioBusBytes) :=
    mmioToSocXbar :=
    TLFIFOFixer() :=
    AXI4ToTL(wcorrupt = false) :=
    AXI4PMAUserAdapter(stripUser = true) :=
    mmio_xbar

  beu_local_int_source_buffer := beu_local_int_source

  class Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val beu_errors = Input(chiselTypeOf(beu.module.io.errors))
      val reset_vector = new Bundle {
        val fromTile = Input(UInt(PAddrBits.W))
        val toCore = Output(UInt(PAddrBits.W))
      }
      val hartId = new Bundle() {
        val fromTile = Input(UInt(64.W))
        val toCore = Output(UInt(64.W))
      }
      val msiInfo = new Bundle() {
        val fromTile = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
        val toCore = Output(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      }
      val msiAck = new Bundle {
        val fromCore = Input(Bool())
        val toTile = Output(Bool())
      }
      val teemsiInfo = Option.when(soc.IMSICParams.HasTEEIMSIC)(new Bundle() {
        val fromTile = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
        val toCore = Output(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      })
      val teemsiAck = Option.when(soc.IMSICParams.HasTEEIMSIC)(new Bundle {
        val fromCore = Input(Bool())
        val toTile = Output(Bool())
      })
      val cpu_wfi = new Bundle() {
        val fromCore = Input(Bool())
        val toTile = Output(Bool())
      }
      val cpu_critical_error = new Bundle() {
        val fromCore = Input(Bool())
        val toTile = Output(Bool())
      }
      val hartIsInReset = new Bundle() {
        val resetInFrontend = Input(Bool())
        val toTile = Output(Bool())
      }
      val traceCoreInterface = new Bundle{
        val fromCore = Flipped(new TraceCoreInterface)
        val toTile   = new TraceCoreInterface
      }
      val debugTopDown = new Bundle() {
        val robTrueCommit = Input(UInt(64.W))
        val robHeadPaddr = Flipped(Valid(UInt(36.W)))
        val l2MissMatch = Output(Bool())
      }
      val l2Miss = Output(Bool())
      val l3Miss = new Bundle {
        val fromTile = Input(Bool())
        val toCore = Output(Bool())
      }
      val clintTime = new Bundle {
        val fromTile = Input(ValidIO(UInt(64.W)))
        val toCore = Output(ValidIO(UInt(64.W)))
      }
      val chi = Option.when(isOpenLLC)(new PortIO)
      val decoupledCHI = Option.when(isZhuJiang)(new DecoupledPortIO)
      val nodeID = Some(Input(UInt(NodeIDWidth.W)))
      val dcache_cchi = Flipped(Vec(numMemChannelsFromDcache, new CCHIType1Port))
      val icache_cchi = Flipped(new CCHIType4Port)
      val ptw_cchi = Flipped(new CCHIType4Port)
      val pfCtrlFromCore = Input(new PrefetchCtrlFromCore)
      val l2_tlb_req = new TlbRequestIO(nRespDups = 2)
      val l2_pmp_resp = Flipped(new PMPRespBundle)
      val l2_hint = Vec(numMemChannelsFromDcache, ValidIO(new L2ToL1Hint()))
      val l2_fdbk_pf_ctrl = Output(new L2ToL1PfCtrl)
      val perfEvents = Output(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
      val l2_flush_en = Option.when(EnablePowerDown) (Input(Bool()))
      val l2_flush_done = Option.when(EnablePowerDown) (Output(Bool()))
      val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
      val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      val dft_out = Option.when(hasDFT)(Output(new SramBroadcastBundle))
      val dft_reset_out = Option.when(hasMbist)(Output(new DFTResetSignals()))
      // val reset_core = IO(Output(Reset()))
    })
    io.dft_out.zip(io.dft).foreach({ case(a, b) => a := b })
    io.dft_reset_out.zip(io.dft_reset).foreach({ case(a, b) => a := b })

    val resetDelayN = Module(new DelayN(UInt(PAddrBits.W), 5))

    val (beu_int_out, _) = beu_local_int_source.out(0)
    beu_int_out(0) := beu.module.io.interrupt

    beu.module.io.errors.icache := io.beu_errors.icache
    beu.module.io.errors.dcache := io.beu_errors.dcache
    beu.module.io.errors.uncache := io.beu_errors.uncache
    resetDelayN.io.in := io.reset_vector.fromTile
    io.reset_vector.toCore := resetDelayN.io.out
    io.hartId.toCore := io.hartId.fromTile
    // add buffer to satisfy PE
    io.msiInfo.toCore.valid := RegNext(io.msiInfo.fromTile.valid)
    io.msiInfo.toCore.bits := RegEnable(io.msiInfo.fromTile.bits, io.msiInfo.fromTile.valid)
    io.teemsiInfo.foreach { teemsiInfo =>
      teemsiInfo.toCore.valid := RegNext(teemsiInfo.fromTile.valid)
      teemsiInfo.toCore.bits := RegEnable(teemsiInfo.fromTile.bits, teemsiInfo.fromTile.valid)
    }
    io.cpu_wfi.toTile := RegNext(io.cpu_wfi.fromCore)
    io.cpu_critical_error.toTile := RegNext(io.cpu_critical_error.fromCore)
    io.msiAck.toTile := io.msiAck.fromCore
    io.teemsiAck.foreach( teemsiAck => teemsiAck.toTile := teemsiAck.fromCore)
    io.l3Miss.toCore := RegNext(io.l3Miss.fromTile)
    io.clintTime.toCore := DelayNWithValid(io.clintTime.fromTile, 1)
    // trace interface
    val traceToTile = io.traceCoreInterface.toTile
    val traceFromCore = io.traceCoreInterface.fromCore
    traceFromCore.fromEncoder := RegNext(traceToTile.fromEncoder)
    traceToTile.toEncoder.trap := RegEnable(
      traceFromCore.toEncoder.trap,
      traceFromCore.toEncoder.groups(0).valid && Itype.isTrap(traceFromCore.toEncoder.groups(0).bits.itype)
    )
    traceToTile.toEncoder.priv := RegEnable(
      traceFromCore.toEncoder.priv,
      traceFromCore.toEncoder.groups(0).valid
    )
    traceToTile.toEncoder.mstatus := RegNext(traceFromCore.toEncoder.mstatus)
    (0 until TraceGroupNum).foreach{ i =>
      traceToTile.toEncoder.groups(i).valid := RegNext(traceFromCore.toEncoder.groups(i).valid)
      traceToTile.toEncoder.groups(i).bits.iretire := RegNext(traceFromCore.toEncoder.groups(i).bits.iretire)
      traceToTile.toEncoder.groups(i).bits.itype := RegNext(traceFromCore.toEncoder.groups(i).bits.itype)
      traceToTile.toEncoder.groups(i).bits.ilastsize := RegEnable(
        traceFromCore.toEncoder.groups(i).bits.ilastsize,
        traceFromCore.toEncoder.groups(i).valid
      )
      traceToTile.toEncoder.groups(i).bits.iaddr := RegEnable(
        traceFromCore.toEncoder.groups(i).bits.iaddr,
        traceFromCore.toEncoder.groups(i).valid
      )
    }

    dontTouch(io.hartId)
    dontTouch(io.cpu_wfi)
    dontTouch(io.cpu_critical_error)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }
    dontTouch(io.dcache_cchi)
    dontTouch(io.icache_cchi)
    dontTouch(io.ptw_cchi)

    val hartIsInReset = RegInit(true.B)
    hartIsInReset := io.hartIsInReset.resetInFrontend
    io.hartIsInReset.toTile := hartIsInReset

    io.debugTopDown.l2MissMatch := false.B
    io.l2Miss := false.B
    io.l2_flush_done.foreach(_ := false.B)
    io.perfEvents := DontCare
    beu.module.io.errors.l2 := 0.U.asTypeOf(beu.module.io.errors.l2)
    io.decoupledCHI.foreach(_ := DontCare)

    // Reserved L2 prefetch / L1-hint ports (CoupledL2 mapping).
    // Oceanus L2 has no prefetcher yet; keep the L1-facing mapping and do not
    // connect these wires to oceanusL2.
    val l2Pf = Wire(new Bundle {
      val pfCtrlFromCore = new PrefetchCtrlFromCore
      val l2_hint = chiselTypeOf(io.l2_hint)
      val l2_fdbk_pf_ctrl = new L2ToL1PfCtrl
      val l2_tlb_req = chiselTypeOf(io.l2_tlb_req)
      val l2_pmp_resp = chiselTypeOf(io.l2_pmp_resp)
    })
    l2Pf.pfCtrlFromCore := io.pfCtrlFromCore
    io.l2_hint := l2Pf.l2_hint
    io.l2_fdbk_pf_ctrl := l2Pf.l2_fdbk_pf_ctrl

    /* l2 tlb */
    io.l2_tlb_req.req.bits := DontCare
    io.l2_tlb_req.req.valid := l2Pf.l2_tlb_req.req.valid
    io.l2_tlb_req.resp.ready := l2Pf.l2_tlb_req.resp.ready
    io.l2_tlb_req.req.bits.vaddr := l2Pf.l2_tlb_req.req.bits.vaddr
    io.l2_tlb_req.req.bits.cmd := l2Pf.l2_tlb_req.req.bits.cmd
    io.l2_tlb_req.req.bits.size := l2Pf.l2_tlb_req.req.bits.size
    io.l2_tlb_req.req.bits.kill := l2Pf.l2_tlb_req.req.bits.kill
    io.l2_tlb_req.req.bits.isPrefetch := l2Pf.l2_tlb_req.req.bits.isPrefetch
    io.l2_tlb_req.req.bits.no_translate := l2Pf.l2_tlb_req.req.bits.no_translate
    io.l2_tlb_req.req_kill := l2Pf.l2_tlb_req.req_kill
    l2Pf.l2_tlb_req.resp.valid := io.l2_tlb_req.resp.valid
    l2Pf.l2_tlb_req.req.ready := io.l2_tlb_req.req.ready
    l2Pf.l2_tlb_req.resp.bits := io.l2_tlb_req.resp.bits
    l2Pf.l2_pmp_resp := io.l2_pmp_resp

    // Idle L2-side drivers until oceanus prefetch is implemented.
    l2Pf.l2_hint := 0.U.asTypeOf(l2Pf.l2_hint)
    l2Pf.l2_fdbk_pf_ctrl := L2ToL1PfCtrl.default()
    l2Pf.l2_tlb_req.req.valid := false.B
    l2Pf.l2_tlb_req.req.bits := DontCare
    l2Pf.l2_tlb_req.req_kill := false.B
    l2Pf.l2_tlb_req.resp.ready := true.B
    dontTouch(l2Pf)

    pf_recv_node.foreach { n => dontTouch(n.in.head._1) }
    l3_pf_recv_node.foreach { n => dontTouch(n.in.head._1) }

    if (enableL2) {
      require(cchiUpstream.type1.size == numMemChannelsFromDcache)
      require(cchiUpstream.type4.size >= 2)

      val l2cfg = new L2Configuration(
        nodeId = coreParams.HartId,
        eSAM = true,
        slices = 0 until coreParams.L2NBanks,
        upstream = cchiUpstream
      )
      val oceanusL2 = Module(new L2Top(l2cfg))

      require(oceanusL2.io.t1p.size == io.dcache_cchi.size)
      oceanusL2.io.t1p.zip(io.dcache_cchi).foreach { case (t1, l1) => CCHIConnect.type1(t1, l1) }
      CCHIConnect.type4(oceanusL2.io.t4p(0), io.icache_cchi)
      CCHIConnect.type4(oceanusL2.io.t4p(1), io.ptw_cchi)

      if (isOpenLLC) {
        connectOceanusChi(oceanusL2.io.chi, io.chi.get)
      }

      // Collect oceanus L2 SRAM DFT ports (CoupledL2 used to do this).
      val sigFromSrams = Option.when(hasDFT)(SramHelper.genBroadCastBundleTop())
      val cg = Option.when(hasMbist)(ClockGate.genTeSrc)
      if (hasMbist) {
        cg.get.cgen := io.dft.get.cgen
      }
      sigFromSrams.foreach(_ := DontCare)
      sigFromSrams.zip(io.dft).foreach {
        case (sig, dft) =>
          if (hasMbist) {
            sig.ram_hold := dft.ram_hold
            sig.ram_bypass := dft.ram_bypass
            sig.ram_bp_clken := dft.ram_bp_clken
            sig.ram_aux_clk := dft.ram_aux_clk
            sig.ram_aux_ckbp := dft.ram_aux_ckbp
            sig.ram_mcp_hold := dft.ram_mcp_hold
            sig.cgen := dft.cgen
          }
          if (hasSramCtl) {
            sig.ram_ctl := dft.ram_ctl
          }
      }
      val mbistPl = MbistPipeline.PlaceMbistPipeline(Int.MaxValue, "L2Cache", hasMbist)
      if (hasMbist) {
        val intf = Module(new MbistInterface(
          params = Seq(mbistPl.get.nodeParams),
          ids = Seq(mbistPl.get.childrenIds),
          name = "MbistIntfL2",
          pipelineNum = 1
        ))
        intf.toPipeline.head <> mbistPl.get.mbist
        if (coreParams.HartId == 0) mbistPl.get.registerCSV(intf.info, "MbistL2")
        intf.mbist := DontCare
        dontTouch(intf.mbist)
      }
    } else {
      io.chi.foreach(_ := DontCare)
      io.dcache_cchi.foreach { p =>
        p.upEVT.ready := true.B
        p.upREQ.ready := true.B
        p.upRSP.ready := true.B
        p.upDAT.ready := true.B
        p.dnSNP.valid := false.B
        p.dnSNP.bits := DontCare
        p.dnRSP.valid := false.B
        p.dnRSP.bits := DontCare
        p.dnDAT.valid := false.B
        p.dnDAT.bits := DontCare
      }
      Seq(io.icache_cchi, io.ptw_cchi).foreach { p =>
        p.upREQ.ready := true.B
        p.dnDAT.valid := false.B
        p.dnDAT.bits := DontCare
      }
    }

    def connectOceanusChi(l2chi: oceanus.chi.intf.CHIRNFInterface, rn: PortIO): Unit = {
      val adpTxReq = Wire(ChannelIO(new CHIBundleREQ))
      OceanusChannelAdapter.connectRX(l2chi.txreq, adpTxReq, EnumCHIChannel.REQ)
      rn.tx.req.flitpend := adpTxReq.flitpend
      rn.tx.req.flitv := adpTxReq.flitv
      rn.tx.req.flit := adpTxReq.flit
      adpTxReq.lcrdv := rn.tx.req.lcrdv

      val adpTxRsp = Wire(ChannelIO(new CHIBundleRSP))
      OceanusChannelAdapter.connectRX(l2chi.txrsp, adpTxRsp, EnumCHIChannel.RSP)
      rn.tx.rsp.flitpend := adpTxRsp.flitpend
      rn.tx.rsp.flitv := adpTxRsp.flitv
      rn.tx.rsp.flit := adpTxRsp.flit
      adpTxRsp.lcrdv := rn.tx.rsp.lcrdv

      val adpTxDat = Wire(ChannelIO(new CHIBundleDAT))
      OceanusChannelAdapter.connectRX(l2chi.txdat, adpTxDat, EnumCHIChannel.DAT)
      rn.tx.dat.flitpend := adpTxDat.flitpend
      rn.tx.dat.flitv := adpTxDat.flitv
      rn.tx.dat.flit := adpTxDat.flit
      adpTxDat.lcrdv := rn.tx.dat.lcrdv

      val adpRxSnp = Wire(ChannelIO(new CHIBundleSNP))
      adpRxSnp.flitpend := rn.rx.snp.flitpend
      adpRxSnp.flitv := rn.rx.snp.flitv
      adpRxSnp.flit := rn.rx.snp.flit
      rn.rx.snp.lcrdv := adpRxSnp.lcrdv
      OceanusChannelAdapter.connectTX(adpRxSnp, l2chi.rxsnp, EnumCHIChannel.SNP)

      val adpRxRsp = Wire(ChannelIO(new CHIBundleRSP))
      adpRxRsp.flitpend := rn.rx.rsp.flitpend
      adpRxRsp.flitv := rn.rx.rsp.flitv
      adpRxRsp.flit := rn.rx.rsp.flit
      rn.rx.rsp.lcrdv := adpRxRsp.lcrdv
      OceanusChannelAdapter.connectTX(adpRxRsp, l2chi.rxrsp, EnumCHIChannel.RSP)

      val adpRxDat = Wire(ChannelIO(new CHIBundleDAT))
      adpRxDat.flitpend := rn.rx.dat.flitpend
      adpRxDat.flitv := rn.rx.dat.flitv
      adpRxDat.flit := rn.rx.dat.flit
      rn.rx.dat.lcrdv := adpRxDat.lcrdv
      OceanusChannelAdapter.connectTX(adpRxDat, l2chi.rxdat, EnumCHIChannel.DAT)

      rn.tx.linkactivereq := l2chi.txlinkactivereq
      l2chi.txlinkactiveack := rn.tx.linkactiveack
      l2chi.rxlinkactivereq := rn.rx.linkactivereq
      rn.rx.linkactiveack := l2chi.rxlinkactiveack
      rn.txsactive := l2chi.txsactive
      l2chi.rxsactive := rn.rxsactive
      rn.syscoreq := l2chi.syscoreq
      l2chi.syscoack := rn.syscoack
    }
  }

  lazy val module = new Imp(this)
}

class L2TopWrapper()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {

  override def shouldBeInlined: Boolean = false

  val inner = LazyModule(new L2TopWrapperInlined())

  class Imp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(inner.module.io.cloneType)
    val reset_core = IO(Output(Reset()))
    io <> inner.module.io

    if (debugOpts.ResetGen) {
      ResetGen(ResetGenNode(Seq(
        CellNode(reset_core),
        ModuleNode(inner.module)
      )), reset, sim = false, io.dft_reset)
    } else {
      reset_core := DontCare
    }
  }

  lazy val module = new Imp(this)
}
