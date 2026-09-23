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
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnitParams, BusErrors, MaxHartIdBits}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import xscache.coupledL2.{
  CoupledL2, EnableL2DecoupledDownstreamCHI, L2ParamKey, L2ToL1PfCtrl,
  MemBackTypeMMField, MemPageTypeNCField, PrefetchCtrlFromCore
}
import xscache.chi.{CHIDataCheckKey, CHIIssue, CHIAddrWidthKey, CHIPoisonKey, DecoupledPortIO, NonSecureKey, PortIO}
import xscache.oceanus.compactchi.CCHIParametersKey
import xscache.common.BankBitsKey
import system.HasSoCParameter
import top.BusPerfMonitor
import utility._
import utility.sram.SramBroadcastBundle
import xiangshan.cache.{CCHIType1Port, CCHIType4Port}
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
  *   L2Top contains everything between Core and XSTile-IO
  */
class L2TopInlined()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = true

  def chainBuffer(depth: Int, n: String): (Seq[LazyModule], TLNode) = {
    val buffers = Seq.fill(depth){ LazyModule(new TLBuffer()) }
    buffers.zipWithIndex.foreach{ case (b, i) => {
      b.suggestName(s"${n}_${i}")
    }}
    val node = buffers.map(_.node.asInstanceOf[TLNode]).reduce(_ :*=* _)
    (buffers, node)
  }
  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Components ============
  val l1_xbar = TLXbar()
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

  val misc_l2_pmu = BusPerfMonitor(name = "Misc_L2", enable = !debugOpts.FPGAPlatform) // l1D & l1I & PTW
  val xbar_l2_buffer = TLBuffer()

  val enbale_tllog = !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB
  val l1d_logger = Seq.tabulate(numMemChannelsFromDcache)(i =>
    TLLogger(s"L2_L1D_${coreParams.HartId}_ch$i", enbale_tllog)
  )
  val l1i_logger = TLLogger(s"L2_L1I_${coreParams.HartId}", enbale_tllog)
  val ptw_logger = TLLogger(s"L2_PTW_${coreParams.HartId}", enbale_tllog)
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val i_mmio_buffer = LazyModule(new AXI4Buffer())

  val clint_int_node = IntIdentityNode()
  val debug_int_node = IntIdentityNode()
  val plic_int_node = IntIdentityNode()
  val nmi_int_node = IntIdentityNode()
  val beu_local_int_source = IntSourceNode(IntSourcePortSimple())
  val beu_local_int_source_buffer = IntBuffer()

  val l2cache = if (enableL2) {
    val sliceCoherentClientMap =
      if (coreParams.dcacheParametersOpt.exists(p => p.numMemChannels == 2 && p.channelSelByAddr) &&
          coreParams.L2NBanks % 2 == 0) {
        Some(Seq.tabulate(coreParams.L2NBanks)(i => i % 2))
      } else {
        None
      }
    val config = new Config((_, _, _) => {
      case L2ParamKey => coreParams.L2CacheParamsOpt.get.copy(
        hartId = p(XSCoreParamsKey).HartId,
        FPGAPlatform = debugOpts.FPGAPlatform,
        hasMbist = hasMbist,
        PrivateClintRange = if(UsePrivateClint) Some(TIMERRange) else None,
        sliceCoherentClientMap = sliceCoherentClientMap
      )
      case CHIIssue => p(CHIIssue)
      case CHIAddrWidthKey => p(CHIAddrWidthKey)
      case CCHIParametersKey => p(CCHIParametersKey)
      case NonSecureKey => p(NonSecureKey)
      case CHIDataCheckKey if isZhuJiang => "none"
      case CHIPoisonKey if isZhuJiang => false
      case EnableL2DecoupledDownstreamCHI => isZhuJiang
      case BankBitsKey => log2Ceil(coreParams.L2NBanks)
      case MaxHartIdBits => p(MaxHartIdBits)
      case LogUtilsOptionsKey => p(LogUtilsOptionsKey)
      case PerfCounterOptionsKey => p(PerfCounterOptionsKey)
    })
    Some(LazyModule(new CoupledL2()(new Config(config))))
  } else None
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  // =========== Connection ============
  // l2 to l2_binder, then to memory_port
  val l2MmioStubOpt = l2cache.map { l2 =>
    val stub = TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "l2_mmio_stub",
        sourceId = IdRange(0, 1)
      )),
      requestFields = Seq(MemBackTypeMMField(), MemPageTypeNCField())
    )))
    l2.mmioNode := stub
    stub
  }

  l2cache match {
    case Some(l2) =>
      l2_binder.get :*= l2.node :*= xbar_l2_buffer :*= l1_xbar :=* misc_l2_pmu
      l2.managerNode := TLXbar() :=* l2_binder.get
    case None =>
      memory_port.get := l1_xbar
  }

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
    AXI4PMAUserAdapter() :=
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
      // Compact CHI from L1; not wired to CoupledL2 yet
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

    l2MmioStubOpt.foreach { stubNode =>
      val (stub, _) = stubNode.out.head
      stub.a.valid := false.B
      stub.a.bits := DontCare
      stub.d.ready := true.B
      stub.b.ready := true.B
      stub.c.valid := false.B
      stub.c.bits := DontCare
      stub.e.valid := false.B
      stub.e.bits := DontCare
    }

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

    // Drain L1 CCHI until CoupledL2 Compact CHI is connected.
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

    val hartIsInReset = RegInit(true.B)
    hartIsInReset := io.hartIsInReset.resetInFrontend
    io.hartIsInReset.toTile := hartIsInReset

    if (l2cache.isDefined) {
      val l2 = l2cache.get.module

      l2.io.pfCtrlFromCore := io.pfCtrlFromCore
      l2.io.dft.zip(io.dft).foreach({ case(a, b) => a := b })
      l2.io.dft_reset.zip(io.dft_reset).foreach({ case(a, b) => a := b })
      io.l2_hint := l2.io.l2_hint
      io.l2_fdbk_pf_ctrl := l2.io.l2_fdbk_pf_ctrl
      l2.io.debugTopDown.robHeadPaddr := DontCare
      l2.io.hartId := io.hartId.fromTile
      l2.io.debugTopDown.robHeadPaddr := io.debugTopDown.robHeadPaddr
      l2.io.debugTopDown.robTrueCommit := io.debugTopDown.robTrueCommit
      io.debugTopDown.l2MissMatch := l2.io.debugTopDown.l2MissMatch
      io.l2Miss := l2.io.l2Miss
      io.l2_flush_done.foreach { _ := l2.io.l2FlushDone.getOrElse(false.B) }
      l2.io.l2Flush.foreach { _ := io.l2_flush_en.getOrElse(false.B) }

      /* l2 tlb */
      io.l2_tlb_req.req.bits := DontCare
      io.l2_tlb_req.req.valid := l2.io.l2_tlb_req.req.valid
      io.l2_tlb_req.resp.ready := l2.io.l2_tlb_req.resp.ready
      io.l2_tlb_req.req.bits.vaddr := l2.io.l2_tlb_req.req.bits.vaddr
      io.l2_tlb_req.req.bits.cmd := l2.io.l2_tlb_req.req.bits.cmd
      io.l2_tlb_req.req.bits.size := l2.io.l2_tlb_req.req.bits.size
      io.l2_tlb_req.req.bits.kill := l2.io.l2_tlb_req.req.bits.kill
      io.l2_tlb_req.req.bits.isPrefetch := l2.io.l2_tlb_req.req.bits.isPrefetch
      io.l2_tlb_req.req.bits.no_translate := l2.io.l2_tlb_req.req.bits.no_translate
      io.l2_tlb_req.req_kill := l2.io.l2_tlb_req.req_kill
      io.perfEvents := l2.io_perf

      val allPerfEvents = l2.getPerfEvents
      if (printEventCoding) {
        for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
          println("L2 Cache perfEvents Set", name, inc, i)
        }
      }

      l2.io.l2_tlb_req.resp.valid := io.l2_tlb_req.resp.valid
      l2.io.l2_tlb_req.req.ready := io.l2_tlb_req.req.ready
      l2.io.l2_tlb_req.resp.bits.paddr.head := io.l2_tlb_req.resp.bits.paddr.head
      l2.io.l2_tlb_req.resp.bits.pbmt := io.l2_tlb_req.resp.bits.pbmt.head
      l2.io.l2_tlb_req.resp.bits.miss := io.l2_tlb_req.resp.bits.miss
      l2.io.l2_tlb_req.resp.bits.excp.head.gpf := io.l2_tlb_req.resp.bits.excp.head.gpf
      l2.io.l2_tlb_req.resp.bits.excp.head.pf := io.l2_tlb_req.resp.bits.excp.head.pf
      l2.io.l2_tlb_req.resp.bits.excp.head.af := io.l2_tlb_req.resp.bits.excp.head.af
      l2.io.l2_tlb_req.pmp_resp.ld := io.l2_pmp_resp.ld
      l2.io.l2_tlb_req.pmp_resp.st := io.l2_pmp_resp.st
      l2.io.l2_tlb_req.pmp_resp.instr := io.l2_pmp_resp.instr
      l2.io.l2_tlb_req.pmp_resp.mmio := io.l2_pmp_resp.mmio
      l2.io.l2_tlb_req.pmp_resp.atomic := io.l2_pmp_resp.atomic
      l2cache.get match {
        case l2cache: CoupledL2 =>
          val l2 = l2cache.module
          l2.io.nodeID := io.nodeID.get
          if (isOpenLLC) {
            io.chi.get <> l2.io.lcreditCHI.get
          } else {
            io.decoupledCHI.get <> l2.io.decoupledCHI.get
          }
          l2.io.cpu_wfi.foreach { _ := io.cpu_wfi.fromCore }
      }

      beu.module.io.errors.l2.ecc_error.valid := l2.io.error.valid
      beu.module.io.errors.l2.ecc_error.bits := l2.io.error.address
    } else {
      io.l2_hint := 0.U.asTypeOf(io.l2_hint)
      io.l2_fdbk_pf_ctrl := L2ToL1PfCtrl.default()
      io.debugTopDown <> DontCare
      io.l2Miss := false.B

      io.l2_tlb_req.req.valid := false.B
      io.l2_tlb_req.req.bits := DontCare
      io.l2_tlb_req.req_kill := DontCare
      io.l2_tlb_req.resp.ready := true.B
      io.perfEvents := DontCare

      beu.module.io.errors.l2 := 0.U.asTypeOf(beu.module.io.errors.l2)
    }
  }

  lazy val module = new Imp(this)
}

class L2Top()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter {

  override def shouldBeInlined: Boolean = false

  val inner = LazyModule(new L2TopInlined())

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
