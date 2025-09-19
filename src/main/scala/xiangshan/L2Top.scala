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
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors, MaxHartIdBits}
import freechips.rocketchip.tilelink._
import coupledL2.{EnableCHI, L2ParamKey, PrefetchCtrlFromCore}
import coupledL2.tl2tl.TL2TLCoupledL2
import coupledL2.tl2chi.{CHIIssue, PortIO, TL2CHICoupledL2, CHIAddrWidthKey, NonSecureKey}
import huancun.BankBitsKey
import system.HasSoCParameter
import top.BusPerfMonitor
import utility._
import utility.sram.SramBroadcastBundle
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.trace.{Itype, TraceCoreInterface}

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
  val mmio_xbar = TLXbar()
  val mmio_port = TLIdentityNode() // to L3
  val memory_port = if (enableCHI && enableL2) None else Some(TLIdentityNode())
  val beu = LazyModule(new BusErrorUnit(
    new XSL1BusErrors(),
    BusErrorUnitParams(soc.BEURange.base, soc.BEURange.mask.toInt + 1)
  ))

  val i_mmio_port = TLTempNode()
  val d_mmio_port = TLTempNode()
  val icachectrl_port_opt = Option.when(icacheParameters.cacheCtrlAddressOpt.nonEmpty)(TLTempNode())
  val sep_tl_port_opt = Option.when(SeperateTLBus)(TLTempNode())

  val misc_l2_pmu = BusPerfMonitor(name = "Misc_L2", enable = !debugOpts.FPGAPlatform) // l1D & l1I & PTW
  val l2_l3_pmu = BusPerfMonitor(name = "L2_L3", enable = !debugOpts.FPGAPlatform && !enableCHI, stat_latency = true)
  val xbar_l2_buffer = TLBuffer()

  val enbale_tllog = !debugOpts.FPGAPlatform && debugOpts.AlwaysBasicDB
  val l1d_logger = TLLogger(s"L2_L1D_${coreParams.HartId}", enbale_tllog)
  val l1i_logger = TLLogger(s"L2_L1I_${coreParams.HartId}", enbale_tllog)
  val ptw_logger = TLLogger(s"L2_PTW_${coreParams.HartId}", enbale_tllog)
  val ptw_to_l2_buffer = LazyModule(new TLBuffer)
  val i_mmio_buffer = LazyModule(new TLBuffer)

  val clint_int_node = IntIdentityNode()
  val debug_int_node = IntIdentityNode()
  val plic_int_node = IntIdentityNode()
  val nmi_int_node = IntIdentityNode()
  val beu_local_int_source = IntSourceNode(IntSourcePortSimple())

  println(s"enableCHI: ${enableCHI}")
  val l2cache = if (enableL2) {
    val config = new Config((_, _, _) => {
      case L2ParamKey => coreParams.L2CacheParamsOpt.get.copy(
        hartId = p(XSCoreParamsKey).HartId,
        FPGAPlatform = debugOpts.FPGAPlatform,
        hasMbist = hasMbist,
        EnablePrivateClint = SeperateTLBus
      )
      case EnableCHI => p(EnableCHI)
      case CHIIssue => p(CHIIssue)
      case CHIAddrWidthKey => p(CHIAddrWidthKey)
      case NonSecureKey => p(NonSecureKey)
      case BankBitsKey => log2Ceil(coreParams.L2NBanks)
      case MaxHartIdBits => p(MaxHartIdBits)
      case LogUtilsOptionsKey => p(LogUtilsOptionsKey)
      case PerfCounterOptionsKey => p(PerfCounterOptionsKey)
    })
    if (enableCHI) Some(LazyModule(new TL2CHICoupledL2()(new Config(config))))
    else Some(LazyModule(new TL2TLCoupledL2()(new Config(config))))
  } else None
  val l2_binder = coreParams.L2CacheParamsOpt.map(_ => BankBinder(coreParams.L2NBanks, 64))

  // =========== Connection ============
  // l2 to l2_binder, then to memory_port
  l2cache match {
    case Some(l2) =>
      l2_binder.get :*= l2.node :*= xbar_l2_buffer :*= l1_xbar :=* misc_l2_pmu
      l2 match {
        case l2: TL2TLCoupledL2 =>
          memory_port.get := l2_l3_pmu := TLClientsMerger() := TLXbar() :=* l2_binder.get
        case l2: TL2CHICoupledL2 =>
          l2.managerNode := TLXbar() :=* l2_binder.get
          l2.mmioNode := mmio_port
      }
    case None =>
      memory_port.get := l1_xbar
  }

  mmio_xbar := TLBuffer.chainNode(2) := i_mmio_port
  mmio_xbar := TLBuffer.chainNode(2) := d_mmio_port
  beu.node := TLBuffer.chainNode(1) := mmio_xbar
  if (icacheParameters.cacheCtrlAddressOpt.nonEmpty) {
    icachectrl_port_opt.get := TLBuffer.chainNode(1) := mmio_xbar
  }
  if (SeperateTLBus) {
    sep_tl_port_opt.get := TLBuffer.chainNode(1) := mmio_xbar
  }

  // filter out in-core addresses before sent to mmio_port
  // Option[AddressSet] ++ Option[AddressSet] => List[AddressSet]
  private def cacheAddressSet: Seq[AddressSet] = (icacheParameters.cacheCtrlAddressOpt ++ dcacheParameters.cacheCtrlAddressOpt).toSeq
  private def mmioFilters = if(SeperateTLBus) (SeperateTLBusRanges ++ cacheAddressSet) else cacheAddressSet
  mmio_port :=
    TLFilter(TLFilter.mSubtract(mmioFilters)) :=
    TLBuffer() :=
    mmio_xbar

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
      val cpu_halt = new Bundle() {
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
      val chi = if (enableCHI) Some(new PortIO) else None
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val pfCtrlFromCore = Input(new PrefetchCtrlFromCore)
      val l2_tlb_req = new TlbRequestIO(nRespDups = 2)
      val l2_pmp_resp = Flipped(new PMPRespBundle)
      val l2_hint = ValidIO(new L2ToL1Hint())
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
    io.msiInfo.toCore := io.msiInfo.fromTile
    io.cpu_halt.toTile := io.cpu_halt.fromCore
    io.cpu_critical_error.toTile := io.cpu_critical_error.fromCore
    io.msiAck.toTile := io.msiAck.fromCore
    io.l3Miss.toCore := io.l3Miss.fromTile
    io.clintTime.toCore := io.clintTime.fromTile
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
    dontTouch(io.cpu_halt)
    dontTouch(io.cpu_critical_error)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }

    val hartIsInReset = RegInit(true.B)
    hartIsInReset := io.hartIsInReset.resetInFrontend
    io.hartIsInReset.toTile := hartIsInReset

    if (l2cache.isDefined) {
      val l2 = l2cache.get.module

      l2.io.pfCtrlFromCore := io.pfCtrlFromCore
      l2.io.dft.zip(io.dft).foreach({ case(a, b) => a := b })
      l2.io.dft_reset.zip(io.dft_reset).foreach({ case(a, b) => a := b })
      io.l2_hint := l2.io.l2_hint
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
        case l2cache: TL2CHICoupledL2 =>
          val l2 = l2cache.module
          l2.io_nodeID := io.nodeID.get
          io.chi.get <> l2.io_chi
          l2.io_cpu_halt.foreach { _:= io.cpu_halt.fromCore }
        case l2cache: TL2TLCoupledL2 =>
      }

      beu.module.io.errors.l2.ecc_error.valid := l2.io.error.valid
      beu.module.io.errors.l2.ecc_error.bits := l2.io.error.address
    } else {
      io.l2_hint := 0.U.asTypeOf(io.l2_hint)
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
